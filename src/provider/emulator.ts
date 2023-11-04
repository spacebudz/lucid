import { C } from "../core/core.ts";
import {
  Address,
  Assets,
  Credential,
  Datum,
  DatumHash,
  Delegation,
  Lovelace,
  OutputData,
  OutRef,
  PoolId,
  ProtocolParameters,
  Provider,
  RewardAddress,
  ScriptHash,
  Transaction,
  TxHash,
  Unit,
  UnixTime,
  UTxO,
} from "../types/types.ts";
import { FreeableBucket } from "../utils/freeable.ts";
import { PROTOCOL_PARAMETERS_DEFAULT } from "../utils/mod.ts";
import {
  coreToUtxo,
  fromHex,
  getAddressDetails,
  toHex,
} from "../utils/utils.ts";

/** Concatentation of txHash + outputIndex */
type FlatOutRef = string;

export class Emulator implements Provider {
  ledger: Record<FlatOutRef, { utxo: UTxO; spent: boolean }>;
  mempool: Record<FlatOutRef, { utxo: UTxO; spent: boolean }> = {};
  /**
   * Only stake key registrations/delegations and rewards are tracked.
   * Other certificates are not tracked.
   */
  chain: Record<
    RewardAddress,
    { registeredStake: boolean; delegation: Delegation }
  > = {};
  blockHeight: number;
  slot: number;
  time: UnixTime;
  protocolParameters: ProtocolParameters;
  datumTable: Record<DatumHash, Datum> = {};

  constructor(
    accounts: {
      address: Address;
      assets: Assets;
      outputData?: OutputData;
    }[],
    protocolParameters: ProtocolParameters = PROTOCOL_PARAMETERS_DEFAULT
  ) {
    const GENESIS_HASH = "00".repeat(32);
    this.blockHeight = 0;
    this.slot = 0;
    this.time = Date.now();
    this.ledger = {};
    accounts.forEach(({ address, assets, outputData }, index) => {
      if (
        [outputData?.hash, outputData?.asHash, outputData?.inline].filter(
          (b) => b
        ).length > 1
      ) {
        throw new Error(
          "Not allowed to set hash, asHash and inline at the same time."
        );
      }

      this.ledger[GENESIS_HASH + index] = {
        utxo: {
          txHash: GENESIS_HASH,
          outputIndex: index,
          address,
          assets,
          datumHash: outputData?.asHash
            ? C.hash_plutus_data(
                C.PlutusData.from_bytes(fromHex(outputData.asHash))
              ).to_hex()
            : outputData?.hash,
          datum: outputData?.inline,
          scriptRef: outputData?.scriptRef,
        },
        spent: false,
      };
    });
    this.protocolParameters = protocolParameters;
  }

  now(): UnixTime {
    return this.time;
  }

  awaitSlot(length = 1) {
    this.slot += length;
    this.time += length * 1000;
    const currentHeight = this.blockHeight;
    this.blockHeight = Math.floor(this.slot / 20);

    if (this.blockHeight > currentHeight) {
      for (const [outRef, { utxo, spent }] of Object.entries(this.mempool)) {
        this.ledger[outRef] = { utxo, spent };
      }

      for (const [outRef, { spent }] of Object.entries(this.ledger)) {
        if (spent) delete this.ledger[outRef];
      }

      this.mempool = {};
    }
  }

  awaitBlock(height = 1) {
    this.blockHeight += height;
    this.slot += height * 20;
    this.time += height * 20 * 1000;

    for (const [outRef, { utxo, spent }] of Object.entries(this.mempool)) {
      this.ledger[outRef] = { utxo, spent };
    }

    for (const [outRef, { spent }] of Object.entries(this.ledger)) {
      if (spent) delete this.ledger[outRef];
    }

    this.mempool = {};
  }

  getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]> {
    const utxos: UTxO[] = Object.values(this.ledger).flatMap(({ utxo }) => {
      if (typeof addressOrCredential === "string") {
        return addressOrCredential === utxo.address ? utxo : [];
      } else {
        const { paymentCredential } = getAddressDetails(utxo.address);
        return paymentCredential?.hash === addressOrCredential.hash ? utxo : [];
      }
    });

    return Promise.resolve(utxos);
  }

  getProtocolParameters(): Promise<ProtocolParameters> {
    return Promise.resolve(this.protocolParameters);
  }

  getDatum(datumHash: DatumHash): Promise<Datum> {
    return Promise.resolve(this.datumTable[datumHash]);
  }

  getUtxosWithUnit(
    addressOrCredential: Address | Credential,
    unit: Unit
  ): Promise<UTxO[]> {
    const utxos: UTxO[] = Object.values(this.ledger).flatMap(({ utxo }) => {
      if (typeof addressOrCredential === "string") {
        return addressOrCredential === utxo.address && utxo.assets[unit] > 0n
          ? utxo
          : [];
      } else {
        const { paymentCredential } = getAddressDetails(utxo.address);
        return paymentCredential?.hash === addressOrCredential.hash &&
          utxo.assets[unit] > 0n
          ? utxo
          : [];
      }
    });

    return Promise.resolve(utxos);
  }

  getUtxosByOutRef(outRefs: OutRef[]): Promise<UTxO[]> {
    return Promise.resolve(
      outRefs.flatMap(
        (outRef) => this.ledger[outRef.txHash + outRef.outputIndex]?.utxo || []
      )
    );
  }

  getUtxoByUnit(unit: string): Promise<UTxO> {
    const utxos: UTxO[] = Object.values(this.ledger).flatMap(({ utxo }) =>
      utxo.assets[unit] > 0n ? utxo : []
    );

    if (utxos.length > 1) {
      throw new Error("Unit needs to be an NFT or only held by one address.");
    }

    return Promise.resolve(utxos[0]);
  }

  getDelegation(rewardAddress: RewardAddress): Promise<Delegation> {
    return Promise.resolve({
      poolId: this.chain[rewardAddress]?.delegation?.poolId || null,
      rewards: this.chain[rewardAddress]?.delegation?.rewards || 0n,
    });
  }

  awaitTx(txHash: string): Promise<boolean> {
    if (this.mempool[txHash + 0]) {
      this.awaitBlock();
      return Promise.resolve(true);
    }
    return Promise.resolve(true);
  }

  /**
   * Emulates the behaviour of the reward distribution at epoch boundaries.
   * Stake keys need to be registered and delegated like on a real chain in order to receive rewards.
   */
  distributeRewards(rewards: Lovelace) {
    for (const [
      rewardAddress,
      { registeredStake, delegation },
    ] of Object.entries(this.chain)) {
      if (registeredStake && delegation.poolId) {
        this.chain[rewardAddress] = {
          registeredStake,
          delegation: {
            poolId: delegation.poolId,
            rewards: (delegation.rewards += rewards),
          },
        };
      }
    }
    this.awaitBlock();
  }

  submitTx(tx: Transaction): Promise<TxHash> {
    /*
        Checks that are already handled by the transaction builder:
          - Fee calculation
          - Phase 2 evaluation
          - Input value == Output value (including mint value)
          - Min ada requirement
          - Stake key registration deposit amount
          - Collateral

        Checks that need to be done:
          - Verify witnesses
          - Correct count of scripts and vkeys
          - Stake key registration
          - Withdrawals
          - Validity interval
     */

    const bucket: FreeableBucket = [];
    const desTx = C.Transaction.from_bytes(fromHex(tx));
    bucket.push(desTx);

    const body = desTx.body();
    bucket.push(body);
    const witnesses = desTx.witness_set();
    bucket.push(witnesses);
    const datums = witnesses.plutus_data();
    bucket.push(datums);

    const transactionHash = C.hash_transaction(body);
    bucket.push(transactionHash);
    const txHash = transactionHash.to_hex();

    // Validity interval
    // Lower bound is inclusive?
    // Upper bound is inclusive?
    const validityStartInterval = body.validity_start_interval();
    bucket.push(validityStartInterval);
    const lowerBound = validityStartInterval
      ? parseInt(validityStartInterval.to_str())
      : null;
    const ttl = body.ttl();
    bucket.push(ttl);
    const upperBound = ttl ? parseInt(ttl.to_str()) : null;

    if (Number.isInteger(lowerBound) && this.slot < lowerBound!) {
      throw new Error(
        `Lower bound (${lowerBound}) not in slot range (${this.slot}).`
      );
    }

    if (Number.isInteger(upperBound) && this.slot > upperBound!) {
      throw new Error(
        `Upper bound (${upperBound}) not in slot range (${this.slot}).`
      );
    }

    // Datums in witness set
    const datumTable = (() => {
      const table: Record<DatumHash, Datum> = {};
      for (let i = 0; i < (datums?.len() || 0); i++) {
        const datum = datums!.get(i);
        bucket.push(datum);
        const plutusDataHash = C.hash_plutus_data(datum);
        bucket.push(plutusDataHash);
        const datumHash = plutusDataHash.to_hex();
        table[datumHash] = toHex(datum.to_bytes());
      }
      return table;
    })();

    const consumedHashes = new Set();

    // Witness keys
    const keyHashes = (() => {
      const keyHashes = [];
      const vkeys = witnesses.vkeys();
      bucket.push(vkeys);
      for (let i = 0; i < (vkeys?.len() || 0); i++) {
        const witness = vkeys!.get(i);
        bucket.push(witness);
        const vkey = witness.vkey();
        bucket.push(vkey);
        const publicKey = vkey.public_key();
        bucket.push(publicKey);
        const hash = publicKey.hash();
        bucket.push(hash);
        const keyHash = hash.to_hex();

        if (!publicKey.verify(fromHex(txHash), witness.signature())) {
          throw new Error(`Invalid vkey witness. Key hash: ${keyHash}`);
        }
        keyHashes.push(keyHash);
      }
      return keyHashes;
    })();

    // We only need this to verify native scripts. The check happens in the CML.
    const edKeyHashes = C.Ed25519KeyHashes.new();
    bucket.push(edKeyHashes);
    keyHashes.forEach((keyHash) => {
      const ed25519KeyHash = C.Ed25519KeyHash.from_hex(keyHash);
      bucket.push(ed25519KeyHash);
      edKeyHashes.add(ed25519KeyHash);
    });

    const nativeHashes = (() => {
      const scriptHashes = [];
      const nativeScripts = witnesses.native_scripts();
      bucket.push(nativeScripts);
      for (let i = 0; i < (nativeScripts?.len() || 0); i++) {
        const witness = nativeScripts!.get(i);
        bucket.push(witness);
        const hash = witness.hash(C.ScriptHashNamespace.NativeScript);
        bucket.push(hash);
        const scriptHash = hash.to_hex();

        const lBound = Number.isInteger(lowerBound)
          ? C.BigNum.from_str(lowerBound!.toString())
          : undefined;
        bucket.push(lBound);
        const uBound = Number.isInteger(upperBound)
          ? C.BigNum.from_str(upperBound!.toString())
          : undefined;
        bucket.push(uBound);
        if (!witness.verify(lBound, uBound, edKeyHashes)) {
          throw new Error(
            `Invalid native script witness. Script hash: ${scriptHash}`
          );
        }
        const requiredSigners = witness.get_required_signers();
        bucket.push(requiredSigners);
        for (let i = 0; i < requiredSigners.len(); i++) {
          const hash = requiredSigners.get(i);
          bucket.push(hash);
          const keyHash = hash.to_hex();
          consumedHashes.add(keyHash);
        }
        scriptHashes.push(scriptHash);
      }
      return scriptHashes;
    })();

    const nativeHashesOptional: Record<ScriptHash, C.NativeScript> = {};
    const plutusHashesOptional: ScriptHash[] = [];

    const plutusHashes = (() => {
      const scriptHashes = [];
      const plutusScripts = witnesses.plutus_scripts();
      bucket.push(plutusScripts);
      for (let i = 0; i < (plutusScripts?.len() || 0); i++) {
        const script = plutusScripts!.get(i);
        bucket.push(script);
        const hash = script.hash(C.ScriptHashNamespace.PlutusV1);
        bucket.push(hash);
        const scriptHash = hash.to_hex();

        scriptHashes.push(scriptHash);
      }

      const plutusV2Scripts = witnesses.plutus_v2_scripts();
      bucket.push(plutusV2Scripts);
      for (let i = 0; i < (plutusV2Scripts?.len() || 0); i++) {
        const script = plutusV2Scripts!.get(i);
        bucket.push(script);
        const hash = script.hash(C.ScriptHashNamespace.PlutusV2);
        bucket.push(hash);
        const scriptHash = hash.to_hex();

        scriptHashes.push(scriptHash);
      }
      return scriptHashes;
    })();

    const inputs = body.inputs();
    bucket.push(inputs);
    inputs.sort();

    type ResolvedInput = {
      entry: { utxo: UTxO; spent: boolean };
      type: "Ledger" | "Mempool";
    };

    const resolvedInputs: ResolvedInput[] = [];

    // Check existence of inputs and look for script refs.
    for (let i = 0; i < inputs.len(); i++) {
      const input = inputs.get(i);
      bucket.push(input);
      const transactionId = input.transaction_id();
      bucket.push(transactionId);
      const transactionIndex = input.index();
      bucket.push(transactionIndex);

      const outRef = transactionId.to_hex() + transactionIndex.to_str();

      const entryLedger = this.ledger[outRef];

      const { entry, type }: ResolvedInput = !entryLedger
        ? { entry: this.mempool[outRef]!, type: "Mempool" }
        : { entry: entryLedger, type: "Ledger" };

      if (!entry || entry.spent) {
        throw new Error(
          `Could not spend UTxO: ${JSON.stringify({
            txHash: entry?.utxo.txHash,
            outputIndex: entry?.utxo.outputIndex,
          })}\nIt does not exist or was already spent.`
        );
      }

      const scriptRef = entry.utxo.scriptRef;
      if (scriptRef) {
        switch (scriptRef.type) {
          case "Native": {
            const script = C.NativeScript.from_bytes(fromHex(scriptRef.script));
            bucket.push(script);
            const hash = script.hash(C.ScriptHashNamespace.NativeScript);
            bucket.push(hash);

            nativeHashesOptional[hash.to_hex()] = script;
            break;
          }
          case "PlutusV1": {
            const script = C.PlutusScript.from_bytes(fromHex(scriptRef.script));
            bucket.push(script);
            const hash = script.hash(C.ScriptHashNamespace.PlutusV1);
            bucket.push(hash);
            plutusHashesOptional.push(hash.to_hex());
            break;
          }
          case "PlutusV2": {
            const script = C.PlutusScript.from_bytes(fromHex(scriptRef.script));
            bucket.push(script);
            const hash = script.hash(C.ScriptHashNamespace.PlutusV2);
            bucket.push(hash);
            plutusHashesOptional.push(hash.to_hex());
            break;
          }
        }
      }

      if (entry.utxo.datumHash) consumedHashes.add(entry.utxo.datumHash);

      resolvedInputs.push({ entry, type });
    }

    // Check existence of reference inputs and look for script refs.
    const referenceInputs = body.reference_inputs();
    bucket.push(referenceInputs);
    for (let i = 0; i < (referenceInputs?.len() || 0); i++) {
      const input = referenceInputs!.get(i);
      bucket.push(input);

      const inputId = input.transaction_id();
      bucket.push(inputId);
      const inputIndex = input.index();
      bucket.push(inputIndex);

      const outRef = inputId.to_hex() + inputIndex.to_str();

      const entry = this.ledger[outRef] || this.mempool[outRef];

      if (!entry || entry.spent) {
        throw new Error(
          `Could not read UTxO: ${JSON.stringify({
            txHash: entry?.utxo.txHash,
            outputIndex: entry?.utxo.outputIndex,
          })}\nIt does not exist or was already spent.`
        );
      }

      const scriptRef = entry.utxo.scriptRef;
      if (scriptRef) {
        switch (scriptRef.type) {
          case "Native": {
            const script = C.NativeScript.from_bytes(fromHex(scriptRef.script));
            bucket.push(script);
            const hash = script.hash(C.ScriptHashNamespace.NativeScript);
            bucket.push(hash);

            nativeHashesOptional[hash.to_hex()] = script;
            break;
          }
          case "PlutusV1": {
            const script = C.PlutusScript.from_bytes(fromHex(scriptRef.script));
            bucket.push(script);
            const hash = script.hash(C.ScriptHashNamespace.PlutusV1);
            bucket.push(hash);
            plutusHashesOptional.push(hash.to_hex());
            break;
          }
          case "PlutusV2": {
            const script = C.PlutusScript.from_bytes(fromHex(scriptRef.script));
            bucket.push(script);
            const hash = script.hash(C.ScriptHashNamespace.PlutusV2);
            bucket.push(hash);
            plutusHashesOptional.push(hash.to_hex());
            break;
          }
        }
      }

      if (entry.utxo.datumHash) consumedHashes.add(entry.utxo.datumHash);
    }

    type Tag = "Spend" | "Mint" | "Cert" | "Reward";

    const redeemers = (() => {
      const tagMap: Record<number, Tag> = {
        0: "Spend",
        1: "Mint",
        2: "Cert",
        3: "Reward",
      };
      const collected = [];
      const redeemers = witnesses.redeemers();
      bucket.push(redeemers);
      for (let i = 0; i < (redeemers?.len() || 0); i++) {
        const redeemer = redeemers!.get(i);
        bucket.push(redeemer);
        const tag = redeemer.tag();
        bucket.push(tag);

        const redeemerIndex = redeemer.index();
        bucket.push(redeemerIndex);
        collected.push({
          tag: tagMap[tag.kind()],
          index: parseInt(redeemerIndex.to_str()),
        });
      }
      return collected;
    })();

    function checkAndConsumeHash(
      credential: Credential,
      tag: Tag | null,
      index: number | null
    ) {
      switch (credential.type) {
        case "Key": {
          if (!keyHashes.includes(credential.hash)) {
            throw new Error(
              `Missing vkey witness. Key hash: ${credential.hash}`
            );
          }
          consumedHashes.add(credential.hash);
          break;
        }
        case "Script": {
          if (nativeHashes.includes(credential.hash)) {
            consumedHashes.add(credential.hash);
            break;
          } else if (nativeHashesOptional[credential.hash]) {
            const lBound = Number.isInteger(lowerBound)
              ? C.BigNum.from_str(lowerBound!.toString())
              : undefined;
            bucket.push(lBound);
            const uBound = Number.isInteger(upperBound)
              ? C.BigNum.from_str(upperBound!.toString())
              : undefined;
            bucket.push(uBound);

            if (
              !nativeHashesOptional[credential.hash].verify(
                lBound,
                uBound,
                edKeyHashes
              )
            ) {
              throw new Error(
                `Invalid native script witness. Script hash: ${credential.hash}`
              );
            }
            break;
          } else if (
            plutusHashes.includes(credential.hash) ||
            plutusHashesOptional.includes(credential.hash)
          ) {
            if (
              redeemers.find(
                (redeemer) => redeemer.tag === tag && redeemer.index === index
              )
            ) {
              consumedHashes.add(credential.hash);
              break;
            }
          }
          throw new Error(
            `Missing script witness. Script hash: ${credential.hash}`
          );
        }
      }
    }

    // Check collateral inputs
    const collateral = body.collateral();
    bucket.push(collateral);
    for (let i = 0; i < (collateral?.len() || 0); i++) {
      const input = collateral!.get(i);
      bucket.push(input);

      const transactionId = input.transaction_id();
      bucket.push(transactionId);
      const transactionIndex = input.index();
      bucket.push(transactionIndex);

      const outRef = transactionId.to_hex() + transactionIndex.to_str();

      const entry = this.ledger[outRef] || this.mempool[outRef];

      if (!entry || entry.spent) {
        throw new Error(
          `Could not read UTxO: ${JSON.stringify({
            txHash: entry?.utxo.txHash,
            outputIndex: entry?.utxo.outputIndex,
          })}\nIt does not exist or was already spent.`
        );
      }

      const { paymentCredential } = getAddressDetails(entry.utxo.address);
      if (paymentCredential?.type === "Script") {
        throw new Error("Collateral inputs can only contain vkeys.");
      }
      checkAndConsumeHash(paymentCredential!, null, null);
    }

    // Check required signers
    const requiredSigners = body.required_signers();
    bucket.push(requiredSigners);
    for (let i = 0; i < (requiredSigners?.len() || 0); i++) {
      const signer = requiredSigners!.get(i);
      bucket.push(signer);
      checkAndConsumeHash({ type: "Key", hash: signer.to_hex() }, null, null);
    }

    // Check mint witnesses
    const mint = body.mint();
    bucket.push(mint);
    const mintKeys = mint?.keys();
    bucket.push(mintKeys);
    for (let index = 0; index < (mintKeys?.len() || 0); index++) {
      const policy = mintKeys!.get(index);
      bucket.push(policy);
      const hash = policy.to_hex();
      checkAndConsumeHash({ type: "Script", hash }, "Mint", index);
    }

    // Check withdrawal witnesses

    const withdrawalRequests: {
      rewardAddress: RewardAddress;
      withdrawal: Lovelace;
    }[] = [];

    const withdrawals = body.withdrawals();
    const withdrawalKeys = withdrawals?.keys();
    for (let index = 0; index < (withdrawalKeys?.len() || 0); index++) {
      const rawAddress = withdrawalKeys!.get(index);
      bucket.push(rawAddress);
      const cWithdrawal = withdrawals!.get(rawAddress);
      bucket.push(cWithdrawal);
      const withdrawal: Lovelace = BigInt(cWithdrawal!.to_str());
      const cAddress = rawAddress.to_address();
      bucket.push(cAddress);
      const rewardAddress = cAddress.to_bech32(undefined);
      const { stakeCredential } = getAddressDetails(rewardAddress);
      checkAndConsumeHash(stakeCredential!, "Reward", index);
      if (this.chain[rewardAddress]?.delegation.rewards !== withdrawal) {
        throw new Error(
          "Withdrawal amount doesn't match actual reward balance."
        );
      }
      withdrawalRequests.push({ rewardAddress, withdrawal });
    }

    // Check cert witnesses

    const certRequests: {
      type: "Registration" | "Deregistration" | "Delegation";
      rewardAddress: RewardAddress;
      poolId?: PoolId;
    }[] = [];

    const certs = body.certs();
    bucket.push(certs);
    for (let index = 0; index < (certs?.len() || 0); index++) {
      /*
        Checking only:
        1. Stake registration
        2. Stake deregistration
        3. Stake delegation

        All other certificate types are not checked and considered valid.
      */
      const cert = certs!.get(index);
      bucket.push(cert);
      switch (cert.kind()) {
        case 0: {
          const registration = cert.as_stake_registration()!;
          bucket.push(registration);
          const networkInfo = C.NetworkInfo.testnet();
          bucket.push(networkInfo);
          const stakeCredential = registration.stake_credential();
          bucket.push(stakeCredential);
          const cRewardAddress = C.RewardAddress.new(
            networkInfo.network_id(),
            stakeCredential
          );
          bucket.push(cRewardAddress);
          const address = cRewardAddress.to_address();
          bucket.push(address);
          const rewardAddress = address.to_bech32(undefined);
          if (this.chain[rewardAddress]?.registeredStake) {
            throw new Error(
              `Stake key is already registered. Reward address: ${rewardAddress}`
            );
          }
          certRequests.push({ type: "Registration", rewardAddress });
          break;
        }
        case 1: {
          const deregistration = cert.as_stake_deregistration()!;
          bucket.push(deregistration);
          const networkInfo = C.NetworkInfo.testnet();
          bucket.push(networkInfo);
          const cStakeCredential = deregistration.stake_credential();
          bucket.push(cStakeCredential);
          const cRewardAddress = C.RewardAddress.new(
            networkInfo.network_id(),
            cStakeCredential
          );
          bucket.push(cRewardAddress);
          const address = cRewardAddress.to_address();
          bucket.push(address);
          const rewardAddress = address.to_bech32(undefined);

          const { stakeCredential } = getAddressDetails(rewardAddress);
          checkAndConsumeHash(stakeCredential!, "Cert", index);

          if (!this.chain[rewardAddress]?.registeredStake) {
            throw new Error(
              `Stake key is already deregistered. Reward address: ${rewardAddress}`
            );
          }
          certRequests.push({ type: "Deregistration", rewardAddress });
          break;
        }
        case 2: {
          const delegation = cert.as_stake_delegation()!;
          bucket.push(delegation);
          const networkInfo = C.NetworkInfo.testnet();
          bucket.push(networkInfo);
          const cStakeCredential = delegation.stake_credential();
          bucket.push(cStakeCredential);
          const cRewardAddress = C.RewardAddress.new(
            networkInfo.network_id(),
            cStakeCredential
          );
          bucket.push(cRewardAddress);
          const address = cRewardAddress.to_address();
          bucket.push(address);
          const rewardAddress = address.to_bech32(undefined);
          const poolKeyHash = delegation.pool_keyhash();
          bucket.push(poolKeyHash);
          const poolId = poolKeyHash.to_bech32("pool");

          const { stakeCredential } = getAddressDetails(rewardAddress);
          checkAndConsumeHash(stakeCredential!, "Cert", index);

          if (
            !this.chain[rewardAddress]?.registeredStake &&
            !certRequests.find(
              (request) =>
                request.type === "Registration" &&
                request.rewardAddress === rewardAddress
            )
          ) {
            throw new Error(
              `Stake key is not registered. Reward address: ${rewardAddress}`
            );
          }
          certRequests.push({ type: "Delegation", rewardAddress, poolId });
          break;
        }
      }
    }

    // Check input witnesses

    resolvedInputs.forEach(({ entry: { utxo } }, index) => {
      const { paymentCredential } = getAddressDetails(utxo.address);
      checkAndConsumeHash(paymentCredential!, "Spend", index);
    });

    // Create outputs and consume datum hashes
    const outputs = (() => {
      const outputs = body.outputs();
      bucket.push(outputs);
      const collected = [];
      for (let i = 0; i < outputs.len(); i++) {
        const output = outputs.get(i);
        bucket.push(output);
        const transactionHash = C.TransactionHash.from_hex(txHash);
        bucket.push(transactionHash);
        const index = C.BigNum.from_str(i.toString());
        bucket.push(index);
        const transactionInput = C.TransactionInput.new(transactionHash, index);
        bucket.push(transactionInput);
        const unspentOutput = C.TransactionUnspentOutput.new(
          transactionInput,
          output
        );
        bucket.push(unspentOutput);

        const utxo = coreToUtxo(unspentOutput);

        if (utxo.datumHash) consumedHashes.add(utxo.datumHash);

        collected.push({
          utxo,
          spent: false,
        });
      }
      return collected;
    })();

    // Check consumed witnesses

    const [extraKeyHash] = keyHashes.filter(
      (keyHash) => !consumedHashes.has(keyHash)
    );
    if (extraKeyHash) {
      throw new Error(`Extraneous vkey witness. Key hash: ${extraKeyHash}`);
    }

    const [extraNativeHash] = nativeHashes.filter(
      (scriptHash) => !consumedHashes.has(scriptHash)
    );
    if (extraNativeHash) {
      throw new Error(
        `Extraneous native script. Script hash: ${extraNativeHash}`
      );
    }

    const [extraPlutusHash] = plutusHashes.filter(
      (scriptHash) => !consumedHashes.has(scriptHash)
    );
    if (extraPlutusHash) {
      throw new Error(
        `Extraneous plutus script. Script hash: ${extraPlutusHash}`
      );
    }

    const [extraDatumHash] = Object.keys(datumTable).filter(
      (datumHash) => !consumedHashes.has(datumHash)
    );
    if (extraDatumHash) {
      throw new Error(`Extraneous plutus data. Datum hash: ${extraDatumHash}`);
    }

    // Apply transitions

    resolvedInputs.forEach(({ entry, type }) => {
      const outRef = entry.utxo.txHash + entry.utxo.outputIndex;
      entry.spent = true;

      if (type === "Ledger") this.ledger[outRef] = entry;
      else if (type === "Mempool") this.mempool[outRef] = entry;
    });

    withdrawalRequests.forEach(({ rewardAddress, withdrawal }) => {
      this.chain[rewardAddress].delegation.rewards -= withdrawal;
    });

    certRequests.forEach(({ type, rewardAddress, poolId }) => {
      switch (type) {
        case "Registration": {
          if (this.chain[rewardAddress]) {
            this.chain[rewardAddress].registeredStake = true;
          } else {
            this.chain[rewardAddress] = {
              registeredStake: true,
              delegation: { poolId: null, rewards: 0n },
            };
          }
          break;
        }
        case "Deregistration": {
          this.chain[rewardAddress].registeredStake = false;
          this.chain[rewardAddress].delegation.poolId = null;
          break;
        }
        case "Delegation": {
          this.chain[rewardAddress].delegation.poolId = poolId!;
        }
      }
    });

    outputs.forEach(({ utxo, spent }) => {
      this.mempool[utxo.txHash + utxo.outputIndex] = {
        utxo,
        spent,
      };
    });

    for (const [datumHash, datum] of Object.entries(datumTable)) {
      this.datumTable[datumHash] = datum;
    }

    return Promise.resolve(txHash);
  }

  log() {
    function getRandomColor(unit: Unit) {
      const seed = unit === "lovelace" ? "1" : unit;
      // Convert the seed string to a number
      let num = 0;
      for (let i = 0; i < seed.length; i++) {
        num += seed.charCodeAt(i);
      }

      // Generate a color based on the seed number
      const r = (num * 123) % 256;
      const g = (num * 321) % 256;
      const b = (num * 213) % 256;

      // Return the color as a hex string
      return "#" + ((1 << 24) + (r << 16) + (g << 8) + b).toString(16).slice(1);
    }

    const totalBalances: Assets = {};

    const balances: Record<Address, Assets> = {};
    for (const { utxo } of Object.values(this.ledger)) {
      for (const [unit, quantity] of Object.entries(utxo.assets)) {
        if (!balances[utxo.address]) {
          balances[utxo.address] = { [unit]: quantity };
        } else if (!balances[utxo.address]?.[unit]) {
          balances[utxo.address][unit] = quantity;
        } else {
          balances[utxo.address][unit] += quantity;
        }

        if (!totalBalances[unit]) {
          totalBalances[unit] = quantity;
        } else {
          totalBalances[unit] += quantity;
        }
      }
    }

    console.log("\n%cBlockchain state", "color:purple");
    console.log(
      `
    Block height:   %c${this.blockHeight}%c
    Slot:           %c${this.slot}%c
    Unix time:      %c${this.time}
  `,
      "color:yellow",
      "color:white",
      "color:yellow",
      "color:white",
      "color:yellow"
    );
    console.log("\n");
    for (const [address, assets] of Object.entries(balances)) {
      console.log(`Address: %c${address}`, "color:blue", "\n");
      for (const [unit, quantity] of Object.entries(assets)) {
        const barLength = Math.max(
          Math.floor(60 * (Number(quantity) / Number(totalBalances[unit]))),
          1
        );
        console.log(
          `%c${"\u2586".repeat(barLength) + " ".repeat(60 - barLength)}`,
          `color: ${getRandomColor(unit)}`,
          "",
          `${unit}:`,
          quantity,
          ""
        );
      }
      console.log(`\n${"\u2581".repeat(60)}\n`);
    }
  }
}
