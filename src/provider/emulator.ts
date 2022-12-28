import { C } from "../core/core.ts";
import {
  Address,
  Assets,
  Credential,
  Datum,
  DatumHash,
  Delegation,
  Lovelace,
  OutRef,
  PoolId,
  ProtocolParameters,
  Provider,
  RewardAddress,
  Transaction,
  TxHash,
  UnixTime,
  UTxO,
} from "../types/types.ts";
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
    accounts: { address: Address; assets: Assets }[],
    protocolParameters: ProtocolParameters = PROTOCOL_PARAMETERS_DEFAULT,
  ) {
    const GENESIS_HASH = "00".repeat(32);
    this.blockHeight = 0;
    this.slot = 0;
    this.time = Date.now();
    this.ledger = {};
    accounts.forEach(({ address, assets }, index) => {
      this.ledger[GENESIS_HASH + index] = {
        utxo: {
          txHash: GENESIS_HASH,
          outputIndex: index,
          address,
          assets,
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

  getUtxos(address: string): Promise<UTxO[]> {
    const utxos: UTxO[] = Object.values(this.ledger).flatMap(({ utxo }) => {
      const { paymentCredential: utxoCredential } = getAddressDetails(
        utxo.address,
      );
      const { paymentCredential } = getAddressDetails(address);
      if (
        utxoCredential?.hash === paymentCredential?.hash
      ) return utxo;
      else return [];
    });

    return Promise.resolve(
      utxos,
    );
  }

  getProtocolParameters(): Promise<ProtocolParameters> {
    return Promise.resolve(this.protocolParameters);
  }

  getDatum(datumHash: string): Promise<string> {
    return Promise.resolve(this.datumTable[datumHash]);
  }

  getUtxosWithUnit(address: string, unit: string): Promise<UTxO[]> {
    const utxos: UTxO[] = Object.values(this.ledger).flatMap(({ utxo }) => {
      const { paymentCredential: utxoCredential } = getAddressDetails(
        utxo.address,
      );
      const { paymentCredential } = getAddressDetails(address);
      if (
        utxoCredential?.hash === paymentCredential?.hash &&
        utxo.assets[unit] > 0n
      ) return utxo;
      else return [];
    });

    return Promise.resolve(
      utxos,
    );
  }

  getUtxosByOutRef(outRefs: OutRef[]): Promise<UTxO[]> {
    return Promise.resolve(
      outRefs.map((outRef) =>
        this.ledger[outRef.txHash + outRef.outputIndex]!.utxo
      ),
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
    for (
      const [rewardAddress, { registeredStake, delegation }] of Object.entries(
        this.chain,
      )
    ) {
      if (registeredStake && delegation.poolId) {
        this.chain[rewardAddress] = {
          registeredStake,
          delegation: {
            poolId: delegation.poolId,
            rewards: delegation.rewards += rewards,
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

    const desTx = C.Transaction.from_bytes(fromHex(tx));

    const body = desTx.body();
    const witnesses = desTx.witness_set();
    const datums = witnesses.plutus_data();

    const txHash = toHex(C.hash_blake2b256(body.to_bytes()));

    // Datum table
    if (datums) {
      for (let i = 0; i < datums.len(); i++) {
        const datum = datums.get(i);
        const datumHash = C.hash_plutus_data(datum).to_hex();
        this.datumTable[datumHash] = toHex(datum.to_bytes());
      }
    }

    // Validity interval
    // Lower bound is inclusive
    // Upper bound is exclusive
    const lowerBound = body.validity_start_interval()?.to_str()
      ? parseInt(body.validity_start_interval()!.to_str())
      : null;
    const upperBound = body.ttl()?.to_str()
      ? parseInt(body.ttl()!.to_str())
      : null;

    if (Number.isInteger(lowerBound) && this.slot < lowerBound!) {
      throw new Error(
        `Lower bound (${lowerBound}) not in slot range (${this.slot}).`,
      );
    }

    if (Number.isInteger(upperBound) && this.slot >= upperBound!) {
      throw new Error(
        `Upper bound (${upperBound}) not in slot range (${this.slot}).`,
      );
    }

    // Witness keys
    const keyHashes = (() => {
      const keyHashes = [];
      for (let i = 0; i < (witnesses.vkeys()?.len() || 0); i++) {
        const witness = witnesses.vkeys()!.get(i);
        const publicKey = witness.vkey().public_key();
        const keyHash = publicKey.hash().to_hex();

        if (!publicKey.verify(fromHex(txHash), witness.signature())) {
          throw new Error(
            `Invalid vkey witness. Key hash: ${keyHash}`,
          );
        }
        keyHashes.push(keyHash);
      }
      return keyHashes;
    })();

    const nativeHashes = (() => {
      const scriptHashes = [];
      const edKeyHashes = C.Ed25519KeyHashes.new();
      keyHashes.forEach((keyHash) =>
        edKeyHashes.add(C.Ed25519KeyHash.from_hex(keyHash))
      );
      for (let i = 0; i < (witnesses.native_scripts()?.len() || 0); i++) {
        const witness = witnesses.native_scripts()!.get(i);
        const scriptHash = witness.hash(C.ScriptHashNamespace.NativeScript)
          .to_hex();

        if (
          !witness.verify(
            Number.isInteger(lowerBound)
              ? C.BigNum.from_str(lowerBound!.toString())
              : undefined,
            Number.isInteger(upperBound)
              ? C.BigNum.from_str(upperBound!.toString())
              : undefined,
            edKeyHashes,
          )
        ) {
          throw new Error(
            `Invalid native script witness. Script hash: ${scriptHash}`,
          );
        }
        scriptHashes.push(scriptHash);
      }
      return scriptHashes;
    })();

    const plutusHashes = (() => {
      const scriptHashes = [];
      for (let i = 0; i < (witnesses.plutus_scripts()?.len() || 0); i++) {
        const script = witnesses.plutus_scripts()!.get(i);
        const scriptHash = script.hash(C.ScriptHashNamespace.PlutusV1)
          .to_hex();

        scriptHashes.push(scriptHash);
      }
      for (let i = 0; i < (witnesses.plutus_v2_scripts()?.len() || 0); i++) {
        const script = witnesses.plutus_v2_scripts()!.get(i);
        const scriptHash = script.hash(C.ScriptHashNamespace.PlutusV2)
          .to_hex();

        scriptHashes.push(scriptHash);
      }
      return scriptHashes;
    })();

    const redeemers = (() => {
      const tagMap: Record<number, string> = {
        0: "Spend",
        1: "Mint",
        2: "Cert",
        3: "Reward",
      };
      const collected = [];
      for (let i = 0; i < (witnesses.redeemers()?.len() || 0); i++) {
        const redeemer = witnesses.redeemers()!.get(i);
        collected.push({
          tag: tagMap[redeemer.tag().kind()],
          index: parseInt(redeemer.index().to_str()),
        });
      }
      return collected;
    })();

    const consumedHashes = new Set();

    function checkAndConsumeHash(
      credential: Credential,
      tag: string,
      index: number,
    ) {
      switch (credential.type) {
        case "Key": {
          if (!keyHashes.includes(credential.hash)) {
            throw new Error(
              `Missing vkey witness. Key hash: ${credential.hash}`,
            );
          }
          consumedHashes.add(credential.hash);
          break;
        }
        case "Script":
          if (nativeHashes.includes(credential.hash)) {
            consumedHashes.add(credential.hash);
            break;
          } else if (plutusHashes.includes(credential.hash)) {
            if (
              redeemers.find((redeemer) =>
                redeemer.tag === tag && redeemer.index === index
              )
            ) {
              consumedHashes.add(credential.hash);
              break;
            }
          }
          throw new Error(
            `Missing script witness. Script hash: ${credential.hash}`,
          );
      }
    }

    // Check mint witnesses

    for (let index = 0; index < (body.mint()?.keys().len() || 0); index++) {
      const policyId = body.mint()!.keys().get(index).to_hex();
      checkAndConsumeHash({ type: "Script", hash: policyId }, "Mint", index);
    }

    // Check withdrawal witnesses

    const withdrawalRequests = [];

    for (
      let index = 0;
      index < (body.withdrawals()?.keys().len() || 0);
      index++
    ) {
      const rawAddress = body.withdrawals()!.keys().get(index);
      const withdrawal: Lovelace = BigInt(
        body.withdrawals()!.get(rawAddress)!.to_str(),
      );
      const rewardAddress = rawAddress.to_address().to_bech32(undefined);
      const { stakeCredential } = getAddressDetails(
        rewardAddress,
      );
      checkAndConsumeHash(stakeCredential!, "Reward", index);
      if (this.chain[rewardAddress]?.delegation.rewards !== withdrawal) {
        throw new Error(
          "Withdrawal amount doesn't match actual reward balance.",
        );
      }
      withdrawalRequests.push({ rewardAddress, withdrawal });
    }

    // Check cert witnesses

    const certRequests: {
      type: string;
      rewardAddress: RewardAddress;
      poolId?: PoolId;
    }[] = [];

    for (let index = 0; index < (body.certs()?.len() || 0); index++) {
      /*
        Checking only:
        1. Stake registration
        2. Stake deregistration
        3. Stake delegation

        All other certificate types are not checked and considered valid.
      */
      const cert = body.certs()!.get(index);
      switch (cert.kind()) {
        case 0: {
          const registration = cert.as_stake_registration()!;
          const rewardAddress = C.RewardAddress.new(
            C.NetworkInfo.testnet().network_id(),
            registration.stake_credential(),
          ).to_address().to_bech32(undefined);
          if (this.chain[rewardAddress]?.registeredStake) {
            throw new Error(
              `Stake key is already registered. Reward address: ${rewardAddress}`,
            );
          }
          certRequests.push({ type: "Registration", rewardAddress });
          break;
        }
        case 1: {
          const deregistration = cert.as_stake_deregistration()!;
          const rewardAddress = C.RewardAddress.new(
            C.NetworkInfo.testnet().network_id(),
            deregistration.stake_credential(),
          ).to_address().to_bech32(undefined);

          const { stakeCredential } = getAddressDetails(rewardAddress);
          checkAndConsumeHash(stakeCredential!, "Cert", index);

          if (!this.chain[rewardAddress]?.registeredStake) {
            throw new Error(
              `Stake key is already deregistered. Reward address: ${rewardAddress}`,
            );
          }
          certRequests.push({ type: "Deregistration", rewardAddress });
          break;
        }
        case 2: {
          const delegation = cert.as_stake_delegation()!;
          const rewardAddress = C.RewardAddress.new(
            C.NetworkInfo.testnet().network_id(),
            delegation.stake_credential(),
          ).to_address().to_bech32(undefined);
          const poolId = delegation.pool_keyhash().to_bech32("pool");

          const { stakeCredential } = getAddressDetails(rewardAddress);
          checkAndConsumeHash(stakeCredential!, "Cert", index);

          if (
            !this.chain[rewardAddress]?.registeredStake &&
            !certRequests.find((request) =>
              request.type === "Registration" &&
              request.rewardAddress === rewardAddress
            )
          ) {
            throw new Error(
              `Stake key is not registered. Reward address: ${rewardAddress}`,
            );
          }
          certRequests.push({ type: "Delegation", rewardAddress, poolId });
          break;
        }
      }
    }

    // Check input witnesses

    const inputs = body.inputs();
    inputs.sort();

    const resolvedInputs = [];

    for (let i = 0; i < inputs.len(); i++) {
      const input = inputs.get(i);

      const outRef = input.transaction_id().to_hex() + input.index().to_str();

      const entryLedger = this.ledger[outRef];

      const { entry, type } = !entryLedger
        ? { entry: this.mempool[outRef]!, type: "Mempool" }
        : { entry: entryLedger, type: "Ledger" };

      if (!entry || entry.spent) {
        throw new Error(
          `Could not spend UTxO: ${
            JSON.stringify({
              txHash: entry?.utxo.txHash,
              outputIndex: entry?.utxo.outputIndex,
            })
          }\nIt does not exist or was already spent.`,
        );
      }

      resolvedInputs.push({ entry, type });
    }

    resolvedInputs.forEach(({ entry: { utxo } }, index) => {
      const { paymentCredential } = getAddressDetails(utxo.address);
      checkAndConsumeHash(paymentCredential!, "Spend", index);
    });

    if (!keyHashes.every((keyHash) => consumedHashes.has(keyHash))) {
      throw new Error(`Extraneous vkey witness.`);
    }

    if (!nativeHashes.every((scriptHash) => consumedHashes.has(scriptHash))) {
      throw new Error(`Extraneous native script witness.`);
    }

    if (!plutusHashes.every((scriptHash) => consumedHashes.has(scriptHash))) {
      throw new Error(`Extraneous plutus script witness.`);
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

    for (let i = 0; i < body.outputs().len(); i++) {
      const output = body.outputs().get(i);
      const unspentOutput = C.TransactionUnspentOutput.new(
        C.TransactionInput.new(
          C.TransactionHash.from_hex(txHash),
          C.BigNum.from_str(i.toString()),
        ),
        output,
      );
      this.mempool[txHash + i] = {
        utxo: coreToUtxo(unspentOutput),
        spent: false,
      };
    }

    return Promise.resolve(txHash);
  }
}
