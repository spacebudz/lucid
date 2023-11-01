import { C } from "../core/mod.ts";
import {
  coreToUtxo,
  fromHex,
  fromUnit,
  paymentCredentialOf,
  toHex,
  toUnit,
  Utils,
  utxoToCore,
} from "../utils/mod.ts";
import {
  Address,
  Credential,
  Delegation,
  ExternalWallet,
  Json,
  KeyHash,
  Network,
  OutRef,
  Payload,
  PrivateKey,
  ProtocolParameters,
  Provider,
  RewardAddress,
  SignedMessage,
  Slot,
  SlotConfig,
  Transaction,
  TxHash,
  Unit,
  UTxO,
  Wallet,
  WalletApi,
} from "../types/mod.ts";
import { Tx } from "./tx.ts";
import { TxComplete } from "./tx_complete.ts";
import { discoverOwnUsedTxKeyHashes, walletFromSeed } from "../misc/wallet.ts";
import { signData, verifyData } from "../misc/sign_data.ts";
import { Message } from "./message.ts";
import { SLOT_CONFIG_NETWORK } from "../plutus/time.ts";
import { Constr, Data } from "../plutus/data.ts";
import { Emulator } from "../provider/emulator.ts";
import { Freeable, Freeables } from "../utils/freeable.ts";
import { getTransactionBuilderConfig } from "../utils/transaction_builder_config.ts";

export class Lucid {
  protocolParameters?: ProtocolParameters;
  slotConfig!: SlotConfig;
  wallet!: Wallet;
  provider!: Provider;
  network: Network = "Mainnet";
  utils!: Utils;

  static async new(
    provider?: Provider,
    network?: Network,
    protocolParameters?: ProtocolParameters,
  ): Promise<Lucid> {
    const lucid = new this();
    if (network) lucid.network = network;
    if (protocolParameters) {
      lucid.protocolParameters = protocolParameters;
    }
    if (provider) {
      lucid.provider = provider;

      if (lucid.provider instanceof Emulator) {
        lucid.network = "Custom";
        SLOT_CONFIG_NETWORK[lucid.network] = {
          zeroTime: lucid.provider.now(),
          zeroSlot: 0,
          slotLength: 1000,
        };
      }
    }
    if (provider && !lucid.protocolParameters) {
      const protocolParameters = await provider.getProtocolParameters();
      lucid.protocolParameters = protocolParameters;
    }
    lucid.slotConfig = SLOT_CONFIG_NETWORK[lucid.network];

    lucid.utils = new Utils(lucid);
    return lucid;
  }

  getTransactionBuilderConfig(): C.TransactionBuilderConfig {
    if (!this.protocolParameters) {
      throw new Error(
        "Protocol parameters or slot config not set. Set a provider or iniatilize with protocol parameters.",
      );
    }
    return getTransactionBuilderConfig(
      this.protocolParameters,
      this.slotConfig,
      {
        // deno-lint-ignore no-explicit-any
        url: (this.provider as any)?.url,
        // deno-lint-ignore no-explicit-any
        projectId: (this.provider as any)?.projectId,
      },
    );
  }

  /**
   * Switch provider and/or network.
   * If provider or network unset, no overwriting happens. Provider or network from current instance are taken then.
   */
  async switchProvider(provider?: Provider, network?: Network): Promise<Lucid> {
    if (this.network === "Custom") {
      throw new Error("Cannot switch when on custom network.");
    }
    const lucid = await Lucid.new(provider, network);
    this.protocolParameters = lucid.protocolParameters;
    this.slotConfig = lucid.slotConfig;
    this.provider = provider || this.provider;
    // Given that protoclParameters and provider are optional we should fetch protocol parameters if they are not set when switiching providers
    if (!this.protocolParameters && provider) {
      this.protocolParameters = await provider.getProtocolParameters();
    }
    this.network = network || this.network;
    this.wallet = lucid.wallet;

    return this;
  }

  newTx(): Tx {
    return new Tx(this);
  }

  fromTx(tx: Transaction): TxComplete {
    return new TxComplete(this, C.Transaction.from_bytes(fromHex(tx)));
  }

  /** Signs a message. Expects the payload to be Hex encoded. */
  newMessage(address: Address | RewardAddress, payload: Payload): Message {
    return new Message(this, address, payload);
  }

  /** Verify a message. Expects the payload to be Hex encoded. */
  verifyMessage(
    address: Address | RewardAddress,
    payload: Payload,
    signedMessage: SignedMessage,
  ): boolean {
    const {
      paymentCredential,
      stakeCredential,
      address: { hex: addressHex },
    } = this.utils.getAddressDetails(address);
    const keyHash = paymentCredential?.hash || stakeCredential?.hash;
    if (!keyHash) throw new Error("Not a valid address provided.");

    return verifyData(addressHex, keyHash, payload, signedMessage);
  }

  currentSlot(): Slot {
    return this.utils.unixTimeToSlot(Date.now());
  }

  utxosAt(addressOrCredential: Address | Credential): Promise<UTxO[]> {
    return this.provider.getUtxos(addressOrCredential);
  }

  utxosAtWithUnit(
    addressOrCredential: Address | Credential,
    unit: Unit,
  ): Promise<UTxO[]> {
    return this.provider.getUtxosWithUnit(addressOrCredential, unit);
  }

  /** Unit needs to be an NFT (or optionally the entire supply in one UTxO). */
  utxoByUnit(unit: Unit): Promise<UTxO> {
    return this.provider.getUtxoByUnit(unit);
  }

  utxosByOutRef(outRefs: Array<OutRef>): Promise<UTxO[]> {
    return this.provider.getUtxosByOutRef(outRefs);
  }

  delegationAt(rewardAddress: RewardAddress): Promise<Delegation> {
    return this.provider.getDelegation(rewardAddress);
  }

  awaitTx(txHash: TxHash, checkInterval = 3000): Promise<boolean> {
    return this.provider.awaitTx(txHash, checkInterval);
  }

  async datumOf<T = Data>(utxo: UTxO, type?: T): Promise<T> {
    if (!utxo.datum) {
      if (!utxo.datumHash) {
        throw new Error("This UTxO does not have a datum hash.");
      }
      utxo.datum = await this.provider.getDatum(utxo.datumHash);
    }
    return Data.from<T>(utxo.datum, type);
  }

  /** Query CIP-0068 metadata for a specifc asset. */
  async metadataOf<T = Json>(unit: Unit): Promise<T> {
    const { policyId, name, label } = fromUnit(unit);
    switch (label) {
      case 222:
      case 333:
      case 444: {
        const utxo = await this.utxoByUnit(toUnit(policyId, name, 100));
        const metadata = (await this.datumOf(utxo)) as Constr<Data>;
        return Data.toJson(metadata.fields[0]);
      }
      default:
        throw new Error("No variant matched.");
    }
  }

  /**
   * Cardano Private key in bech32; not the BIP32 private key or any key that is not fully derived.
   * Only an Enteprise address (without stake credential) is derived.
   */
  selectWalletFromPrivateKey(privateKey: PrivateKey): Lucid {
    const priv = C.PrivateKey.from_bech32(privateKey);
    const publicKey = priv.to_public();
    priv.free();
    const pubKeyHash = publicKey.hash();
    publicKey.free();

    this.wallet = {
      address: (): Promise<Address> => {
        const bucket: Freeable[] = [];
        const stakeCredential = C.StakeCredential.from_keyhash(pubKeyHash);
        bucket.push(stakeCredential);
        const enterpriseAddress = C.EnterpriseAddress.new(
          this.network === "Mainnet" ? 1 : 0,
          stakeCredential,
        );
        bucket.push(enterpriseAddress);
        const address = enterpriseAddress.to_address();
        bucket.push(address);
        const bech32 = address.to_bech32(undefined);
        Freeables.free(...bucket);

        return Promise.resolve(bech32);
      },

      rewardAddress: (): Promise<RewardAddress | null> => Promise.resolve(null),
      getUtxos: async (): Promise<UTxO[]> => {
        return await this.utxosAt(
          paymentCredentialOf(await this.wallet.address()),
        );
      },
      getUtxosCore: async (): Promise<C.TransactionUnspentOutputs> => {
        const utxos = await this.utxosAt(
          paymentCredentialOf(await this.wallet.address()),
        );
        const coreUtxos = C.TransactionUnspentOutputs.new();
        utxos.forEach((utxo) => {
          const coreUtxo = utxoToCore(utxo);
          coreUtxos.add(coreUtxo);
          coreUtxo.free();
        });
        return coreUtxos;
      },
      getDelegation: (): Promise<Delegation> => {
        return Promise.resolve({ poolId: null, rewards: 0n });
      },
      signTx: (tx: C.Transaction): Promise<C.TransactionWitnessSet> => {
        const bucket: Freeable[] = [];
        const txBody = tx.body();
        bucket.push(txBody);
        const hash = C.hash_transaction(txBody);
        bucket.push(hash);
        const witness = C.make_vkey_witness(hash, priv);
        bucket.push(witness);
        const txWitnessSetBuilder = C.TransactionWitnessSetBuilder.new();
        bucket.push(txWitnessSetBuilder);
        txWitnessSetBuilder.add_vkey(witness);
        const witnessSet = txWitnessSetBuilder.build();

        Freeables.free(...bucket);
        return Promise.resolve(witnessSet);
      },
      signMessage: (
        address: Address | RewardAddress,
        payload: Payload,
      ): Promise<SignedMessage> => {
        const {
          paymentCredential,
          address: { hex: hexAddress },
        } = this.utils.getAddressDetails(address);
        const keyHash = paymentCredential?.hash;

        const originalKeyHash = pubKeyHash.to_hex();

        if (!keyHash || keyHash !== originalKeyHash) {
          throw new Error(`Cannot sign message for address: ${address}.`);
        }

        return Promise.resolve(signData(hexAddress, payload, privateKey));
      },
      submitTx: async (tx: Transaction): Promise<TxHash> => {
        return await this.provider.submitTx(tx);
      },
    };

    return this;
  }

  selectWallet(api: WalletApi): Lucid {
    const getAddressHex = async () => {
      const [addressHex] = await api.getUsedAddresses();
      if (addressHex) return addressHex;

      const [unusedAddressHex] = await api.getUnusedAddresses();
      return unusedAddressHex;
    };

    this.wallet = {
      address: async (): Promise<Address> => {
        const addressHex = await getAddressHex();
        const address = C.Address.from_bytes(fromHex(addressHex));
        const bech32 = address.to_bech32(undefined);
        address.free();
        return bech32;
      },

      rewardAddress: async (): Promise<RewardAddress | null> => {
        const [rewardAddressHex] = await api.getRewardAddresses();
        if (rewardAddressHex) {
          const address = C.Address.from_bytes(fromHex(rewardAddressHex));
          const rewardAddress = C.RewardAddress.from_address(address)!;
          address.free();
          const addr = rewardAddress.to_address();
          rewardAddress.free();
          const bech32 = addr.to_bech32(undefined);
          addr.free();
          return bech32;
        }
        return null;
      },
      getUtxos: async (): Promise<UTxO[]> => {
        const utxos = ((await api.getUtxos()) || []).map((utxo) => {
          const parsedUtxo = C.TransactionUnspentOutput.from_bytes(
            fromHex(utxo),
          );
          const finalUtxo = coreToUtxo(parsedUtxo);
          parsedUtxo.free();
          return finalUtxo;
        });
        return utxos;
      },
      getUtxosCore: async (): Promise<C.TransactionUnspentOutputs> => {
        const utxos = C.TransactionUnspentOutputs.new();
        ((await api.getUtxos()) || []).forEach((utxo) => {
          const coreUtxo = C.TransactionUnspentOutput.from_bytes(fromHex(utxo));
          utxos.add(coreUtxo);
          coreUtxo.free();
        });
        return utxos;
      },
      getDelegation: async (): Promise<Delegation> => {
        const rewardAddr = await this.wallet.rewardAddress();

        return rewardAddr
          ? await this.delegationAt(rewardAddr)
          : { poolId: null, rewards: 0n };
      },
      signTx: async (tx: C.Transaction): Promise<C.TransactionWitnessSet> => {
        const witnessSet = await api.signTx(toHex(tx.to_bytes()), true);
        return C.TransactionWitnessSet.from_bytes(fromHex(witnessSet));
      },
      signMessage: async (
        address: Address | RewardAddress,
        payload: Payload,
      ): Promise<SignedMessage> => {
        const cAddress = C.Address.from_bech32(address);
        const hexAddress = toHex(cAddress.to_bytes());
        cAddress.free();
        return await api.signData(hexAddress, payload);
      },
      submitTx: async (tx: Transaction): Promise<TxHash> => {
        const txHash = await api.submitTx(tx);
        return txHash;
      },
    };
    return this;
  }

  /**
   * Emulates a wallet by constructing it with the utxos and an address.
   * If utxos are not set, utxos are fetched from the provided address.
   */
  selectWalletFrom({ address, utxos, rewardAddress }: ExternalWallet): Lucid {
    const addressDetails = this.utils.getAddressDetails(address);
    this.wallet = {
      address: (): Promise<Address> => Promise.resolve(address),
      rewardAddress: (): Promise<RewardAddress | null> => {
        if (!rewardAddress && addressDetails.stakeCredential) {
          if (addressDetails.stakeCredential.type === "Key") {
            const keyHash = C.Ed25519KeyHash.from_hex(
              addressDetails.stakeCredential.hash,
            );
            const stakeCredential = C.StakeCredential.from_keyhash(keyHash);
            keyHash.free();
            const rewardAddress = C.RewardAddress.new(
              this.network === "Mainnet" ? 1 : 0,
              stakeCredential,
            );
            stakeCredential.free();
            const address = rewardAddress.to_address();
            rewardAddress.free();
            const bech32 = address.to_bech32(undefined);
            address.free();
            return Promise.resolve(bech32);
          }
        }

        return Promise.resolve(rewardAddress ?? null);
      },
      getUtxos: async (): Promise<UTxO[]> => {
        return utxos ? utxos : await this.utxosAt(paymentCredentialOf(address));
      },
      getUtxosCore: async (): Promise<C.TransactionUnspentOutputs> => {
        const coreUtxos = C.TransactionUnspentOutputs.new();
        (utxos ? utxos : await this.utxosAt(paymentCredentialOf(address)))
          .forEach((utxo) => {
            const coreUtxo = utxoToCore(utxo);
            coreUtxos.add(coreUtxo);
            coreUtxo.free();
          });

        return coreUtxos;
      },
      getDelegation: async (): Promise<Delegation> => {
        const rewardAddr = await this.wallet.rewardAddress();

        return rewardAddr
          ? await this.delegationAt(rewardAddr)
          : { poolId: null, rewards: 0n };
      },
      signTx: (): Promise<C.TransactionWitnessSet> =>
        Promise.reject("Not implemented"),

      signMessage: (): Promise<SignedMessage> =>
        Promise.reject("Not implemented"),

      submitTx: (tx: Transaction): Promise<TxHash> =>
        this.provider.submitTx(tx),
    };
    return this;
  }

  /**
   * Select wallet from a seed phrase (e.g. 15 or 24 words). You have the option to choose between a Base address (with stake credential)
   * and Enterprise address (without stake credential). You can also decide which account index to derive. By default account 0 is derived.
   */
  selectWalletFromSeed(
    seed: string,
    options?: {
      addressType?: "Base" | "Enterprise";
      accountIndex?: number;
      password?: string;
    },
  ): Lucid {
    const bucket: Freeable[] = [];
    const { address, rewardAddress, paymentKey, stakeKey } = walletFromSeed(
      seed,
      {
        addressType: options?.addressType || "Base",
        accountIndex: options?.accountIndex || 0,
        password: options?.password,
        network: this.network,
      },
    );

    const paymentPrivateKey = C.PrivateKey.from_bech32(paymentKey);
    bucket.push(paymentPrivateKey);
    const paymentPublicKey = paymentPrivateKey.to_public();
    bucket.push(paymentPublicKey);
    const paymentPubKeyHash = paymentPublicKey.hash();
    bucket.push(paymentPubKeyHash);
    const paymentKeyHash = paymentPubKeyHash.to_hex();

    const getStakeKeyHash = (stakeKey: string) => {
      const stakePrivateKey = C.PrivateKey.from_bech32(stakeKey);
      bucket.push(stakePrivateKey);
      const stakePublicKey = stakePrivateKey.to_public();
      bucket.push(stakePublicKey);
      const stakePubKeyHash = stakePublicKey.hash();
      bucket.push(stakePubKeyHash);
      const stakeKeyHash = stakePubKeyHash.to_hex();
      return stakeKeyHash;
    };

    const stakeKeyHash = stakeKey ? getStakeKeyHash(stakeKey) : "";

    const privKeyHashMap = {
      [paymentKeyHash]: paymentKey,
      [stakeKeyHash]: stakeKey,
    };

    this.wallet = {
      address: (): Promise<Address> => Promise.resolve(address),
      rewardAddress: (): Promise<RewardAddress | null> =>
        Promise.resolve(rewardAddress || null),
      getUtxos: (): Promise<UTxO[]> =>
        this.utxosAt(paymentCredentialOf(address)),
      getUtxosCore: async (): Promise<C.TransactionUnspentOutputs> => {
        const coreUtxos = C.TransactionUnspentOutputs.new();
        (await this.utxosAt(paymentCredentialOf(address))).forEach((utxo) => {
          const coreUtxo = utxoToCore(utxo);
          coreUtxos.add(coreUtxo);
          coreUtxo.free();
        });
        return coreUtxos;
      },
      getDelegation: async (): Promise<Delegation> => {
        const rewardAddr = await this.wallet.rewardAddress();

        return rewardAddr
          ? await this.delegationAt(rewardAddr)
          : { poolId: null, rewards: 0n };
      },
      signTx: async (tx: C.Transaction): Promise<C.TransactionWitnessSet> => {
        const utxos = await this.utxosAt(address);

        const ownKeyHashes: Array<KeyHash> = [paymentKeyHash, stakeKeyHash];

        const usedKeyHashes = discoverOwnUsedTxKeyHashes(
          tx,
          ownKeyHashes,
          utxos,
        );

        const txWitnessSetBuilder = C.TransactionWitnessSetBuilder.new();
        usedKeyHashes.forEach((keyHash) => {
          const txBody = tx.body();
          const hash = C.hash_transaction(txBody);
          txBody.free();
          const privateKey = C.PrivateKey.from_bech32(privKeyHashMap[keyHash]!);
          const witness = C.make_vkey_witness(hash, privateKey);
          hash.free();
          privateKey.free();
          txWitnessSetBuilder.add_vkey(witness);
          witness.free();
        });

        const txWitnessSet = txWitnessSetBuilder.build();
        txWitnessSetBuilder.free();
        return txWitnessSet;
      },
      signMessage: (
        address: Address | RewardAddress,
        payload: Payload,
      ): Promise<SignedMessage> => {
        const {
          paymentCredential,
          stakeCredential,
          address: { hex: hexAddress },
        } = this.utils.getAddressDetails(address);

        const keyHash = paymentCredential?.hash || stakeCredential?.hash;

        const privateKey = privKeyHashMap[keyHash!];

        if (!privateKey) {
          throw new Error(`Cannot sign message for address: ${address}.`);
        }

        return Promise.resolve(signData(hexAddress, payload, privateKey));
      },
      submitTx: async (tx: Transaction): Promise<TxHash> => {
        return await this.provider.submitTx(tx);
      },
    };

    Freeables.free(...bucket);
    return this;
  }
}
