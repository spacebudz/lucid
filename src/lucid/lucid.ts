import {
  ActiveDelegation,
  Addresses,
  Codec,
  Constr,
  Credential,
  Crypto,
  Data,
  Exact,
  fromUnit,
  Instruction,
  InstructionBuilder,
  InstructionSigner,
  Json,
  Message,
  NativeScript,
  Network,
  OutRef,
  Provider,
  ReadOnlyWallet,
  Script,
  ScriptUtility,
  SignedMessage,
  toUnit,
  Tx,
  TxComplete,
  Utxo,
  Wallet,
  WalletApi,
} from "../mod.ts";
import { signMessage, verifyMessage } from "../misc/sign_message.ts";

export class Lucid {
  wallet!: Wallet;
  provider!: Provider;
  network: Network;
  utils: {
    scriptToAddress: (
      script: Script,
      delegation?: Credential | undefined,
    ) => string;
    credentialToAddress: (
      payment: Credential,
      delegation?: Credential | undefined,
    ) => string;
    credentialToRewardAddress: (delegation: Credential) => string;
    scriptToRewardAddress: (script: Script) => string;
    unixTimeToSlots: (unixTime: number) => number;
  };

  constructor(
    { provider, network }: {
      provider?: Provider;
      network?: Network;
    } = {},
  ) {
    if (provider) this.provider = provider;
    this.network = provider?.network || network || "Mainnet";

    this.utils = {
      scriptToAddress: Addresses.scriptToAddress.bind(null, this.network),
      credentialToAddress: Addresses.credentialToAddress.bind(
        null,
        this.network,
      ),
      credentialToRewardAddress: Addresses.credentialToRewardAddress.bind(
        null,
        this.network,
      ),
      scriptToRewardAddress: Addresses.scriptToRewardAddress.bind(
        null,
        this.network,
      ),
      unixTimeToSlots: (unixTime) => {
        const slotConfig = (() => {
          switch (this.network) {
            case "Mainnet":
              return {
                zeroTime: 1596059091000,
                zeroSlot: 4492800,
                slotLength: 1000,
              };
            case "Preprod":
              return {
                zeroTime: 1655769600000,
                zeroSlot: 86400,
                slotLength: 1000,
              };
            case "Preview":
              return {
                zeroTime: 1666656000000,
                zeroSlot: 0,
                slotLength: 1000,
              };
            default:
              return {
                zeroTime: this.network.Emulator,
                zeroSlot: 0,
                slotLength: 1000,
              };
          }
        })();

        return Math.floor(
          (unixTime - slotConfig.zeroTime) / slotConfig.slotLength,
        ) + slotConfig.zeroSlot;
      },
    };
  }

  newScript<T extends unknown[] = Data[]>(
    script: Script | NativeScript,
    params?: Exact<[...T]>,
    type?: T,
  ): ScriptUtility {
    return new ScriptUtility(this, script, params, type);
  }

  newTx(): Tx {
    return new Tx(this);
  }

  async fromTx(tx: string): Promise<TxComplete> {
    const utxos = this.wallet ? await this.wallet.getUtxos() : [];
    return new TxComplete(this, InstructionSigner.fromTx(tx, utxos));
  }

  async fromInstructions(instructions: Instruction[]): Promise<TxComplete> {
    if (!this.wallet || !this.provider) {
      throw new Error("Wallet or provider not set");
    }

    const utxos = await this.wallet.getUtxos();
    const protocolParameters = await this.provider
      .getProtocolParameters();
    const address = await this.wallet.address();

    const instructionSigner = new InstructionBuilder(
      this.network,
      protocolParameters,
      utxos,
      { address },
    ).commit(instructions);

    return new TxComplete(
      this,
      instructionSigner,
    );
  }

  /** Signs a message. Expects the payload to be Hex encoded. */
  newMessage(address: string, payload: string): Message {
    return new Message(this, address, payload);
  }

  /** Verify a message. Expects the payload to be Hex encoded. */
  verifyMessage(
    address: string,
    payload: string,
    signedMessage: SignedMessage,
  ): boolean {
    return verifyMessage(address, payload, signedMessage);
  }

  utxosAt(addressOrCredential: string | Credential): Promise<Utxo[]> {
    return this.provider.getUtxos(addressOrCredential);
  }

  utxosAtWithUnit(
    addressOrCredential: string | Credential,
    unit: string,
  ): Promise<Utxo[]> {
    return this.provider.getUtxosWithUnit(addressOrCredential, unit);
  }

  /** Unit needs to be an NFT (or optionally the entire supply in one UTxO). */
  utxoByUnit(unit: string): Promise<Utxo> {
    return this.provider.getUtxoByUnit(unit);
  }

  utxosByOutRef(outRefs: Array<OutRef>): Promise<Utxo[]> {
    return this.provider.getUtxosByOutRef(outRefs);
  }

  delegationAt(rewardAddress: string): Promise<ActiveDelegation> {
    return this.provider.getDelegation(rewardAddress);
  }

  awaitTx(txHash: string, checkInterval = 3000): Promise<boolean> {
    return this.provider.awaitTx(txHash, checkInterval);
  }

  async datumOf<T = Data>(utxo: Utxo, type?: T): Promise<T> {
    if (!utxo.datum) {
      if (!utxo.datumHash) {
        throw new Error("This UTxO does not have a datum hash.");
      }
      utxo.datum = await this.provider.getDatum(utxo.datumHash);
    }
    return Data.from<T>(utxo.datum, type);
  }

  /** Query CIP-0068 metadata for a specifc asset. */
  async metadataOf<T = Json>(unit: string): Promise<T> {
    const { policyId, name, label } = fromUnit(unit);
    switch (label) {
      case 222:
      case 333:
      case 444: {
        const utxo = await this.utxoByUnit(toUnit(policyId, name, 100));
        const metadata = await this.datumOf(utxo) as Constr<Data>;
        return Data.toMetadata(metadata.fields[0]);
      }
      default:
        throw new Error("No variant matched.");
    }
  }

  /**
   * Only an Enteprise address (without stake credential) is derived.
   */
  selectWalletFromPrivateKey(privateKey: string): Lucid {
    const { credential } = Crypto.privateKeyToDetails(privateKey);
    const address = this.utils.credentialToAddress(credential);

    this.wallet = {
      // deno-lint-ignore require-await
      address: async (): Promise<string> => address,
      // deno-lint-ignore require-await
      rewardAddress: async (): Promise<string | null> => null,
      getUtxos: async (): Promise<Utxo[]> => {
        return await this.utxosAt(
          credential,
        );
      },
      // deno-lint-ignore require-await
      getDelegation: async (): Promise<ActiveDelegation> => {
        return { poolId: null, rewards: 0n };
      },
      // deno-lint-ignore require-await
      sign: async (
        instructionSigner: InstructionSigner,
      ): Promise<string> => {
        return instructionSigner
          .signWithKey(privateKey)
          .getPartialWitnessSet();
      },
      // deno-lint-ignore require-await
      signMessage: async (
        address: string,
        payload: string,
      ): Promise<SignedMessage> => {
        const { payment } = Addresses.inspect(address);

        if (payment?.hash !== credential.hash) {
          throw new Error(`Cannot sign message for address: ${address}.`);
        }

        return signMessage(address, payload, privateKey);
      },
      submit: async (tx: string): Promise<string> => {
        return await this.provider.submit(tx);
      },
    };
    return this;
  }

  selectWalletFromApi(api: WalletApi): Lucid {
    const getAddressRaw = async () => {
      const [addressRaw] = await api.getUsedAddresses();
      if (addressRaw) return addressRaw;

      const [unusedAddressRaw] = await api.getUnusedAddresses();
      return unusedAddressRaw;
    };

    this.wallet = {
      address: async (): Promise<string> =>
        Addresses.inspect(await getAddressRaw()).address,
      rewardAddress: async (): Promise<string | null> => {
        const [rewardAddressRaw] = await api.getRewardAddresses();
        const rewardAddress = rewardAddressRaw
          ? Addresses.inspect(rewardAddressRaw).address
          : null;
        return rewardAddress;
      },
      getUtxos: async (): Promise<Utxo[]> => {
        const utxos = ((await api.getUtxos()) || []).map((utxo) => {
          return Codec.decodeUtxo(utxo);
        });
        return utxos;
      },
      getDelegation: async (): Promise<ActiveDelegation> => {
        const rewardAddress = await this.wallet.rewardAddress();

        return rewardAddress
          ? await this.delegationAt(rewardAddress)
          : { poolId: null, rewards: 0n };
      },
      sign: async (
        instructionSigner: InstructionSigner,
      ): Promise<string> => {
        const tx = instructionSigner.commit();
        const witnessSet = await api.signTx(tx, true);
        instructionSigner.signWithWitnessSet(witnessSet);
        return witnessSet;
      },
      signMessage: async (
        address: string,
        payload: string,
      ): Promise<SignedMessage> => {
        const { addressRaw } = Addresses.inspect(address);
        return await api.signData(addressRaw, payload);
      },
      submit: async (tx: string): Promise<string> => {
        const txHash = await api.submitTx(tx);
        return txHash;
      },
    };
    return this;
  }

  /**
   * If utxos are not set, utxos are fetched from the provided address.
   */
  selectReadOnlyWallet({
    address,
    rewardAddress,
    utxos,
  }: ReadOnlyWallet): Lucid {
    const { payment, delegation } = Addresses.inspect(address);
    this.wallet = {
      // deno-lint-ignore require-await
      address: async (): Promise<string> => address,
      // deno-lint-ignore require-await
      rewardAddress: async (): Promise<string | null> => {
        return rewardAddress
          ? rewardAddress
          : delegation
          ? this.utils.credentialToRewardAddress(delegation)
          : null;
      },
      getUtxos: async (): Promise<Utxo[]> => {
        return utxos ? utxos : await this.utxosAt(payment!);
      },
      getDelegation: async (): Promise<ActiveDelegation> => {
        const rewardAddress = await this.wallet.rewardAddress();

        return rewardAddress
          ? await this.delegationAt(rewardAddress)
          : { poolId: null, rewards: 0n };
      },
      // deno-lint-ignore require-await
      sign: async (): Promise<string> => {
        throw new Error("Wallet is read only");
      },
      // deno-lint-ignore require-await
      signMessage: async (): Promise<SignedMessage> => {
        throw new Error("Wallet is read only");
      },
      submit: async (tx: string): Promise<string> => {
        return await this.provider.submit(tx);
      },
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
      type?: "Base" | "Enterprise";
      index?: number;
    },
  ): Lucid {
    const index = options?.index || 0;
    const paymentDetails = Crypto.seedToDetails(seed, index, "Payment");
    const delegationDetails = options?.type === "Enterprise"
      ? null
      : Crypto.seedToDetails(seed, options?.index || 0, "Delegation");

    const address = this.utils.credentialToAddress(
      paymentDetails.credential,
      delegationDetails?.credential,
    );
    const rewardAddress = delegationDetails
      ? this.utils.credentialToRewardAddress(delegationDetails.credential)
      : null;

    const paymentKeyHash = paymentDetails.credential.hash;
    const delegationKeyHash = delegationDetails?.credential.hash || "";

    const privKeyMap = {
      [paymentKeyHash]: paymentDetails.privateKey,
      [delegationKeyHash]: delegationDetails?.privateKey,
    };

    this.wallet = {
      // deno-lint-ignore require-await
      address: async (): Promise<string> => address,
      // deno-lint-ignore require-await
      rewardAddress: async (): Promise<string | null> => rewardAddress,
      // deno-lint-ignore require-await
      getUtxos: async (): Promise<Utxo[]> =>
        this.utxosAt(paymentDetails.credential),
      getDelegation: async (): Promise<ActiveDelegation> => {
        const rewardAddress = await this.wallet.rewardAddress();

        return rewardAddress
          ? await this.delegationAt(rewardAddress)
          : { poolId: null, rewards: 0n };
      },
      // deno-lint-ignore require-await
      sign: async (
        instructionSigner: InstructionSigner,
      ): Promise<string> => {
        return instructionSigner
          .signWithSeed(seed, index)
          .getPartialWitnessSet();
      },
      // deno-lint-ignore require-await
      signMessage: async (
        address: string,
        payload: string,
      ): Promise<SignedMessage> => {
        const {
          payment,
          delegation,
        } = Addresses.inspect(address);

        const keyHash = payment?.hash || delegation?.hash;

        const privateKey = privKeyMap[keyHash!];

        if (!privateKey) {
          throw new Error(`Cannot sign message for address: ${address}.`);
        }

        return signMessage(address, payload, privateKey);
      },
      submit: async (tx: string): Promise<string> => {
        return await this.provider.submit(tx);
      },
    };
    return this;
  }
}
