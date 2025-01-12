import {
  Assets,
  AuxMetadata,
  Change,
  Data,
  DatumVariant,
  Hasher,
  Instruction,
  InstructionBuilder,
  Lucid,
  OutputData,
  PoolRegistration,
  Script,
  toHex,
  TxComplete,
  Utxo,
} from "../mod.ts";

export class Tx {
  private tasks: ((that: Tx) => Promise<Instruction> | Instruction)[];
  private lucid: Lucid;

  constructor(lucid: Lucid) {
    this.lucid = lucid;
    this.tasks = [];
  }

  /** Read data from utxos. These utxos are only referenced and not spent. */
  readFrom(utxos: Utxo[]): Tx {
    this.tasks.push(async (that) => {
      for (const utxo of utxos) {
        if (utxo.datumHash) {
          utxo.datum = Data.to(await that.lucid.datumOf(utxo));
        }
      }
      return { type: "ReadFrom", utxos };
    });
    return this;
  }

  /**
   * A public key or native script input.
   * With redeemer it's a plutus script input.
   */
  collectFrom(utxos: Utxo[], redeemer?: string): Tx {
    this.tasks.push(async (that) => {
      for (const utxo of utxos) {
        if (utxo.datumHash && !utxo.datum) {
          utxo.datum = Data.to(await that.lucid.datumOf(utxo));
        }
      }
      return { type: "CollectFrom", utxos, redeemer };
    });
    return this;
  }

  /**
   * All assets should be of the same policy id.
   * You can chain mintAssets functions together if you need to mint assets with different policy ids.
   * If the plutus script doesn't need a redeemer, you still need to specifiy the void redeemer.
   */
  mint(assets: Assets, redeemer?: string): Tx {
    this.tasks.push(() => ({ type: "Mint", assets, redeemer }));
    return this;
  }

  /** Pay to a public key or native script address. */
  payTo(address: string, assets: Assets): Tx {
    this.tasks.push(() => ({ type: "PayTo", address, assets }));
    return this;
  }

  /** Pay to a public key or native script address with datum or scriptRef. */
  payToWithData(
    address: string,
    outputData: string | OutputData,
    assets: Assets,
  ): Tx {
    this.tasks.push(() => {
      const { scriptRef, ...datumVariant } = typeof outputData === "string"
        ? { AsHash: outputData }
        : outputData;

      return {
        type: "PayTo",
        address,
        assets,
        datumVariant: datumVariant as DatumVariant | undefined,
        scriptRef,
      };
    });
    return this;
  }

  /** Pay to a plutus script address with datum or scriptRef. */
  payToContract(
    address: string,
    outputData: string | OutputData,
    assets: Assets,
  ): Tx {
    this.tasks.push(() => {
      const { scriptRef, ...datumVariant } = typeof outputData === "string"
        ? { AsHash: outputData }
        : outputData;

      return {
        type: "PayToContract",
        address,
        assets,
        datumVariant: datumVariant as DatumVariant,
        scriptRef,
      };
    });
    return this;
  }

  /** Delegate to a stake pool. */
  delegateTo(
    rewardAddress: string,
    poolId: string,
    redeemer?: string,
  ): Tx {
    this.tasks.push(() => ({
      type: "DelegateTo",
      delegation: {
        rewardAddress,
        poolId,
      },
      redeemer,
    }));
    return this;
  }

  /** Register a reward address in order to delegate to a pool and receive rewards. */
  registerStake(rewardAddress: string): Tx {
    this.tasks.push(() => ({ type: "RegisterStake", rewardAddress }));
    return this;
  }

  /** Deregister a reward address. */
  deregisterStake(rewardAddress: string, redeemer?: string): Tx {
    this.tasks.push(() => ({
      type: "DeregisterStake",
      rewardAddress,
      redeemer,
    }));
    return this;
  }

  /** Register a stake pool. A pool deposit is required. The metadataUrl needs to be hosted already before making the registration. */
  registerPool(params: PoolRegistration): Tx {
    this.tasks.push(async () => {
      if (params.metadataUrl && !params.metadataHash) {
        const metadata = await fetch(
          params.metadataUrl,
        )
          .then((res) => res.arrayBuffer());

        const metadataHash = Hasher.hashWithBlake2b256(
          toHex(new Uint8Array(metadata)),
        );

        params.metadataHash = metadataHash;
      }
      return { type: "RegisterPool", ...params };
    });
    return this;
  }

  /** Update a stake pool. No pool deposit is required. The metadataUrl needs to be hosted already before making the update. */
  updatePool(params: PoolRegistration): Tx {
    this.tasks.push(async () => {
      if (params.metadataUrl && !params.metadataHash) {
        const metadata = await fetch(
          params.metadataUrl,
        )
          .then((res) => res.arrayBuffer());

        const metadataHash = Hasher.hashWithBlake2b256(
          toHex(new Uint8Array(metadata)),
        );

        params.metadataHash = metadataHash;
      }
      return { type: "UpdatePool", ...params };
    });
    return this;
  }
  /**
   * Retire a stake pool. The epoch needs to be the greater than the current epoch + 1 and less than current epoch + eMax.
   * The pool deposit will be sent to reward address as reward after full retirement of the pool.
   */
  retirePool(poolId: string, epoch: number): Tx {
    this.tasks.push(() => ({ type: "RetirePool", poolId, epoch }));
    return this;
  }

  withdraw(
    rewardAddress: string,
    amount: bigint,
    redeemer?: string,
  ): Tx {
    this.tasks.push(() => ({
      type: "Withdraw",
      withdrawal: { rewardAddress, amount: Number(amount) },
      redeemer,
    }));
    return this;
  }

  /** Add a payment or stake key hash as a required signer of the transaction. */
  addSigner(keyHash: string): Tx {
    this.tasks.push(() => ({ type: "AddSigner", keyHash }));
    return this;
  }

  validFrom(unixTime: number): Tx {
    this.tasks.push(() => ({ type: "ValidFrom", unixTime }));
    return this;
  }

  validTo(unixTime: number): Tx {
    this.tasks.push(() => ({ type: "ValidTo", unixTime }));
    return this;
  }

  attachMetadata(label: number, metadata: AuxMetadata): Tx {
    this.tasks.push(() => ({
      type: "AttachMetadata",
      metadata: [label, metadata],
    }));
    return this;
  }

  /** Converts strings to bytes if prefixed with **'0x'**. */
  attachMetadataWithConversion(label: number, metadata: AuxMetadata): Tx {
    this.tasks.push(() => ({
      type: "AttachMetadataWithConversion",
      metadata: [label, metadata],
    }));
    return this;
  }

  /** Explicitely set the network id in the transaction body. */
  addNetworkId(id: number): Tx {
    this.tasks.push(() => ({ type: "AddNetworkId", id }));
    return this;
  }

  attachScript(script: Script): Tx {
    this.tasks.push(() => ({ type: "AttachScript", script }));
    return this;
  }

  withChangeTo(change: Change): Tx {
    this.tasks.push(() => ({
      type: "WithChangeTo",
      address: change.address,
      datumVariant: change.datumVariant,
    }));
    return this;
  }

  withoutCoinSelection(): Tx {
    this.tasks.push(() => ({ type: "WithoutCoinSelection" }));
    return this;
  }

  /** Compose transactions. */
  compose(tx: Tx | null): Tx {
    if (tx) this.tasks = this.tasks.concat(tx.tasks);
    return this;
  }

  async commit(): Promise<TxComplete> {
    const instructions = await this.toInstructions();

    const utxos = await this.lucid.wallet.getUtxos();
    const protocolParameters = await this.lucid.provider
      .getProtocolParameters();
    const address = await this.lucid.wallet.address();

    const instructionSigner = new InstructionBuilder(
      this.lucid.network,
      protocolParameters,
      utxos,
      { address },
    ).commit(instructions);

    return new TxComplete(
      this.lucid,
      instructionSigner,
    );
  }

  async toInstructions(): Promise<Instruction[]> {
    const instructions = await Promise.all(
      this.tasks.map((task) => task(this)),
    );

    return instructions;
  }
}
