import { C } from "../core/mod.ts";
import {
  Credential,
  PrivateKey,
  Transaction,
  TransactionWitnesses,
  TxHash,
  UTxO,
} from "../types/mod.ts";
import {
  coresToOutRefs,
  fromHex,
  getAddressDetails,
  paymentCredentialOf,
  producedUtxosFrom,
  toHex,
} from "../utils/mod.ts";
import { Lucid } from "./lucid.ts";
import { Tx } from "./tx.ts";
import { TxSigned } from "./tx_signed.ts";

export class TxComplete {
  txComplete: C.Transaction;
  witnessSetBuilder: C.TransactionWitnessSetBuilder;
  private tasks: (() => Promise<void>)[];
  /** Stores the available input utxo set for this tx (for tx chaining), if undefined falls back to wallet utxos */
  private utxos?: UTxO[];
  private lucid: Lucid;
  fee: number;
  exUnits: { cpu: number; mem: number } | null = null;

  constructor(lucid: Lucid, tx: C.Transaction, utxos?: UTxO[]) {
    this.lucid = lucid;
    this.txComplete = tx;
    this.witnessSetBuilder = C.TransactionWitnessSetBuilder.new();
    this.tasks = [];
    this.utxos = utxos;

    this.fee = parseInt(tx.body().fee().to_str());
    const redeemers = tx.witness_set().redeemers();
    if (redeemers) {
      const exUnits = { cpu: 0, mem: 0 };
      for (let i = 0; i < redeemers.len(); i++) {
        const redeemer = redeemers.get(i);
        exUnits.cpu += parseInt(redeemer.ex_units().steps().to_str());
        exUnits.mem += parseInt(redeemer.ex_units().mem().to_str());
      }
      this.exUnits = exUnits;
    }
  }
  sign(): TxComplete {
    this.tasks.push(async () => {
      const witnesses = await this.lucid.wallet.signTx(this.txComplete);
      this.witnessSetBuilder.add_existing(witnesses);
    });
    return this;
  }

  /** Add an extra signature from a private key. */
  signWithPrivateKey(privateKey: PrivateKey): TxComplete {
    const priv = C.PrivateKey.from_bech32(privateKey);
    const witness = C.make_vkey_witness(
      C.hash_transaction(this.txComplete.body()),
      priv,
    );
    this.witnessSetBuilder.add_vkey(witness);
    return this;
  }

  /** Sign the transaction and return the witnesses that were just made. */
  async partialSign(): Promise<TransactionWitnesses> {
    const witnesses = await this.lucid.wallet.signTx(this.txComplete);
    this.witnessSetBuilder.add_existing(witnesses);
    return toHex(witnesses.to_bytes());
  }

  /**
   * Sign the transaction and return the witnesses that were just made.
   * Add an extra signature from a private key.
   */
  partialSignWithPrivateKey(privateKey: PrivateKey): TransactionWitnesses {
    const priv = C.PrivateKey.from_bech32(privateKey);
    const witness = C.make_vkey_witness(
      C.hash_transaction(this.txComplete.body()),
      priv,
    );
    this.witnessSetBuilder.add_vkey(witness);
    const witnesses = C.TransactionWitnessSetBuilder.new();
    witnesses.add_vkey(witness);
    return toHex(witnesses.build().to_bytes());
  }

  /** Sign the transaction with the given witnesses. */
  assemble(witnesses: TransactionWitnesses[]): TxComplete {
    witnesses.forEach((witness) => {
      const witnessParsed = C.TransactionWitnessSet.from_bytes(
        fromHex(witness),
      );
      this.witnessSetBuilder.add_existing(witnessParsed);
    });
    return this;
  }

  async complete(): Promise<TxSigned> {
    for (const task of this.tasks) {
      await task();
    }

    this.witnessSetBuilder.add_existing(this.txComplete.witness_set());
    const signedTx = C.Transaction.new(
      this.txComplete.body(),
      this.witnessSetBuilder.build(),
      this.txComplete.auxiliary_data(),
    );
    return new TxSigned(this.lucid, signedTx);
  }

  /** Return the transaction in Hex encoded Cbor. */
  toString(): Transaction {
    return toHex(this.txComplete.to_bytes());
  }

  /** Return the transaction hash. */
  toHash(): TxHash {
    return C.hash_transaction(this.txComplete.body()).to_hex();
  }

  /**
   * This function provides access to the produced outputs of the current transaction
   * that can be selectively picked to be chained with a new transaction which is returned
   * as result.
   *
   * @param outputChainSelector provides the tx outputs of the transaction that can be used for chaining a new tx.
   * If undefined is returned from this function, all outputs that are spendable from this wallet are chained.
   * @param redeemer this arguments is expected to match the number of selected chained outputs from the first argument and can be used
   * to chain script outputs with specific redeemers.
   * @returns a new transaction that already has inputs set defined by the *outputChainSelector* function.
   */
  chain(
    outputChainSelector: (utxos: UTxO[]) => UTxO | UTxO[] | undefined,
    redeemer?: string | string[] | undefined,
  ): Tx {
    const txOutputs = producedUtxosFrom(this);
    let chainedOutputs = outputChainSelector(txOutputs);
    const inputUTxOs = this.getUpdatedInputUTxOs(this.utxos);
    const chainedTx = this.lucid
      .newTx()
      .collectTxInputsFrom(inputUTxOs);

    if (
      !chainedOutputs ||
      Array.isArray(chainedOutputs) && chainedOutputs.length === 0
    ) {
      // chain all spendable unspent transaction outputs
      chainedOutputs = inputUTxOs;
    }

    if (Array.isArray(chainedOutputs) && Array.isArray(redeemer)) {
      if (!redeemer || chainedOutputs.length === redeemer.length) {
        chainedOutputs.forEach((utxo, i) =>
          chainedTx.collectFrom([utxo], redeemer.at(i))
        );
      } else {
        throw new Error(
          `Mismatching number of chained outputs (${chainedOutputs.length}) & redeemers (${redeemer.length})`,
        );
      }
    } else if (!Array.isArray(chainedOutputs) && !Array.isArray(redeemer)) {
      chainedTx.collectFrom([chainedOutputs], redeemer);
    } else {
      throw new Error(
        "Mismatching types for provided chained output(s) and redeemer(s).",
      );
    }
    return chainedTx;
  }

  private getUpdatedInputUTxOs(
    inputUTxOs?: UTxO[],
  ): UTxO[] {
    if (!inputUTxOs) return [];
    const paymentCredentials = inputUTxOs.map(({ address }) =>
      paymentCredentialOf(address)
    );
    const consumedOutRefs = coresToOutRefs(this.txComplete.body().inputs());
    const isSpendableByCreds =
      (walletPaymentCredentials: Credential[]) => ({ address }: UTxO) =>
        walletPaymentCredentials.find(({ hash: walletPKeyHash }) => {
          const { paymentCredential: outputPayCred } = getAddressDetails(
            address,
          );
          return (outputPayCred && walletPKeyHash === outputPayCred.hash &&
            outputPayCred.type === "Key");
        }) !== undefined;
    const producedUtxos = producedUtxosFrom(this);
    const isNotConsumed = ({ txHash, outputIndex }: UTxO) =>
      consumedOutRefs.find((outRef) =>
        outRef.txHash === txHash && outRef.outputIndex === outputIndex
      ) === undefined;
    const isSpendable = isSpendableByCreds(paymentCredentials);
    return inputUTxOs.filter(isNotConsumed).concat(
      producedUtxos.filter(isSpendable),
    );
  }
}
