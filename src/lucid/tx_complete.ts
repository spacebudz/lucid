import { C, Core } from "../core/mod.ts";
import { PrivateKey, Transaction, TransactionWitnesses, TxHash } from "../types/mod.ts";
import { Lucid } from "./lucid.ts";
import { TxSigned } from "./tx_signed.ts";
import { coreToUtxo, fromHex, toHex } from "../utils/mod.ts";

export class TxComplete {
  txComplete: Core.Transaction;
  witnessSetBuilder: Core.TransactionWitnessSetBuilder;
  private tasks: (() => Promise<void>)[];
  private lucid: Lucid;

  constructor(lucid: Lucid, tx: Core.Transaction) {
    this.lucid = lucid;
    this.txComplete = tx;
    this.witnessSetBuilder = C.TransactionWitnessSetBuilder.new();
    this.tasks = [];
  }
  sign(): TxComplete {
    this.tasks.push(async () => {
      const witnesses = await this.lucid.wallet.signTx(this.txComplete);
      this.witnessSetBuilder.add_existing(witnesses);
    });
    return this;
  }

  /** Add an extra signature from a private key */
  signWithPrivateKey(privateKey: PrivateKey): TxComplete {
    const priv = C.PrivateKey.from_bech32(privateKey);
    const witness = C.make_vkey_witness(
      C.hash_transaction(this.txComplete.body()),
      priv,
    );
    this.witnessSetBuilder.add_vkey(witness);
    return this;
  }

  /** Sign the transaction and return the witnesses that were just made */
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

  /** Sign the transaction with the given witnesses */
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

  /** **UNSTABLE** */
  // deno-lint-ignore no-explicit-any
  toObject(): any {
    console.info("toObject is unstable and not ready yet");
    const body = this.txComplete.body();
    const witnesses = this.txComplete.witness_set();
    const auxiliary_data = this.txComplete.auxiliary_data();

    const inputs = body
      .inputs()
      .to_js_value()
      // deno-lint-ignore no-explicit-any
      .map((i: any) => ({
        txHash: i.transaction_id,
        outputIndex: parseInt(i.index),
      }));
    const outputs = (() => {
      const dummyInput = C.TransactionInput.from_json(
        JSON.stringify({ transaction_id: "0".repeat(64), index: "0" }),
      );
      const outputs = [];
      const coreOutputs = body.outputs();
      for (let i = 0; i < body.outputs().len(); i++) {
        const o = coreOutputs.get(i);
        const coreUtxo = C.TransactionUnspentOutput.new(dummyInput, o);
        const utxo = coreToUtxo(coreUtxo);
        // @ts-ignore : txHash not needed in output
        delete utxo["txHash"];
        // @ts-ignore : txHash not needed in output
        delete utxo["outputIndex"];
        outputs.push(utxo);
      }
      return outputs;
    })();
    const fee = BigInt(body.fee().to_str());
    const validTo = body.ttl() ? parseInt(body.ttl()!.to_str()) : "always";
    const certificates = {};
    const withdrawals = {};
    const validFrom = body.validity_start_interval()
      ? parseInt(body.validity_start_interval()!.to_str())
      : "always";

    return {
      inputs,
      outputs,
      fee,
      validTo,
      certificates,
      withdrawals,
      validFrom,
    };
  }

  /** Return the transaction in Hex encoded Cbor */
  toString(): Transaction {
    return toHex(this.txComplete.to_bytes());
  }

  /** Return the transaction hash */
  toHash(): TxHash {
    return C.hash_transaction(this.txComplete.body()).to_hex();
  }
}
