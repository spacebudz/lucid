import { C } from '../core';
import Core from 'core/types';
import { PrivateKey, TransactionWitnesses } from '../types';
import { Lucid } from './lucid';
import { TxSigned } from './txSigned';
import { fromHex, toHex } from '../utils';

export class TxComplete {
  txComplete: Core.Transaction;
  witnessSetBuilder: Core.TransactionWitnessSetBuilder;
  private tasks: Function[];
  private lucid: Lucid;

  constructor(lucid: Lucid, tx: Core.Transaction) {
    this.lucid = lucid;
    this.txComplete = tx;
    this.witnessSetBuilder = C.TransactionWitnessSetBuilder.new();
    this.tasks = [];
  }
  sign() {
    this.tasks.push(async () => {
      const witnesses = await this.lucid.wallet.signTx(this.txComplete);
      this.witnessSetBuilder.add_existing(witnesses);
    });
    return this;
  }

  /** Add an extra signature from a private key */
  signWithPrivateKey(privateKey: PrivateKey) {
    const priv = C.PrivateKey.from_bech32(privateKey);
    const witness = C.make_vkey_witness(
      C.hash_transaction(this.txComplete.body()),
      priv
    );
    this.witnessSetBuilder.add_vkey(witness);
    return this;
  }

  /**
   * Signs the transaction and returns the witnesses that were just made
   */
  async partialSign(): Promise<TransactionWitnesses> {
    const witnesses = await this.lucid.wallet.signTx(this.txComplete);
    this.witnessSetBuilder.add_existing(witnesses);
    return toHex(witnesses.to_bytes());
  }

  /**
   * Signs the transaction and returns the witnesses that were just made
   *
   * Add an extra signature from a private key */
  partialSignWithPrivateKey(privateKey: PrivateKey): TransactionWitnesses {
    const priv = C.PrivateKey.from_bech32(privateKey);
    const witness = C.make_vkey_witness(
      C.hash_transaction(this.txComplete.body()),
      priv
    );
    this.witnessSetBuilder.add_vkey(witness);
    const witnesses = C.TransactionWitnessSetBuilder.new();
    witnesses.add_vkey(witness);
    return toHex(witnesses.build().to_bytes());
  }

  /**
   * Signs the transaction with the given witnesses
   */
  assemble(witnesses: TransactionWitnesses[]) {
    witnesses.forEach(witness => {
      const witnessParsed = C.TransactionWitnessSet.from_bytes(
        fromHex(witness)
      );
      this.witnessSetBuilder.add_existing(witnessParsed);
    });
    return this;
  }

  async complete() {
    for (const task of this.tasks) {
      await task();
    }

    this.witnessSetBuilder.add_existing(this.txComplete.witness_set());
    const signedTx = C.Transaction.new(
      this.txComplete.body(),
      this.witnessSetBuilder.build(),
      this.txComplete.auxiliary_data()
    );
    return new TxSigned(this.lucid, signedTx);
  }
}
