import { C } from '../core';
import Core from 'core/types';
import { PrivateKey } from '../types';
import { Lucid } from './lucid';
import { TxSigned } from './txSigned';

export class TxComplete {
  txComplete: Core.Transaction;
  witnessSetBuilder: Core.TransactionWitnessSetBuilder;
  /**
   * @private
   */
  tasks: Function[];

  constructor(tx: Core.Transaction) {
    this.txComplete = tx;
    this.witnessSetBuilder = C.TransactionWitnessSetBuilder.new();
    this.tasks = [];
  }
  sign() {
    this.tasks.push(async () => {
      const witness = await Lucid.wallet.signTx(this.txComplete);
      this.witnessSetBuilder.add_existing(witness);
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
    return new TxSigned(signedTx);
  }
}
