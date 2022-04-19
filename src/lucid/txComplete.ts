import { C } from '../core/index.js';
import Core from 'core/types';
import { PrivateKey } from '../types/index.js';
import { Lucid } from './lucid.js';
import { TxSigned } from './txSigned.js';

export class TxComplete {
  txComplete: Core.Transaction;
  witnessSetBuilder: Core.TransactionWitnessSetBuilder;
  constructor(tx: Core.Transaction) {
    this.txComplete = tx;
    this.witnessSetBuilder = C.TransactionWitnessSetBuilder.new();
    this.witnessSetBuilder.add_existing(this.txComplete.witness_set());
  }
  async sign() {
    const witness = await Lucid.wallet.signTx(this.txComplete);
    this.witnessSetBuilder.add_existing(witness);
    return this;
  }

  /** Add an extra signature from a private key */
  signWithPrivateKey(privateKey: PrivateKey) {
    const priv = C.PrivateKey.from_bech32(privateKey);
    const witness = C.make_vkey_witness(
      C.hash_transaction(this.txComplete.body()),
      priv,
    );
    this.witnessSetBuilder.add_vkey(witness);
  }

  complete() {
    const signedTx = C.Transaction.new(
      this.txComplete.body(),
      this.witnessSetBuilder.build(),
      this.txComplete.auxiliary_data(),
    );
    return new TxSigned(signedTx);
  }
}
