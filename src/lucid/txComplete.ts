import Core from 'core/types';
import { PrivateKey } from '../types';
import { Lucid } from './lucid';
import { TxSigned } from './txSigned';

export class TxComplete {
  txComplete: Core.Transaction;
  witnessSetBuilder: Core.TransactionWitnessSetBuilder;
  C: typeof Core;

  constructor(tx: Core.Transaction) {
    this.txComplete = tx;
    this.witnessSetBuilder = Lucid.C.TransactionWitnessSetBuilder.new();
    this.witnessSetBuilder.add_existing(this.txComplete.witness_set());
    this.C = Lucid.C;
  }

  async sign() {
    const witness = await Lucid.wallet.signTx(this.txComplete);
    this.witnessSetBuilder.add_existing(witness);
    return this;
  }

  /** Add an extra signature from a private key */
  signWithPrivateKey(privateKey: PrivateKey) {
    const priv = this.C.PrivateKey.from_bech32(privateKey);
    const witness = this.C.make_vkey_witness(
      this.C.hash_transaction(this.txComplete.body()),
      priv,
    );
    this.witnessSetBuilder.add_vkey(witness);
  }

  complete() {
    const signedTx = this.C.Transaction.new(
      this.txComplete.body(),
      this.witnessSetBuilder.build(),
      this.txComplete.auxiliary_data(),
    );
    return new TxSigned(signedTx);
  }
}
