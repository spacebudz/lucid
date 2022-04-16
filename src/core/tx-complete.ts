import { S } from '.';
import { PrivateKey } from '..';
import {
  Transaction,
  TransactionWitnessSetBuilder,
} from '../../custom_modules/cardano-multiplatform-lib-browser/cardano_multiplatform_lib';
import { Lucid } from './lucid';
import { TxSigned } from './tx-signed';

export class TxComplete {
  txComplete: Transaction;
  witnessSetBuilder: TransactionWitnessSetBuilder;
  constructor(tx: Transaction) {
    this.txComplete = tx;
    this.witnessSetBuilder = S.TransactionWitnessSetBuilder.new();
    this.witnessSetBuilder.add_existing(this.txComplete.witness_set());
  }
  async sign() {
    const witness = await Lucid.wallet.signTx(this.txComplete);
    this.witnessSetBuilder.add_existing(witness);
    return this;
  }

  /** Add an extra signature from a private key */
  signWithPrivateKey(privateKey: PrivateKey) {
    const priv = S.PrivateKey.from_bech32(privateKey);
    const witness = S.make_vkey_witness(
      S.hash_transaction(this.txComplete.body()),
      priv,
    );
    this.witnessSetBuilder.add_vkey(witness);
  }

  complete() {
    const signedTx = S.Transaction.new(
      this.txComplete.body(),
      this.witnessSetBuilder.build(),
      this.txComplete.auxiliary_data(),
    );
    return new TxSigned(signedTx);
  }
}
