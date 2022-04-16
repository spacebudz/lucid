import { TxHash } from '..';
import { Transaction } from '../../custom_modules/cardano-multiplatform-lib-browser/cardano_multiplatform_lib';
import { Lucid } from './lucid';

export class TxSigned {
  txSigned: Transaction;
  constructor(tx: Transaction) {
    this.txSigned = tx;
  }

  async submit(): Promise<TxHash> {
    return await Lucid.wallet.submitTx(this.txSigned);
  }
}
