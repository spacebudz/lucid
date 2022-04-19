import Core from 'core/types';
import { TxHash } from '../types';
import { Lucid } from './lucid';

export class TxSigned {
  txSigned: Core.Transaction;
  constructor(tx: Core.Transaction) {
    this.txSigned = tx;
  }

  async submit(): Promise<TxHash> {
    return await Lucid.wallet.submitTx(this.txSigned);
  }
}
