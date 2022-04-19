import Core from 'core/types';
import { TxHash } from '../types/index.js';
import { Lucid } from './lucid.js';

export class TxSigned {
  txSigned: Core.Transaction;
  constructor(tx: Core.Transaction) {
    this.txSigned = tx;
  }

  async submit(): Promise<TxHash> {
    return await Lucid.wallet.submitTx(this.txSigned);
  }
}
