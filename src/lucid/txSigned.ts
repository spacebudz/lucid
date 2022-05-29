import Core from 'core/types';
import { TxHash } from '../types';
import { Lucid } from './lucid';

export class TxSigned {
  txSigned: Core.Transaction;
  private lucid: Lucid;
  constructor(lucid: Lucid, tx: Core.Transaction) {
    this.lucid = lucid;
    this.txSigned = tx;
  }

  async submit(): Promise<TxHash> {
    return await this.lucid.wallet.submitTx(this.txSigned);
  }
}
