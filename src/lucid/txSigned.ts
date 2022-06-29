import { Core } from "../core/mod.ts";
import { TxHash } from "../types/mod.ts";
import { Lucid } from "./lucid.ts";

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
