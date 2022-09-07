import { Core } from "../core/mod.ts";
import { Transaction, TxHash } from "../types/mod.ts";
import { Lucid } from "./lucid.ts";
import { toHex } from "../utils/mod.ts";

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

  /** Returns the transaction in Hex encoded Cbor */
  toString(): Transaction {
    return toHex(this.txSigned.to_bytes());
  }
}
