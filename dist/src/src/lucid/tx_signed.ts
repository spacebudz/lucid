import { C } from "../core/mod.js";
import { Transaction, TxHash } from "../types/mod.js";
import { Lucid } from "./lucid.js";
import { toHex } from "../utils/mod.js";

export class TxSigned {
  txSigned: C.Transaction;
  private lucid: Lucid;
  constructor(lucid: Lucid, tx: C.Transaction) {
    this.lucid = lucid;
    this.txSigned = tx;
  }

  async submit(): Promise<TxHash> {
    return await (this.lucid.wallet || this.lucid.provider).submitTx(
      toHex(this.txSigned.to_bytes()),
    );
  }

  /** Returns the transaction in Hex encoded Cbor. */
  toString(): Transaction {
    return toHex(this.txSigned.to_bytes());
  }

  /** Return the transaction hash. */
  toHash(): TxHash {
    return C.hash_transaction(this.txSigned.body()).to_hex();
  }
}
