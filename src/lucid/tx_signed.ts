import { C } from "../core/mod.ts";
import { Transaction, TxHash } from "../types/mod.ts";
import { Lucid } from "./lucid.ts";
import { toHex } from "../utils/mod.ts";

export class TxSigned {
  txSigned: C.Transaction;
  private lucid: Lucid;
  constructor(lucid: Lucid, tx: C.Transaction) {
    this.lucid = lucid;
    this.txSigned = tx;
  }

  async submit(): Promise<TxHash> {
    return await (this.lucid.wallet || this.lucid.provider).submitTx(
      toHex(this.txSigned.to_bytes())
    );
  }

  /** Returns the transaction in Hex encoded Cbor. */
  toString(): Transaction {
    return toHex(this.txSigned.to_bytes());
  }

  /** Return the transaction hash. */
  toHash(): TxHash {
    const hash = C.hash_transaction(this.txSigned.body());
    const txHash = hash.to_hex();
    hash.free();
    return txHash;
  }

  /** Since this object has WASM fields, we must use the free method to free the fields */
  free() {
    this.txSigned.free();
  }
}
