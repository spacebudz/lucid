import { C } from "../core/mod.js";
import { Transaction, TxHash } from "../types/mod.js";
import { Lucid } from "./lucid.js";
export declare class TxSigned {
    txSigned: C.Transaction;
    private lucid;
    constructor(lucid: Lucid, tx: C.Transaction);
    submit(): Promise<TxHash>;
    /** Returns the transaction in Hex encoded Cbor. */
    toString(): Transaction;
    /** Return the transaction hash. */
    toHash(): TxHash;
}
