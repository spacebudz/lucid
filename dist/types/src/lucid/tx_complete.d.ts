import { C } from "../core/mod.js";
import { PrivateKey, Transaction, TransactionWitnesses, TxHash } from "../types/mod.js";
import { Lucid } from "./lucid.js";
import { TxSigned } from "./tx_signed.js";
export declare class TxComplete {
    txComplete: C.Transaction;
    witnessSetBuilder: C.TransactionWitnessSetBuilder;
    private tasks;
    private lucid;
    fee: number;
    exUnits: {
        cpu: number;
        mem: number;
    } | null;
    constructor(lucid: Lucid, tx: C.Transaction);
    sign(): TxComplete;
    /** Add an extra signature from a private key. */
    signWithPrivateKey(privateKey: PrivateKey): TxComplete;
    /** Sign the transaction and return the witnesses that were just made. */
    partialSign(): Promise<TransactionWitnesses>;
    /**
     * Sign the transaction and return the witnesses that were just made.
     * Add an extra signature from a private key.
     */
    partialSignWithPrivateKey(privateKey: PrivateKey): TransactionWitnesses;
    /** Sign the transaction with the given witnesses. */
    assemble(witnesses: TransactionWitnesses[]): TxComplete;
    complete(): Promise<TxSigned>;
    /** Return the transaction in Hex encoded Cbor. */
    toString(): Transaction;
    /** Return the transaction hash. */
    toHash(): TxHash;
}
