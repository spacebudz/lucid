import { Core } from '../core';
import { PrivateKey, TransactionWitnesses } from '../types';
import { Lucid } from './lucid';
import { TxSigned } from './txSigned';
export declare class TxComplete {
    txComplete: Core.Transaction;
    witnessSetBuilder: Core.TransactionWitnessSetBuilder;
    private tasks;
    private lucid;
    constructor(lucid: Lucid, tx: Core.Transaction);
    sign(): this;
    /** Add an extra signature from a private key */
    signWithPrivateKey(privateKey: PrivateKey): this;
    /**
     * Signs the transaction and returns the witnesses that were just made
     */
    partialSign(): Promise<TransactionWitnesses>;
    /**
     * Signs the transaction and returns the witnesses that were just made
     *
     * Add an extra signature from a private key */
    partialSignWithPrivateKey(privateKey: PrivateKey): TransactionWitnesses;
    /**
     * Signs the transaction with the given witnesses
     */
    assemble(witnesses: TransactionWitnesses[]): this;
    complete(): Promise<TxSigned>;
}
