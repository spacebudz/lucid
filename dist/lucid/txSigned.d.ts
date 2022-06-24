import { Core } from '../core';
import { TxHash } from '../types';
import { Lucid } from './lucid';
export declare class TxSigned {
    txSigned: Core.Transaction;
    private lucid;
    constructor(lucid: Lucid, tx: Core.Transaction);
    submit(): Promise<TxHash>;
}
