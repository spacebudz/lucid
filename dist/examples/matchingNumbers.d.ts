import { Lovelace, TxHash } from '..';
export declare const lockUtxo: (number: number, lovelace: Lovelace) => Promise<TxHash>;
export declare const redeemUtxo: (number: number) => Promise<TxHash>;
