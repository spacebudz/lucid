import { Address, Credential, Datum, DatumHash, Delegation, OutRef, ProtocolParameters, Provider, RewardAddress, Transaction, TxHash, Unit, UTxO } from "../types/mod.js";
export declare class Blockfrost implements Provider {
    url: string;
    projectId: string;
    constructor(url: string, projectId?: string);
    getProtocolParameters(): Promise<ProtocolParameters>;
    getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]>;
    getUtxosWithUnit(addressOrCredential: Address | Credential, unit: Unit): Promise<UTxO[]>;
    getUtxoByUnit(unit: Unit): Promise<UTxO>;
    getUtxosByOutRef(outRefs: OutRef[]): Promise<UTxO[]>;
    getDelegation(rewardAddress: RewardAddress): Promise<Delegation>;
    getDatum(datumHash: DatumHash): Promise<Datum>;
    awaitTx(txHash: TxHash, checkInterval?: number): Promise<boolean>;
    submitTx(tx: Transaction): Promise<TxHash>;
    private blockfrostUtxosToUtxos;
}
/**
 * This function is temporarily needed only, until Blockfrost returns the datum natively in Cbor.
 * The conversion is ambigious, that's why it's better to get the datum directly in Cbor.
 */
export declare function datumJsonToCbor(json: DatumJson): Datum;
type DatumJson = {
    int?: number;
    bytes?: string;
    list?: Array<DatumJson>;
    map?: Array<{
        k: unknown;
        v: unknown;
    }>;
    fields?: Array<DatumJson>;
    [constructor: string]: unknown;
};
export {};
