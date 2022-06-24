import { Core } from '../core';
import { Address, Datum, DatumHash, ProtocolParameters, ProviderSchema, Slot, TxHash, Unit, UTxO } from '../types';
export declare class Blockfrost implements ProviderSchema {
    url: string;
    projectId: string;
    constructor(url: string, projectId: string);
    getProtocolParameters(): Promise<ProtocolParameters>;
    getCurrentSlot(): Promise<Slot>;
    getUtxos(address: string): Promise<UTxO[]>;
    getUtxosWithUnit(address: Address, unit: Unit): Promise<UTxO[]>;
    getDatum(datumHash: DatumHash): Promise<Datum>;
    awaitTx(txHash: TxHash): Promise<boolean>;
    submitTx(tx: Core.Transaction): Promise<TxHash>;
}
/** This function is temporarily needed only, until Blockfrost returns the datum natively in cbor
 *
 * The conversion is ambigious, that's why it's better to get the datum directly in cbor
 */
export declare const datumJsonToCbor: (json: any) => Datum;
