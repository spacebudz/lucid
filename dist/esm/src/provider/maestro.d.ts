import { Address, Credential, Datum, DatumHash, Delegation, OutRef, ProtocolParameters, Provider, RewardAddress, Transaction, TxHash, Unit, UTxO } from "../types/mod.js";
export type MaestroSupportedNetworks = "Mainnet" | "Preprod" | "Preview";
export interface MaestroConfig {
    network: MaestroSupportedNetworks;
    apiKey: string;
    turboSubmit?: boolean;
}
export declare class Maestro implements Provider {
    url: string;
    apiKey: string;
    turboSubmit: boolean;
    constructor({ network, apiKey, turboSubmit }: MaestroConfig);
    getProtocolParameters(): Promise<ProtocolParameters>;
    private getUtxosInternal;
    getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]>;
    getUtxosWithUnit(addressOrCredential: Address | Credential, unit: Unit): Promise<UTxO[]>;
    getUtxoByUnit(unit: Unit): Promise<UTxO>;
    getUtxosByOutRef(outRefs: OutRef[]): Promise<UTxO[]>;
    getDelegation(rewardAddress: RewardAddress): Promise<Delegation>;
    getDatum(datumHash: DatumHash): Promise<Datum>;
    awaitTx(txHash: TxHash, checkInterval?: number): Promise<boolean>;
    submitTx(tx: Transaction): Promise<TxHash>;
    private commonHeaders;
    private maestroUtxoToUtxo;
    private getAllPagesData;
}
