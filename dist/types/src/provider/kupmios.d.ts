import { Address, Credential, Datum, DatumHash, Delegation, OutRef, ProtocolParameters, Provider, RewardAddress, Transaction, TxHash, Unit, UTxO } from "../types/mod.js";
export declare class Kupmios implements Provider {
    kupoUrl: string;
    ogmiosUrl: string;
    /**
     * @param kupoUrl: http(s)://localhost:1442
     * @param ogmiosUrl: ws(s)://localhost:1337
     */
    constructor(kupoUrl: string, ogmiosUrl: string);
    getProtocolParameters(): Promise<ProtocolParameters>;
    getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]>;
    getUtxosWithUnit(addressOrCredential: Address | Credential, unit: Unit): Promise<UTxO[]>;
    getUtxoByUnit(unit: Unit): Promise<UTxO>;
    getUtxosByOutRef(outRefs: Array<OutRef>): Promise<UTxO[]>;
    getDelegation(rewardAddress: RewardAddress): Promise<Delegation>;
    getDatum(datumHash: DatumHash): Promise<Datum>;
    awaitTx(txHash: TxHash, checkInterval?: number): Promise<boolean>;
    submitTx(tx: Transaction): Promise<TxHash>;
    private kupmiosUtxosToUtxos;
    private ogmiosWsp;
}
