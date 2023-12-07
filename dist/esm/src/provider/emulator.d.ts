import { Address, Assets, Credential, Datum, DatumHash, Delegation, Lovelace, OutputData, OutRef, ProtocolParameters, Provider, RewardAddress, Transaction, TxHash, Unit, UnixTime, UTxO } from "../types/types.js";
/** Concatentation of txHash + outputIndex */
type FlatOutRef = string;
export declare class Emulator implements Provider {
    ledger: Record<FlatOutRef, {
        utxo: UTxO;
        spent: boolean;
    }>;
    mempool: Record<FlatOutRef, {
        utxo: UTxO;
        spent: boolean;
    }>;
    /**
     * Only stake key registrations/delegations and rewards are tracked.
     * Other certificates are not tracked.
     */
    chain: Record<RewardAddress, {
        registeredStake: boolean;
        delegation: Delegation;
    }>;
    blockHeight: number;
    slot: number;
    time: UnixTime;
    protocolParameters: ProtocolParameters;
    datumTable: Record<DatumHash, Datum>;
    constructor(accounts: {
        address: Address;
        assets: Assets;
        outputData?: OutputData;
    }[], protocolParameters?: ProtocolParameters);
    now(): UnixTime;
    awaitSlot(length?: number): void;
    awaitBlock(height?: number): void;
    getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]>;
    getProtocolParameters(): Promise<ProtocolParameters>;
    getDatum(datumHash: DatumHash): Promise<Datum>;
    getUtxosWithUnit(addressOrCredential: Address | Credential, unit: Unit): Promise<UTxO[]>;
    getUtxosByOutRef(outRefs: OutRef[]): Promise<UTxO[]>;
    getUtxoByUnit(unit: string): Promise<UTxO>;
    getDelegation(rewardAddress: RewardAddress): Promise<Delegation>;
    awaitTx(txHash: string): Promise<boolean>;
    /**
     * Emulates the behaviour of the reward distribution at epoch boundaries.
     * Stake keys need to be registered and delegated like on a real chain in order to receive rewards.
     */
    distributeRewards(rewards: Lovelace): void;
    submitTx(tx: Transaction): Promise<TxHash>;
    log(): void;
}
export {};
