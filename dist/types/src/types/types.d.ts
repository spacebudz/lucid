import { C } from "../core/mod.js";
declare type CostModel = Record<string, number>;
export declare type CostModels = Record<PlutusVersion, CostModel>;
export interface Configuration {
    enableChangeSplitting: boolean;
    changeNativeAssetChunkSize: number;
    changeMinUtxo: string;
    changeCollateral: string;
}
export declare type ProtocolParameters = {
    minFeeA: number;
    minFeeB: number;
    maxTxSize: number;
    maxValSize: number;
    keyDeposit: bigint;
    poolDeposit: bigint;
    priceMem: number;
    priceStep: number;
    maxTxExMem: bigint;
    maxTxExSteps: bigint;
    coinsPerUtxoByte: bigint;
    collateralPercentage: number;
    maxCollateralInputs: number;
    costModels: CostModels;
};
export declare type Slot = number;
export interface Provider {
    getProtocolParameters(): Promise<ProtocolParameters>;
    /** Query UTxOs by address or payment credential. */
    getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]>;
    /** Query UTxOs by address or payment credential filtered by a specific unit. */
    getUtxosWithUnit(addressOrCredential: Address | Credential, unit: Unit): Promise<UTxO[]>;
    /** Query a UTxO by a unit. It needs to be an NFT (or optionally the entire supply in one UTxO). */
    getUtxoByUnit(unit: Unit): Promise<UTxO>;
    /** Query UTxOs by the output reference (tx hash and index). */
    getUtxosByOutRef(outRefs: Array<OutRef>): Promise<UTxO[]>;
    getDelegation(rewardAddress: RewardAddress): Promise<Delegation>;
    getDatum(datumHash: DatumHash): Promise<Datum>;
    awaitTx(txHash: TxHash, checkInterval?: number): Promise<boolean>;
    submitTx(tx: Transaction): Promise<TxHash>;
}
export declare type Credential = {
    type: "Key" | "Script";
    hash: KeyHash | ScriptHash;
};
/** Concatenation of policy id and asset name in Hex. */
export declare type Unit = string;
export declare type Assets = Record<Unit | "lovelace", bigint>;
export declare type ScriptType = "Native" | PlutusVersion;
export declare type PlutusVersion = "PlutusV1" | "PlutusV2";
/** Hex */
export declare type PolicyId = string;
export declare type Script = {
    type: ScriptType;
    script: string;
};
export declare type Validator = MintingPolicy | SpendingValidator | CertificateValidator | WithdrawalValidator;
export declare type MintingPolicy = Script;
export declare type SpendingValidator = Script;
export declare type CertificateValidator = Script;
export declare type WithdrawalValidator = Script;
/** Bech32 */
export declare type Address = string;
/** Bech32 */
export declare type RewardAddress = string;
/** Hex */
export declare type PaymentKeyHash = string;
/** Hex */
export declare type StakeKeyHash = string;
/** Hex */
export declare type KeyHash = string | PaymentKeyHash | StakeKeyHash;
/** Hex */
export declare type VrfKeyHash = string;
/** Hex */
export declare type ScriptHash = string;
/** Hex */
export declare type TxHash = string;
/** Bech32 */
export declare type PoolId = string;
/** Hex */
export declare type Datum = string;
/**
 * **hash** adds the datum hash to the output.
 *
 * **asHash** hashes the datum and adds the datum hash to the output and the datum to the witness set.
 *
 * **inline** adds the datum to the output.
 *
 * **scriptRef** will add any script to the output.
 *
 * You can either specify **hash**, **asHash** or **inline**, only one option is allowed.
 */
export declare type OutputData = {
    hash?: DatumHash;
    asHash?: Datum;
    inline?: Datum;
    scriptRef?: Script;
};
/** Hex */
export declare type DatumHash = string;
/** Hex (Redeemer is only PlutusData, same as Datum) */
export declare type Redeemer = string;
export declare type Lovelace = bigint;
export declare type Label = number;
/** Hex */
export declare type TransactionWitnesses = string;
/** Hex */
export declare type Transaction = string;
/** Bech32 */
export declare type PrivateKey = string;
/** Bech32 */
export declare type PublicKey = string;
/** Hex */
export declare type ScriptRef = string;
/** Hex */
export declare type Payload = string;
export declare type UTxO = {
    txHash: TxHash;
    outputIndex: number;
    assets: Assets;
    address: Address;
    datumHash?: DatumHash | null;
    datum?: Datum | null;
    scriptRef?: Script | null;
};
export declare type OutRef = {
    txHash: TxHash;
    outputIndex: number;
};
export declare type AddressType = "Base" | "Enterprise" | "Pointer" | "Reward" | "Byron";
export declare type Network = "Mainnet" | "Preview" | "Preprod" | "Custom";
export declare type AddressDetails = {
    type: AddressType;
    networkId: number;
    address: {
        bech32: Address;
        hex: string;
    };
    paymentCredential?: Credential;
    stakeCredential?: Credential;
};
export declare type Delegation = {
    poolId: PoolId | null;
    rewards: Lovelace;
};
/**
 * A wallet that can be constructed from external data e.g utxos and an address.
 * It doesn't allow you to sign transactions/messages. This needs to be handled separately.
 */
export interface ExternalWallet {
    address: Address;
    utxos?: UTxO[];
    rewardAddress?: RewardAddress;
    collateral?: UTxO[];
}
export declare type SignedMessage = {
    signature: string;
    key: string;
};
export interface Wallet {
    address(): Promise<Address>;
    rewardAddress(): Promise<RewardAddress | null>;
    getUtxos(): Promise<UTxO[]>;
    getCollateralCore(): C.TransactionUnspentOutputs | undefined;
    getUtxosCore(): Promise<C.TransactionUnspentOutputs>;
    getDelegation(): Promise<Delegation>;
    signTx(tx: C.Transaction): Promise<C.TransactionWitnessSet>;
    signMessage(address: Address | RewardAddress, payload: Payload): Promise<SignedMessage>;
    submitTx(signedTx: Transaction): Promise<TxHash>;
}
/** JSON object */
export declare type Json = any;
/**
 * These are the arguments that conform a BuiltinData in Plutus:
 *
 * ```hs
 * data Data =
 *   Constr Integer [Data]
 * | Map [(Data, Data)]
 * | List [Data]
 * | I Integer
 * | B BS.ByteString
 *   deriving stock (Show, Eq, Ord, Generic)
 *   deriving anyclass (NFData)
 * ```
 * So we can define an arbitrary mapping for these types
 *
 *```
 * bigint -> I
 * string -> B
 * Map    -> Map
 * list   -> List
 * ```
 *

/** Time in milliseconds */
export declare type UnixTime = number;
export declare type PoolParams = {
    poolId: PoolId;
    vrfKeyHash: VrfKeyHash;
    pledge: Lovelace;
    cost: Lovelace;
    margin: number;
    rewardAddress: RewardAddress;
    owners: Array<RewardAddress>;
    relays: Array<Relay>;
    metadataUrl?: string;
};
export declare type Relay = {
    type: "SingleHostIp" | "SingleHostDomainName" | "MultiHost";
    ipV4?: string;
    ipV6?: string;
    port?: number;
    domainName?: string;
};
export declare type NativeScript = {
    type: "sig" | "all" | "any" | "before" | "atLeast" | "after";
    keyHash?: KeyHash;
    required?: number;
    slot?: Slot;
    scripts?: NativeScript[];
};
export declare type SlotConfig = {
    zeroTime: UnixTime;
    zeroSlot: Slot;
    slotLength: number;
};
export declare type Exact<T> = T extends infer U ? U : never;
export declare type Metadata = {
    222: {
        name: string;
        image: string;
        mediaType?: string;
        description?: string;
        files?: {
            name?: string;
            mediaType: string;
            src: string;
        }[];
        [key: string]: Json;
    };
    333: {
        name: string;
        description: string;
        ticker?: string;
        url?: string;
        logo?: string;
        decimals?: number;
        [key: string]: Json;
    };
    444: Metadata["222"] & {
        decimals?: number;
    };
};
export {};
