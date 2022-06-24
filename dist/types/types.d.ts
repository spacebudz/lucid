import { Core } from '../core';
import { Construct } from '../utils';
import { Blockfrost } from '../provider';
export declare type Provider = Blockfrost;
export declare type ProtocolParameters = {
    minFeeA: number;
    minFeeB: number;
    maxTxSize: number;
    maxValSize: number;
    keyDeposit: BigInt;
    poolDeposit: BigInt;
    priceMem: number;
    priceStep: number;
    coinsPerUtxoByte: BigInt;
    collateralPercentage: number;
    maxCollateralInputs: number;
};
export declare type Slot = number;
export interface ProviderSchema {
    getProtocolParameters(): Promise<ProtocolParameters>;
    getCurrentSlot(): Promise<Slot>;
    getUtxos(address: Address): Promise<UTxO[]>;
    getUtxosWithUnit?(address: Address, unit: Unit): Promise<UTxO[]>;
    getDatum?(datumHash: DatumHash): Promise<Datum>;
    awaitTx?(txHash: TxHash): Promise<boolean>;
    submitTx?(tx: Core.Transaction): Promise<TxHash>;
}
export declare type Credential = {
    type: 'Key' | 'Script';
    hash: KeyHash | ScriptHash;
};
/** Concatenation of Policy Id and asset name in hex */
export declare type Unit = string;
export declare type Assets = {
    [unit: string]: BigInt;
};
export declare type ScriptType = 'Native' | 'PlutusV1' | 'PlutusV2';
/** Note: Plutus scripts need to be cbor encoded. Raw compiled script without the cbor encoding do not work.
 *
 * E.g. taking the cborHex coming from writeFileTextEnvelope works
 */
export declare type Script = {
    type: ScriptType;
    script: string;
};
/** Hex */
export declare type PolicyId = string;
export declare type Validator = MintingPolicy | SpendingValidator | CertificateValidator | WithdrawalValidator;
/** Note: Plutus scripts need to be cbor encoded. Raw compiled script without the cbor encoding do not work.
 *
 * E.g. taking the cborHex coming from writeFileTextEnvelope works
 */
export declare type MintingPolicy = Script;
/** Note: Plutus scripts need to be cbor encoded. Raw compiled script without the cbor encoding do not work.
 *
 * E.g. taking the cborHex coming from writeFileTextEnvelope works
 */
export declare type SpendingValidator = Script;
/** Note: Plutus scripts need to be cbor encoded. Raw compiled script without the cbor encoding do not work.
 *
 * E.g. taking the cborHex coming from writeFileTextEnvelope works
 */
export declare type CertificateValidator = Script;
/** Note: Plutus scripts need to be cbor encoded. Raw compiled script without the cbor encoding do not work.
 *
 * E.g. taking the cborHex coming from writeFileTextEnvelope works
 */
export declare type WithdrawalValidator = Script;
/** bech32 */
export declare type Address = string;
/** bech32 */
export declare type RewardAddress = string;
/** Hex */
export declare type PaymentKeyHash = string;
/** Hex */
export declare type StakeKeyHash = string;
/** Hex */
export declare type KeyHash = string | PaymentKeyHash | StakeKeyHash;
/** Hex */
export declare type ScriptHash = string;
/** Hex */
export declare type TxHash = string;
/** bech32 */
export declare type PoolId = string;
/** Hex */
export declare type Datum = string;
/**
 * asHash will add the datum hash to the output and the datum to the witness set
 *
 * inline will add the datum to the output
 *
 * scriptRef will add any script to the output
 *
 * You can only specify asHash or inline, not both at the same time
 */
export declare type OutputData = {
    asHash?: Datum;
    inline?: Datum;
    scriptRef?: Script;
};
/** Hex */
export declare type DatumHash = string;
/** Hex (Redeemer is only PlutusData, same as Datum) */
export declare type Redeemer = string;
export declare type Lovelace = BigInt;
export declare type Label = number;
/** Hex */
export declare type TransactionWitnesses = string;
/** Hex */
export declare type Transaction = string;
/** bech32 */
export declare type PrivateKey = string;
/** Hex */
export declare type ScriptRef = string;
export declare type UTxO = {
    txHash: TxHash;
    outputIndex: number;
    assets: Assets;
    address: Address;
    datumHash?: DatumHash;
    datum?: Datum;
    scriptRef?: ScriptRef;
};
export declare type AddressType = {
    type: 'Base' | 'Enterprise' | 'Pointer' | 'Reward';
    address: Address;
};
export declare type Network = 'Mainnet' | 'Testnet';
export declare type AddressDetails = {
    address: AddressType;
    paymentCredential?: Credential;
    stakeCredential?: Credential;
};
/**
 * A wallet that can be constructed from external data
 * e.g UTxOs, collateral, and address
 */
export interface ExternalWallet {
    address: Address;
    utxos?: UTxO[];
    collateral?: UTxO[];
    rewardAddress?: RewardAddress;
}
export interface Wallet {
    address(): Promise<Address>;
    rewardAddress(): Promise<RewardAddress | undefined>;
    getCollateral(): Promise<UTxO[]>;
    getCollateralCore(): Promise<Core.TransactionUnspentOutput[]>;
    getUtxos(): Promise<UTxO[]>;
    getUtxosCore(): Promise<Core.TransactionUnspentOutputs>;
    signTx(tx: Core.Transaction): Promise<Core.TransactionWitnessSet>;
    submitTx(signedTx: Core.Transaction): Promise<TxHash>;
}
export declare type WalletProvider = 'nami' | 'eternl' | 'flint';
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
 * Note: We need to wrap it in an object to prevent circular references
 */
export declare type PlutusData = string | bigint | PlutusData[] | Map<PlutusData, PlutusData> | Construct;
/** JSON object */
export declare type Json = any;
/** Time in milliseconds */
export declare type UnixTime = number;
declare type NFTFile = {
    name: string;
    mediaType: string;
    src: string | string[];
};
export declare type NFTMetadataDetails = {
    name: string;
    image: string;
    mediaType?: string;
    description?: string | string[];
    files?: NFTFile[];
    [key: string]: any;
};
export declare type NFTMetadata = {
    [policyId: string]: {
        [assetName: string]: NFTMetadataDetails;
    };
    version?: any;
};
export {};
