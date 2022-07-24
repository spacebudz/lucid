import { Core } from "../core/mod.ts";
import { Construct } from "../utils/mod.ts";

export type ProtocolParameters = {
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

export type Slot = number;

export interface Provider {
  data: { url: string; projectId: string; [key: string]: unknown };
  getProtocolParameters(): Promise<ProtocolParameters>;
  getUtxos(address: Address): Promise<UTxO[]>;
  getCurrentSlot(): Promise<Slot>;
  getUtxosWithUnit(address: Address, unit: Unit): Promise<UTxO[]>;
  getDatum(datumHash: DatumHash): Promise<Datum>;
  awaitTx(txHash: TxHash): Promise<boolean>;
  submitTx(tx: Core.Transaction): Promise<TxHash>;
}

export type Credential = {
  type: "Key" | "Script";
  hash: KeyHash | ScriptHash;
};

/** Concatenation of Policy Id and asset name in Hex */
export type Unit = string;
export type Assets = {
  [unit: string]: BigInt;
};
export type ScriptType = "Native" | "PlutusV1" | "PlutusV2";

/** Note: Plutus scripts need to be CBOR encoded. Raw compiled script without the CBOR encoding do not work.
 *
 * E.g. taking in the CBOR Hex string coming from writeFileTextEnvelope (Haskell) works
 */
export type Script = { type: ScriptType; script: string };

/** Hex */
export type PolicyId = string;

export type Validator =
  | MintingPolicy
  | SpendingValidator
  | CertificateValidator
  | WithdrawalValidator;

/** Note: Plutus scripts need to be CBOR encoded. Raw compiled script without the CBOR encoding do not work.
 *
 * E.g. taking the CBOR Hex string coming from writeFileTextEnvelope works
 */
export type MintingPolicy = Script;
/** Note: Plutus scripts need to be CBOR encoded. Raw compiled script without the CBOR encoding do not work.
 *
 * E.g. taking the CBOR Hex string coming from writeFileTextEnvelope works
 */
export type SpendingValidator = Script;
/** Note: Plutus scripts need to be cbor encoded. Raw compiled script without the CBOR encoding do not work.
 *
 * E.g. taking the CBOR Hex string coming from writeFileTextEnvelope works
 */
export type CertificateValidator = Script;
/** Note: Plutus scripts need to be cbor encoded. Raw compiled script without the CBOR encoding do not work.
 *
 * E.g. taking the CBOR Hex string coming from writeFileTextEnvelope works
 */
export type WithdrawalValidator = Script;
/** Bech32 */
export type Address = string;
/** Bech32 */
export type RewardAddress = string;
/** Hex */
export type PaymentKeyHash = string;
/** Hex */
export type StakeKeyHash = string;
/** Hex */
export type KeyHash = string | PaymentKeyHash | StakeKeyHash;
/** Hex */
export type ScriptHash = string;
/** Hex */
export type TxHash = string;
/** Bech32 */
export type PoolId = string;
/** Hex */
export type Datum = string;
/**
 * asHash will add the datum hash to the output and the datum to the witness set
 *
 * inline will add the datum to the output
 *
 * scriptRef will add any script to the output
 *
 * You can only specify asHash or inline, not both at the same time
 */
export type OutputData = { asHash?: Datum; inline?: Datum; scriptRef?: Script };
/** Hex */
export type DatumHash = string;
/** Hex (Redeemer is only PlutusData, same as Datum) */
export type Redeemer = string; // Plutus Data (same as Datum)
export type Lovelace = BigInt;
export type Label = number;
/** Hex */
export type TransactionWitnesses = string;
/** Hex */
export type Transaction = string;
/** Bech32 */
export type PrivateKey = string;
/** Hex */
export type ScriptRef = string;

export type UTxO = {
  txHash: TxHash;
  outputIndex: number;
  assets: Assets;
  address: Address;
  datumHash?: DatumHash;
  datum?: Datum;
  scriptRef?: ScriptRef;
};

export type AddressType = {
  type: "Base" | "Enterprise" | "Pointer" | "Reward";
  address: Address;
};

export type Network = "Mainnet" | "Testnet";

export type AddressDetails = {
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
 * ```
 * bigint -> I
 * string -> B
 * Map    -> Map
 * list   -> List
 * ```
 *
 * Note: We need to wrap it in an object to prevent circular references
 */

export type PlutusData =
  | bytes
  | bigint
  | Array<PlutusData>
  | Map<PlutusData, PlutusData>
  | Construct; // We emulate the constr like this

type bytes = string | Uint8Array;

/** JSON object */
// deno-lint-ignore no-explicit-any
export type Json = any;

/** Time in milliseconds */
export type UnixTime = number;

type NFTFile = {
  name: string;
  mediaType: string;
  src: string | string[];
};

export type NFTMetadataDetails = {
  name: string;
  image: string;
  mediaType?: string;
  description?: string | string[];
  files?: NFTFile[];
  [key: string]: unknown;
};

export type NFTMetadata = {
  [policyId: string]: {
    [assetName: string]: NFTMetadataDetails;
  };
  //@ts-ignore: Cannot use map of strings and version as number in a type def
  version?: number; // number
};
