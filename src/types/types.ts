import Core from 'core/types';
import { Construct } from 'utils/plutusData';
import { Blockfrost } from '../provider';

export type Provider = Blockfrost; // more providers can be added here

export type ProtocolParameters = {
  minFeeA: number;
  minFeeB: number;
  maxTxSize: number;
  maxValSize: number;
  keyDeposit: BigInt;
  poolDeposit: BigInt;
  priceMem: number;
  priceStep: number;
  coinsPerUtxoWord: BigInt;
};

export type Slot = number;

export interface ProviderSchema {
  getProtocolParameters(): Promise<ProtocolParameters>;
  getCurrentSlot(): Promise<Slot>;
  getUtxos(address: Address): Promise<UTxO[]>;
  getUtxosWithUnit?(address: Address, unit: Unit): Promise<UTxO[]>;
  getDatum?(datumHash: DatumHash): Promise<Datum>;
  awaitTx?(txHash: TxHash): Promise<boolean>;
  submitTx?(tx: Core.Transaction): Promise<TxHash>;
}

export type CredentialType = 'Key' | 'Script';
/** Concatenation of Policy Id and asset name in hex */
export type Unit = string;
export type Assets = {
  [unit: Unit]: BigInt;
};
export type ScriptType = 'Native' | 'Plutus';

/** Note: Plutus scripts need to be cbor encoded. Raw compiled script without the cbor encoding do not work.
 *
 * E.g. taking the cborHex coming from writeFileTextEnvelope works
 */
export type Script = string;

/** Hex */
export type PolicyId = string;

/** Note: Plutus scripts need to be cbor encoded. Raw compiled script without the cbor encoding do not work.
 *
 * E.g. taking the cborHex coming from writeFileTextEnvelope works
 */
export type MintingPolicy = {
  type: ScriptType;
  script: Script;
};
/** Note: Plutus scripts need to be cbor encoded. Raw compiled script without the cbor encoding do not work.
 *
 * E.g. taking the cborHex coming from writeFileTextEnvelope works
 */
export type SpendingValidator = {
  type: ScriptType;
  script: Script;
};
/** Note: Plutus scripts need to be cbor encoded. Raw compiled script without the cbor encoding do not work.
 *
 * E.g. taking the cborHex coming from writeFileTextEnvelope works
 */
export type CertificateValidator = {
  type: ScriptType;
  script: Script;
};
/** Note: Plutus scripts need to be cbor encoded. Raw compiled script without the cbor encoding do not work.
 *
 * E.g. taking the cborHex coming from writeFileTextEnvelope works
 */
export type WithdrawalValidator = {
  type: ScriptType;
  script: Script;
};
/** bech32 */
export type Address = string;
/** bech32 */
export type RewardAddress = string;
/** Hex */
export type PaymentKeyHash = string;
/** Hex */
export type StakeKeyHash = string;
/** Hex */
export type TxHash = string;
/** bech32 */
export type PoolId = string;
/** Hex */
export type Datum = string;
/** Hex */
export type DatumHash = string;
/** Hex (Redeemer is only PlutusData, same as Datum) */
export type Redeemer = string; // Plutus Data (same as Datum)
export type Lovelace = BigInt;
export type Label = number;

/** Hex */
export type RawUTxO = string;

/** bech32 */
export type PrivateKey = string;

export type UTxO = {
  txHash: TxHash;
  outputIndex: number;
  assets: Assets;
  address: Address;
  datumHash?: DatumHash;
  datum?: Datum; // some providers may be able to return the datum as well in an efficient way
};

export type AddressType = 'Base' | 'Enterprise' | 'Pointer' | 'Reward';

export type Network = 'Mainnet' | 'Testnet';

export type AddressDetailed = {
  type: AddressType;
  credentialType: CredentialType;
  address: string;
  paymentKeyHash?: PaymentKeyHash;
  stakeKeyHash?: StakeKeyHash;
};

/**
 * A wallet that can be constructed from external data
 * e.g UTxOs, collateral, and address
 */
export interface ExternalWallet {
  address: Address;
  utxos: RawUTxO[];
  collateral?: RawUTxO[];
  rewardAddress?: RewardAddress;
}

export interface Wallet {
  address: Address;
  rewardAddress?: RewardAddress;
  getCollateral(): Promise<UTxO[]>;
  getCollateralCore(): Promise<Core.TransactionUnspentOutput[]>;
  getUtxos(): Promise<UTxO[]>;
  getUtxosCore(): Promise<Core.TransactionUnspentOutputs>;
  signTx(tx: Core.Transaction): Promise<Core.TransactionWitnessSet>;
  submitTx(signedTx: Core.Transaction): Promise<TxHash>;
}

/** Represents an empty Datum or Redeemer */
export const EmptyData = '00' as Redeemer | Datum;

export type WalletProvider = 'nami' | 'eternl' | 'flint';

/** JSON object */
export type Json = any;

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
// TODO: Fix circular reference
// @ts-ignore
export type PlutusDataJS =
  | string
  | bigint
  | PlutusDataJS[]
  | Record<string, PlutusDataJS>
  | Construct; // We emulate the constr like this

/** Time in milliseconds */
export type UnixTime = number;
