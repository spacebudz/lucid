import {
  TransactionUnspentOutput,
  Transaction,
  TransactionWitnessSet,
  TransactionUnspentOutputs,
} from '../custom_modules/cardano-multiplatform-lib-browser/cardano_multiplatform_lib';

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
  getUtxosWithUnit(address: Address, unit: Unit): Promise<UTxO[]>;
  // getDatum(datumHash: DatumHash): Promise<Datum>;
  awaitTx(txHash: TxHash): Promise<boolean>;
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

export interface Wallet {
  isBrowserWallet?: boolean;
  address: Address;
  rewardAddress?: RewardAddress;
  getCollateral(): Promise<UTxO[]>;
  getCollateralRaw(): Promise<TransactionUnspentOutput[]>;
  getUtxos(): Promise<UTxO[]>;
  getUtxosRaw(): Promise<TransactionUnspentOutputs>;
  signTx(tx: Transaction): Promise<TransactionWitnessSet>;
  submitTx(signedTx: Transaction): Promise<TxHash>;
}

/** Represents an empty Datum or Redeemer */
export const EmptyData = '00' as Redeemer | Datum;

export type WalletProvider = 'nami' | 'eternl' | 'flint';

/** JSON object */
export type Json = any;
