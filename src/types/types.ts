import { C } from "../core/mod.ts";

type CostModel = Record<string, number>;

export type CostModels = Record<PlutusVersion, CostModel>;

export type ProtocolParameters = {
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

export type Slot = number;

export abstract class Provider {
  abstract getProtocolParameters(): Promise<ProtocolParameters>;
  /** Query UTxOs by address or payment credential. */
  abstract getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]>;
  /** Query UTxOs by address or payment credential filtered by a specific unit. */
  abstract getUtxosWithUnit(
    addressOrCredential: Address | Credential,
    unit: Unit,
  ): Promise<UTxO[]>;
  /** Query a UTxO by a unit. It needs to be an NFT (or optionally the entire supply in one UTxO). */
  abstract getUtxoByUnit(unit: Unit): Promise<UTxO>;
  /** Query UTxOs by the output reference (tx hash and index). */
  abstract getUtxosByOutRef(outRefs: Array<OutRef>): Promise<UTxO[]>;
  abstract getDelegation(rewardAddress: RewardAddress): Promise<Delegation>;
  abstract getDatum(datumHash: DatumHash): Promise<Datum>;
  abstract awaitTx(txHash: TxHash, checkInterval?: number): Promise<boolean>;
  abstract submitTx(tx: Transaction): Promise<TxHash>;
  abstract getUtxosByPolicyId(policyId: PolicyId): Promise<UTxO[]>;

  async getUtxosByOutRefWithDatum(outRefs: OutRef[]): Promise<UTxO[]> {
    const utxos = await this.getUtxosByOutRef(outRefs);
    return Promise.all(utxos.map(async (utxo) => {
      if (utxo.datumHash) {
        utxo.datum = await this.getDatum(utxo.datumHash);
      }
      return utxo;
    }));
  }
}

export type Credential = {
  type: "Key" | "Script";
  hash: KeyHash | ScriptHash;
};

/** Concatenation of policy id and asset name in Hex. */
export type Unit = string;
export type Assets = Record<Unit | "lovelace", bigint>;
export type ScriptType = "Native" | PlutusVersion;
export type PlutusVersion = "PlutusV1" | "PlutusV2";

/** Hex */
export type PolicyId = string;

export type Script = { type: ScriptType; script: string };

export type Validator =
  | MintingPolicy
  | SpendingValidator
  | CertificateValidator
  | WithdrawalValidator;

export type MintingPolicy = Script;
export type SpendingValidator = Script;
export type CertificateValidator = Script;
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
export type VrfKeyHash = string;
/** Hex */
export type ScriptHash = string;
/** Hex */
export type TxHash = string;
/** Bech32 */
export type PoolId = string;
/** Hex */
export type Datum = string;
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
export type OutputData = {
  hash?: DatumHash;
  asHash?: Datum;
  inline?: Datum;
  scriptRef?: Script;
};
/** Hex */
export type DatumHash = string;
/** Hex (Redeemer is only PlutusData, same as Datum) */
export type Redeemer = string; // Plutus Data (same as Datum)
export type Lovelace = bigint;
export type Label = number;
/** Hex */
export type TransactionWitnesses = string;
/** Hex */
export type Transaction = string;
/** Bech32 */
export type PrivateKey = string;
/** Bech32 */
export type PublicKey = string;
/** Hex */
export type ScriptRef = string;
/** Hex */
export type Payload = string;

export type UTxO = {
  txHash: TxHash;
  outputIndex: number;
  assets: Assets;
  address: Address;
  datumHash?: DatumHash | null;
  datum?: Datum | null;
  scriptRef?: Script | null;
};

export type OutRef = { txHash: TxHash; outputIndex: number };

export type AddressType =
  | "Base"
  | "Enterprise"
  | "Pointer"
  | "Reward"
  | "Byron";

export type Network = "Mainnet" | "Preview" | "Preprod" | "Custom";

export type AddressDetails = {
  type: AddressType;
  networkId: number;
  address: { bech32: Address; hex: string };
  paymentCredential?: Credential;
  stakeCredential?: Credential;
};

export type Delegation = {
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
}

export type SignedMessage = { signature: string; key: string };

export interface Wallet {
  address(): Promise<Address>;
  rewardAddress(): Promise<RewardAddress | null>;
  getUtxos(): Promise<UTxO[]>;
  getUtxosCore(): Promise<C.TransactionUnspentOutputs>;
  getDelegation(): Promise<Delegation>;
  signTx(tx: C.Transaction): Promise<C.TransactionWitnessSet>;
  signMessage(
    address: Address | RewardAddress,
    payload: Payload,
  ): Promise<SignedMessage>;
  submitTx(signedTx: Transaction): Promise<TxHash>;
}

/** JSON object */
// deno-lint-ignore no-explicit-any
export type Json = any;

/** Time in milliseconds */
export type UnixTime = number;

export type PoolParams = {
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

export type Relay = {
  type: "SingleHostIp" | "SingleHostDomainName" | "MultiHost";
  ipV4?: string;
  ipV6?: string;
  port?: number;
  domainName?: string;
};

export type NativeScript = {
  type: "sig" | "all" | "any" | "before" | "atLeast" | "after";
  keyHash?: KeyHash;
  required?: number;
  slot?: Slot;
  scripts?: NativeScript[];
};

export type SlotConfig = {
  zeroTime: UnixTime;
  zeroSlot: Slot;
  slotLength: number; // number of milliseconds.
};

export type Exact<T> = T extends infer U ? U : never;

export type Metadata = {
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
  444: Metadata["222"] & { decimals?: number };
};
