/* tslint:disable */
/* eslint-disable */
export function main(): void;
export type Network = "Mainnet" | "Preprod" | "Preview" | { Emulator: number };

export type Credential = { type: "Key"; hash: string } | { type: "Script"; hash: string };

export interface AddressDetailsInner {
    networkId: number;
    address: string;
    addressRaw: string;
    payment: Credential | undefined;
    delegation: Credential | undefined;
}

export type AddressDetails = ({ type: "Base" } & AddressDetailsInner) | ({ type: "Enterprise" } & AddressDetailsInner) | ({ type: "Reward" } & AddressDetailsInner) | ({ type: "Byron" } & AddressDetailsInner);

export type NativeScript = { type: "Sig"; keyHash: string } | { type: "Any"; scripts: NativeScript[] } | { type: "All"; scripts: NativeScript[] } | { type: "AtLeast"; required: number; scripts: NativeScript[] } | { type: "Before"; slot: number } | { type: "After"; slot: number };

export type DataJson = { int: bigint } | { bytes: string } | { list: DataJson[] } | { map: MapEntry[] } | { constructor: number; fields: DataJson[] };

export interface MapEntry {
    k: DataJson;
    v: DataJson;
}

export type Utxos = Utxo[];

export interface Utxo {
    txHash: string;
    outputIndex: number;
    address: string;
    assets: Assets;
    datumHash?: string;
    datum?: string;
    scriptRef?: Script;
}

export type Script = { type: "Native"; script: string } | { type: "PlutusV1"; script: string } | { type: "PlutusV2"; script: string } | { type: "PlutusV3"; script: string };

export type Assets = Record<string, bigint>;

export type Certificate = ({ type: "Delegation" } & Delegation) | { type: "StakeRegistration"; rewardAddress: string } | { type: "StakeDeregistration"; rewardAddress: string } | ({ type: "PoolRegistration" } & PoolRegistration) | ({ type: "PoolRetirement" } & PoolRetirement);

export interface Delegation {
    rewardAddress: string;
    variant: DelegVariant;
}

export type DelegVariant = "Abstain" | "NoConfidence" | { DRep: string } | { Pool: string };

export interface PoolRegistration {
    poolId: string;
    vrfKeyHash: string;
    pledge: number;
    cost: number;
    margin: number;
    rewardAddress: string;
    owners: string[];
    relays: Relay[];
    metadataUrl?: string;
    metadataHash?: string;
}

export type Relay = { type: "SingleHostIp"; ipV4: string | undefined; ipV6: string | undefined; port: number | undefined } | { type: "SingleHostDomainName"; domainName: string; port?: number } | { type: "MultiHost"; domainName: string };

export interface PoolRetirement {
    poolId: string;
    epoch: number;
}

export interface Withdrawal {
    rewardAddress: string;
    amount: number;
}

export type AuxMetadata = number | string | any | any;

export type Part = "Payment" | "Delegation";

export interface KeyDetails {
    privateKey: string;
    publicKey: string;
    credential: Credential;
}

export interface Staking {
    registered: boolean;
    rewards: number;
    poolId: string | undefined;
    drep: DRep | undefined;
}

export type DRep = "Abstain" | "NoConfidence" | { Id: string };

export type Instructions = Instruction[];

export type Instruction = { type: "CollectFrom"; utxos: Utxo[]; redeemer?: string } | { type: "ReadFrom"; utxos: Utxo[] } | { type: "Mint"; assets: Assets; redeemer?: string } | { type: "PayTo"; assets: Assets; address: string; datumVariant?: DatumVariant; scriptRef?: Script } | { type: "PayToContract"; assets: Assets; address: string; datumVariant: DatumVariant; scriptRef?: Script } | { type: "DelegateTo"; delegation: Delegation; redeemer?: string } | { type: "RegisterStake"; rewardAddress: string } | { type: "DeregisterStake"; rewardAddress: string; redeemer?: string } | ({ type: "RegisterPool" } & PoolRegistration) | ({ type: "UpdatePool" } & PoolRegistration) | ({ type: "RetirePool" } & PoolRetirement) | { type: "Withdraw"; withdrawal: Withdrawal; redeemer?: string } | { type: "AddSigner"; keyHash: string } | { type: "AddNetworkId"; id: number } | { type: "ValidFrom"; unixTime: number } | { type: "ValidTo"; unixTime: number } | { type: "AttachMetadata"; metadata: [number, AuxMetadata] } | { type: "AttachMetadataWithConversion"; metadata: [number, AuxMetadata] } | { type: "AttachScript"; script: Script } | ({ type: "WithChangeTo" } & Change) | { type: "WithoutCoinSelection" };

export interface Change {
    address: string;
    datumVariant?: DatumVariant;
}

export type DatumVariant = { Hash: string } | { AsHash: string } | { Inline: string };

export interface RelevantProtocolParameters {
    minFeeA: number;
    minFeeB: number;
    maxTxSize: number;
    maxValSize: number;
    keyDeposit: number;
    poolDeposit: number;
    priceMem: number;
    priceStep: number;
    maxTxExMem: number;
    maxTxExSteps: number;
    coinsPerUtxoByte: number;
    collateralPercentage: number;
    maxCollateralInputs: number;
    costModels: Record<string, number[]>;
    minfeeRefscriptCostPerByte: number;
}

export interface SignerResult {
    tx: string;
    witnessSet: string;
}

export class Addresses {
  private constructor();
  free(): void;
  static keyHashToCredential(hash: string): Credential;
  static scriptHashToCredential(hash: string): Credential;
  static scriptToCredential(script: Script): Credential;
  static scriptToAddress(network: Network, script: Script, delegation?: Credential | null): string;
  static scriptToRewardAddress(network: Network, script: Script): string;
  static scriptToDrep(script: Script): string;
  static credentialToDrep(credential: Credential): string;
  static drepToCredential(id: string): Credential;
  static credentialToAddress(network: Network, payment: Credential, delegation?: Credential | null): string;
  static credentialToRewardAddress(network: Network, delegation: Credential): string;
  static addressToCredential(address: string): Credential;
  static rewardAddressToCredential(address: string): Credential;
  /**
   * Address can be bech32 or hex encoded
   */
  static inspect(address: string): AddressDetails;
}
export class Codec {
  private constructor();
  free(): void;
  static encodeData(d: DataJson): string;
  static decodeData(s: string): DataJson;
  static encodeUtxo(utxo: Utxo): string;
  static decodeUtxo(s: string): Utxo;
  static encodeNativeScript(s: NativeScript): string;
}
export class Crypto {
  private constructor();
  free(): void;
  static privateKeyToDetails(key: string): KeyDetails;
  static seedToDetails(seed: string, index: number, part: Part): KeyDetails;
  static seedToXpub(seed: string, index: number): string;
  static xpubToPublicKey(xpub: string, part: Part): string;
  static generateSeed(): string;
  /**
   * generates extended ed25519 private key
   */
  static generatePrivateKey(): string;
  static sign(key: string, message: string): string;
  static verify(pubkey: string, message: string, signature: string): boolean;
}
export class EmulatorState {
  free(): void;
  constructor(time: number, utxos?: Utxos | null);
  validate(tx: string): string;
  getTime(): number;
  getDatum(hash: string): string | undefined;
  getStaking(address: string): Staking | undefined;
  getLedger(): Utxos;
  getMempool(): Utxos;
  /**
   * Emulates the behaviour of the reward distribution at epoch boundaries.
   */
  distributeRewards(rewards: bigint): void;
  awaitSlot(slot?: number | null): void;
  awaitBlock(height?: number | null): void;
}
export class Hasher {
  private constructor();
  free(): void;
  static hashData(data: string): string;
  static hashVrfKey(pubkey: string): string;
  static hashPublicKey(pubkey: string): string;
  static hashWithBlake2b224(s: string): string;
  static hashWithBlake2b256(s: string): string;
  static hashScript(script: Script): string;
  static hashTransaction(tx: string): string;
}
export class InstructionBuilder {
  free(): void;
  constructor(network: Network, pp: RelevantProtocolParameters, selection: Utxos, change: Change);
  commit(instructions: Instructions): InstructionSigner;
}
export class InstructionSigner {
  private constructor();
  free(): void;
  static fromTx(tx: string, utxos?: Utxos | null): InstructionSigner;
  /**
   * ed25519 private key hex or bech32 encoded
   */
  signWithKey(key: string): InstructionSigner;
  signWithSeed(seed: string, index: number): InstructionSigner;
  signWithWitness(witness: string): InstructionSigner;
  signWithWitnessSet(set: string): InstructionSigner;
  getPartialWitnessSet(): string;
  commit(): SignerResult;
}
export class Utils {
  private constructor();
  free(): void;
  static applyParamsToScript(params: string, script: string): string;
  static encodeBech32(hrp: string, data: string): string;
  static applySingleCborEncoding(script: string): string;
  static applyDoubleCborEncoding(script: string): string;
}
