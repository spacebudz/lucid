/* tslint:disable */
/* eslint-disable */
/**
 * @param {Transaction} tx
 * @param {LinearFee} linear_fee
 * @param {ExUnitPrices} ex_unit_prices
 * @returns {BigNum}
 */
export function min_fee(
  tx: Transaction,
  linear_fee: LinearFee,
  ex_unit_prices: ExUnitPrices,
): BigNum;
/**
 * @param {Uint8Array} bytes
 * @returns {TransactionMetadatum}
 */
export function encode_arbitrary_bytes_as_metadatum(
  bytes: Uint8Array,
): TransactionMetadatum;
/**
 * @param {TransactionMetadatum} metadata
 * @returns {Uint8Array}
 */
export function decode_arbitrary_bytes_from_metadatum(
  metadata: TransactionMetadatum,
): Uint8Array;
/**
 * @param {string} json
 * @param {number} schema
 * @returns {TransactionMetadatum}
 */
export function encode_json_str_to_metadatum(
  json: string,
  schema: number,
): TransactionMetadatum;
/**
 * @param {TransactionMetadatum} metadatum
 * @param {number} schema
 * @returns {string}
 */
export function decode_metadatum_to_json_str(
  metadatum: TransactionMetadatum,
  schema: number,
): string;
/**
 * @param {string} password
 * @param {string} salt
 * @param {string} nonce
 * @param {string} data
 * @returns {string}
 */
export function encrypt_with_password(
  password: string,
  salt: string,
  nonce: string,
  data: string,
): string;
/**
 * @param {string} password
 * @param {string} data
 * @returns {string}
 */
export function decrypt_with_password(password: string, data: string): string;
/**
 * @param {PlutusList} params
 * @param {PlutusScript} plutus_script
 * @returns {PlutusScript}
 */
export function apply_params_to_plutus_script(
  params: PlutusList,
  plutus_script: PlutusScript,
): PlutusScript;
/**
 * @param {TransactionHash} tx_body_hash
 * @param {ByronAddress} addr
 * @param {LegacyDaedalusPrivateKey} key
 * @returns {BootstrapWitness}
 */
export function make_daedalus_bootstrap_witness(
  tx_body_hash: TransactionHash,
  addr: ByronAddress,
  key: LegacyDaedalusPrivateKey,
): BootstrapWitness;
/**
 * @param {TransactionHash} tx_body_hash
 * @param {ByronAddress} addr
 * @param {Bip32PrivateKey} key
 * @returns {BootstrapWitness}
 */
export function make_icarus_bootstrap_witness(
  tx_body_hash: TransactionHash,
  addr: ByronAddress,
  key: Bip32PrivateKey,
): BootstrapWitness;
/**
 * @param {TransactionHash} tx_body_hash
 * @param {PrivateKey} sk
 * @returns {Vkeywitness}
 */
export function make_vkey_witness(
  tx_body_hash: TransactionHash,
  sk: PrivateKey,
): Vkeywitness;
/**
 * @param {AuxiliaryData} auxiliary_data
 * @returns {AuxiliaryDataHash}
 */
export function hash_auxiliary_data(
  auxiliary_data: AuxiliaryData,
): AuxiliaryDataHash;
/**
 * @param {TransactionBody} tx_body
 * @returns {TransactionHash}
 */
export function hash_transaction(tx_body: TransactionBody): TransactionHash;
/**
 * @param {PlutusData} plutus_data
 * @returns {DataHash}
 */
export function hash_plutus_data(plutus_data: PlutusData): DataHash;
/**
 * @param {Uint8Array} data
 * @returns {Uint8Array}
 */
export function hash_blake2b256(data: Uint8Array): Uint8Array;
/**
 * @param {Uint8Array} data
 * @returns {Uint8Array}
 */
export function hash_blake2b224(data: Uint8Array): Uint8Array;
/**
 * @param {Redeemers} redeemers
 * @param {Costmdls} cost_models
 * @param {PlutusList | undefined} datums
 * @returns {ScriptDataHash}
 */
export function hash_script_data(
  redeemers: Redeemers,
  cost_models: Costmdls,
  datums?: PlutusList,
): ScriptDataHash;
/**
 * @param {TransactionBody} txbody
 * @param {BigNum} pool_deposit
 * @param {BigNum} key_deposit
 * @returns {Value}
 */
export function get_implicit_input(
  txbody: TransactionBody,
  pool_deposit: BigNum,
  key_deposit: BigNum,
): Value;
/**
 * @param {TransactionBody} txbody
 * @param {BigNum} pool_deposit
 * @param {BigNum} key_deposit
 * @returns {BigNum}
 */
export function get_deposit(
  txbody: TransactionBody,
  pool_deposit: BigNum,
  key_deposit: BigNum,
): BigNum;
/**
 * @param {TransactionOutput} output
 * @param {BigNum} coins_per_utxo_byte
 * @returns {BigNum}
 */
export function min_ada_required(
  output: TransactionOutput,
  coins_per_utxo_byte: BigNum,
): BigNum;
/**
 * Receives a script JSON string
 * and returns a NativeScript.
 * Cardano Wallet and Node styles are supported.
 *
 * * wallet: https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/api/swagger.yaml
 * * node: https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md
 *
 * self_xpub is expected to be a Bip32PublicKey as hex-encoded bytes
 * @param {string} json
 * @param {string} self_xpub
 * @param {number} schema
 * @returns {NativeScript}
 */
export function encode_json_str_to_native_script(
  json: string,
  self_xpub: string,
  schema: number,
): NativeScript;
/**
 * @param {string} json
 * @param {number} schema
 * @returns {PlutusData}
 */
export function encode_json_str_to_plutus_datum(
  json: string,
  schema: number,
): PlutusData;
/**
 * @param {PlutusData} datum
 * @param {number} schema
 * @returns {string}
 */
export function decode_plutus_datum_to_json_str(
  datum: PlutusData,
  schema: number,
): string;
/** */
export enum CertificateKind {
  StakeRegistration,
  StakeDeregistration,
  StakeDelegation,
  PoolRegistration,
  PoolRetirement,
  GenesisKeyDelegation,
  MoveInstantaneousRewardsCert,
}
/** */
export enum MIRPot {
  Reserves,
  Treasury,
}
/** */
export enum MIRKind {
  ToOtherPot,
  ToStakeCredentials,
}
/** */
export enum RelayKind {
  SingleHostAddr,
  SingleHostName,
  MultiHostName,
}
/** */
export enum NativeScriptKind {
  ScriptPubkey,
  ScriptAll,
  ScriptAny,
  ScriptNOfK,
  TimelockStart,
  TimelockExpiry,
}
/** */
export enum NetworkIdKind {
  Testnet,
  Mainnet,
}
/** */
export enum TransactionMetadatumKind {
  MetadataMap,
  MetadataList,
  Int,
  Bytes,
  Text,
}
/** */
export enum MetadataJsonSchema {
  NoConversions,
  BasicConversions,
  DetailedSchema,
}
/** */
export enum StakeCredKind {
  Key,
  Script,
}
/** */
export enum ScriptWitnessKind {
  NativeWitness,
  PlutusWitness,
}
/**
 * Each new language uses a different namespace for hashing its script
 * This is because you could have a language where the same bytes have different semantics
 * So this avoids scripts in different languages mapping to the same hash
 * Note that the enum value here is different than the enum value for deciding the cost model of a script
 * https://github.com/input-output-hk/cardano-ledger/blob/9c3b4737b13b30f71529e76c5330f403165e28a6/eras/alonzo/impl/src/Cardano/Ledger/Alonzo.hs#L127
 */
export enum ScriptHashNamespace {
  NativeScript,
  PlutusV1,
  PlutusV2,
}
/**
 * Used to choose the schema for a script JSON string
 */
export enum ScriptSchema {
  Wallet,
  Node,
}
/** */
export enum LanguageKind {
  PlutusV1,
  PlutusV2,
}
/** */
export enum PlutusDataKind {
  ConstrPlutusData,
  Map,
  List,
  Integer,
  Bytes,
}
/** */
export enum RedeemerTagKind {
  Spend,
  Mint,
  Cert,
  Reward,
}
/**
 * JSON <-> PlutusData conversion schemas.
 * Follows ScriptDataJsonSchema in cardano-cli defined at:
 * https://github.com/input-output-hk/cardano-node/blob/master/cardano-api/src/Cardano/Api/ScriptData.hs#L254
 *
 * All methods here have the following restrictions due to limitations on dependencies:
 * * JSON numbers above u64::MAX (positive) or below i64::MIN (negative) will throw errors
 * * Hex strings for bytes don't accept odd-length (half-byte) strings.
 *      cardano-cli seems to support these however but it seems to be different than just 0-padding
 *      on either side when tested so proceed with caution
 */
export enum PlutusDatumSchema {
  /**
   * ScriptDataJsonNoSchema in cardano-node.
   *
   * This is the format used by --script-data-value in cardano-cli
   * This tries to accept most JSON but does not support the full spectrum of Plutus datums.
   * From JSON:
   * * null/true/false/floats NOT supported
   * * strings starting with 0x are treated as hex bytes. All other strings are encoded as their utf8 bytes.
   * To JSON:
   * * ConstrPlutusData not supported in ANY FORM (neither keys nor values)
   * * Lists not supported in keys
   * * Maps not supported in keys
   */
  BasicConversions,
  /**
   * ScriptDataJsonDetailedSchema in cardano-node.
   *
   * This is the format used by --script-data-file in cardano-cli
   * This covers almost all (only minor exceptions) Plutus datums, but the JSON must conform to a strict schema.
   * The schema specifies that ALL keys and ALL values must be contained in a JSON map with 2 cases:
   * 1. For ConstrPlutusData there must be two fields "constructor" contianing a number and "fields" containing its fields
   *    e.g. { "constructor": 2, "fields": [{"int": 2}, {"list": [{"bytes": "CAFEF00D"}]}]}
   * 2. For all other cases there must be only one field named "int", "bytes", "list" or "map"
   *    Integer's value is a JSON number e.g. {"int": 100}
   *    Bytes' value is a hex string representing the bytes WITHOUT any prefix e.g. {"bytes": "CAFEF00D"}
   *    Lists' value is a JSON list of its elements encoded via the same schema e.g. {"list": [{"bytes": "CAFEF00D"}]}
   *    Maps' value is a JSON list of objects, one for each key-value pair in the map, with keys "k" and "v"
   *          respectively with their values being the plutus datum encoded via this same schema
   *          e.g. {"map": [
   *              {"k": {"int": 2}, "v": {"int": 5}},
   *              {"k": {"map": [{"k": {"list": [{"int": 1}]}, "v": {"bytes": "FF03"}}]}, "v": {"list": []}}
   *          ]}
   * From JSON:
   * * null/true/false/floats NOT supported
   * * the JSON must conform to a very specific schema
   * To JSON:
   * * all Plutus datums should be fully supported outside of the integer range limitations outlined above.
   */
  DetailedSchema,
}
/** */
export enum ScriptKind {
  NativeScript,
  PlutusScriptV1,
  PlutusScriptV2,
}
/** */
export enum DatumKind {
  Hash,
  Data,
}
/** */
export class Address {
  free(): void;
  /**
   * @param {Uint8Array} data
   * @returns {Address}
   */
  static from_bytes(data: Uint8Array): Address;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {AddressJSON}
   */
  to_js_value(): AddressJSON;
  /**
   * @param {string} json
   * @returns {Address}
   */
  static from_json(json: string): Address;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string | undefined} prefix
   * @returns {string}
   */
  to_bech32(prefix?: string): string;
  /**
   * @param {string} bech_str
   * @returns {Address}
   */
  static from_bech32(bech_str: string): Address;
  /**
   * @returns {number}
   */
  network_id(): number;
  /**
   * @returns {ByronAddress | undefined}
   */
  as_byron(): ByronAddress | undefined;
  /**
   * @returns {RewardAddress | undefined}
   */
  as_reward(): RewardAddress | undefined;
  /**
   * @returns {PointerAddress | undefined}
   */
  as_pointer(): PointerAddress | undefined;
  /**
   * @returns {EnterpriseAddress | undefined}
   */
  as_enterprise(): EnterpriseAddress | undefined;
  /**
   * @returns {BaseAddress | undefined}
   */
  as_base(): BaseAddress | undefined;
}
/** */
export class AssetName {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {AssetName}
   */
  static from_bytes(bytes: Uint8Array): AssetName;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {AssetNameJSON}
   */
  to_js_value(): AssetNameJSON;
  /**
   * @param {string} json
   * @returns {AssetName}
   */
  static from_json(json: string): AssetName;
  /**
   * @param {Uint8Array} name
   * @returns {AssetName}
   */
  static new(name: Uint8Array): AssetName;
  /**
   * @returns {Uint8Array}
   */
  name(): Uint8Array;
}
/** */
export class AssetNames {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {AssetNames}
   */
  static from_bytes(bytes: Uint8Array): AssetNames;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {AssetNamesJSON}
   */
  to_js_value(): AssetNamesJSON;
  /**
   * @param {string} json
   * @returns {AssetNames}
   */
  static from_json(json: string): AssetNames;
  /**
   * @returns {AssetNames}
   */
  static new(): AssetNames;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {AssetName}
   */
  get(index: number): AssetName;
  /**
   * @param {AssetName} elem
   */
  add(elem: AssetName): void;
}
/** */
export class Assets {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Assets}
   */
  static from_bytes(bytes: Uint8Array): Assets;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {AssetsJSON}
   */
  to_js_value(): AssetsJSON;
  /**
   * @param {string} json
   * @returns {Assets}
   */
  static from_json(json: string): Assets;
  /**
   * @returns {Assets}
   */
  static new(): Assets;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {AssetName} key
   * @param {BigNum} value
   * @returns {BigNum | undefined}
   */
  insert(key: AssetName, value: BigNum): BigNum | undefined;
  /**
   * @param {AssetName} key
   * @returns {BigNum | undefined}
   */
  get(key: AssetName): BigNum | undefined;
  /**
   * @returns {AssetNames}
   */
  keys(): AssetNames;
}
/** */
export class AuxiliaryData {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {AuxiliaryData}
   */
  static from_bytes(bytes: Uint8Array): AuxiliaryData;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {AuxiliaryDataJSON}
   */
  to_js_value(): AuxiliaryDataJSON;
  /**
   * @param {string} json
   * @returns {AuxiliaryData}
   */
  static from_json(json: string): AuxiliaryData;
  /**
   * @returns {AuxiliaryData}
   */
  static new(): AuxiliaryData;
  /**
   * @returns {GeneralTransactionMetadata | undefined}
   */
  metadata(): GeneralTransactionMetadata | undefined;
  /**
   * @param {GeneralTransactionMetadata} metadata
   */
  set_metadata(metadata: GeneralTransactionMetadata): void;
  /**
   * @returns {NativeScripts | undefined}
   */
  native_scripts(): NativeScripts | undefined;
  /**
   * @param {NativeScripts} native_scripts
   */
  set_native_scripts(native_scripts: NativeScripts): void;
  /**
   * @returns {PlutusScripts | undefined}
   */
  plutus_scripts(): PlutusScripts | undefined;
  /**
   * @param {PlutusScripts} plutus_scripts
   */
  set_plutus_scripts(plutus_scripts: PlutusScripts): void;
  /**
   * @param {PlutusScripts} plutus_scripts
   */
  set_plutus_v2_scripts(plutus_scripts: PlutusScripts): void;
}
/** */
export class AuxiliaryDataHash {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {AuxiliaryDataHash}
   */
  static from_bytes(bytes: Uint8Array): AuxiliaryDataHash;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string} prefix
   * @returns {string}
   */
  to_bech32(prefix: string): string;
  /**
   * @param {string} bech_str
   * @returns {AuxiliaryDataHash}
   */
  static from_bech32(bech_str: string): AuxiliaryDataHash;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} hex
   * @returns {AuxiliaryDataHash}
   */
  static from_hex(hex: string): AuxiliaryDataHash;
}
/** */
export class AuxiliaryDataSet {
  free(): void;
  /**
   * @returns {AuxiliaryDataSet}
   */
  static new(): AuxiliaryDataSet;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {BigNum} tx_index
   * @param {AuxiliaryData} data
   * @returns {AuxiliaryData | undefined}
   */
  insert(tx_index: BigNum, data: AuxiliaryData): AuxiliaryData | undefined;
  /**
   * @param {BigNum} tx_index
   * @returns {AuxiliaryData | undefined}
   */
  get(tx_index: BigNum): AuxiliaryData | undefined;
  /**
   * @returns {TransactionIndexes}
   */
  indices(): TransactionIndexes;
}
/** */
export class BaseAddress {
  free(): void;
  /**
   * @param {number} network
   * @param {StakeCredential} payment
   * @param {StakeCredential} stake
   * @returns {BaseAddress}
   */
  static new(
    network: number,
    payment: StakeCredential,
    stake: StakeCredential,
  ): BaseAddress;
  /**
   * @returns {StakeCredential}
   */
  payment_cred(): StakeCredential;
  /**
   * @returns {StakeCredential}
   */
  stake_cred(): StakeCredential;
  /**
   * @returns {Address}
   */
  to_address(): Address;
  /**
   * @param {Address} addr
   * @returns {BaseAddress | undefined}
   */
  static from_address(addr: Address): BaseAddress | undefined;
}
/** */
export class BigInt {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {BigInt}
   */
  static from_bytes(bytes: Uint8Array): BigInt;
  /**
   * @returns {BigNum | undefined}
   */
  as_u64(): BigNum | undefined;
  /**
   * @returns {Int | undefined}
   */
  as_int(): Int | undefined;
  /**
   * @param {string} text
   * @returns {BigInt}
   */
  static from_str(text: string): BigInt;
  /**
   * @returns {string}
   */
  to_str(): string;
}
/** */
export class BigNum {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {BigNum}
   */
  static from_bytes(bytes: Uint8Array): BigNum;
  /**
   * @param {string} string
   * @returns {BigNum}
   */
  static from_str(string: string): BigNum;
  /**
   * @returns {string}
   */
  to_str(): string;
  /**
   * @returns {BigNum}
   */
  static zero(): BigNum;
  /**
   * @returns {boolean}
   */
  is_zero(): boolean;
  /**
   * @param {BigNum} other
   * @returns {BigNum}
   */
  checked_mul(other: BigNum): BigNum;
  /**
   * @param {BigNum} other
   * @returns {BigNum}
   */
  checked_add(other: BigNum): BigNum;
  /**
   * @param {BigNum} other
   * @returns {BigNum}
   */
  checked_sub(other: BigNum): BigNum;
  /**
   * @param {BigNum} other
   * @returns {BigNum}
   */
  checked_div(other: BigNum): BigNum;
  /**
   * @param {BigNum} other
   * @returns {BigNum}
   */
  checked_div_ceil(other: BigNum): BigNum;
  /**
   * returns 0 if it would otherwise underflow
   * @param {BigNum} other
   * @returns {BigNum}
   */
  clamped_sub(other: BigNum): BigNum;
  /**
   * @param {BigNum} rhs_value
   * @returns {number}
   */
  compare(rhs_value: BigNum): number;
}
/** */
export class Bip32PrivateKey {
  free(): void;
  /**
   * derive this private key with the given index.
   *
   * # Security considerations
   *
   * * hard derivation index cannot be soft derived with the public key
   *
   * # Hard derivation vs Soft derivation
   *
   * If you pass an index below 0x80000000 then it is a soft derivation.
   * The advantage of soft derivation is that it is possible to derive the
   * public key too. I.e. derivation the private key with a soft derivation
   * index and then retrieving the associated public key is equivalent to
   * deriving the public key associated to the parent private key.
   *
   * Hard derivation index does not allow public key derivation.
   *
   * This is why deriving the private key should not fail while deriving
   * the public key may fail (if the derivation index is invalid).
   * @param {number} index
   * @returns {Bip32PrivateKey}
   */
  derive(index: number): Bip32PrivateKey;
  /**
   * 128-byte xprv a key format in Cardano that some software still uses or requires
   * the traditional 96-byte xprv is simply encoded as
   * prv | chaincode
   * however, because some software may not know how to compute a public key from a private key,
   * the 128-byte inlines the public key in the following format
   * prv | pub | chaincode
   * so be careful if you see the term "xprv" as it could refer to either one
   * our library does not require the pub (instead we compute the pub key when needed)
   * @param {Uint8Array} bytes
   * @returns {Bip32PrivateKey}
   */
  static from_128_xprv(bytes: Uint8Array): Bip32PrivateKey;
  /**
   * see from_128_xprv
   * @returns {Uint8Array}
   */
  to_128_xprv(): Uint8Array;
  /**
   * @returns {Bip32PrivateKey}
   */
  static generate_ed25519_bip32(): Bip32PrivateKey;
  /**
   * @returns {PrivateKey}
   */
  to_raw_key(): PrivateKey;
  /**
   * @returns {Bip32PublicKey}
   */
  to_public(): Bip32PublicKey;
  /**
   * @param {Uint8Array} bytes
   * @returns {Bip32PrivateKey}
   */
  static from_bytes(bytes: Uint8Array): Bip32PrivateKey;
  /**
   * @returns {Uint8Array}
   */
  as_bytes(): Uint8Array;
  /**
   * @param {string} bech32_str
   * @returns {Bip32PrivateKey}
   */
  static from_bech32(bech32_str: string): Bip32PrivateKey;
  /**
   * @returns {string}
   */
  to_bech32(): string;
  /**
   * @param {Uint8Array} entropy
   * @param {Uint8Array} password
   * @returns {Bip32PrivateKey}
   */
  static from_bip39_entropy(
    entropy: Uint8Array,
    password: Uint8Array,
  ): Bip32PrivateKey;
  /**
   * @returns {Uint8Array}
   */
  chaincode(): Uint8Array;
}
/** */
export class Bip32PublicKey {
  free(): void;
  /**
   * derive this public key with the given index.
   *
   * # Errors
   *
   * If the index is not a soft derivation index (< 0x80000000) then
   * calling this method will fail.
   *
   * # Security considerations
   *
   * * hard derivation index cannot be soft derived with the public key
   *
   * # Hard derivation vs Soft derivation
   *
   * If you pass an index below 0x80000000 then it is a soft derivation.
   * The advantage of soft derivation is that it is possible to derive the
   * public key too. I.e. derivation the private key with a soft derivation
   * index and then retrieving the associated public key is equivalent to
   * deriving the public key associated to the parent private key.
   *
   * Hard derivation index does not allow public key derivation.
   *
   * This is why deriving the private key should not fail while deriving
   * the public key may fail (if the derivation index is invalid).
   * @param {number} index
   * @returns {Bip32PublicKey}
   */
  derive(index: number): Bip32PublicKey;
  /**
   * @returns {PublicKey}
   */
  to_raw_key(): PublicKey;
  /**
   * @param {Uint8Array} bytes
   * @returns {Bip32PublicKey}
   */
  static from_bytes(bytes: Uint8Array): Bip32PublicKey;
  /**
   * @returns {Uint8Array}
   */
  as_bytes(): Uint8Array;
  /**
   * @param {string} bech32_str
   * @returns {Bip32PublicKey}
   */
  static from_bech32(bech32_str: string): Bip32PublicKey;
  /**
   * @returns {string}
   */
  to_bech32(): string;
  /**
   * @returns {Uint8Array}
   */
  chaincode(): Uint8Array;
}
/** */
export class Block {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Block}
   */
  static from_bytes(bytes: Uint8Array): Block;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {BlockJSON}
   */
  to_js_value(): BlockJSON;
  /**
   * @param {string} json
   * @returns {Block}
   */
  static from_json(json: string): Block;
  /**
   * @returns {Header}
   */
  header(): Header;
  /**
   * @returns {TransactionBodies}
   */
  transaction_bodies(): TransactionBodies;
  /**
   * @returns {TransactionWitnessSets}
   */
  transaction_witness_sets(): TransactionWitnessSets;
  /**
   * @returns {AuxiliaryDataSet}
   */
  auxiliary_data_set(): AuxiliaryDataSet;
  /**
   * @returns {TransactionIndexes}
   */
  invalid_transactions(): TransactionIndexes;
  /**
   * @param {Header} header
   * @param {TransactionBodies} transaction_bodies
   * @param {TransactionWitnessSets} transaction_witness_sets
   * @param {AuxiliaryDataSet} auxiliary_data_set
   * @param {TransactionIndexes} invalid_transactions
   * @returns {Block}
   */
  static new(
    header: Header,
    transaction_bodies: TransactionBodies,
    transaction_witness_sets: TransactionWitnessSets,
    auxiliary_data_set: AuxiliaryDataSet,
    invalid_transactions: TransactionIndexes,
  ): Block;
}
/** */
export class BlockHash {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {BlockHash}
   */
  static from_bytes(bytes: Uint8Array): BlockHash;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string} prefix
   * @returns {string}
   */
  to_bech32(prefix: string): string;
  /**
   * @param {string} bech_str
   * @returns {BlockHash}
   */
  static from_bech32(bech_str: string): BlockHash;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} hex
   * @returns {BlockHash}
   */
  static from_hex(hex: string): BlockHash;
}
/** */
export class Blockfrost {
  free(): void;
  /**
   * @param {string} url
   * @param {string} project_id
   * @returns {Blockfrost}
   */
  static new(url: string, project_id: string): Blockfrost;
  /**
   * @returns {string}
   */
  url(): string;
  /**
   * @returns {string}
   */
  project_id(): string;
}
/** */
export class BootstrapWitness {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {BootstrapWitness}
   */
  static from_bytes(bytes: Uint8Array): BootstrapWitness;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {BootstrapWitnessJSON}
   */
  to_js_value(): BootstrapWitnessJSON;
  /**
   * @param {string} json
   * @returns {BootstrapWitness}
   */
  static from_json(json: string): BootstrapWitness;
  /**
   * @returns {Vkey}
   */
  vkey(): Vkey;
  /**
   * @returns {Ed25519Signature}
   */
  signature(): Ed25519Signature;
  /**
   * @returns {Uint8Array}
   */
  chain_code(): Uint8Array;
  /**
   * @returns {Uint8Array}
   */
  attributes(): Uint8Array;
  /**
   * @param {Vkey} vkey
   * @param {Ed25519Signature} signature
   * @param {Uint8Array} chain_code
   * @param {Uint8Array} attributes
   * @returns {BootstrapWitness}
   */
  static new(
    vkey: Vkey,
    signature: Ed25519Signature,
    chain_code: Uint8Array,
    attributes: Uint8Array,
  ): BootstrapWitness;
}
/** */
export class BootstrapWitnesses {
  free(): void;
  /**
   * @returns {BootstrapWitnesses}
   */
  static new(): BootstrapWitnesses;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {BootstrapWitness}
   */
  get(index: number): BootstrapWitness;
  /**
   * @param {BootstrapWitness} elem
   */
  add(elem: BootstrapWitness): void;
}
/** */
export class ByronAddress {
  free(): void;
  /**
   * @returns {string}
   */
  to_base58(): string;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ByronAddress}
   */
  static from_bytes(bytes: Uint8Array): ByronAddress;
  /**
   * returns the byron protocol magic embedded in the address, or mainnet id if none is present
   * note: for bech32 addresses, you need to use network_id instead
   * @returns {number}
   */
  byron_protocol_magic(): number;
  /**
   * @returns {Uint8Array}
   */
  attributes(): Uint8Array;
  /**
   * @returns {number}
   */
  network_id(): number;
  /**
   * @param {string} s
   * @returns {ByronAddress}
   */
  static from_base58(s: string): ByronAddress;
  /**
   * @param {Bip32PublicKey} key
   * @param {number} protocol_magic
   * @returns {ByronAddress}
   */
  static icarus_from_key(
    key: Bip32PublicKey,
    protocol_magic: number,
  ): ByronAddress;
  /**
   * @param {string} s
   * @returns {boolean}
   */
  static is_valid(s: string): boolean;
  /**
   * @returns {Address}
   */
  to_address(): Address;
  /**
   * @param {Address} addr
   * @returns {ByronAddress | undefined}
   */
  static from_address(addr: Address): ByronAddress | undefined;
}
/** */
export class Certificate {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Certificate}
   */
  static from_bytes(bytes: Uint8Array): Certificate;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {CertificateJSON}
   */
  to_js_value(): CertificateJSON;
  /**
   * @param {string} json
   * @returns {Certificate}
   */
  static from_json(json: string): Certificate;
  /**
   * @param {StakeRegistration} stake_registration
   * @returns {Certificate}
   */
  static new_stake_registration(
    stake_registration: StakeRegistration,
  ): Certificate;
  /**
   * @param {StakeDeregistration} stake_deregistration
   * @returns {Certificate}
   */
  static new_stake_deregistration(
    stake_deregistration: StakeDeregistration,
  ): Certificate;
  /**
   * @param {StakeDelegation} stake_delegation
   * @returns {Certificate}
   */
  static new_stake_delegation(stake_delegation: StakeDelegation): Certificate;
  /**
   * @param {PoolRegistration} pool_registration
   * @returns {Certificate}
   */
  static new_pool_registration(
    pool_registration: PoolRegistration,
  ): Certificate;
  /**
   * @param {PoolRetirement} pool_retirement
   * @returns {Certificate}
   */
  static new_pool_retirement(pool_retirement: PoolRetirement): Certificate;
  /**
   * @param {GenesisKeyDelegation} genesis_key_delegation
   * @returns {Certificate}
   */
  static new_genesis_key_delegation(
    genesis_key_delegation: GenesisKeyDelegation,
  ): Certificate;
  /**
   * @param {MoveInstantaneousRewardsCert} move_instantaneous_rewards_cert
   * @returns {Certificate}
   */
  static new_move_instantaneous_rewards_cert(
    move_instantaneous_rewards_cert: MoveInstantaneousRewardsCert,
  ): Certificate;
  /**
   * @returns {number}
   */
  kind(): number;
  /**
   * @returns {StakeRegistration | undefined}
   */
  as_stake_registration(): StakeRegistration | undefined;
  /**
   * @returns {StakeDeregistration | undefined}
   */
  as_stake_deregistration(): StakeDeregistration | undefined;
  /**
   * @returns {StakeDelegation | undefined}
   */
  as_stake_delegation(): StakeDelegation | undefined;
  /**
   * @returns {PoolRegistration | undefined}
   */
  as_pool_registration(): PoolRegistration | undefined;
  /**
   * @returns {PoolRetirement | undefined}
   */
  as_pool_retirement(): PoolRetirement | undefined;
  /**
   * @returns {GenesisKeyDelegation | undefined}
   */
  as_genesis_key_delegation(): GenesisKeyDelegation | undefined;
  /**
   * @returns {MoveInstantaneousRewardsCert | undefined}
   */
  as_move_instantaneous_rewards_cert():
    | MoveInstantaneousRewardsCert
    | undefined;
}
/** */
export class Certificates {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Certificates}
   */
  static from_bytes(bytes: Uint8Array): Certificates;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {CertificatesJSON}
   */
  to_js_value(): CertificatesJSON;
  /**
   * @param {string} json
   * @returns {Certificates}
   */
  static from_json(json: string): Certificates;
  /**
   * @returns {Certificates}
   */
  static new(): Certificates;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {Certificate}
   */
  get(index: number): Certificate;
  /**
   * @param {Certificate} elem
   */
  add(elem: Certificate): void;
}
/** */
export class ConstrPlutusData {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ConstrPlutusData}
   */
  static from_bytes(bytes: Uint8Array): ConstrPlutusData;
  /**
   * @returns {BigNum}
   */
  alternative(): BigNum;
  /**
   * @returns {PlutusList}
   */
  data(): PlutusList;
  /**
   * @param {BigNum} alternative
   * @param {PlutusList} data
   * @returns {ConstrPlutusData}
   */
  static new(alternative: BigNum, data: PlutusList): ConstrPlutusData;
}
/** */
export class CostModel {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {CostModel}
   */
  static from_bytes(bytes: Uint8Array): CostModel;
  /**
   * @returns {CostModel}
   */
  static new(): CostModel;
  /**
   * @returns {CostModel}
   */
  static new_plutus_v2(): CostModel;
  /**
   * @param {number} operation
   * @param {Int} cost
   * @returns {Int}
   */
  set(operation: number, cost: Int): Int;
  /**
   * @param {number} operation
   * @returns {Int}
   */
  get(operation: number): Int;
  /**
   * @returns {number}
   */
  len(): number;
}
/** */
export class Costmdls {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Costmdls}
   */
  static from_bytes(bytes: Uint8Array): Costmdls;
  /**
   * @returns {Costmdls}
   */
  static new(): Costmdls;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {Language} key
   * @param {CostModel} value
   * @returns {CostModel | undefined}
   */
  insert(key: Language, value: CostModel): CostModel | undefined;
  /**
   * @param {Language} key
   * @returns {CostModel | undefined}
   */
  get(key: Language): CostModel | undefined;
  /**
   * @returns {Languages}
   */
  keys(): Languages;
}
/** */
export class DNSRecordAorAAAA {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {DNSRecordAorAAAA}
   */
  static from_bytes(bytes: Uint8Array): DNSRecordAorAAAA;
  /**
   * @param {string} dns_name
   * @returns {DNSRecordAorAAAA}
   */
  static new(dns_name: string): DNSRecordAorAAAA;
  /**
   * @returns {string}
   */
  record(): string;
}
/** */
export class DNSRecordSRV {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {DNSRecordSRV}
   */
  static from_bytes(bytes: Uint8Array): DNSRecordSRV;
  /**
   * @param {string} dns_name
   * @returns {DNSRecordSRV}
   */
  static new(dns_name: string): DNSRecordSRV;
  /**
   * @returns {string}
   */
  record(): string;
}
/** */
export class Data {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Data}
   */
  static from_bytes(bytes: Uint8Array): Data;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {DataJSON}
   */
  to_js_value(): DataJSON;
  /**
   * @param {string} json
   * @returns {Data}
   */
  static from_json(json: string): Data;
  /**
   * @param {PlutusData} plutus_data
   * @returns {Data}
   */
  static new(plutus_data: PlutusData): Data;
  /**
   * @returns {PlutusData}
   */
  get(): PlutusData;
}
/** */
export class DataHash {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {DataHash}
   */
  static from_bytes(bytes: Uint8Array): DataHash;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string} prefix
   * @returns {string}
   */
  to_bech32(prefix: string): string;
  /**
   * @param {string} bech_str
   * @returns {DataHash}
   */
  static from_bech32(bech_str: string): DataHash;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} hex
   * @returns {DataHash}
   */
  static from_hex(hex: string): DataHash;
}
/** */
export class Datum {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Datum}
   */
  static from_bytes(bytes: Uint8Array): Datum;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {DatumJSON}
   */
  to_js_value(): DatumJSON;
  /**
   * @param {string} json
   * @returns {Datum}
   */
  static from_json(json: string): Datum;
  /**
   * @param {DataHash} data_hash
   * @returns {Datum}
   */
  static new_data_hash(data_hash: DataHash): Datum;
  /**
   * @param {Data} data
   * @returns {Datum}
   */
  static new_data(data: Data): Datum;
  /**
   * @returns {number}
   */
  kind(): number;
  /**
   * @returns {DataHash | undefined}
   */
  as_data_hash(): DataHash | undefined;
  /**
   * @returns {Data | undefined}
   */
  as_data(): Data | undefined;
}
/** */
export class Ed25519KeyHash {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {Ed25519KeyHash}
   */
  static from_bytes(bytes: Uint8Array): Ed25519KeyHash;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string} prefix
   * @returns {string}
   */
  to_bech32(prefix: string): string;
  /**
   * @param {string} bech_str
   * @returns {Ed25519KeyHash}
   */
  static from_bech32(bech_str: string): Ed25519KeyHash;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} hex
   * @returns {Ed25519KeyHash}
   */
  static from_hex(hex: string): Ed25519KeyHash;
}
/** */
export class Ed25519KeyHashes {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Ed25519KeyHashes}
   */
  static from_bytes(bytes: Uint8Array): Ed25519KeyHashes;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {Ed25519KeyHashesJSON}
   */
  to_js_value(): Ed25519KeyHashesJSON;
  /**
   * @param {string} json
   * @returns {Ed25519KeyHashes}
   */
  static from_json(json: string): Ed25519KeyHashes;
  /**
   * @returns {Ed25519KeyHashes}
   */
  static new(): Ed25519KeyHashes;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {Ed25519KeyHash}
   */
  get(index: number): Ed25519KeyHash;
  /**
   * @param {Ed25519KeyHash} elem
   */
  add(elem: Ed25519KeyHash): void;
}
/** */
export class Ed25519Signature {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @returns {string}
   */
  to_bech32(): string;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} bech32_str
   * @returns {Ed25519Signature}
   */
  static from_bech32(bech32_str: string): Ed25519Signature;
  /**
   * @param {string} input
   * @returns {Ed25519Signature}
   */
  static from_hex(input: string): Ed25519Signature;
  /**
   * @param {Uint8Array} bytes
   * @returns {Ed25519Signature}
   */
  static from_bytes(bytes: Uint8Array): Ed25519Signature;
}
/** */
export class EnterpriseAddress {
  free(): void;
  /**
   * @param {number} network
   * @param {StakeCredential} payment
   * @returns {EnterpriseAddress}
   */
  static new(network: number, payment: StakeCredential): EnterpriseAddress;
  /**
   * @returns {StakeCredential}
   */
  payment_cred(): StakeCredential;
  /**
   * @returns {Address}
   */
  to_address(): Address;
  /**
   * @param {Address} addr
   * @returns {EnterpriseAddress | undefined}
   */
  static from_address(addr: Address): EnterpriseAddress | undefined;
}
/** */
export class ExUnitPrices {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ExUnitPrices}
   */
  static from_bytes(bytes: Uint8Array): ExUnitPrices;
  /**
   * @returns {UnitInterval}
   */
  mem_price(): UnitInterval;
  /**
   * @returns {UnitInterval}
   */
  step_price(): UnitInterval;
  /**
   * @param {UnitInterval} mem_price
   * @param {UnitInterval} step_price
   * @returns {ExUnitPrices}
   */
  static new(mem_price: UnitInterval, step_price: UnitInterval): ExUnitPrices;
  /**
   * @param {number} mem_price
   * @param {number} step_price
   * @returns {ExUnitPrices}
   */
  static from_float(mem_price: number, step_price: number): ExUnitPrices;
}
/** */
export class ExUnits {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ExUnits}
   */
  static from_bytes(bytes: Uint8Array): ExUnits;
  /**
   * @returns {BigNum}
   */
  mem(): BigNum;
  /**
   * @returns {BigNum}
   */
  steps(): BigNum;
  /**
   * @param {BigNum} mem
   * @param {BigNum} steps
   * @returns {ExUnits}
   */
  static new(mem: BigNum, steps: BigNum): ExUnits;
}
/** */
export class GeneralTransactionMetadata {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {GeneralTransactionMetadata}
   */
  static from_bytes(bytes: Uint8Array): GeneralTransactionMetadata;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {GeneralTransactionMetadataJSON}
   */
  to_js_value(): GeneralTransactionMetadataJSON;
  /**
   * @param {string} json
   * @returns {GeneralTransactionMetadata}
   */
  static from_json(json: string): GeneralTransactionMetadata;
  /**
   * @returns {GeneralTransactionMetadata}
   */
  static new(): GeneralTransactionMetadata;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {BigNum} key
   * @param {TransactionMetadatum} value
   * @returns {TransactionMetadatum | undefined}
   */
  insert(
    key: BigNum,
    value: TransactionMetadatum,
  ): TransactionMetadatum | undefined;
  /**
   * @param {BigNum} key
   * @returns {TransactionMetadatum | undefined}
   */
  get(key: BigNum): TransactionMetadatum | undefined;
  /**
   * @returns {TransactionMetadatumLabels}
   */
  keys(): TransactionMetadatumLabels;
}
/** */
export class GenesisDelegateHash {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {GenesisDelegateHash}
   */
  static from_bytes(bytes: Uint8Array): GenesisDelegateHash;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string} prefix
   * @returns {string}
   */
  to_bech32(prefix: string): string;
  /**
   * @param {string} bech_str
   * @returns {GenesisDelegateHash}
   */
  static from_bech32(bech_str: string): GenesisDelegateHash;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} hex
   * @returns {GenesisDelegateHash}
   */
  static from_hex(hex: string): GenesisDelegateHash;
}
/** */
export class GenesisHash {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {GenesisHash}
   */
  static from_bytes(bytes: Uint8Array): GenesisHash;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string} prefix
   * @returns {string}
   */
  to_bech32(prefix: string): string;
  /**
   * @param {string} bech_str
   * @returns {GenesisHash}
   */
  static from_bech32(bech_str: string): GenesisHash;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} hex
   * @returns {GenesisHash}
   */
  static from_hex(hex: string): GenesisHash;
}
/** */
export class GenesisHashes {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {GenesisHashes}
   */
  static from_bytes(bytes: Uint8Array): GenesisHashes;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {GenesisHashesJSON}
   */
  to_js_value(): GenesisHashesJSON;
  /**
   * @param {string} json
   * @returns {GenesisHashes}
   */
  static from_json(json: string): GenesisHashes;
  /**
   * @returns {GenesisHashes}
   */
  static new(): GenesisHashes;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {GenesisHash}
   */
  get(index: number): GenesisHash;
  /**
   * @param {GenesisHash} elem
   */
  add(elem: GenesisHash): void;
}
/** */
export class GenesisKeyDelegation {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {GenesisKeyDelegation}
   */
  static from_bytes(bytes: Uint8Array): GenesisKeyDelegation;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {GenesisKeyDelegationJSON}
   */
  to_js_value(): GenesisKeyDelegationJSON;
  /**
   * @param {string} json
   * @returns {GenesisKeyDelegation}
   */
  static from_json(json: string): GenesisKeyDelegation;
  /**
   * @returns {GenesisHash}
   */
  genesishash(): GenesisHash;
  /**
   * @returns {GenesisDelegateHash}
   */
  genesis_delegate_hash(): GenesisDelegateHash;
  /**
   * @returns {VRFKeyHash}
   */
  vrf_keyhash(): VRFKeyHash;
  /**
   * @param {GenesisHash} genesishash
   * @param {GenesisDelegateHash} genesis_delegate_hash
   * @param {VRFKeyHash} vrf_keyhash
   * @returns {GenesisKeyDelegation}
   */
  static new(
    genesishash: GenesisHash,
    genesis_delegate_hash: GenesisDelegateHash,
    vrf_keyhash: VRFKeyHash,
  ): GenesisKeyDelegation;
}
/** */
export class Header {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Header}
   */
  static from_bytes(bytes: Uint8Array): Header;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {HeaderJSON}
   */
  to_js_value(): HeaderJSON;
  /**
   * @param {string} json
   * @returns {Header}
   */
  static from_json(json: string): Header;
  /**
   * @returns {HeaderBody}
   */
  header_body(): HeaderBody;
  /**
   * @returns {KESSignature}
   */
  body_signature(): KESSignature;
  /**
   * @param {HeaderBody} header_body
   * @param {KESSignature} body_signature
   * @returns {Header}
   */
  static new(header_body: HeaderBody, body_signature: KESSignature): Header;
}
/** */
export class HeaderBody {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {HeaderBody}
   */
  static from_bytes(bytes: Uint8Array): HeaderBody;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {HeaderBodyJSON}
   */
  to_js_value(): HeaderBodyJSON;
  /**
   * @param {string} json
   * @returns {HeaderBody}
   */
  static from_json(json: string): HeaderBody;
  /**
   * @returns {number}
   */
  block_number(): number;
  /**
   * @returns {BigNum}
   */
  slot(): BigNum;
  /**
   * @returns {BlockHash | undefined}
   */
  prev_hash(): BlockHash | undefined;
  /**
   * @returns {Vkey}
   */
  issuer_vkey(): Vkey;
  /**
   * @returns {VRFVKey}
   */
  vrf_vkey(): VRFVKey;
  /**
   * @returns {VRFCert}
   */
  nonce_vrf(): VRFCert;
  /**
   * @returns {VRFCert}
   */
  leader_vrf(): VRFCert;
  /**
   * @returns {number}
   */
  block_body_size(): number;
  /**
   * @returns {BlockHash}
   */
  block_body_hash(): BlockHash;
  /**
   * @returns {OperationalCert}
   */
  operational_cert(): OperationalCert;
  /**
   * @returns {ProtocolVersion}
   */
  protocol_version(): ProtocolVersion;
  /**
   * @param {number} block_number
   * @param {BigNum} slot
   * @param {BlockHash | undefined} prev_hash
   * @param {Vkey} issuer_vkey
   * @param {VRFVKey} vrf_vkey
   * @param {VRFCert} nonce_vrf
   * @param {VRFCert} leader_vrf
   * @param {number} block_body_size
   * @param {BlockHash} block_body_hash
   * @param {OperationalCert} operational_cert
   * @param {ProtocolVersion} protocol_version
   * @returns {HeaderBody}
   */
  static new(
    block_number: number,
    slot: BigNum,
    prev_hash: BlockHash | undefined,
    issuer_vkey: Vkey,
    vrf_vkey: VRFVKey,
    nonce_vrf: VRFCert,
    leader_vrf: VRFCert,
    block_body_size: number,
    block_body_hash: BlockHash,
    operational_cert: OperationalCert,
    protocol_version: ProtocolVersion,
  ): HeaderBody;
}
/** */
export class Int {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Int}
   */
  static from_bytes(bytes: Uint8Array): Int;
  /**
   * @param {BigNum} x
   * @returns {Int}
   */
  static new(x: BigNum): Int;
  /**
   * @param {BigNum} x
   * @returns {Int}
   */
  static new_negative(x: BigNum): Int;
  /**
   * @param {number} x
   * @returns {Int}
   */
  static new_i32(x: number): Int;
  /**
   * @returns {boolean}
   */
  is_positive(): boolean;
  /**
   * BigNum can only contain unsigned u64 values
   *
   * This function will return the BigNum representation
   * only in case the underlying i128 value is positive.
   *
   * Otherwise nothing will be returned (undefined).
   * @returns {BigNum | undefined}
   */
  as_positive(): BigNum | undefined;
  /**
   * BigNum can only contain unsigned u64 values
   *
   * This function will return the *absolute* BigNum representation
   * only in case the underlying i128 value is negative.
   *
   * Otherwise nothing will be returned (undefined).
   * @returns {BigNum | undefined}
   */
  as_negative(): BigNum | undefined;
  /**
   * !!! DEPRECATED !!!
   * Returns an i32 value in case the underlying original i128 value is within the limits.
   * Otherwise will just return an empty value (undefined).
   * @returns {number | undefined}
   */
  as_i32(): number | undefined;
  /**
   * Returns the underlying value converted to i32 if possible (within limits)
   * Otherwise will just return an empty value (undefined).
   * @returns {number | undefined}
   */
  as_i32_or_nothing(): number | undefined;
  /**
   * Returns the underlying value converted to i32 if possible (within limits)
   * JsError in case of out of boundary overflow
   * @returns {number}
   */
  as_i32_or_fail(): number;
  /**
   * Returns string representation of the underlying i128 value directly.
   * Might contain the minus sign (-) in case of negative value.
   * @returns {string}
   */
  to_str(): string;
  /**
   * @param {string} string
   * @returns {Int}
   */
  static from_str(string: string): Int;
}
/** */
export class Ipv4 {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Ipv4}
   */
  static from_bytes(bytes: Uint8Array): Ipv4;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {Ipv4JSON}
   */
  to_js_value(): Ipv4JSON;
  /**
   * @param {string} json
   * @returns {Ipv4}
   */
  static from_json(json: string): Ipv4;
  /**
   * @param {Uint8Array} data
   * @returns {Ipv4}
   */
  static new(data: Uint8Array): Ipv4;
  /**
   * @returns {Uint8Array}
   */
  ip(): Uint8Array;
}
/** */
export class Ipv6 {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Ipv6}
   */
  static from_bytes(bytes: Uint8Array): Ipv6;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {Ipv6JSON}
   */
  to_js_value(): Ipv6JSON;
  /**
   * @param {string} json
   * @returns {Ipv6}
   */
  static from_json(json: string): Ipv6;
  /**
   * @param {Uint8Array} data
   * @returns {Ipv6}
   */
  static new(data: Uint8Array): Ipv6;
  /**
   * @returns {Uint8Array}
   */
  ip(): Uint8Array;
}
/** */
export class KESSignature {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {KESSignature}
   */
  static from_bytes(bytes: Uint8Array): KESSignature;
}
/** */
export class KESVKey {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {KESVKey}
   */
  static from_bytes(bytes: Uint8Array): KESVKey;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string} prefix
   * @returns {string}
   */
  to_bech32(prefix: string): string;
  /**
   * @param {string} bech_str
   * @returns {KESVKey}
   */
  static from_bech32(bech_str: string): KESVKey;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} hex
   * @returns {KESVKey}
   */
  static from_hex(hex: string): KESVKey;
}
/** */
export class Language {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Language}
   */
  static from_bytes(bytes: Uint8Array): Language;
  /**
   * @returns {Language}
   */
  static new_plutus_v1(): Language;
  /**
   * @returns {Language}
   */
  static new_plutus_v2(): Language;
  /**
   * @returns {number}
   */
  kind(): number;
}
/** */
export class Languages {
  free(): void;
  /**
   * @returns {Languages}
   */
  static new(): Languages;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {Language}
   */
  get(index: number): Language;
  /**
   * @param {Language} elem
   */
  add(elem: Language): void;
}
/** */
export class LegacyDaedalusPrivateKey {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {LegacyDaedalusPrivateKey}
   */
  static from_bytes(bytes: Uint8Array): LegacyDaedalusPrivateKey;
  /**
   * @returns {Uint8Array}
   */
  as_bytes(): Uint8Array;
  /**
   * @returns {Uint8Array}
   */
  chaincode(): Uint8Array;
}
/** */
export class LinearFee {
  free(): void;
  /**
   * @returns {BigNum}
   */
  constant(): BigNum;
  /**
   * @returns {BigNum}
   */
  coefficient(): BigNum;
  /**
   * @param {BigNum} coefficient
   * @param {BigNum} constant
   * @returns {LinearFee}
   */
  static new(coefficient: BigNum, constant: BigNum): LinearFee;
}
/** */
export class MIRToStakeCredentials {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {MIRToStakeCredentials}
   */
  static from_bytes(bytes: Uint8Array): MIRToStakeCredentials;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {MIRToStakeCredentialsJSON}
   */
  to_js_value(): MIRToStakeCredentialsJSON;
  /**
   * @param {string} json
   * @returns {MIRToStakeCredentials}
   */
  static from_json(json: string): MIRToStakeCredentials;
  /**
   * @returns {MIRToStakeCredentials}
   */
  static new(): MIRToStakeCredentials;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {StakeCredential} cred
   * @param {Int} delta
   * @returns {Int | undefined}
   */
  insert(cred: StakeCredential, delta: Int): Int | undefined;
  /**
   * @param {StakeCredential} cred
   * @returns {Int | undefined}
   */
  get(cred: StakeCredential): Int | undefined;
  /**
   * @returns {StakeCredentials}
   */
  keys(): StakeCredentials;
}
/** */
export class MetadataList {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {MetadataList}
   */
  static from_bytes(bytes: Uint8Array): MetadataList;
  /**
   * @returns {MetadataList}
   */
  static new(): MetadataList;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {TransactionMetadatum}
   */
  get(index: number): TransactionMetadatum;
  /**
   * @param {TransactionMetadatum} elem
   */
  add(elem: TransactionMetadatum): void;
}
/** */
export class MetadataMap {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {MetadataMap}
   */
  static from_bytes(bytes: Uint8Array): MetadataMap;
  /**
   * @returns {MetadataMap}
   */
  static new(): MetadataMap;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {TransactionMetadatum} key
   * @param {TransactionMetadatum} value
   * @returns {TransactionMetadatum | undefined}
   */
  insert(
    key: TransactionMetadatum,
    value: TransactionMetadatum,
  ): TransactionMetadatum | undefined;
  /**
   * @param {string} key
   * @param {TransactionMetadatum} value
   * @returns {TransactionMetadatum | undefined}
   */
  insert_str(
    key: string,
    value: TransactionMetadatum,
  ): TransactionMetadatum | undefined;
  /**
   * @param {number} key
   * @param {TransactionMetadatum} value
   * @returns {TransactionMetadatum | undefined}
   */
  insert_i32(
    key: number,
    value: TransactionMetadatum,
  ): TransactionMetadatum | undefined;
  /**
   * @param {TransactionMetadatum} key
   * @returns {TransactionMetadatum}
   */
  get(key: TransactionMetadatum): TransactionMetadatum;
  /**
   * @param {string} key
   * @returns {TransactionMetadatum}
   */
  get_str(key: string): TransactionMetadatum;
  /**
   * @param {number} key
   * @returns {TransactionMetadatum}
   */
  get_i32(key: number): TransactionMetadatum;
  /**
   * @param {TransactionMetadatum} key
   * @returns {boolean}
   */
  has(key: TransactionMetadatum): boolean;
  /**
   * @returns {MetadataList}
   */
  keys(): MetadataList;
}
/** */
export class Mint {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Mint}
   */
  static from_bytes(bytes: Uint8Array): Mint;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {MintJSON}
   */
  to_js_value(): MintJSON;
  /**
   * @param {string} json
   * @returns {Mint}
   */
  static from_json(json: string): Mint;
  /**
   * @returns {Mint}
   */
  static new(): Mint;
  /**
   * @param {ScriptHash} key
   * @param {MintAssets} value
   * @returns {Mint}
   */
  static new_from_entry(key: ScriptHash, value: MintAssets): Mint;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {ScriptHash} key
   * @param {MintAssets} value
   * @returns {MintAssets | undefined}
   */
  insert(key: ScriptHash, value: MintAssets): MintAssets | undefined;
  /**
   * @param {ScriptHash} key
   * @returns {MintAssets | undefined}
   */
  get(key: ScriptHash): MintAssets | undefined;
  /**
   * @returns {ScriptHashes}
   */
  keys(): ScriptHashes;
  /**
   * Returns the multiasset where only positive (minting) entries are present
   * @returns {MultiAsset}
   */
  as_positive_multiasset(): MultiAsset;
  /**
   * Returns the multiasset where only negative (burning) entries are present
   * @returns {MultiAsset}
   */
  as_negative_multiasset(): MultiAsset;
}
/** */
export class MintAssets {
  free(): void;
  /**
   * @returns {MintAssets}
   */
  static new(): MintAssets;
  /**
   * @param {AssetName} key
   * @param {Int} value
   * @returns {MintAssets}
   */
  static new_from_entry(key: AssetName, value: Int): MintAssets;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {AssetName} key
   * @param {Int} value
   * @returns {Int | undefined}
   */
  insert(key: AssetName, value: Int): Int | undefined;
  /**
   * @param {AssetName} key
   * @returns {Int | undefined}
   */
  get(key: AssetName): Int | undefined;
  /**
   * @returns {AssetNames}
   */
  keys(): AssetNames;
}
/** */
export class MoveInstantaneousReward {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {MoveInstantaneousReward}
   */
  static from_bytes(bytes: Uint8Array): MoveInstantaneousReward;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {MoveInstantaneousRewardJSON}
   */
  to_js_value(): MoveInstantaneousRewardJSON;
  /**
   * @param {string} json
   * @returns {MoveInstantaneousReward}
   */
  static from_json(json: string): MoveInstantaneousReward;
  /**
   * @param {number} pot
   * @param {BigNum} amount
   * @returns {MoveInstantaneousReward}
   */
  static new_to_other_pot(pot: number, amount: BigNum): MoveInstantaneousReward;
  /**
   * @param {number} pot
   * @param {MIRToStakeCredentials} amounts
   * @returns {MoveInstantaneousReward}
   */
  static new_to_stake_creds(
    pot: number,
    amounts: MIRToStakeCredentials,
  ): MoveInstantaneousReward;
  /**
   * @returns {number}
   */
  pot(): number;
  /**
   * @returns {number}
   */
  kind(): number;
  /**
   * @returns {BigNum | undefined}
   */
  as_to_other_pot(): BigNum | undefined;
  /**
   * @returns {MIRToStakeCredentials | undefined}
   */
  as_to_stake_creds(): MIRToStakeCredentials | undefined;
}
/** */
export class MoveInstantaneousRewardsCert {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {MoveInstantaneousRewardsCert}
   */
  static from_bytes(bytes: Uint8Array): MoveInstantaneousRewardsCert;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {MoveInstantaneousRewardsCertJSON}
   */
  to_js_value(): MoveInstantaneousRewardsCertJSON;
  /**
   * @param {string} json
   * @returns {MoveInstantaneousRewardsCert}
   */
  static from_json(json: string): MoveInstantaneousRewardsCert;
  /**
   * @returns {MoveInstantaneousReward}
   */
  move_instantaneous_reward(): MoveInstantaneousReward;
  /**
   * @param {MoveInstantaneousReward} move_instantaneous_reward
   * @returns {MoveInstantaneousRewardsCert}
   */
  static new(
    move_instantaneous_reward: MoveInstantaneousReward,
  ): MoveInstantaneousRewardsCert;
}
/** */
export class MultiAsset {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {MultiAsset}
   */
  static from_bytes(bytes: Uint8Array): MultiAsset;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {MultiAssetJSON}
   */
  to_js_value(): MultiAssetJSON;
  /**
   * @param {string} json
   * @returns {MultiAsset}
   */
  static from_json(json: string): MultiAsset;
  /**
   * @returns {MultiAsset}
   */
  static new(): MultiAsset;
  /**
   * the number of unique policy IDs in the multiasset
   * @returns {number}
   */
  len(): number;
  /**
   * set (and replace if it exists) all assets with policy {policy_id} to a copy of {assets}
   * @param {ScriptHash} policy_id
   * @param {Assets} assets
   * @returns {Assets | undefined}
   */
  insert(policy_id: ScriptHash, assets: Assets): Assets | undefined;
  /**
   * all assets under {policy_id}, if any exist, or else None (undefined in JS)
   * @param {ScriptHash} policy_id
   * @returns {Assets | undefined}
   */
  get(policy_id: ScriptHash): Assets | undefined;
  /**
   * sets the asset {asset_name} to {value} under policy {policy_id}
   * returns the previous amount if it was set, or else None (undefined in JS)
   * @param {ScriptHash} policy_id
   * @param {AssetName} asset_name
   * @param {BigNum} value
   * @returns {BigNum | undefined}
   */
  set_asset(
    policy_id: ScriptHash,
    asset_name: AssetName,
    value: BigNum,
  ): BigNum | undefined;
  /**
   * returns the amount of asset {asset_name} under policy {policy_id}
   * If such an asset does not exist, 0 is returned.
   * @param {ScriptHash} policy_id
   * @param {AssetName} asset_name
   * @returns {BigNum}
   */
  get_asset(policy_id: ScriptHash, asset_name: AssetName): BigNum;
  /**
   * returns all policy IDs used by assets in this multiasset
   * @returns {ScriptHashes}
   */
  keys(): ScriptHashes;
  /**
   * removes an asset from the list if the result is 0 or less
   * does not modify this object, instead the result is returned
   * @param {MultiAsset} rhs_ma
   * @returns {MultiAsset}
   */
  sub(rhs_ma: MultiAsset): MultiAsset;
}
/** */
export class MultiHostName {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {MultiHostName}
   */
  static from_bytes(bytes: Uint8Array): MultiHostName;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {MultiHostNameJSON}
   */
  to_js_value(): MultiHostNameJSON;
  /**
   * @param {string} json
   * @returns {MultiHostName}
   */
  static from_json(json: string): MultiHostName;
  /**
   * @returns {DNSRecordSRV}
   */
  dns_name(): DNSRecordSRV;
  /**
   * @param {DNSRecordSRV} dns_name
   * @returns {MultiHostName}
   */
  static new(dns_name: DNSRecordSRV): MultiHostName;
}
/** */
export class NativeScript {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {NativeScript}
   */
  static from_bytes(bytes: Uint8Array): NativeScript;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {NativeScriptJSON}
   */
  to_js_value(): NativeScriptJSON;
  /**
   * @param {string} json
   * @returns {NativeScript}
   */
  static from_json(json: string): NativeScript;
  /**
   * @param {number} namespace
   * @returns {ScriptHash}
   */
  hash(namespace: number): ScriptHash;
  /**
   * @param {ScriptPubkey} script_pubkey
   * @returns {NativeScript}
   */
  static new_script_pubkey(script_pubkey: ScriptPubkey): NativeScript;
  /**
   * @param {ScriptAll} script_all
   * @returns {NativeScript}
   */
  static new_script_all(script_all: ScriptAll): NativeScript;
  /**
   * @param {ScriptAny} script_any
   * @returns {NativeScript}
   */
  static new_script_any(script_any: ScriptAny): NativeScript;
  /**
   * @param {ScriptNOfK} script_n_of_k
   * @returns {NativeScript}
   */
  static new_script_n_of_k(script_n_of_k: ScriptNOfK): NativeScript;
  /**
   * @param {TimelockStart} timelock_start
   * @returns {NativeScript}
   */
  static new_timelock_start(timelock_start: TimelockStart): NativeScript;
  /**
   * @param {TimelockExpiry} timelock_expiry
   * @returns {NativeScript}
   */
  static new_timelock_expiry(timelock_expiry: TimelockExpiry): NativeScript;
  /**
   * @returns {number}
   */
  kind(): number;
  /**
   * @returns {ScriptPubkey | undefined}
   */
  as_script_pubkey(): ScriptPubkey | undefined;
  /**
   * @returns {ScriptAll | undefined}
   */
  as_script_all(): ScriptAll | undefined;
  /**
   * @returns {ScriptAny | undefined}
   */
  as_script_any(): ScriptAny | undefined;
  /**
   * @returns {ScriptNOfK | undefined}
   */
  as_script_n_of_k(): ScriptNOfK | undefined;
  /**
   * @returns {TimelockStart | undefined}
   */
  as_timelock_start(): TimelockStart | undefined;
  /**
   * @returns {TimelockExpiry | undefined}
   */
  as_timelock_expiry(): TimelockExpiry | undefined;
  /**
   * Returns an array of unique Ed25519KeyHashes
   * contained within this script recursively on any depth level.
   * The order of the keys in the result is not determined in any way.
   * @returns {Ed25519KeyHashes}
   */
  get_required_signers(): Ed25519KeyHashes;
  /**
   * @param {BigNum | undefined} lower_bound
   * @param {BigNum | undefined} upper_bound
   * @param {Ed25519KeyHashes} key_hashes
   * @returns {boolean}
   */
  verify(
    lower_bound: BigNum | undefined,
    upper_bound: BigNum | undefined,
    key_hashes: Ed25519KeyHashes,
  ): boolean;
}
/** */
export class NativeScripts {
  free(): void;
  /**
   * @returns {NativeScripts}
   */
  static new(): NativeScripts;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {NativeScript}
   */
  get(index: number): NativeScript;
  /**
   * @param {NativeScript} elem
   */
  add(elem: NativeScript): void;
}
/** */
export class NetworkId {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {NetworkId}
   */
  static from_bytes(bytes: Uint8Array): NetworkId;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {NetworkIdJSON}
   */
  to_js_value(): NetworkIdJSON;
  /**
   * @param {string} json
   * @returns {NetworkId}
   */
  static from_json(json: string): NetworkId;
  /**
   * @returns {NetworkId}
   */
  static testnet(): NetworkId;
  /**
   * @returns {NetworkId}
   */
  static mainnet(): NetworkId;
  /**
   * @returns {number}
   */
  kind(): number;
}
/** */
export class NetworkInfo {
  free(): void;
  /**
   * @param {number} network_id
   * @param {number} protocol_magic
   * @returns {NetworkInfo}
   */
  static new(network_id: number, protocol_magic: number): NetworkInfo;
  /**
   * @returns {number}
   */
  network_id(): number;
  /**
   * @returns {number}
   */
  protocol_magic(): number;
  /**
   * @returns {NetworkInfo}
   */
  static testnet(): NetworkInfo;
  /**
   * @returns {NetworkInfo}
   */
  static mainnet(): NetworkInfo;
}
/** */
export class Nonce {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Nonce}
   */
  static from_bytes(bytes: Uint8Array): Nonce;
  /**
   * @returns {Nonce}
   */
  static new_identity(): Nonce;
  /**
   * @param {Uint8Array} hash
   * @returns {Nonce}
   */
  static new_from_hash(hash: Uint8Array): Nonce;
  /**
   * @returns {Uint8Array | undefined}
   */
  get_hash(): Uint8Array | undefined;
}
/** */
export class OperationalCert {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {OperationalCert}
   */
  static from_bytes(bytes: Uint8Array): OperationalCert;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {OperationalCertJSON}
   */
  to_js_value(): OperationalCertJSON;
  /**
   * @param {string} json
   * @returns {OperationalCert}
   */
  static from_json(json: string): OperationalCert;
  /**
   * @returns {KESVKey}
   */
  hot_vkey(): KESVKey;
  /**
   * @returns {number}
   */
  sequence_number(): number;
  /**
   * @returns {number}
   */
  kes_period(): number;
  /**
   * @returns {Ed25519Signature}
   */
  sigma(): Ed25519Signature;
  /**
   * @param {KESVKey} hot_vkey
   * @param {number} sequence_number
   * @param {number} kes_period
   * @param {Ed25519Signature} sigma
   * @returns {OperationalCert}
   */
  static new(
    hot_vkey: KESVKey,
    sequence_number: number,
    kes_period: number,
    sigma: Ed25519Signature,
  ): OperationalCert;
}
/** */
export class PlutusData {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {PlutusData}
   */
  static from_bytes(bytes: Uint8Array): PlutusData;
  /**
   * @param {ConstrPlutusData} constr_plutus_data
   * @returns {PlutusData}
   */
  static new_constr_plutus_data(
    constr_plutus_data: ConstrPlutusData,
  ): PlutusData;
  /**
   * @param {PlutusMap} map
   * @returns {PlutusData}
   */
  static new_map(map: PlutusMap): PlutusData;
  /**
   * @param {PlutusList} list
   * @returns {PlutusData}
   */
  static new_list(list: PlutusList): PlutusData;
  /**
   * @param {BigInt} integer
   * @returns {PlutusData}
   */
  static new_integer(integer: BigInt): PlutusData;
  /**
   * @param {Uint8Array} bytes
   * @returns {PlutusData}
   */
  static new_bytes(bytes: Uint8Array): PlutusData;
  /**
   * @returns {number}
   */
  kind(): number;
  /**
   * @returns {ConstrPlutusData | undefined}
   */
  as_constr_plutus_data(): ConstrPlutusData | undefined;
  /**
   * @returns {PlutusMap | undefined}
   */
  as_map(): PlutusMap | undefined;
  /**
   * @returns {PlutusList | undefined}
   */
  as_list(): PlutusList | undefined;
  /**
   * @returns {BigInt | undefined}
   */
  as_integer(): BigInt | undefined;
  /**
   * @returns {Uint8Array | undefined}
   */
  as_bytes(): Uint8Array | undefined;
}
/** */
export class PlutusList {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {PlutusList}
   */
  static from_bytes(bytes: Uint8Array): PlutusList;
  /**
   * @returns {PlutusList}
   */
  static new(): PlutusList;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {PlutusData}
   */
  get(index: number): PlutusData;
  /**
   * @param {PlutusData} elem
   */
  add(elem: PlutusData): void;
}
/** */
export class PlutusMap {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {PlutusMap}
   */
  static from_bytes(bytes: Uint8Array): PlutusMap;
  /**
   * @returns {PlutusMap}
   */
  static new(): PlutusMap;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {PlutusData} key
   * @param {PlutusData} value
   * @returns {PlutusData | undefined}
   */
  insert(key: PlutusData, value: PlutusData): PlutusData | undefined;
  /**
   * @param {PlutusData} key
   * @returns {PlutusData | undefined}
   */
  get(key: PlutusData): PlutusData | undefined;
  /**
   * @returns {PlutusList}
   */
  keys(): PlutusList;
}
/** */
export class PlutusScript {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {PlutusScript}
   */
  static from_bytes(bytes: Uint8Array): PlutusScript;
  /**
   * @param {number} namespace
   * @returns {ScriptHash}
   */
  hash(namespace: number): ScriptHash;
  /**
   *     * Creates a new Plutus script from the RAW bytes of the compiled script.
   *     * This does NOT include any CBOR encoding around these bytes (e.g. from "cborBytes" in cardano-cli)
   *     * If you creating this from those you should use PlutusScript::from_bytes() instead.
   *
   * @param {Uint8Array} bytes
   * @returns {PlutusScript}
   */
  static new(bytes: Uint8Array): PlutusScript;
  /**
   *     * The raw bytes of this compiled Plutus script.
   *     * If you need "cborBytes" for cardano-cli use PlutusScript::to_bytes() instead.
   *
   * @returns {Uint8Array}
   */
  bytes(): Uint8Array;
}
/** */
export class PlutusScripts {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {PlutusScripts}
   */
  static from_bytes(bytes: Uint8Array): PlutusScripts;
  /**
   * @returns {PlutusScripts}
   */
  static new(): PlutusScripts;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {PlutusScript}
   */
  get(index: number): PlutusScript;
  /**
   * @param {PlutusScript} elem
   */
  add(elem: PlutusScript): void;
}
/** */
export class PlutusWitness {
  free(): void;
  /**
   * Plutus V1 witness or witness where no script is attached and so version doesn't matter
   * @param {PlutusData} redeemer
   * @param {PlutusData | undefined} plutus_data
   * @param {PlutusScript | undefined} script
   * @returns {PlutusWitness}
   */
  static new(
    redeemer: PlutusData,
    plutus_data?: PlutusData,
    script?: PlutusScript,
  ): PlutusWitness;
  /**
   * @param {PlutusData} redeemer
   * @param {PlutusData | undefined} plutus_data
   * @param {PlutusScript | undefined} script
   * @returns {PlutusWitness}
   */
  static new_plutus_v2(
    redeemer: PlutusData,
    plutus_data?: PlutusData,
    script?: PlutusScript,
  ): PlutusWitness;
  /**
   * @returns {PlutusData | undefined}
   */
  plutus_data(): PlutusData | undefined;
  /**
   * @returns {PlutusData}
   */
  redeemer(): PlutusData;
  /**
   * @returns {PlutusScript | undefined}
   */
  script(): PlutusScript | undefined;
  /**
   * @returns {number}
   */
  version(): number;
}
/** */
export class Pointer {
  free(): void;
  /**
   * @param {BigNum} slot
   * @param {BigNum} tx_index
   * @param {BigNum} cert_index
   * @returns {Pointer}
   */
  static new(slot: BigNum, tx_index: BigNum, cert_index: BigNum): Pointer;
  /**
   * @returns {BigNum}
   */
  slot(): BigNum;
  /**
   * @returns {BigNum}
   */
  tx_index(): BigNum;
  /**
   * @returns {BigNum}
   */
  cert_index(): BigNum;
}
/** */
export class PointerAddress {
  free(): void;
  /**
   * @param {number} network
   * @param {StakeCredential} payment
   * @param {Pointer} stake
   * @returns {PointerAddress}
   */
  static new(
    network: number,
    payment: StakeCredential,
    stake: Pointer,
  ): PointerAddress;
  /**
   * @returns {StakeCredential}
   */
  payment_cred(): StakeCredential;
  /**
   * @returns {Pointer}
   */
  stake_pointer(): Pointer;
  /**
   * @returns {Address}
   */
  to_address(): Address;
  /**
   * @param {Address} addr
   * @returns {PointerAddress | undefined}
   */
  static from_address(addr: Address): PointerAddress | undefined;
}
/** */
export class PoolMetadata {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {PoolMetadata}
   */
  static from_bytes(bytes: Uint8Array): PoolMetadata;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {PoolMetadataJSON}
   */
  to_js_value(): PoolMetadataJSON;
  /**
   * @param {string} json
   * @returns {PoolMetadata}
   */
  static from_json(json: string): PoolMetadata;
  /**
   * @returns {URL}
   */
  url(): URL;
  /**
   * @returns {PoolMetadataHash}
   */
  pool_metadata_hash(): PoolMetadataHash;
  /**
   * @param {URL} url
   * @param {PoolMetadataHash} pool_metadata_hash
   * @returns {PoolMetadata}
   */
  static new(url: URL, pool_metadata_hash: PoolMetadataHash): PoolMetadata;
}
/** */
export class PoolMetadataHash {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {PoolMetadataHash}
   */
  static from_bytes(bytes: Uint8Array): PoolMetadataHash;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string} prefix
   * @returns {string}
   */
  to_bech32(prefix: string): string;
  /**
   * @param {string} bech_str
   * @returns {PoolMetadataHash}
   */
  static from_bech32(bech_str: string): PoolMetadataHash;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} hex
   * @returns {PoolMetadataHash}
   */
  static from_hex(hex: string): PoolMetadataHash;
}
/** */
export class PoolParams {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {PoolParams}
   */
  static from_bytes(bytes: Uint8Array): PoolParams;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {PoolParamsJSON}
   */
  to_js_value(): PoolParamsJSON;
  /**
   * @param {string} json
   * @returns {PoolParams}
   */
  static from_json(json: string): PoolParams;
  /**
   * @returns {Ed25519KeyHash}
   */
  operator(): Ed25519KeyHash;
  /**
   * @returns {VRFKeyHash}
   */
  vrf_keyhash(): VRFKeyHash;
  /**
   * @returns {BigNum}
   */
  pledge(): BigNum;
  /**
   * @returns {BigNum}
   */
  cost(): BigNum;
  /**
   * @returns {UnitInterval}
   */
  margin(): UnitInterval;
  /**
   * @returns {RewardAddress}
   */
  reward_account(): RewardAddress;
  /**
   * @returns {Ed25519KeyHashes}
   */
  pool_owners(): Ed25519KeyHashes;
  /**
   * @returns {Relays}
   */
  relays(): Relays;
  /**
   * @returns {PoolMetadata | undefined}
   */
  pool_metadata(): PoolMetadata | undefined;
  /**
   * @param {Ed25519KeyHash} operator
   * @param {VRFKeyHash} vrf_keyhash
   * @param {BigNum} pledge
   * @param {BigNum} cost
   * @param {UnitInterval} margin
   * @param {RewardAddress} reward_account
   * @param {Ed25519KeyHashes} pool_owners
   * @param {Relays} relays
   * @param {PoolMetadata | undefined} pool_metadata
   * @returns {PoolParams}
   */
  static new(
    operator: Ed25519KeyHash,
    vrf_keyhash: VRFKeyHash,
    pledge: BigNum,
    cost: BigNum,
    margin: UnitInterval,
    reward_account: RewardAddress,
    pool_owners: Ed25519KeyHashes,
    relays: Relays,
    pool_metadata?: PoolMetadata,
  ): PoolParams;
}
/** */
export class PoolRegistration {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {PoolRegistration}
   */
  static from_bytes(bytes: Uint8Array): PoolRegistration;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {PoolRegistrationJSON}
   */
  to_js_value(): PoolRegistrationJSON;
  /**
   * @param {string} json
   * @returns {PoolRegistration}
   */
  static from_json(json: string): PoolRegistration;
  /**
   * @returns {PoolParams}
   */
  pool_params(): PoolParams;
  /**
   * @param {PoolParams} pool_params
   * @returns {PoolRegistration}
   */
  static new(pool_params: PoolParams): PoolRegistration;
  /**
   * @param {boolean} update
   */
  set_is_update(update: boolean): void;
}
/** */
export class PoolRetirement {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {PoolRetirement}
   */
  static from_bytes(bytes: Uint8Array): PoolRetirement;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {PoolRetirementJSON}
   */
  to_js_value(): PoolRetirementJSON;
  /**
   * @param {string} json
   * @returns {PoolRetirement}
   */
  static from_json(json: string): PoolRetirement;
  /**
   * @returns {Ed25519KeyHash}
   */
  pool_keyhash(): Ed25519KeyHash;
  /**
   * @returns {number}
   */
  epoch(): number;
  /**
   * @param {Ed25519KeyHash} pool_keyhash
   * @param {number} epoch
   * @returns {PoolRetirement}
   */
  static new(pool_keyhash: Ed25519KeyHash, epoch: number): PoolRetirement;
}
/** */
export class PrivateKey {
  free(): void;
  /**
   * @returns {PublicKey}
   */
  to_public(): PublicKey;
  /**
   * @returns {PrivateKey}
   */
  static generate_ed25519(): PrivateKey;
  /**
   * @returns {PrivateKey}
   */
  static generate_ed25519extended(): PrivateKey;
  /**
   * Get private key from its bech32 representation
   * ```javascript
   * PrivateKey.from_bech32(&#39;ed25519_sk1ahfetf02qwwg4dkq7mgp4a25lx5vh9920cr5wnxmpzz9906qvm8qwvlts0&#39;);
   * ```
   * For an extended 25519 key
   * ```javascript
   * PrivateKey.from_bech32(&#39;ed25519e_sk1gqwl4szuwwh6d0yk3nsqcc6xxc3fpvjlevgwvt60df59v8zd8f8prazt8ln3lmz096ux3xvhhvm3ca9wj2yctdh3pnw0szrma07rt5gl748fp&#39;);
   * ```
   * @param {string} bech32_str
   * @returns {PrivateKey}
   */
  static from_bech32(bech32_str: string): PrivateKey;
  /**
   * @returns {string}
   */
  to_bech32(): string;
  /**
   * @returns {Uint8Array}
   */
  as_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {PrivateKey}
   */
  static from_extended_bytes(bytes: Uint8Array): PrivateKey;
  /**
   * @param {Uint8Array} bytes
   * @returns {PrivateKey}
   */
  static from_normal_bytes(bytes: Uint8Array): PrivateKey;
  /**
   * @param {Uint8Array} message
   * @returns {Ed25519Signature}
   */
  sign(message: Uint8Array): Ed25519Signature;
  /**
   * @param {Uint8Array} bytes
   * @returns {PrivateKey}
   */
  static from_bytes(bytes: Uint8Array): PrivateKey;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
}
/** */
export class ProposedProtocolParameterUpdates {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ProposedProtocolParameterUpdates}
   */
  static from_bytes(bytes: Uint8Array): ProposedProtocolParameterUpdates;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {ProposedProtocolParameterUpdatesJSON}
   */
  to_js_value(): ProposedProtocolParameterUpdatesJSON;
  /**
   * @param {string} json
   * @returns {ProposedProtocolParameterUpdates}
   */
  static from_json(json: string): ProposedProtocolParameterUpdates;
  /**
   * @returns {ProposedProtocolParameterUpdates}
   */
  static new(): ProposedProtocolParameterUpdates;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {GenesisHash} key
   * @param {ProtocolParamUpdate} value
   * @returns {ProtocolParamUpdate | undefined}
   */
  insert(
    key: GenesisHash,
    value: ProtocolParamUpdate,
  ): ProtocolParamUpdate | undefined;
  /**
   * @param {GenesisHash} key
   * @returns {ProtocolParamUpdate | undefined}
   */
  get(key: GenesisHash): ProtocolParamUpdate | undefined;
  /**
   * @returns {GenesisHashes}
   */
  keys(): GenesisHashes;
}
/** */
export class ProtocolParamUpdate {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ProtocolParamUpdate}
   */
  static from_bytes(bytes: Uint8Array): ProtocolParamUpdate;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {ProtocolParamUpdateJSON}
   */
  to_js_value(): ProtocolParamUpdateJSON;
  /**
   * @param {string} json
   * @returns {ProtocolParamUpdate}
   */
  static from_json(json: string): ProtocolParamUpdate;
  /**
   * @param {BigNum} minfee_a
   */
  set_minfee_a(minfee_a: BigNum): void;
  /**
   * @returns {BigNum | undefined}
   */
  minfee_a(): BigNum | undefined;
  /**
   * @param {BigNum} minfee_b
   */
  set_minfee_b(minfee_b: BigNum): void;
  /**
   * @returns {BigNum | undefined}
   */
  minfee_b(): BigNum | undefined;
  /**
   * @param {number} max_block_body_size
   */
  set_max_block_body_size(max_block_body_size: number): void;
  /**
   * @returns {number | undefined}
   */
  max_block_body_size(): number | undefined;
  /**
   * @param {number} max_tx_size
   */
  set_max_tx_size(max_tx_size: number): void;
  /**
   * @returns {number | undefined}
   */
  max_tx_size(): number | undefined;
  /**
   * @param {number} max_block_header_size
   */
  set_max_block_header_size(max_block_header_size: number): void;
  /**
   * @returns {number | undefined}
   */
  max_block_header_size(): number | undefined;
  /**
   * @param {BigNum} key_deposit
   */
  set_key_deposit(key_deposit: BigNum): void;
  /**
   * @returns {BigNum | undefined}
   */
  key_deposit(): BigNum | undefined;
  /**
   * @param {BigNum} pool_deposit
   */
  set_pool_deposit(pool_deposit: BigNum): void;
  /**
   * @returns {BigNum | undefined}
   */
  pool_deposit(): BigNum | undefined;
  /**
   * @param {number} max_epoch
   */
  set_max_epoch(max_epoch: number): void;
  /**
   * @returns {number | undefined}
   */
  max_epoch(): number | undefined;
  /**
   * @param {number} n_opt
   */
  set_n_opt(n_opt: number): void;
  /**
   * @returns {number | undefined}
   */
  n_opt(): number | undefined;
  /**
   * @param {UnitInterval} pool_pledge_influence
   */
  set_pool_pledge_influence(pool_pledge_influence: UnitInterval): void;
  /**
   * @returns {UnitInterval | undefined}
   */
  pool_pledge_influence(): UnitInterval | undefined;
  /**
   * @param {UnitInterval} expansion_rate
   */
  set_expansion_rate(expansion_rate: UnitInterval): void;
  /**
   * @returns {UnitInterval | undefined}
   */
  expansion_rate(): UnitInterval | undefined;
  /**
   * @param {UnitInterval} treasury_growth_rate
   */
  set_treasury_growth_rate(treasury_growth_rate: UnitInterval): void;
  /**
   * @returns {UnitInterval | undefined}
   */
  treasury_growth_rate(): UnitInterval | undefined;
  /**
   * @param {UnitInterval} d
   */
  set_d(d: UnitInterval): void;
  /**
   * @returns {UnitInterval | undefined}
   */
  d(): UnitInterval | undefined;
  /**
   * @param {Nonce} extra_entropy
   */
  set_extra_entropy(extra_entropy: Nonce): void;
  /**
   * @returns {Nonce | undefined}
   */
  extra_entropy(): Nonce | undefined;
  /**
   * @param {ProtocolVersion} protocol_version
   */
  set_protocol_version(protocol_version: ProtocolVersion): void;
  /**
   * @returns {ProtocolVersion | undefined}
   */
  protocol_version(): ProtocolVersion | undefined;
  /**
   * @param {BigNum} min_pool_cost
   */
  set_min_pool_cost(min_pool_cost: BigNum): void;
  /**
   * @returns {BigNum | undefined}
   */
  min_pool_cost(): BigNum | undefined;
  /**
   * @param {BigNum} ada_per_utxo_byte
   */
  set_ada_per_utxo_byte(ada_per_utxo_byte: BigNum): void;
  /**
   * @returns {BigNum | undefined}
   */
  ada_per_utxo_byte(): BigNum | undefined;
  /**
   * @param {Costmdls} cost_models
   */
  set_cost_models(cost_models: Costmdls): void;
  /**
   * @returns {Costmdls | undefined}
   */
  cost_models(): Costmdls | undefined;
  /**
   * @param {ExUnitPrices} execution_costs
   */
  set_execution_costs(execution_costs: ExUnitPrices): void;
  /**
   * @returns {ExUnitPrices | undefined}
   */
  execution_costs(): ExUnitPrices | undefined;
  /**
   * @param {ExUnits} max_tx_ex_units
   */
  set_max_tx_ex_units(max_tx_ex_units: ExUnits): void;
  /**
   * @returns {ExUnits | undefined}
   */
  max_tx_ex_units(): ExUnits | undefined;
  /**
   * @param {ExUnits} max_block_ex_units
   */
  set_max_block_ex_units(max_block_ex_units: ExUnits): void;
  /**
   * @returns {ExUnits | undefined}
   */
  max_block_ex_units(): ExUnits | undefined;
  /**
   * @param {number} max_value_size
   */
  set_max_value_size(max_value_size: number): void;
  /**
   * @returns {number | undefined}
   */
  max_value_size(): number | undefined;
  /**
   * @param {number} collateral_percentage
   */
  set_collateral_percentage(collateral_percentage: number): void;
  /**
   * @returns {number | undefined}
   */
  collateral_percentage(): number | undefined;
  /**
   * @param {number} max_collateral_inputs
   */
  set_max_collateral_inputs(max_collateral_inputs: number): void;
  /**
   * @returns {number | undefined}
   */
  max_collateral_inputs(): number | undefined;
  /**
   * @returns {ProtocolParamUpdate}
   */
  static new(): ProtocolParamUpdate;
}
/** */
export class ProtocolVersion {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ProtocolVersion}
   */
  static from_bytes(bytes: Uint8Array): ProtocolVersion;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {ProtocolVersionJSON}
   */
  to_js_value(): ProtocolVersionJSON;
  /**
   * @param {string} json
   * @returns {ProtocolVersion}
   */
  static from_json(json: string): ProtocolVersion;
  /**
   * @returns {number}
   */
  major(): number;
  /**
   * @returns {number}
   */
  minor(): number;
  /**
   * @param {number} major
   * @param {number} minor
   * @returns {ProtocolVersion}
   */
  static new(major: number, minor: number): ProtocolVersion;
}
/**
 * ED25519 key used as public key
 */
export class PublicKey {
  free(): void;
  /**
   * Get public key from its bech32 representation
   * Example:
   * ```javascript
   * const pkey = PublicKey.from_bech32(&#39;ed25519_pk1dgaagyh470y66p899txcl3r0jaeaxu6yd7z2dxyk55qcycdml8gszkxze2&#39;);
   * ```
   * @param {string} bech32_str
   * @returns {PublicKey}
   */
  static from_bech32(bech32_str: string): PublicKey;
  /**
   * @returns {string}
   */
  to_bech32(): string;
  /**
   * @returns {Uint8Array}
   */
  as_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {PublicKey}
   */
  static from_bytes(bytes: Uint8Array): PublicKey;
  /**
   * @param {Uint8Array} data
   * @param {Ed25519Signature} signature
   * @returns {boolean}
   */
  verify(data: Uint8Array, signature: Ed25519Signature): boolean;
  /**
   * @returns {Ed25519KeyHash}
   */
  hash(): Ed25519KeyHash;
}
/** */
export class PublicKeys {
  free(): void;
  /** */
  constructor();
  /**
   * @returns {number}
   */
  size(): number;
  /**
   * @param {number} index
   * @returns {PublicKey}
   */
  get(index: number): PublicKey;
  /**
   * @param {PublicKey} key
   */
  add(key: PublicKey): void;
}
/** */
export class Redeemer {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Redeemer}
   */
  static from_bytes(bytes: Uint8Array): Redeemer;
  /**
   * @returns {RedeemerTag}
   */
  tag(): RedeemerTag;
  /**
   * @returns {BigNum}
   */
  index(): BigNum;
  /**
   * @returns {PlutusData}
   */
  data(): PlutusData;
  /**
   * @returns {ExUnits}
   */
  ex_units(): ExUnits;
  /**
   * @param {RedeemerTag} tag
   * @param {BigNum} index
   * @param {PlutusData} data
   * @param {ExUnits} ex_units
   * @returns {Redeemer}
   */
  static new(
    tag: RedeemerTag,
    index: BigNum,
    data: PlutusData,
    ex_units: ExUnits,
  ): Redeemer;
}
/** */
export class RedeemerTag {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {RedeemerTag}
   */
  static from_bytes(bytes: Uint8Array): RedeemerTag;
  /**
   * @returns {RedeemerTag}
   */
  static new_spend(): RedeemerTag;
  /**
   * @returns {RedeemerTag}
   */
  static new_mint(): RedeemerTag;
  /**
   * @returns {RedeemerTag}
   */
  static new_cert(): RedeemerTag;
  /**
   * @returns {RedeemerTag}
   */
  static new_reward(): RedeemerTag;
  /**
   * @returns {number}
   */
  kind(): number;
}
/** */
export class RedeemerWitnessKey {
  free(): void;
  /**
   * @returns {RedeemerTag}
   */
  tag(): RedeemerTag;
  /**
   * @returns {BigNum}
   */
  index(): BigNum;
  /**
   * @param {RedeemerTag} tag
   * @param {BigNum} index
   * @returns {RedeemerWitnessKey}
   */
  static new(tag: RedeemerTag, index: BigNum): RedeemerWitnessKey;
}
/** */
export class Redeemers {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Redeemers}
   */
  static from_bytes(bytes: Uint8Array): Redeemers;
  /**
   * @returns {Redeemers}
   */
  static new(): Redeemers;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {Redeemer}
   */
  get(index: number): Redeemer;
  /**
   * @param {Redeemer} elem
   */
  add(elem: Redeemer): void;
}
/** */
export class Relay {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Relay}
   */
  static from_bytes(bytes: Uint8Array): Relay;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {RelayJSON}
   */
  to_js_value(): RelayJSON;
  /**
   * @param {string} json
   * @returns {Relay}
   */
  static from_json(json: string): Relay;
  /**
   * @param {SingleHostAddr} single_host_addr
   * @returns {Relay}
   */
  static new_single_host_addr(single_host_addr: SingleHostAddr): Relay;
  /**
   * @param {SingleHostName} single_host_name
   * @returns {Relay}
   */
  static new_single_host_name(single_host_name: SingleHostName): Relay;
  /**
   * @param {MultiHostName} multi_host_name
   * @returns {Relay}
   */
  static new_multi_host_name(multi_host_name: MultiHostName): Relay;
  /**
   * @returns {number}
   */
  kind(): number;
  /**
   * @returns {SingleHostAddr | undefined}
   */
  as_single_host_addr(): SingleHostAddr | undefined;
  /**
   * @returns {SingleHostName | undefined}
   */
  as_single_host_name(): SingleHostName | undefined;
  /**
   * @returns {MultiHostName | undefined}
   */
  as_multi_host_name(): MultiHostName | undefined;
}
/** */
export class Relays {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Relays}
   */
  static from_bytes(bytes: Uint8Array): Relays;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {RelaysJSON}
   */
  to_js_value(): RelaysJSON;
  /**
   * @param {string} json
   * @returns {Relays}
   */
  static from_json(json: string): Relays;
  /**
   * @returns {Relays}
   */
  static new(): Relays;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {Relay}
   */
  get(index: number): Relay;
  /**
   * @param {Relay} elem
   */
  add(elem: Relay): void;
}
/** */
export class RequiredWitnessSet {
  free(): void;
  /**
   * @param {Vkeywitness} vkey
   */
  add_vkey(vkey: Vkeywitness): void;
  /**
   * @param {Vkey} vkey
   */
  add_vkey_key(vkey: Vkey): void;
  /**
   * @param {Ed25519KeyHash} hash
   */
  add_vkey_key_hash(hash: Ed25519KeyHash): void;
  /**
   * @param {BootstrapWitness} bootstrap
   */
  add_bootstrap(bootstrap: BootstrapWitness): void;
  /**
   * @param {Vkey} bootstrap
   */
  add_bootstrap_key(bootstrap: Vkey): void;
  /**
   * @param {Ed25519KeyHash} hash
   */
  add_bootstrap_key_hash(hash: Ed25519KeyHash): void;
  /**
   * @param {NativeScript} native_script
   */
  add_native_script(native_script: NativeScript): void;
  /**
   * @param {ScriptHash} native_script
   */
  add_native_script_hash(native_script: ScriptHash): void;
  /**
   * @param {PlutusScript} plutus_script
   */
  add_plutus_script(plutus_script: PlutusScript): void;
  /**
   * @param {PlutusScript} plutus_script
   */
  add_plutus_v2_script(plutus_script: PlutusScript): void;
  /**
   * @param {ScriptHash} plutus_script
   */
  add_plutus_hash(plutus_script: ScriptHash): void;
  /**
   * @param {PlutusData} plutus_datum
   */
  add_plutus_datum(plutus_datum: PlutusData): void;
  /**
   * @param {DataHash} plutus_datum
   */
  add_plutus_datum_hash(plutus_datum: DataHash): void;
  /**
   * @param {Redeemer} redeemer
   */
  add_redeemer(redeemer: Redeemer): void;
  /**
   * @param {RedeemerWitnessKey} redeemer
   */
  add_redeemer_tag(redeemer: RedeemerWitnessKey): void;
  /**
   * @param {RequiredWitnessSet} requirements
   */
  add_all(requirements: RequiredWitnessSet): void;
  /**
   * @returns {RequiredWitnessSet}
   */
  static new(): RequiredWitnessSet;
}
/** */
export class RewardAddress {
  free(): void;
  /**
   * @param {number} network
   * @param {StakeCredential} payment
   * @returns {RewardAddress}
   */
  static new(network: number, payment: StakeCredential): RewardAddress;
  /**
   * @returns {StakeCredential}
   */
  payment_cred(): StakeCredential;
  /**
   * @returns {Address}
   */
  to_address(): Address;
  /**
   * @param {Address} addr
   * @returns {RewardAddress | undefined}
   */
  static from_address(addr: Address): RewardAddress | undefined;
}
/** */
export class RewardAddresses {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {RewardAddresses}
   */
  static from_bytes(bytes: Uint8Array): RewardAddresses;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {RewardAddressesJSON}
   */
  to_js_value(): RewardAddressesJSON;
  /**
   * @param {string} json
   * @returns {RewardAddresses}
   */
  static from_json(json: string): RewardAddresses;
  /**
   * @returns {RewardAddresses}
   */
  static new(): RewardAddresses;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {RewardAddress}
   */
  get(index: number): RewardAddress;
  /**
   * @param {RewardAddress} elem
   */
  add(elem: RewardAddress): void;
}
/** */
export class Script {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Script}
   */
  static from_bytes(bytes: Uint8Array): Script;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {ScriptJSON}
   */
  to_js_value(): ScriptJSON;
  /**
   * @param {string} json
   * @returns {Script}
   */
  static from_json(json: string): Script;
  /**
   * @param {NativeScript} native_script
   * @returns {Script}
   */
  static new_native(native_script: NativeScript): Script;
  /**
   * @param {PlutusScript} plutus_script
   * @returns {Script}
   */
  static new_plutus_v1(plutus_script: PlutusScript): Script;
  /**
   * @param {PlutusScript} plutus_script
   * @returns {Script}
   */
  static new_plutus_v2(plutus_script: PlutusScript): Script;
  /**
   * @returns {number}
   */
  kind(): number;
  /**
   * @returns {NativeScript | undefined}
   */
  as_native(): NativeScript | undefined;
  /**
   * @returns {PlutusScript | undefined}
   */
  as_plutus_v1(): PlutusScript | undefined;
  /**
   * @returns {PlutusScript | undefined}
   */
  as_plutus_v2(): PlutusScript | undefined;
}
/** */
export class ScriptAll {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ScriptAll}
   */
  static from_bytes(bytes: Uint8Array): ScriptAll;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {ScriptAllJSON}
   */
  to_js_value(): ScriptAllJSON;
  /**
   * @param {string} json
   * @returns {ScriptAll}
   */
  static from_json(json: string): ScriptAll;
  /**
   * @returns {NativeScripts}
   */
  native_scripts(): NativeScripts;
  /**
   * @param {NativeScripts} native_scripts
   * @returns {ScriptAll}
   */
  static new(native_scripts: NativeScripts): ScriptAll;
}
/** */
export class ScriptAny {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ScriptAny}
   */
  static from_bytes(bytes: Uint8Array): ScriptAny;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {ScriptAnyJSON}
   */
  to_js_value(): ScriptAnyJSON;
  /**
   * @param {string} json
   * @returns {ScriptAny}
   */
  static from_json(json: string): ScriptAny;
  /**
   * @returns {NativeScripts}
   */
  native_scripts(): NativeScripts;
  /**
   * @param {NativeScripts} native_scripts
   * @returns {ScriptAny}
   */
  static new(native_scripts: NativeScripts): ScriptAny;
}
/** */
export class ScriptDataHash {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {ScriptDataHash}
   */
  static from_bytes(bytes: Uint8Array): ScriptDataHash;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string} prefix
   * @returns {string}
   */
  to_bech32(prefix: string): string;
  /**
   * @param {string} bech_str
   * @returns {ScriptDataHash}
   */
  static from_bech32(bech_str: string): ScriptDataHash;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} hex
   * @returns {ScriptDataHash}
   */
  static from_hex(hex: string): ScriptDataHash;
}
/** */
export class ScriptHash {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {ScriptHash}
   */
  static from_bytes(bytes: Uint8Array): ScriptHash;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string} prefix
   * @returns {string}
   */
  to_bech32(prefix: string): string;
  /**
   * @param {string} bech_str
   * @returns {ScriptHash}
   */
  static from_bech32(bech_str: string): ScriptHash;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} hex
   * @returns {ScriptHash}
   */
  static from_hex(hex: string): ScriptHash;
}
/** */
export class ScriptHashes {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ScriptHashes}
   */
  static from_bytes(bytes: Uint8Array): ScriptHashes;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {ScriptHashesJSON}
   */
  to_js_value(): ScriptHashesJSON;
  /**
   * @param {string} json
   * @returns {ScriptHashes}
   */
  static from_json(json: string): ScriptHashes;
  /**
   * @returns {ScriptHashes}
   */
  static new(): ScriptHashes;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {ScriptHash}
   */
  get(index: number): ScriptHash;
  /**
   * @param {ScriptHash} elem
   */
  add(elem: ScriptHash): void;
}
/** */
export class ScriptNOfK {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ScriptNOfK}
   */
  static from_bytes(bytes: Uint8Array): ScriptNOfK;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {ScriptNOfKJSON}
   */
  to_js_value(): ScriptNOfKJSON;
  /**
   * @param {string} json
   * @returns {ScriptNOfK}
   */
  static from_json(json: string): ScriptNOfK;
  /**
   * @returns {number}
   */
  n(): number;
  /**
   * @returns {NativeScripts}
   */
  native_scripts(): NativeScripts;
  /**
   * @param {number} n
   * @param {NativeScripts} native_scripts
   * @returns {ScriptNOfK}
   */
  static new(n: number, native_scripts: NativeScripts): ScriptNOfK;
}
/** */
export class ScriptPubkey {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ScriptPubkey}
   */
  static from_bytes(bytes: Uint8Array): ScriptPubkey;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {ScriptPubkeyJSON}
   */
  to_js_value(): ScriptPubkeyJSON;
  /**
   * @param {string} json
   * @returns {ScriptPubkey}
   */
  static from_json(json: string): ScriptPubkey;
  /**
   * @returns {Ed25519KeyHash}
   */
  addr_keyhash(): Ed25519KeyHash;
  /**
   * @param {Ed25519KeyHash} addr_keyhash
   * @returns {ScriptPubkey}
   */
  static new(addr_keyhash: Ed25519KeyHash): ScriptPubkey;
}
/** */
export class ScriptRef {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {ScriptRef}
   */
  static from_bytes(bytes: Uint8Array): ScriptRef;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {ScriptRefJSON}
   */
  to_js_value(): ScriptRefJSON;
  /**
   * @param {string} json
   * @returns {ScriptRef}
   */
  static from_json(json: string): ScriptRef;
  /**
   * @param {Script} script
   * @returns {ScriptRef}
   */
  static new(script: Script): ScriptRef;
  /**
   * @returns {Script}
   */
  get(): Script;
}
/** */
export class ScriptWitness {
  free(): void;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {ScriptWitnessJSON}
   */
  to_js_value(): ScriptWitnessJSON;
  /**
   * @param {string} json
   * @returns {ScriptWitness}
   */
  static from_json(json: string): ScriptWitness;
  /**
   * @param {NativeScript} native_script
   * @returns {ScriptWitness}
   */
  static new_native_witness(native_script: NativeScript): ScriptWitness;
  /**
   * @param {PlutusWitness} plutus_witness
   * @returns {ScriptWitness}
   */
  static new_plutus_witness(plutus_witness: PlutusWitness): ScriptWitness;
  /**
   * @returns {number}
   */
  kind(): number;
  /**
   * @returns {NativeScript | undefined}
   */
  as_native_witness(): NativeScript | undefined;
  /**
   * @returns {PlutusWitness | undefined}
   */
  as_plutus_witness(): PlutusWitness | undefined;
}
/** */
export class SingleHostAddr {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {SingleHostAddr}
   */
  static from_bytes(bytes: Uint8Array): SingleHostAddr;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {SingleHostAddrJSON}
   */
  to_js_value(): SingleHostAddrJSON;
  /**
   * @param {string} json
   * @returns {SingleHostAddr}
   */
  static from_json(json: string): SingleHostAddr;
  /**
   * @returns {number | undefined}
   */
  port(): number | undefined;
  /**
   * @returns {Ipv4 | undefined}
   */
  ipv4(): Ipv4 | undefined;
  /**
   * @returns {Ipv6 | undefined}
   */
  ipv6(): Ipv6 | undefined;
  /**
   * @param {number | undefined} port
   * @param {Ipv4 | undefined} ipv4
   * @param {Ipv6 | undefined} ipv6
   * @returns {SingleHostAddr}
   */
  static new(port?: number, ipv4?: Ipv4, ipv6?: Ipv6): SingleHostAddr;
}
/** */
export class SingleHostName {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {SingleHostName}
   */
  static from_bytes(bytes: Uint8Array): SingleHostName;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {SingleHostNameJSON}
   */
  to_js_value(): SingleHostNameJSON;
  /**
   * @param {string} json
   * @returns {SingleHostName}
   */
  static from_json(json: string): SingleHostName;
  /**
   * @returns {number | undefined}
   */
  port(): number | undefined;
  /**
   * @returns {DNSRecordAorAAAA}
   */
  dns_name(): DNSRecordAorAAAA;
  /**
   * @param {number | undefined} port
   * @param {DNSRecordAorAAAA} dns_name
   * @returns {SingleHostName}
   */
  static new(
    port: number | undefined,
    dns_name: DNSRecordAorAAAA,
  ): SingleHostName;
}
/** */
export class StakeCredential {
  free(): void;
  /**
   * @param {Ed25519KeyHash} hash
   * @returns {StakeCredential}
   */
  static from_keyhash(hash: Ed25519KeyHash): StakeCredential;
  /**
   * @param {ScriptHash} hash
   * @returns {StakeCredential}
   */
  static from_scripthash(hash: ScriptHash): StakeCredential;
  /**
   * @returns {Ed25519KeyHash | undefined}
   */
  to_keyhash(): Ed25519KeyHash | undefined;
  /**
   * @returns {ScriptHash | undefined}
   */
  to_scripthash(): ScriptHash | undefined;
  /**
   * @returns {number}
   */
  kind(): number;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {StakeCredential}
   */
  static from_bytes(bytes: Uint8Array): StakeCredential;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {StakeCredentialJSON}
   */
  to_js_value(): StakeCredentialJSON;
  /**
   * @param {string} json
   * @returns {StakeCredential}
   */
  static from_json(json: string): StakeCredential;
}
/** */
export class StakeCredentials {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {StakeCredentials}
   */
  static from_bytes(bytes: Uint8Array): StakeCredentials;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {StakeCredentialsJSON}
   */
  to_js_value(): StakeCredentialsJSON;
  /**
   * @param {string} json
   * @returns {StakeCredentials}
   */
  static from_json(json: string): StakeCredentials;
  /**
   * @returns {StakeCredentials}
   */
  static new(): StakeCredentials;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {StakeCredential}
   */
  get(index: number): StakeCredential;
  /**
   * @param {StakeCredential} elem
   */
  add(elem: StakeCredential): void;
}
/** */
export class StakeDelegation {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {StakeDelegation}
   */
  static from_bytes(bytes: Uint8Array): StakeDelegation;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {StakeDelegationJSON}
   */
  to_js_value(): StakeDelegationJSON;
  /**
   * @param {string} json
   * @returns {StakeDelegation}
   */
  static from_json(json: string): StakeDelegation;
  /**
   * @returns {StakeCredential}
   */
  stake_credential(): StakeCredential;
  /**
   * @returns {Ed25519KeyHash}
   */
  pool_keyhash(): Ed25519KeyHash;
  /**
   * @param {StakeCredential} stake_credential
   * @param {Ed25519KeyHash} pool_keyhash
   * @returns {StakeDelegation}
   */
  static new(
    stake_credential: StakeCredential,
    pool_keyhash: Ed25519KeyHash,
  ): StakeDelegation;
}
/** */
export class StakeDeregistration {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {StakeDeregistration}
   */
  static from_bytes(bytes: Uint8Array): StakeDeregistration;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {StakeDeregistrationJSON}
   */
  to_js_value(): StakeDeregistrationJSON;
  /**
   * @param {string} json
   * @returns {StakeDeregistration}
   */
  static from_json(json: string): StakeDeregistration;
  /**
   * @returns {StakeCredential}
   */
  stake_credential(): StakeCredential;
  /**
   * @param {StakeCredential} stake_credential
   * @returns {StakeDeregistration}
   */
  static new(stake_credential: StakeCredential): StakeDeregistration;
}
/** */
export class StakeRegistration {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {StakeRegistration}
   */
  static from_bytes(bytes: Uint8Array): StakeRegistration;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {StakeRegistrationJSON}
   */
  to_js_value(): StakeRegistrationJSON;
  /**
   * @param {string} json
   * @returns {StakeRegistration}
   */
  static from_json(json: string): StakeRegistration;
  /**
   * @returns {StakeCredential}
   */
  stake_credential(): StakeCredential;
  /**
   * @param {StakeCredential} stake_credential
   * @returns {StakeRegistration}
   */
  static new(stake_credential: StakeCredential): StakeRegistration;
}
/** */
export class Strings {
  free(): void;
  /**
   * @returns {Strings}
   */
  static new(): Strings;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {string}
   */
  get(index: number): string;
  /**
   * @param {string} elem
   */
  add(elem: string): void;
}
/** */
export class TimelockExpiry {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TimelockExpiry}
   */
  static from_bytes(bytes: Uint8Array): TimelockExpiry;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {TimelockExpiryJSON}
   */
  to_js_value(): TimelockExpiryJSON;
  /**
   * @param {string} json
   * @returns {TimelockExpiry}
   */
  static from_json(json: string): TimelockExpiry;
  /**
   * @returns {BigNum}
   */
  slot(): BigNum;
  /**
   * @param {BigNum} slot
   * @returns {TimelockExpiry}
   */
  static new(slot: BigNum): TimelockExpiry;
}
/** */
export class TimelockStart {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TimelockStart}
   */
  static from_bytes(bytes: Uint8Array): TimelockStart;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {TimelockStartJSON}
   */
  to_js_value(): TimelockStartJSON;
  /**
   * @param {string} json
   * @returns {TimelockStart}
   */
  static from_json(json: string): TimelockStart;
  /**
   * @returns {BigNum}
   */
  slot(): BigNum;
  /**
   * @param {BigNum} slot
   * @returns {TimelockStart}
   */
  static new(slot: BigNum): TimelockStart;
}
/** */
export class Transaction {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Transaction}
   */
  static from_bytes(bytes: Uint8Array): Transaction;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {TransactionJSON}
   */
  to_js_value(): TransactionJSON;
  /**
   * @param {string} json
   * @returns {Transaction}
   */
  static from_json(json: string): Transaction;
  /**
   * @returns {TransactionBody}
   */
  body(): TransactionBody;
  /**
   * @returns {TransactionWitnessSet}
   */
  witness_set(): TransactionWitnessSet;
  /**
   * @returns {boolean}
   */
  is_valid(): boolean;
  /**
   * @returns {AuxiliaryData | undefined}
   */
  auxiliary_data(): AuxiliaryData | undefined;
  /**
   * @param {boolean} valid
   */
  set_is_valid(valid: boolean): void;
  /**
   * @param {TransactionBody} body
   * @param {TransactionWitnessSet} witness_set
   * @param {AuxiliaryData | undefined} auxiliary_data
   * @returns {Transaction}
   */
  static new(
    body: TransactionBody,
    witness_set: TransactionWitnessSet,
    auxiliary_data?: AuxiliaryData,
  ): Transaction;
}
/** */
export class TransactionBodies {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionBodies}
   */
  static from_bytes(bytes: Uint8Array): TransactionBodies;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {TransactionBodiesJSON}
   */
  to_js_value(): TransactionBodiesJSON;
  /**
   * @param {string} json
   * @returns {TransactionBodies}
   */
  static from_json(json: string): TransactionBodies;
  /**
   * @returns {TransactionBodies}
   */
  static new(): TransactionBodies;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {TransactionBody}
   */
  get(index: number): TransactionBody;
  /**
   * @param {TransactionBody} elem
   */
  add(elem: TransactionBody): void;
}
/** */
export class TransactionBody {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionBody}
   */
  static from_bytes(bytes: Uint8Array): TransactionBody;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {TransactionBodyJSON}
   */
  to_js_value(): TransactionBodyJSON;
  /**
   * @param {string} json
   * @returns {TransactionBody}
   */
  static from_json(json: string): TransactionBody;
  /**
   * @returns {TransactionInputs}
   */
  inputs(): TransactionInputs;
  /**
   * @returns {TransactionOutputs}
   */
  outputs(): TransactionOutputs;
  /**
   * @returns {BigNum}
   */
  fee(): BigNum;
  /**
   * @returns {BigNum | undefined}
   */
  ttl(): BigNum | undefined;
  /**
   * @param {Certificates} certs
   */
  set_certs(certs: Certificates): void;
  /**
   * @returns {Certificates | undefined}
   */
  certs(): Certificates | undefined;
  /**
   * @param {Withdrawals} withdrawals
   */
  set_withdrawals(withdrawals: Withdrawals): void;
  /**
   * @returns {Withdrawals | undefined}
   */
  withdrawals(): Withdrawals | undefined;
  /**
   * @param {Update} update
   */
  set_update(update: Update): void;
  /**
   * @returns {Update | undefined}
   */
  update(): Update | undefined;
  /**
   * @param {AuxiliaryDataHash} auxiliary_data_hash
   */
  set_auxiliary_data_hash(auxiliary_data_hash: AuxiliaryDataHash): void;
  /**
   * @returns {AuxiliaryDataHash | undefined}
   */
  auxiliary_data_hash(): AuxiliaryDataHash | undefined;
  /**
   * @param {BigNum} validity_start_interval
   */
  set_validity_start_interval(validity_start_interval: BigNum): void;
  /**
   * @returns {BigNum | undefined}
   */
  validity_start_interval(): BigNum | undefined;
  /**
   * @param {Mint} mint
   */
  set_mint(mint: Mint): void;
  /**
   * @returns {Mint | undefined}
   */
  mint(): Mint | undefined;
  /**
   * @param {ScriptDataHash} script_data_hash
   */
  set_script_data_hash(script_data_hash: ScriptDataHash): void;
  /**
   * @returns {ScriptDataHash | undefined}
   */
  script_data_hash(): ScriptDataHash | undefined;
  /**
   * @param {TransactionInputs} collateral
   */
  set_collateral(collateral: TransactionInputs): void;
  /**
   * @returns {TransactionInputs | undefined}
   */
  collateral(): TransactionInputs | undefined;
  /**
   * @param {Ed25519KeyHashes} required_signers
   */
  set_required_signers(required_signers: Ed25519KeyHashes): void;
  /**
   * @returns {Ed25519KeyHashes | undefined}
   */
  required_signers(): Ed25519KeyHashes | undefined;
  /**
   * @param {NetworkId} network_id
   */
  set_network_id(network_id: NetworkId): void;
  /**
   * @returns {NetworkId | undefined}
   */
  network_id(): NetworkId | undefined;
  /**
   * @param {TransactionOutput} collateral_return
   */
  set_collateral_return(collateral_return: TransactionOutput): void;
  /**
   * @returns {TransactionOutput | undefined}
   */
  collateral_return(): TransactionOutput | undefined;
  /**
   * @param {BigNum} total_collateral
   */
  set_total_collateral(total_collateral: BigNum): void;
  /**
   * @returns {BigNum | undefined}
   */
  total_collateral(): BigNum | undefined;
  /**
   * @param {TransactionInputs} reference_inputs
   */
  set_reference_inputs(reference_inputs: TransactionInputs): void;
  /**
   * @returns {TransactionInputs | undefined}
   */
  reference_inputs(): TransactionInputs | undefined;
  /**
   * @param {TransactionInputs} inputs
   * @param {TransactionOutputs} outputs
   * @param {BigNum} fee
   * @param {BigNum | undefined} ttl
   * @returns {TransactionBody}
   */
  static new(
    inputs: TransactionInputs,
    outputs: TransactionOutputs,
    fee: BigNum,
    ttl?: BigNum,
  ): TransactionBody;
  /**
   * @returns {Uint8Array | undefined}
   */
  raw(): Uint8Array | undefined;
}
/** */
export class TransactionBuilder {
  free(): void;
  /**
   * This automatically selects and adds inputs from {inputs} consisting of just enough to cover
   * the outputs that have already been added.
   * This should be called after adding all certs/outputs/etc and will be an error otherwise.
   * Adding a change output must be called after via TransactionBuilder::balance()
   * inputs to cover the minimum fees. This does not, however, set the txbuilder's fee.
   *
   * change_address is required here in order to determine the min ada requirement precisely
   * @param {TransactionUnspentOutputs} inputs
   * @param {Address} change_address
   */
  add_inputs_from(
    inputs: TransactionUnspentOutputs,
    change_address: Address,
  ): void;
  /**
   * @param {TransactionUnspentOutput} utxo
   * @param {ScriptWitness | undefined} script_witness
   */
  add_input(
    utxo: TransactionUnspentOutput,
    script_witness?: ScriptWitness,
  ): void;
  /**
   * @param {TransactionUnspentOutput} utxo
   */
  add_reference_input(utxo: TransactionUnspentOutput): void;
  /**
   * calculates how much the fee would increase if you added a given output
   * @param {Address} address
   * @param {TransactionInput} input
   * @param {Value} amount
   * @returns {BigNum}
   */
  fee_for_input(
    address: Address,
    input: TransactionInput,
    amount: Value,
  ): BigNum;
  /**
   * Add explicit output via a TransactionOutput object
   * @param {TransactionOutput} output
   */
  add_output(output: TransactionOutput): void;
  /**
   * Add plutus scripts via a PlutusScripts object
   * @param {PlutusScript} plutus_script
   */
  add_plutus_script(plutus_script: PlutusScript): void;
  /**
   * Add plutus v2 scripts via a PlutusScripts object
   * @param {PlutusScript} plutus_script
   */
  add_plutus_v2_script(plutus_script: PlutusScript): void;
  /**
   * Add plutus data via a PlutusData object
   * @param {PlutusData} plutus_data
   */
  add_plutus_data(plutus_data: PlutusData): void;
  /**
   * Add native scripts via a NativeScripts object
   * @param {NativeScript} native_script
   */
  add_native_script(native_script: NativeScript): void;
  /**
   * Add certificate via a Certificates object
   * @param {Certificate} certificate
   * @param {ScriptWitness | undefined} script_witness
   */
  add_certificate(
    certificate: Certificate,
    script_witness?: ScriptWitness,
  ): void;
  /**
   * calculates how much the fee would increase if you added a given output
   * @param {TransactionOutput} output
   * @returns {BigNum}
   */
  fee_for_output(output: TransactionOutput): BigNum;
  /**
   * @param {BigNum} ttl
   */
  set_ttl(ttl: BigNum): void;
  /**
   * @param {BigNum} validity_start_interval
   */
  set_validity_start_interval(validity_start_interval: BigNum): void;
  /**
   * @param {RewardAddress} reward_address
   * @param {BigNum} coin
   * @param {ScriptWitness | undefined} script_witness
   */
  add_withdrawal(
    reward_address: RewardAddress,
    coin: BigNum,
    script_witness?: ScriptWitness,
  ): void;
  /**
   * @returns {AuxiliaryData | undefined}
   */
  auxiliary_data(): AuxiliaryData | undefined;
  /**
   * Set explicit auxiliary data via an AuxiliaryData object
   * It might contain some metadata plus native or Plutus scripts
   * @param {AuxiliaryData} auxiliary_data
   */
  set_auxiliary_data(auxiliary_data: AuxiliaryData): void;
  /**
   * Set metadata using a GeneralTransactionMetadata object
   * It will be set to the existing or new auxiliary data in this builder
   * @param {GeneralTransactionMetadata} metadata
   */
  set_metadata(metadata: GeneralTransactionMetadata): void;
  /**
   * Add a single metadatum using TransactionMetadatumLabel and TransactionMetadatum objects
   * It will be securely added to existing or new metadata in this builder
   * @param {BigNum} key
   * @param {TransactionMetadatum} val
   */
  add_metadatum(key: BigNum, val: TransactionMetadatum): void;
  /**
   * Add a single JSON metadatum using a TransactionMetadatumLabel and a String
   * It will be securely added to existing or new metadata in this builder
   * @param {BigNum} key
   * @param {string} val
   */
  add_json_metadatum(key: BigNum, val: string): void;
  /**
   * Add a single JSON metadatum using a TransactionMetadatumLabel, a String, and a MetadataJsonSchema object
   * It will be securely added to existing or new metadata in this builder
   * @param {BigNum} key
   * @param {string} val
   * @param {number} schema
   */
  add_json_metadatum_with_schema(
    key: BigNum,
    val: string,
    schema: number,
  ): void;
  /**
   * Returns a copy of the current mint state in the builder
   * @returns {Mint | undefined}
   */
  mint(): Mint | undefined;
  /**
   * @returns {Certificates | undefined}
   */
  certificates(): Certificates | undefined;
  /**
   * @returns {Withdrawals | undefined}
   */
  withdrawals(): Withdrawals | undefined;
  /**
   * Returns a copy of the current witness native scripts in the builder
   * @returns {NativeScripts | undefined}
   */
  native_scripts(): NativeScripts | undefined;
  /**
   * Add a mint entry to this builder using a PolicyID and MintAssets object
   * It will be securely added to existing or new Mint in this builder
   * It will securely add assets to an existing PolicyID
   * But it will replace/overwrite any existing mint assets with the same PolicyID
   * first redeemer applied to a PolicyID is taken for all further assets added to the same PolicyID
   * @param {ScriptHash} policy_id
   * @param {MintAssets} mint_assets
   * @param {ScriptWitness | undefined} script_witness
   */
  add_mint(
    policy_id: ScriptHash,
    mint_assets: MintAssets,
    script_witness?: ScriptWitness,
  ): void;
  /**
   * @param {TransactionBuilderConfig} cfg
   * @returns {TransactionBuilder}
   */
  static new(cfg: TransactionBuilderConfig): TransactionBuilder;
  /**
   * @returns {ScriptDataHash | undefined}
   */
  script_data_hash(): ScriptDataHash | undefined;
  /**
   * @param {TransactionUnspentOutput} utxo
   */
  add_collateral(utxo: TransactionUnspentOutput): void;
  /**
   * @returns {TransactionInputs | undefined}
   */
  get_collateral(): TransactionInputs | undefined;
  /**
   * @param {Ed25519KeyHash} required_signer
   */
  add_required_signer(required_signer: Ed25519KeyHash): void;
  /**
   * @returns {Ed25519KeyHashes | undefined}
   */
  required_signers(): Ed25519KeyHashes | undefined;
  /**
   * @param {NetworkId} network_id
   */
  set_network_id(network_id: NetworkId): void;
  /**
   * @returns {NetworkId | undefined}
   */
  network_id(): NetworkId | undefined;
  /**
   * @returns {Redeemers | undefined}
   */
  redeemers(): Redeemers | undefined;
  /**
   * does not include refunds or withdrawals
   * @returns {Value}
   */
  get_explicit_input(): Value;
  /**
   * withdrawals and refunds
   * @returns {Value}
   */
  get_implicit_input(): Value;
  /**
   * Return explicit input plus implicit input plus mint
   * @returns {Value}
   */
  get_total_input(): Value;
  /**
   * Return explicit output plus implicit output plus burn (does not consider fee directly)
   * @returns {Value}
   */
  get_total_output(): Value;
  /**
   * does not include fee
   * @returns {Value}
   */
  get_explicit_output(): Value;
  /**
   * @returns {BigNum}
   */
  get_deposit(): BigNum;
  /**
   * @returns {BigNum | undefined}
   */
  get_fee_if_set(): BigNum | undefined;
  /**
   * Warning: this function will mutate the /fee/ field
   * Make sure to call this function last after setting all other tx-body properties
   * Editing inputs, outputs, mint, etc. after change been calculated
   * might cause a mismatch in calculated fee versus the required fee
   * @param {Address} change_address
   * @param {Datum | undefined} datum
   */
  balance(change_address: Address, datum?: Datum): void;
  /**
   * Returns the TransactionBody.
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @returns {number}
   */
  full_size(): number;
  /**
   * @returns {Uint32Array}
   */
  output_sizes(): Uint32Array;
  /**
   * @returns {TransactionOutputs}
   */
  outputs(): TransactionOutputs;
  /**
   * Returns full Transaction object with the body and the auxiliary data
   *
   * NOTE: witness_set will contain all mint_scripts if any been added or set
   *
   * takes fetched ex units into consideration
   *
   * add collateral utxos and collateral change receiver in case you redeem from plutus script utxos
   *
   * async call
   *
   * NOTE: is_valid set to true
   * @param {TransactionUnspentOutputs | undefined} collateral_utxos
   * @param {Address | undefined} collateral_change_address
   * @param {boolean | undefined} native_uplc
   * @returns {Promise<Transaction>}
   */
  construct(
    collateral_utxos?: TransactionUnspentOutputs,
    collateral_change_address?: Address,
    native_uplc?: boolean,
  ): Promise<Transaction>;
  /**
   * Returns full Transaction object with the body and the auxiliary data
   * NOTE: witness_set will contain all mint_scripts if any been added or set
   * NOTE: is_valid set to true
   * @returns {Transaction}
   */
  build_tx(): Transaction;
  /**
   * warning: sum of all parts of a transaction must equal 0. You cannot just set the fee to the min value and forget about it
   * warning: min_fee may be slightly larger than the actual minimum fee (ex: a few lovelaces)
   * this is done to simplify the library code, but can be fixed later
   * @returns {BigNum}
   */
  min_fee(): BigNum;
}
/** */
export class TransactionBuilderConfig {
  free(): void;
}
/** */
export class TransactionBuilderConfigBuilder {
  free(): void;
  /**
   * @returns {TransactionBuilderConfigBuilder}
   */
  static new(): TransactionBuilderConfigBuilder;
  /**
   * @param {LinearFee} fee_algo
   * @returns {TransactionBuilderConfigBuilder}
   */
  fee_algo(fee_algo: LinearFee): TransactionBuilderConfigBuilder;
  /**
   * @param {BigNum} coins_per_utxo_byte
   * @returns {TransactionBuilderConfigBuilder}
   */
  coins_per_utxo_byte(
    coins_per_utxo_byte: BigNum,
  ): TransactionBuilderConfigBuilder;
  /**
   * @param {BigNum} pool_deposit
   * @returns {TransactionBuilderConfigBuilder}
   */
  pool_deposit(pool_deposit: BigNum): TransactionBuilderConfigBuilder;
  /**
   * @param {BigNum} key_deposit
   * @returns {TransactionBuilderConfigBuilder}
   */
  key_deposit(key_deposit: BigNum): TransactionBuilderConfigBuilder;
  /**
   * @param {number} max_value_size
   * @returns {TransactionBuilderConfigBuilder}
   */
  max_value_size(max_value_size: number): TransactionBuilderConfigBuilder;
  /**
   * @param {number} max_tx_size
   * @returns {TransactionBuilderConfigBuilder}
   */
  max_tx_size(max_tx_size: number): TransactionBuilderConfigBuilder;
  /**
   * @param {ExUnitPrices} ex_unit_prices
   * @returns {TransactionBuilderConfigBuilder}
   */
  ex_unit_prices(ex_unit_prices: ExUnitPrices): TransactionBuilderConfigBuilder;
  /**
   * @param {ExUnits} max_tx_ex_units
   * @returns {TransactionBuilderConfigBuilder}
   */
  max_tx_ex_units(max_tx_ex_units: ExUnits): TransactionBuilderConfigBuilder;
  /**
   * @param {Costmdls} costmdls
   * @returns {TransactionBuilderConfigBuilder}
   */
  costmdls(costmdls: Costmdls): TransactionBuilderConfigBuilder;
  /**
   * @param {number} collateral_percentage
   * @returns {TransactionBuilderConfigBuilder}
   */
  collateral_percentage(
    collateral_percentage: number,
  ): TransactionBuilderConfigBuilder;
  /**
   * @param {number} max_collateral_inputs
   * @returns {TransactionBuilderConfigBuilder}
   */
  max_collateral_inputs(
    max_collateral_inputs: number,
  ): TransactionBuilderConfigBuilder;
  /**
   * @param {BigNum} zero_time
   * @param {BigNum} zero_slot
   * @param {number} slot_length
   * @returns {TransactionBuilderConfigBuilder}
   */
  slot_config(
    zero_time: BigNum,
    zero_slot: BigNum,
    slot_length: number,
  ): TransactionBuilderConfigBuilder;
  /**
   * @param {Blockfrost} blockfrost
   * @returns {TransactionBuilderConfigBuilder}
   */
  blockfrost(blockfrost: Blockfrost): TransactionBuilderConfigBuilder;
  /**
   * @returns {TransactionBuilderConfig}
   */
  build(): TransactionBuilderConfig;
}
/** */
export class TransactionHash {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionHash}
   */
  static from_bytes(bytes: Uint8Array): TransactionHash;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string} prefix
   * @returns {string}
   */
  to_bech32(prefix: string): string;
  /**
   * @param {string} bech_str
   * @returns {TransactionHash}
   */
  static from_bech32(bech_str: string): TransactionHash;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} hex
   * @returns {TransactionHash}
   */
  static from_hex(hex: string): TransactionHash;
}
/** */
export class TransactionIndexes {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionIndexes}
   */
  static from_bytes(bytes: Uint8Array): TransactionIndexes;
  /**
   * @returns {TransactionIndexes}
   */
  static new(): TransactionIndexes;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {BigNum}
   */
  get(index: number): BigNum;
  /**
   * @param {BigNum} elem
   */
  add(elem: BigNum): void;
}
/** */
export class TransactionInput {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionInput}
   */
  static from_bytes(bytes: Uint8Array): TransactionInput;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {TransactionInputJSON}
   */
  to_js_value(): TransactionInputJSON;
  /**
   * @param {string} json
   * @returns {TransactionInput}
   */
  static from_json(json: string): TransactionInput;
  /**
   * @returns {TransactionHash}
   */
  transaction_id(): TransactionHash;
  /**
   * @returns {BigNum}
   */
  index(): BigNum;
  /**
   * @param {TransactionHash} transaction_id
   * @param {BigNum} index
   * @returns {TransactionInput}
   */
  static new(transaction_id: TransactionHash, index: BigNum): TransactionInput;
}
/** */
export class TransactionInputs {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionInputs}
   */
  static from_bytes(bytes: Uint8Array): TransactionInputs;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {TransactionInputsJSON}
   */
  to_js_value(): TransactionInputsJSON;
  /**
   * @param {string} json
   * @returns {TransactionInputs}
   */
  static from_json(json: string): TransactionInputs;
  /**
   * @returns {TransactionInputs}
   */
  static new(): TransactionInputs;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {TransactionInput}
   */
  get(index: number): TransactionInput;
  /**
   * @param {TransactionInput} elem
   */
  add(elem: TransactionInput): void;
  /** */
  sort(): void;
}
/** */
export class TransactionMetadatum {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionMetadatum}
   */
  static from_bytes(bytes: Uint8Array): TransactionMetadatum;
  /**
   * @param {MetadataMap} map
   * @returns {TransactionMetadatum}
   */
  static new_map(map: MetadataMap): TransactionMetadatum;
  /**
   * @param {MetadataList} list
   * @returns {TransactionMetadatum}
   */
  static new_list(list: MetadataList): TransactionMetadatum;
  /**
   * @param {Int} int
   * @returns {TransactionMetadatum}
   */
  static new_int(int: Int): TransactionMetadatum;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionMetadatum}
   */
  static new_bytes(bytes: Uint8Array): TransactionMetadatum;
  /**
   * @param {string} text
   * @returns {TransactionMetadatum}
   */
  static new_text(text: string): TransactionMetadatum;
  /**
   * @returns {number}
   */
  kind(): number;
  /**
   * @returns {MetadataMap}
   */
  as_map(): MetadataMap;
  /**
   * @returns {MetadataList}
   */
  as_list(): MetadataList;
  /**
   * @returns {Int}
   */
  as_int(): Int;
  /**
   * @returns {Uint8Array}
   */
  as_bytes(): Uint8Array;
  /**
   * @returns {string}
   */
  as_text(): string;
}
/** */
export class TransactionMetadatumLabels {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionMetadatumLabels}
   */
  static from_bytes(bytes: Uint8Array): TransactionMetadatumLabels;
  /**
   * @returns {TransactionMetadatumLabels}
   */
  static new(): TransactionMetadatumLabels;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {BigNum}
   */
  get(index: number): BigNum;
  /**
   * @param {BigNum} elem
   */
  add(elem: BigNum): void;
}
/** */
export class TransactionOutput {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionOutput}
   */
  static from_bytes(bytes: Uint8Array): TransactionOutput;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {TransactionOutputJSON}
   */
  to_js_value(): TransactionOutputJSON;
  /**
   * @param {string} json
   * @returns {TransactionOutput}
   */
  static from_json(json: string): TransactionOutput;
  /**
   * @returns {Address}
   */
  address(): Address;
  /**
   * @returns {Value}
   */
  amount(): Value;
  /**
   * @returns {Datum | undefined}
   */
  datum(): Datum | undefined;
  /**
   * @returns {ScriptRef | undefined}
   */
  script_ref(): ScriptRef | undefined;
  /**
   * @param {Datum} datum
   */
  set_datum(datum: Datum): void;
  /**
   * @param {ScriptRef} script_ref
   */
  set_script_ref(script_ref: ScriptRef): void;
  /**
   * @param {Address} address
   * @param {Value} amount
   * @returns {TransactionOutput}
   */
  static new(address: Address, amount: Value): TransactionOutput;
  /**
   * legacy support: serialize output as array array
   *
   * does not support inline datum and script_ref!
   * @returns {Uint8Array}
   */
  to_legacy_bytes(): Uint8Array;
}
/** */
export class TransactionOutputAmountBuilder {
  free(): void;
  /**
   * @param {Value} amount
   * @returns {TransactionOutputAmountBuilder}
   */
  with_value(amount: Value): TransactionOutputAmountBuilder;
  /**
   * @param {BigNum} coin
   * @returns {TransactionOutputAmountBuilder}
   */
  with_coin(coin: BigNum): TransactionOutputAmountBuilder;
  /**
   * @param {BigNum} coin
   * @param {MultiAsset} multiasset
   * @returns {TransactionOutputAmountBuilder}
   */
  with_coin_and_asset(
    coin: BigNum,
    multiasset: MultiAsset,
  ): TransactionOutputAmountBuilder;
  /**
   * @param {MultiAsset} multiasset
   * @param {BigNum} coins_per_utxo_word
   * @returns {TransactionOutputAmountBuilder}
   */
  with_asset_and_min_required_coin(
    multiasset: MultiAsset,
    coins_per_utxo_word: BigNum,
  ): TransactionOutputAmountBuilder;
  /**
   * @returns {TransactionOutput}
   */
  build(): TransactionOutput;
}
/**
 * We introduce a builder-pattern format for creating transaction outputs
 * This is because:
 * 1. Some fields (i.e. data hash) are optional, and we can't easily expose Option<> in WASM
 * 2. Some fields like amounts have many ways it could be set (some depending on other field values being known)
 * 3. Easier to adapt as the output format gets more complicated in future Cardano releases
 */
export class TransactionOutputBuilder {
  free(): void;
  /**
   * @returns {TransactionOutputBuilder}
   */
  static new(): TransactionOutputBuilder;
  /**
   * @param {Address} address
   * @returns {TransactionOutputBuilder}
   */
  with_address(address: Address): TransactionOutputBuilder;
  /**
   * @param {Datum} data_hash
   * @returns {TransactionOutputBuilder}
   */
  with_datum(data_hash: Datum): TransactionOutputBuilder;
  /**
   * @returns {TransactionOutputAmountBuilder}
   */
  next(): TransactionOutputAmountBuilder;
}
/** */
export class TransactionOutputs {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionOutputs}
   */
  static from_bytes(bytes: Uint8Array): TransactionOutputs;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {TransactionOutputsJSON}
   */
  to_js_value(): TransactionOutputsJSON;
  /**
   * @param {string} json
   * @returns {TransactionOutputs}
   */
  static from_json(json: string): TransactionOutputs;
  /**
   * @returns {TransactionOutputs}
   */
  static new(): TransactionOutputs;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {TransactionOutput}
   */
  get(index: number): TransactionOutput;
  /**
   * @param {TransactionOutput} elem
   */
  add(elem: TransactionOutput): void;
}
/** */
export class TransactionUnspentOutput {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionUnspentOutput}
   */
  static from_bytes(bytes: Uint8Array): TransactionUnspentOutput;
  /**
   * @param {TransactionInput} input
   * @param {TransactionOutput} output
   * @returns {TransactionUnspentOutput}
   */
  static new(
    input: TransactionInput,
    output: TransactionOutput,
  ): TransactionUnspentOutput;
  /**
   * @returns {TransactionInput}
   */
  input(): TransactionInput;
  /**
   * @returns {TransactionOutput}
   */
  output(): TransactionOutput;
  /**
   * @returns {Uint8Array}
   */
  to_legacy_bytes(): Uint8Array;
}
/** */
export class TransactionUnspentOutputs {
  free(): void;
  /**
   * @returns {TransactionUnspentOutputs}
   */
  static new(): TransactionUnspentOutputs;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {TransactionUnspentOutput}
   */
  get(index: number): TransactionUnspentOutput;
  /**
   * @param {TransactionUnspentOutput} elem
   */
  add(elem: TransactionUnspentOutput): void;
}
/** */
export class TransactionWitnessSet {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionWitnessSet}
   */
  static from_bytes(bytes: Uint8Array): TransactionWitnessSet;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {TransactionWitnessSetJSON}
   */
  to_js_value(): TransactionWitnessSetJSON;
  /**
   * @param {string} json
   * @returns {TransactionWitnessSet}
   */
  static from_json(json: string): TransactionWitnessSet;
  /**
   * @param {Vkeywitnesses} vkeys
   */
  set_vkeys(vkeys: Vkeywitnesses): void;
  /**
   * @returns {Vkeywitnesses | undefined}
   */
  vkeys(): Vkeywitnesses | undefined;
  /**
   * @param {NativeScripts} native_scripts
   */
  set_native_scripts(native_scripts: NativeScripts): void;
  /**
   * @returns {NativeScripts | undefined}
   */
  native_scripts(): NativeScripts | undefined;
  /**
   * @param {BootstrapWitnesses} bootstraps
   */
  set_bootstraps(bootstraps: BootstrapWitnesses): void;
  /**
   * @returns {BootstrapWitnesses | undefined}
   */
  bootstraps(): BootstrapWitnesses | undefined;
  /**
   * @param {PlutusScripts} plutus_scripts
   */
  set_plutus_scripts(plutus_scripts: PlutusScripts): void;
  /**
   * @returns {PlutusScripts | undefined}
   */
  plutus_scripts(): PlutusScripts | undefined;
  /**
   * @param {PlutusList} plutus_data
   */
  set_plutus_data(plutus_data: PlutusList): void;
  /**
   * @returns {PlutusList | undefined}
   */
  plutus_data(): PlutusList | undefined;
  /**
   * @param {Redeemers} redeemers
   */
  set_redeemers(redeemers: Redeemers): void;
  /**
   * @param {PlutusScripts} plutus_scripts
   */
  set_plutus_v2_scripts(plutus_scripts: PlutusScripts): void;
  /**
   * @returns {Redeemers | undefined}
   */
  redeemers(): Redeemers | undefined;
  /**
   * @returns {PlutusScripts | undefined}
   */
  plutus_v2_scripts(): PlutusScripts | undefined;
  /**
   * @returns {TransactionWitnessSet}
   */
  static new(): TransactionWitnessSet;
}
/**
 * Builder de-duplicates witnesses as they are added
 */
export class TransactionWitnessSetBuilder {
  free(): void;
  /**
   * @param {Vkeywitness} vkey
   */
  add_vkey(vkey: Vkeywitness): void;
  /**
   * @param {BootstrapWitness} bootstrap
   */
  add_bootstrap(bootstrap: BootstrapWitness): void;
  /**
   * @param {NativeScript} native_script
   */
  add_native_script(native_script: NativeScript): void;
  /**
   * @param {PlutusScript} plutus_script
   */
  add_plutus_script(plutus_script: PlutusScript): void;
  /**
   * @param {PlutusScript} plutus_script
   */
  add_plutus_v2_script(plutus_script: PlutusScript): void;
  /**
   * @param {PlutusData} plutus_datum
   */
  add_plutus_datum(plutus_datum: PlutusData): void;
  /**
   * @param {Redeemer} redeemer
   */
  add_redeemer(redeemer: Redeemer): void;
  /**
   * @param {RequiredWitnessSet} required_wits
   */
  add_required_wits(required_wits: RequiredWitnessSet): void;
  /**
   * @returns {TransactionWitnessSetBuilder}
   */
  static new(): TransactionWitnessSetBuilder;
  /**
   * @param {TransactionWitnessSet} wit_set
   */
  add_existing(wit_set: TransactionWitnessSet): void;
  /**
   * @returns {TransactionWitnessSet}
   */
  build(): TransactionWitnessSet;
}
/** */
export class TransactionWitnessSets {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {TransactionWitnessSets}
   */
  static from_bytes(bytes: Uint8Array): TransactionWitnessSets;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {TransactionWitnessSetsJSON}
   */
  to_js_value(): TransactionWitnessSetsJSON;
  /**
   * @param {string} json
   * @returns {TransactionWitnessSets}
   */
  static from_json(json: string): TransactionWitnessSets;
  /**
   * @returns {TransactionWitnessSets}
   */
  static new(): TransactionWitnessSets;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {TransactionWitnessSet}
   */
  get(index: number): TransactionWitnessSet;
  /**
   * @param {TransactionWitnessSet} elem
   */
  add(elem: TransactionWitnessSet): void;
}
/** */
export class URL {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {URL}
   */
  static from_bytes(bytes: Uint8Array): URL;
  /**
   * @param {string} url
   * @returns {URL}
   */
  static new(url: string): URL;
  /**
   * @returns {string}
   */
  url(): string;
}
/** */
export class UnitInterval {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {UnitInterval}
   */
  static from_bytes(bytes: Uint8Array): UnitInterval;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {UnitIntervalJSON}
   */
  to_js_value(): UnitIntervalJSON;
  /**
   * @param {string} json
   * @returns {UnitInterval}
   */
  static from_json(json: string): UnitInterval;
  /**
   * @returns {BigNum}
   */
  numerator(): BigNum;
  /**
   * @returns {BigNum}
   */
  denominator(): BigNum;
  /**
   * @param {BigNum} numerator
   * @param {BigNum} denominator
   * @returns {UnitInterval}
   */
  static new(numerator: BigNum, denominator: BigNum): UnitInterval;
  /**
   * @param {number} float_number
   * @returns {UnitInterval}
   */
  static from_float(float_number: number): UnitInterval;
}
/** */
export class Update {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Update}
   */
  static from_bytes(bytes: Uint8Array): Update;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {UpdateJSON}
   */
  to_js_value(): UpdateJSON;
  /**
   * @param {string} json
   * @returns {Update}
   */
  static from_json(json: string): Update;
  /**
   * @returns {ProposedProtocolParameterUpdates}
   */
  proposed_protocol_parameter_updates(): ProposedProtocolParameterUpdates;
  /**
   * @returns {number}
   */
  epoch(): number;
  /**
   * @param {ProposedProtocolParameterUpdates} proposed_protocol_parameter_updates
   * @param {number} epoch
   * @returns {Update}
   */
  static new(
    proposed_protocol_parameter_updates: ProposedProtocolParameterUpdates,
    epoch: number,
  ): Update;
}
/** */
export class VRFCert {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {VRFCert}
   */
  static from_bytes(bytes: Uint8Array): VRFCert;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {VRFCertJSON}
   */
  to_js_value(): VRFCertJSON;
  /**
   * @param {string} json
   * @returns {VRFCert}
   */
  static from_json(json: string): VRFCert;
  /**
   * @returns {Uint8Array}
   */
  output(): Uint8Array;
  /**
   * @returns {Uint8Array}
   */
  proof(): Uint8Array;
  /**
   * @param {Uint8Array} output
   * @param {Uint8Array} proof
   * @returns {VRFCert}
   */
  static new(output: Uint8Array, proof: Uint8Array): VRFCert;
}
/** */
export class VRFKeyHash {
  free(): void;
  /**
   * @param {Uint8Array} bytes
   * @returns {VRFKeyHash}
   */
  static from_bytes(bytes: Uint8Array): VRFKeyHash;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {string} prefix
   * @returns {string}
   */
  to_bech32(prefix: string): string;
  /**
   * @param {string} bech_str
   * @returns {VRFKeyHash}
   */
  static from_bech32(bech_str: string): VRFKeyHash;
  /**
   * @returns {string}
   */
  to_hex(): string;
  /**
   * @param {string} hex
   * @returns {VRFKeyHash}
   */
  static from_hex(hex: string): VRFKeyHash;
}
/** */
export class VRFVKey {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {VRFVKey}
   */
  static from_bytes(bytes: Uint8Array): VRFVKey;
  /**
   * @returns {VRFKeyHash}
   */
  hash(): VRFKeyHash;
  /**
   * @returns {Uint8Array}
   */
  to_raw_key(): Uint8Array;
}
/** */
export class Value {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Value}
   */
  static from_bytes(bytes: Uint8Array): Value;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {ValueJSON}
   */
  to_js_value(): ValueJSON;
  /**
   * @param {string} json
   * @returns {Value}
   */
  static from_json(json: string): Value;
  /**
   * @param {BigNum} coin
   * @returns {Value}
   */
  static new(coin: BigNum): Value;
  /**
   * @param {MultiAsset} multiasset
   * @returns {Value}
   */
  static new_from_assets(multiasset: MultiAsset): Value;
  /**
   * @returns {Value}
   */
  static zero(): Value;
  /**
   * @returns {boolean}
   */
  is_zero(): boolean;
  /**
   * @returns {BigNum}
   */
  coin(): BigNum;
  /**
   * @param {BigNum} coin
   */
  set_coin(coin: BigNum): void;
  /**
   * @returns {MultiAsset | undefined}
   */
  multiasset(): MultiAsset | undefined;
  /**
   * @param {MultiAsset} multiasset
   */
  set_multiasset(multiasset: MultiAsset): void;
  /**
   * @param {Value} rhs
   * @returns {Value}
   */
  checked_add(rhs: Value): Value;
  /**
   * @param {Value} rhs_value
   * @returns {Value}
   */
  checked_sub(rhs_value: Value): Value;
  /**
   * @param {Value} rhs_value
   * @returns {Value}
   */
  clamped_sub(rhs_value: Value): Value;
  /**
   * note: values are only partially comparable
   * @param {Value} rhs_value
   * @returns {number | undefined}
   */
  compare(rhs_value: Value): number | undefined;
}
/** */
export class Vkey {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Vkey}
   */
  static from_bytes(bytes: Uint8Array): Vkey;
  /**
   * @param {PublicKey} pk
   * @returns {Vkey}
   */
  static new(pk: PublicKey): Vkey;
  /**
   * @returns {PublicKey}
   */
  public_key(): PublicKey;
}
/** */
export class Vkeys {
  free(): void;
  /**
   * @returns {Vkeys}
   */
  static new(): Vkeys;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {Vkey}
   */
  get(index: number): Vkey;
  /**
   * @param {Vkey} elem
   */
  add(elem: Vkey): void;
}
/** */
export class Vkeywitness {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Vkeywitness}
   */
  static from_bytes(bytes: Uint8Array): Vkeywitness;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {VkeywitnessJSON}
   */
  to_js_value(): VkeywitnessJSON;
  /**
   * @param {string} json
   * @returns {Vkeywitness}
   */
  static from_json(json: string): Vkeywitness;
  /**
   * @param {Vkey} vkey
   * @param {Ed25519Signature} signature
   * @returns {Vkeywitness}
   */
  static new(vkey: Vkey, signature: Ed25519Signature): Vkeywitness;
  /**
   * @returns {Vkey}
   */
  vkey(): Vkey;
  /**
   * @returns {Ed25519Signature}
   */
  signature(): Ed25519Signature;
}
/** */
export class Vkeywitnesses {
  free(): void;
  /**
   * @returns {Vkeywitnesses}
   */
  static new(): Vkeywitnesses;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {number} index
   * @returns {Vkeywitness}
   */
  get(index: number): Vkeywitness;
  /**
   * @param {Vkeywitness} elem
   */
  add(elem: Vkeywitness): void;
}
/** */
export class Withdrawals {
  free(): void;
  /**
   * @returns {Uint8Array}
   */
  to_bytes(): Uint8Array;
  /**
   * @param {Uint8Array} bytes
   * @returns {Withdrawals}
   */
  static from_bytes(bytes: Uint8Array): Withdrawals;
  /**
   * @returns {string}
   */
  to_json(): string;
  /**
   * @returns {WithdrawalsJSON}
   */
  to_js_value(): WithdrawalsJSON;
  /**
   * @param {string} json
   * @returns {Withdrawals}
   */
  static from_json(json: string): Withdrawals;
  /**
   * @returns {Withdrawals}
   */
  static new(): Withdrawals;
  /**
   * @returns {number}
   */
  len(): number;
  /**
   * @param {RewardAddress} key
   * @param {BigNum} value
   * @returns {BigNum | undefined}
   */
  insert(key: RewardAddress, value: BigNum): BigNum | undefined;
  /**
   * @param {RewardAddress} key
   * @returns {BigNum | undefined}
   */
  get(key: RewardAddress): BigNum | undefined;
  /**
   * @returns {RewardAddresses}
   */
  keys(): RewardAddresses;
}

export type InitInput =
  | RequestInfo
  | URL
  | Response
  | BufferSource
  | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly __wbg_unitinterval_free: (a: number) => void;
  readonly unitinterval_to_bytes: (a: number, b: number) => void;
  readonly unitinterval_from_bytes: (a: number, b: number) => number;
  readonly unitinterval_to_json: (a: number, b: number) => void;
  readonly unitinterval_to_js_value: (a: number) => number;
  readonly unitinterval_from_json: (a: number, b: number) => number;
  readonly unitinterval_new: (a: number, b: number) => number;
  readonly unitinterval_from_float: (a: number) => number;
  readonly __wbg_transaction_free: (a: number) => void;
  readonly transaction_to_bytes: (a: number, b: number) => void;
  readonly transaction_from_bytes: (a: number, b: number) => number;
  readonly transaction_to_json: (a: number, b: number) => void;
  readonly transaction_to_js_value: (a: number) => number;
  readonly transaction_from_json: (a: number, b: number) => number;
  readonly transaction_body: (a: number) => number;
  readonly transaction_witness_set: (a: number) => number;
  readonly transaction_is_valid: (a: number) => number;
  readonly transaction_auxiliary_data: (a: number) => number;
  readonly transaction_set_is_valid: (a: number, b: number) => void;
  readonly transaction_new: (a: number, b: number, c: number) => number;
  readonly __wbg_transactioninputs_free: (a: number) => void;
  readonly transactioninputs_to_bytes: (a: number, b: number) => void;
  readonly transactioninputs_from_bytes: (a: number, b: number) => number;
  readonly transactioninputs_to_json: (a: number, b: number) => void;
  readonly transactioninputs_to_js_value: (a: number) => number;
  readonly transactioninputs_from_json: (a: number, b: number) => number;
  readonly transactioninputs_get: (a: number, b: number) => number;
  readonly transactioninputs_add: (a: number, b: number) => void;
  readonly transactioninputs_sort: (a: number) => void;
  readonly __wbg_transactionoutputs_free: (a: number) => void;
  readonly transactionoutputs_to_bytes: (a: number, b: number) => void;
  readonly transactionoutputs_from_bytes: (a: number, b: number) => number;
  readonly transactionoutputs_to_json: (a: number, b: number) => void;
  readonly transactionoutputs_to_js_value: (a: number) => number;
  readonly transactionoutputs_from_json: (a: number, b: number) => number;
  readonly transactionoutputs_get: (a: number, b: number) => number;
  readonly transactionoutputs_add: (a: number, b: number) => void;
  readonly __wbg_certificates_free: (a: number) => void;
  readonly certificates_to_bytes: (a: number, b: number) => void;
  readonly certificates_from_bytes: (a: number, b: number) => number;
  readonly certificates_to_json: (a: number, b: number) => void;
  readonly certificates_to_js_value: (a: number) => number;
  readonly certificates_from_json: (a: number, b: number) => number;
  readonly certificates_new: () => number;
  readonly certificates_get: (a: number, b: number) => number;
  readonly certificates_add: (a: number, b: number) => void;
  readonly __wbg_transactionbody_free: (a: number) => void;
  readonly transactionbody_to_bytes: (a: number, b: number) => void;
  readonly transactionbody_from_bytes: (a: number, b: number) => number;
  readonly transactionbody_to_json: (a: number, b: number) => void;
  readonly transactionbody_to_js_value: (a: number) => number;
  readonly transactionbody_from_json: (a: number, b: number) => number;
  readonly transactionbody_inputs: (a: number) => number;
  readonly transactionbody_outputs: (a: number) => number;
  readonly transactionbody_ttl: (a: number) => number;
  readonly transactionbody_set_certs: (a: number, b: number) => void;
  readonly transactionbody_certs: (a: number) => number;
  readonly transactionbody_set_withdrawals: (a: number, b: number) => void;
  readonly transactionbody_withdrawals: (a: number) => number;
  readonly transactionbody_set_update: (a: number, b: number) => void;
  readonly transactionbody_update: (a: number) => number;
  readonly transactionbody_set_auxiliary_data_hash: (
    a: number,
    b: number,
  ) => void;
  readonly transactionbody_auxiliary_data_hash: (a: number) => number;
  readonly transactionbody_set_validity_start_interval: (
    a: number,
    b: number,
  ) => void;
  readonly transactionbody_validity_start_interval: (a: number) => number;
  readonly transactionbody_set_mint: (a: number, b: number) => void;
  readonly transactionbody_mint: (a: number) => number;
  readonly transactionbody_set_script_data_hash: (a: number, b: number) => void;
  readonly transactionbody_script_data_hash: (a: number) => number;
  readonly transactionbody_set_collateral: (a: number, b: number) => void;
  readonly transactionbody_collateral: (a: number) => number;
  readonly transactionbody_set_required_signers: (a: number, b: number) => void;
  readonly transactionbody_required_signers: (a: number) => number;
  readonly transactionbody_set_network_id: (a: number, b: number) => void;
  readonly transactionbody_network_id: (a: number) => number;
  readonly transactionbody_set_collateral_return: (
    a: number,
    b: number,
  ) => void;
  readonly transactionbody_collateral_return: (a: number) => number;
  readonly transactionbody_set_total_collateral: (a: number, b: number) => void;
  readonly transactionbody_total_collateral: (a: number) => number;
  readonly transactionbody_set_reference_inputs: (a: number, b: number) => void;
  readonly transactionbody_reference_inputs: (a: number) => number;
  readonly transactionbody_new: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => number;
  readonly transactionbody_raw: (a: number, b: number) => void;
  readonly __wbg_transactioninput_free: (a: number) => void;
  readonly transactioninput_to_bytes: (a: number, b: number) => void;
  readonly transactioninput_from_bytes: (a: number, b: number) => number;
  readonly transactioninput_to_json: (a: number, b: number) => void;
  readonly transactioninput_to_js_value: (a: number) => number;
  readonly transactioninput_from_json: (a: number, b: number) => number;
  readonly transactioninput_transaction_id: (a: number) => number;
  readonly transactioninput_new: (a: number, b: number) => number;
  readonly __wbg_transactionoutput_free: (a: number) => void;
  readonly transactionoutput_to_bytes: (a: number, b: number) => void;
  readonly transactionoutput_from_bytes: (a: number, b: number) => number;
  readonly transactionoutput_to_json: (a: number, b: number) => void;
  readonly transactionoutput_to_js_value: (a: number) => number;
  readonly transactionoutput_from_json: (a: number, b: number) => number;
  readonly transactionoutput_address: (a: number) => number;
  readonly transactionoutput_amount: (a: number) => number;
  readonly transactionoutput_datum: (a: number) => number;
  readonly transactionoutput_script_ref: (a: number) => number;
  readonly transactionoutput_set_datum: (a: number, b: number) => void;
  readonly transactionoutput_set_script_ref: (a: number, b: number) => void;
  readonly transactionoutput_new: (a: number, b: number) => number;
  readonly transactionoutput_to_legacy_bytes: (a: number, b: number) => void;
  readonly stakeregistration_to_bytes: (a: number, b: number) => void;
  readonly stakeregistration_from_bytes: (a: number, b: number) => number;
  readonly stakeregistration_to_js_value: (a: number) => number;
  readonly stakeregistration_from_json: (a: number, b: number) => number;
  readonly __wbg_stakederegistration_free: (a: number) => void;
  readonly stakederegistration_to_bytes: (a: number, b: number) => void;
  readonly stakederegistration_from_bytes: (a: number, b: number) => number;
  readonly stakederegistration_to_json: (a: number, b: number) => void;
  readonly stakederegistration_to_js_value: (a: number) => number;
  readonly stakederegistration_from_json: (a: number, b: number) => number;
  readonly stakederegistration_new: (a: number) => number;
  readonly __wbg_stakedelegation_free: (a: number) => void;
  readonly stakedelegation_to_bytes: (a: number, b: number) => void;
  readonly stakedelegation_from_bytes: (a: number, b: number) => number;
  readonly stakedelegation_to_json: (a: number, b: number) => void;
  readonly stakedelegation_to_js_value: (a: number) => number;
  readonly stakedelegation_from_json: (a: number, b: number) => number;
  readonly stakedelegation_stake_credential: (a: number) => number;
  readonly stakedelegation_pool_keyhash: (a: number) => number;
  readonly stakedelegation_new: (a: number, b: number) => number;
  readonly __wbg_ed25519keyhashes_free: (a: number) => void;
  readonly ed25519keyhashes_to_bytes: (a: number, b: number) => void;
  readonly ed25519keyhashes_from_bytes: (a: number, b: number) => number;
  readonly ed25519keyhashes_to_json: (a: number, b: number) => void;
  readonly ed25519keyhashes_to_js_value: (a: number) => number;
  readonly ed25519keyhashes_from_json: (a: number, b: number) => number;
  readonly ed25519keyhashes_new: () => number;
  readonly ed25519keyhashes_get: (a: number, b: number) => number;
  readonly ed25519keyhashes_add: (a: number, b: number) => void;
  readonly __wbg_relays_free: (a: number) => void;
  readonly relays_to_bytes: (a: number, b: number) => void;
  readonly relays_from_bytes: (a: number, b: number) => number;
  readonly relays_to_json: (a: number, b: number) => void;
  readonly relays_to_js_value: (a: number) => number;
  readonly relays_from_json: (a: number, b: number) => number;
  readonly relays_get: (a: number, b: number) => number;
  readonly relays_add: (a: number, b: number) => void;
  readonly __wbg_poolparams_free: (a: number) => void;
  readonly poolparams_to_bytes: (a: number, b: number) => void;
  readonly poolparams_from_bytes: (a: number, b: number) => number;
  readonly poolparams_to_json: (a: number, b: number) => void;
  readonly poolparams_to_js_value: (a: number) => number;
  readonly poolparams_from_json: (a: number, b: number) => number;
  readonly poolparams_operator: (a: number) => number;
  readonly poolparams_vrf_keyhash: (a: number) => number;
  readonly poolparams_cost: (a: number) => number;
  readonly poolparams_margin: (a: number) => number;
  readonly poolparams_reward_account: (a: number) => number;
  readonly poolparams_pool_owners: (a: number) => number;
  readonly poolparams_relays: (a: number) => number;
  readonly poolparams_pool_metadata: (a: number) => number;
  readonly poolparams_new: (
    a: number,
    b: number,
    c: number,
    d: number,
    e: number,
    f: number,
    g: number,
    h: number,
    i: number,
  ) => number;
  readonly __wbg_poolregistration_free: (a: number) => void;
  readonly poolregistration_to_bytes: (a: number, b: number) => void;
  readonly poolregistration_from_bytes: (a: number, b: number) => number;
  readonly poolregistration_to_json: (a: number, b: number) => void;
  readonly poolregistration_to_js_value: (a: number) => number;
  readonly poolregistration_from_json: (a: number, b: number) => number;
  readonly poolregistration_pool_params: (a: number) => number;
  readonly poolregistration_new: (a: number) => number;
  readonly poolregistration_set_is_update: (a: number, b: number) => void;
  readonly __wbg_poolretirement_free: (a: number) => void;
  readonly poolretirement_to_bytes: (a: number, b: number) => void;
  readonly poolretirement_from_bytes: (a: number, b: number) => number;
  readonly poolretirement_to_json: (a: number, b: number) => void;
  readonly poolretirement_to_js_value: (a: number) => number;
  readonly poolretirement_from_json: (a: number, b: number) => number;
  readonly poolretirement_pool_keyhash: (a: number) => number;
  readonly poolretirement_new: (a: number, b: number) => number;
  readonly __wbg_genesiskeydelegation_free: (a: number) => void;
  readonly genesiskeydelegation_to_bytes: (a: number, b: number) => void;
  readonly genesiskeydelegation_from_bytes: (a: number, b: number) => number;
  readonly genesiskeydelegation_to_json: (a: number, b: number) => void;
  readonly genesiskeydelegation_to_js_value: (a: number) => number;
  readonly genesiskeydelegation_from_json: (a: number, b: number) => number;
  readonly genesiskeydelegation_genesishash: (a: number) => number;
  readonly genesiskeydelegation_genesis_delegate_hash: (a: number) => number;
  readonly genesiskeydelegation_vrf_keyhash: (a: number) => number;
  readonly genesiskeydelegation_new: (
    a: number,
    b: number,
    c: number,
  ) => number;
  readonly __wbg_moveinstantaneousrewardscert_free: (a: number) => void;
  readonly moveinstantaneousrewardscert_to_bytes: (
    a: number,
    b: number,
  ) => void;
  readonly moveinstantaneousrewardscert_from_bytes: (
    a: number,
    b: number,
  ) => number;
  readonly moveinstantaneousrewardscert_to_json: (a: number, b: number) => void;
  readonly moveinstantaneousrewardscert_to_js_value: (a: number) => number;
  readonly moveinstantaneousrewardscert_from_json: (
    a: number,
    b: number,
  ) => number;
  readonly moveinstantaneousrewardscert_move_instantaneous_reward: (
    a: number,
  ) => number;
  readonly moveinstantaneousrewardscert_new: (a: number) => number;
  readonly __wbg_certificate_free: (a: number) => void;
  readonly certificate_to_bytes: (a: number, b: number) => void;
  readonly certificate_from_bytes: (a: number, b: number) => number;
  readonly certificate_to_json: (a: number, b: number) => void;
  readonly certificate_to_js_value: (a: number) => number;
  readonly certificate_from_json: (a: number, b: number) => number;
  readonly certificate_new_stake_registration: (a: number) => number;
  readonly certificate_new_stake_deregistration: (a: number) => number;
  readonly certificate_new_stake_delegation: (a: number) => number;
  readonly certificate_new_pool_registration: (a: number) => number;
  readonly certificate_new_pool_retirement: (a: number) => number;
  readonly certificate_new_genesis_key_delegation: (a: number) => number;
  readonly certificate_new_move_instantaneous_rewards_cert: (
    a: number,
  ) => number;
  readonly certificate_kind: (a: number) => number;
  readonly certificate_as_stake_registration: (a: number) => number;
  readonly certificate_as_stake_deregistration: (a: number) => number;
  readonly certificate_as_stake_delegation: (a: number) => number;
  readonly certificate_as_pool_registration: (a: number) => number;
  readonly certificate_as_pool_retirement: (a: number) => number;
  readonly certificate_as_genesis_key_delegation: (a: number) => number;
  readonly certificate_as_move_instantaneous_rewards_cert: (
    a: number,
  ) => number;
  readonly __wbg_mirtostakecredentials_free: (a: number) => void;
  readonly mirtostakecredentials_to_bytes: (a: number, b: number) => void;
  readonly mirtostakecredentials_from_bytes: (a: number, b: number) => number;
  readonly mirtostakecredentials_to_json: (a: number, b: number) => void;
  readonly mirtostakecredentials_to_js_value: (a: number) => number;
  readonly mirtostakecredentials_from_json: (a: number, b: number) => number;
  readonly mirtostakecredentials_insert: (
    a: number,
    b: number,
    c: number,
  ) => number;
  readonly mirtostakecredentials_get: (a: number, b: number) => number;
  readonly mirtostakecredentials_keys: (a: number) => number;
  readonly __wbg_moveinstantaneousreward_free: (a: number) => void;
  readonly moveinstantaneousreward_to_bytes: (a: number, b: number) => void;
  readonly moveinstantaneousreward_from_bytes: (a: number, b: number) => number;
  readonly moveinstantaneousreward_to_json: (a: number, b: number) => void;
  readonly moveinstantaneousreward_to_js_value: (a: number) => number;
  readonly moveinstantaneousreward_from_json: (a: number, b: number) => number;
  readonly moveinstantaneousreward_new_to_other_pot: (
    a: number,
    b: number,
  ) => number;
  readonly moveinstantaneousreward_new_to_stake_creds: (
    a: number,
    b: number,
  ) => number;
  readonly moveinstantaneousreward_pot: (a: number) => number;
  readonly moveinstantaneousreward_kind: (a: number) => number;
  readonly moveinstantaneousreward_as_to_other_pot: (a: number) => number;
  readonly moveinstantaneousreward_as_to_stake_creds: (a: number) => number;
  readonly __wbg_ipv4_free: (a: number) => void;
  readonly ipv4_to_bytes: (a: number, b: number) => void;
  readonly ipv4_from_bytes: (a: number, b: number) => number;
  readonly ipv4_to_json: (a: number, b: number) => void;
  readonly ipv4_to_js_value: (a: number) => number;
  readonly ipv4_from_json: (a: number, b: number) => number;
  readonly ipv4_new: (a: number, b: number) => number;
  readonly ipv4_ip: (a: number, b: number) => void;
  readonly __wbg_ipv6_free: (a: number) => void;
  readonly ipv6_to_bytes: (a: number, b: number) => void;
  readonly ipv6_from_bytes: (a: number, b: number) => number;
  readonly ipv6_to_json: (a: number, b: number) => void;
  readonly ipv6_to_js_value: (a: number) => number;
  readonly ipv6_from_json: (a: number, b: number) => number;
  readonly ipv6_new: (a: number, b: number) => number;
  readonly ipv6_ip: (a: number, b: number) => void;
  readonly url_to_bytes: (a: number, b: number) => void;
  readonly url_from_bytes: (a: number, b: number) => number;
  readonly url_new: (a: number, b: number) => number;
  readonly __wbg_dnsrecordaoraaaa_free: (a: number) => void;
  readonly dnsrecordaoraaaa_to_bytes: (a: number, b: number) => void;
  readonly dnsrecordaoraaaa_from_bytes: (a: number, b: number) => number;
  readonly dnsrecordaoraaaa_new: (a: number, b: number) => number;
  readonly dnsrecordaoraaaa_record: (a: number, b: number) => void;
  readonly dnsrecordsrv_to_bytes: (a: number, b: number) => void;
  readonly dnsrecordsrv_from_bytes: (a: number, b: number) => number;
  readonly dnsrecordsrv_new: (a: number, b: number) => number;
  readonly __wbg_singlehostaddr_free: (a: number) => void;
  readonly singlehostaddr_to_bytes: (a: number, b: number) => void;
  readonly singlehostaddr_from_bytes: (a: number, b: number) => number;
  readonly singlehostaddr_to_json: (a: number, b: number) => void;
  readonly singlehostaddr_to_js_value: (a: number) => number;
  readonly singlehostaddr_from_json: (a: number, b: number) => number;
  readonly singlehostaddr_port: (a: number) => number;
  readonly singlehostaddr_ipv4: (a: number) => number;
  readonly singlehostaddr_ipv6: (a: number) => number;
  readonly singlehostaddr_new: (a: number, b: number, c: number) => number;
  readonly __wbg_singlehostname_free: (a: number) => void;
  readonly singlehostname_to_bytes: (a: number, b: number) => void;
  readonly singlehostname_from_bytes: (a: number, b: number) => number;
  readonly singlehostname_to_json: (a: number, b: number) => void;
  readonly singlehostname_to_js_value: (a: number) => number;
  readonly singlehostname_from_json: (a: number, b: number) => number;
  readonly singlehostname_port: (a: number) => number;
  readonly singlehostname_new: (a: number, b: number) => number;
  readonly __wbg_multihostname_free: (a: number) => void;
  readonly multihostname_to_bytes: (a: number, b: number) => void;
  readonly multihostname_from_bytes: (a: number, b: number) => number;
  readonly multihostname_to_json: (a: number, b: number) => void;
  readonly multihostname_to_js_value: (a: number) => number;
  readonly multihostname_from_json: (a: number, b: number) => number;
  readonly multihostname_dns_name: (a: number) => number;
  readonly multihostname_new: (a: number) => number;
  readonly __wbg_relay_free: (a: number) => void;
  readonly relay_to_bytes: (a: number, b: number) => void;
  readonly relay_from_bytes: (a: number, b: number) => number;
  readonly relay_to_json: (a: number, b: number) => void;
  readonly relay_to_js_value: (a: number) => number;
  readonly relay_from_json: (a: number, b: number) => number;
  readonly relay_new_single_host_addr: (a: number) => number;
  readonly relay_new_single_host_name: (a: number) => number;
  readonly relay_new_multi_host_name: (a: number) => number;
  readonly relay_kind: (a: number) => number;
  readonly relay_as_single_host_addr: (a: number) => number;
  readonly relay_as_single_host_name: (a: number) => number;
  readonly relay_as_multi_host_name: (a: number) => number;
  readonly __wbg_poolmetadata_free: (a: number) => void;
  readonly poolmetadata_to_bytes: (a: number, b: number) => void;
  readonly poolmetadata_from_bytes: (a: number, b: number) => number;
  readonly poolmetadata_to_json: (a: number, b: number) => void;
  readonly poolmetadata_to_js_value: (a: number) => number;
  readonly poolmetadata_from_json: (a: number, b: number) => number;
  readonly poolmetadata_pool_metadata_hash: (a: number) => number;
  readonly poolmetadata_new: (a: number, b: number) => number;
  readonly __wbg_stakecredentials_free: (a: number) => void;
  readonly stakecredentials_to_bytes: (a: number, b: number) => void;
  readonly stakecredentials_from_bytes: (a: number, b: number) => number;
  readonly stakecredentials_to_json: (a: number, b: number) => void;
  readonly stakecredentials_to_js_value: (a: number) => number;
  readonly stakecredentials_from_json: (a: number, b: number) => number;
  readonly stakecredentials_get: (a: number, b: number) => number;
  readonly stakecredentials_add: (a: number, b: number) => void;
  readonly __wbg_rewardaddresses_free: (a: number) => void;
  readonly rewardaddresses_to_bytes: (a: number, b: number) => void;
  readonly rewardaddresses_from_bytes: (a: number, b: number) => number;
  readonly rewardaddresses_to_json: (a: number, b: number) => void;
  readonly rewardaddresses_to_js_value: (a: number) => number;
  readonly rewardaddresses_from_json: (a: number, b: number) => number;
  readonly rewardaddresses_get: (a: number, b: number) => number;
  readonly rewardaddresses_add: (a: number, b: number) => void;
  readonly __wbg_withdrawals_free: (a: number) => void;
  readonly withdrawals_to_bytes: (a: number, b: number) => void;
  readonly withdrawals_from_bytes: (a: number, b: number) => number;
  readonly withdrawals_to_json: (a: number, b: number) => void;
  readonly withdrawals_to_js_value: (a: number) => number;
  readonly withdrawals_from_json: (a: number, b: number) => number;
  readonly withdrawals_insert: (a: number, b: number, c: number) => number;
  readonly withdrawals_get: (a: number, b: number) => number;
  readonly withdrawals_keys: (a: number) => number;
  readonly __wbg_transactionwitnessset_free: (a: number) => void;
  readonly transactionwitnessset_to_bytes: (a: number, b: number) => void;
  readonly transactionwitnessset_from_bytes: (a: number, b: number) => number;
  readonly transactionwitnessset_to_json: (a: number, b: number) => void;
  readonly transactionwitnessset_to_js_value: (a: number) => number;
  readonly transactionwitnessset_from_json: (a: number, b: number) => number;
  readonly transactionwitnessset_set_vkeys: (a: number, b: number) => void;
  readonly transactionwitnessset_vkeys: (a: number) => number;
  readonly transactionwitnessset_set_native_scripts: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnessset_native_scripts: (a: number) => number;
  readonly transactionwitnessset_set_bootstraps: (a: number, b: number) => void;
  readonly transactionwitnessset_bootstraps: (a: number) => number;
  readonly transactionwitnessset_set_plutus_scripts: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnessset_plutus_scripts: (a: number) => number;
  readonly transactionwitnessset_set_plutus_data: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnessset_plutus_data: (a: number) => number;
  readonly transactionwitnessset_set_redeemers: (a: number, b: number) => void;
  readonly transactionwitnessset_set_plutus_v2_scripts: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnessset_redeemers: (a: number) => number;
  readonly transactionwitnessset_plutus_v2_scripts: (a: number) => number;
  readonly transactionwitnessset_new: () => number;
  readonly __wbg_scriptpubkey_free: (a: number) => void;
  readonly scriptpubkey_to_bytes: (a: number, b: number) => void;
  readonly scriptpubkey_from_bytes: (a: number, b: number) => number;
  readonly scriptpubkey_to_json: (a: number, b: number) => void;
  readonly scriptpubkey_to_js_value: (a: number) => number;
  readonly scriptpubkey_from_json: (a: number, b: number) => number;
  readonly scriptpubkey_addr_keyhash: (a: number) => number;
  readonly scriptpubkey_new: (a: number) => number;
  readonly __wbg_scriptall_free: (a: number) => void;
  readonly scriptall_to_bytes: (a: number, b: number) => void;
  readonly scriptall_from_bytes: (a: number, b: number) => number;
  readonly scriptall_to_json: (a: number, b: number) => void;
  readonly scriptall_to_js_value: (a: number) => number;
  readonly scriptall_from_json: (a: number, b: number) => number;
  readonly scriptall_native_scripts: (a: number) => number;
  readonly scriptall_new: (a: number) => number;
  readonly scriptany_to_bytes: (a: number, b: number) => void;
  readonly scriptany_from_bytes: (a: number, b: number) => number;
  readonly scriptany_to_js_value: (a: number) => number;
  readonly scriptany_from_json: (a: number, b: number) => number;
  readonly __wbg_scriptnofk_free: (a: number) => void;
  readonly scriptnofk_to_bytes: (a: number, b: number) => void;
  readonly scriptnofk_from_bytes: (a: number, b: number) => number;
  readonly scriptnofk_to_json: (a: number, b: number) => void;
  readonly scriptnofk_to_js_value: (a: number) => number;
  readonly scriptnofk_from_json: (a: number, b: number) => number;
  readonly scriptnofk_native_scripts: (a: number) => number;
  readonly scriptnofk_new: (a: number, b: number) => number;
  readonly timelockstart_to_bytes: (a: number, b: number) => void;
  readonly timelockstart_from_bytes: (a: number, b: number) => number;
  readonly timelockstart_to_js_value: (a: number) => number;
  readonly timelockstart_from_json: (a: number, b: number) => number;
  readonly __wbg_timelockexpiry_free: (a: number) => void;
  readonly timelockexpiry_to_bytes: (a: number, b: number) => void;
  readonly timelockexpiry_from_bytes: (a: number, b: number) => number;
  readonly timelockexpiry_to_json: (a: number, b: number) => void;
  readonly timelockexpiry_to_js_value: (a: number) => number;
  readonly timelockexpiry_from_json: (a: number, b: number) => number;
  readonly timelockexpiry_new: (a: number) => number;
  readonly __wbg_nativescript_free: (a: number) => void;
  readonly nativescript_to_bytes: (a: number, b: number) => void;
  readonly nativescript_from_bytes: (a: number, b: number) => number;
  readonly nativescript_to_json: (a: number, b: number) => void;
  readonly nativescript_to_js_value: (a: number) => number;
  readonly nativescript_from_json: (a: number, b: number) => number;
  readonly nativescript_hash: (a: number, b: number) => number;
  readonly nativescript_new_script_pubkey: (a: number) => number;
  readonly nativescript_new_script_all: (a: number) => number;
  readonly nativescript_new_script_any: (a: number) => number;
  readonly nativescript_new_script_n_of_k: (a: number) => number;
  readonly nativescript_new_timelock_start: (a: number) => number;
  readonly nativescript_new_timelock_expiry: (a: number) => number;
  readonly nativescript_kind: (a: number) => number;
  readonly nativescript_as_script_pubkey: (a: number) => number;
  readonly nativescript_as_script_all: (a: number) => number;
  readonly nativescript_as_script_any: (a: number) => number;
  readonly nativescript_as_script_n_of_k: (a: number) => number;
  readonly nativescript_as_timelock_start: (a: number) => number;
  readonly nativescript_as_timelock_expiry: (a: number) => number;
  readonly nativescript_get_required_signers: (a: number) => number;
  readonly nativescript_verify: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => number;
  readonly __wbg_nativescripts_free: (a: number) => void;
  readonly nativescripts_get: (a: number, b: number) => number;
  readonly nativescripts_add: (a: number, b: number) => void;
  readonly __wbg_update_free: (a: number) => void;
  readonly update_to_bytes: (a: number, b: number) => void;
  readonly update_from_bytes: (a: number, b: number) => number;
  readonly update_to_json: (a: number, b: number) => void;
  readonly update_to_js_value: (a: number) => number;
  readonly update_from_json: (a: number, b: number) => number;
  readonly update_proposed_protocol_parameter_updates: (a: number) => number;
  readonly update_epoch: (a: number) => number;
  readonly update_new: (a: number, b: number) => number;
  readonly __wbg_genesishashes_free: (a: number) => void;
  readonly genesishashes_to_bytes: (a: number, b: number) => void;
  readonly genesishashes_from_bytes: (a: number, b: number) => number;
  readonly genesishashes_to_json: (a: number, b: number) => void;
  readonly genesishashes_to_js_value: (a: number) => number;
  readonly genesishashes_from_json: (a: number, b: number) => number;
  readonly genesishashes_get: (a: number, b: number) => number;
  readonly genesishashes_add: (a: number, b: number) => void;
  readonly __wbg_scripthashes_free: (a: number) => void;
  readonly scripthashes_to_bytes: (a: number, b: number) => void;
  readonly scripthashes_from_bytes: (a: number, b: number) => number;
  readonly scripthashes_to_json: (a: number, b: number) => void;
  readonly scripthashes_to_js_value: (a: number) => number;
  readonly scripthashes_from_json: (a: number, b: number) => number;
  readonly scripthashes_get: (a: number, b: number) => number;
  readonly scripthashes_add: (a: number, b: number) => void;
  readonly __wbg_proposedprotocolparameterupdates_free: (a: number) => void;
  readonly proposedprotocolparameterupdates_to_bytes: (
    a: number,
    b: number,
  ) => void;
  readonly proposedprotocolparameterupdates_from_bytes: (
    a: number,
    b: number,
  ) => number;
  readonly proposedprotocolparameterupdates_to_json: (
    a: number,
    b: number,
  ) => void;
  readonly proposedprotocolparameterupdates_to_js_value: (a: number) => number;
  readonly proposedprotocolparameterupdates_from_json: (
    a: number,
    b: number,
  ) => number;
  readonly proposedprotocolparameterupdates_insert: (
    a: number,
    b: number,
    c: number,
  ) => number;
  readonly proposedprotocolparameterupdates_get: (
    a: number,
    b: number,
  ) => number;
  readonly proposedprotocolparameterupdates_keys: (a: number) => number;
  readonly __wbg_protocolversion_free: (a: number) => void;
  readonly protocolversion_to_bytes: (a: number, b: number) => void;
  readonly protocolversion_from_bytes: (a: number, b: number) => number;
  readonly protocolversion_to_json: (a: number, b: number) => void;
  readonly protocolversion_to_js_value: (a: number) => number;
  readonly protocolversion_from_json: (a: number, b: number) => number;
  readonly protocolversion_new: (a: number, b: number) => number;
  readonly __wbg_protocolparamupdate_free: (a: number) => void;
  readonly protocolparamupdate_to_bytes: (a: number, b: number) => void;
  readonly protocolparamupdate_from_bytes: (a: number, b: number) => number;
  readonly protocolparamupdate_to_json: (a: number, b: number) => void;
  readonly protocolparamupdate_to_js_value: (a: number) => number;
  readonly protocolparamupdate_from_json: (a: number, b: number) => number;
  readonly protocolparamupdate_set_minfee_a: (a: number, b: number) => void;
  readonly protocolparamupdate_minfee_a: (a: number) => number;
  readonly protocolparamupdate_set_minfee_b: (a: number, b: number) => void;
  readonly protocolparamupdate_minfee_b: (a: number) => number;
  readonly protocolparamupdate_set_max_block_body_size: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_max_block_body_size: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_set_max_tx_size: (a: number, b: number) => void;
  readonly protocolparamupdate_max_tx_size: (a: number, b: number) => void;
  readonly protocolparamupdate_set_max_block_header_size: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_max_block_header_size: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_set_key_deposit: (a: number, b: number) => void;
  readonly protocolparamupdate_key_deposit: (a: number) => number;
  readonly protocolparamupdate_set_pool_deposit: (a: number, b: number) => void;
  readonly protocolparamupdate_pool_deposit: (a: number) => number;
  readonly protocolparamupdate_set_max_epoch: (a: number, b: number) => void;
  readonly protocolparamupdate_max_epoch: (a: number, b: number) => void;
  readonly protocolparamupdate_set_n_opt: (a: number, b: number) => void;
  readonly protocolparamupdate_n_opt: (a: number, b: number) => void;
  readonly protocolparamupdate_set_pool_pledge_influence: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_pool_pledge_influence: (a: number) => number;
  readonly protocolparamupdate_set_expansion_rate: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_expansion_rate: (a: number) => number;
  readonly protocolparamupdate_set_treasury_growth_rate: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_treasury_growth_rate: (a: number) => number;
  readonly protocolparamupdate_set_d: (a: number, b: number) => void;
  readonly protocolparamupdate_d: (a: number) => number;
  readonly protocolparamupdate_set_extra_entropy: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_extra_entropy: (a: number) => number;
  readonly protocolparamupdate_set_protocol_version: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_protocol_version: (a: number) => number;
  readonly protocolparamupdate_set_min_pool_cost: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_min_pool_cost: (a: number) => number;
  readonly protocolparamupdate_set_ada_per_utxo_byte: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_ada_per_utxo_byte: (a: number) => number;
  readonly protocolparamupdate_set_cost_models: (a: number, b: number) => void;
  readonly protocolparamupdate_cost_models: (a: number) => number;
  readonly protocolparamupdate_set_execution_costs: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_execution_costs: (a: number) => number;
  readonly protocolparamupdate_set_max_tx_ex_units: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_max_tx_ex_units: (a: number) => number;
  readonly protocolparamupdate_set_max_block_ex_units: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_max_block_ex_units: (a: number) => number;
  readonly protocolparamupdate_set_max_value_size: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_max_value_size: (a: number, b: number) => void;
  readonly protocolparamupdate_set_collateral_percentage: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_collateral_percentage: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_set_max_collateral_inputs: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_max_collateral_inputs: (
    a: number,
    b: number,
  ) => void;
  readonly protocolparamupdate_new: () => number;
  readonly __wbg_transactionbodies_free: (a: number) => void;
  readonly transactionbodies_to_bytes: (a: number, b: number) => void;
  readonly transactionbodies_from_bytes: (a: number, b: number) => number;
  readonly transactionbodies_to_json: (a: number, b: number) => void;
  readonly transactionbodies_to_js_value: (a: number) => number;
  readonly transactionbodies_from_json: (a: number, b: number) => number;
  readonly transactionbodies_get: (a: number, b: number) => number;
  readonly transactionbodies_add: (a: number, b: number) => void;
  readonly __wbg_transactionwitnesssets_free: (a: number) => void;
  readonly transactionwitnesssets_to_bytes: (a: number, b: number) => void;
  readonly transactionwitnesssets_from_bytes: (a: number, b: number) => number;
  readonly transactionwitnesssets_to_json: (a: number, b: number) => void;
  readonly transactionwitnesssets_to_js_value: (a: number) => number;
  readonly transactionwitnesssets_from_json: (a: number, b: number) => number;
  readonly transactionwitnesssets_get: (a: number, b: number) => number;
  readonly transactionwitnesssets_add: (a: number, b: number) => void;
  readonly __wbg_transactionindexes_free: (a: number) => void;
  readonly transactionindexes_to_bytes: (a: number, b: number) => void;
  readonly transactionindexes_from_bytes: (a: number, b: number) => number;
  readonly transactionindexes_get: (a: number, b: number) => number;
  readonly transactionindexes_add: (a: number, b: number) => void;
  readonly __wbg_auxiliarydataset_free: (a: number) => void;
  readonly auxiliarydataset_new: () => number;
  readonly auxiliarydataset_len: (a: number) => number;
  readonly auxiliarydataset_insert: (a: number, b: number, c: number) => number;
  readonly auxiliarydataset_get: (a: number, b: number) => number;
  readonly auxiliarydataset_indices: (a: number) => number;
  readonly __wbg_block_free: (a: number) => void;
  readonly block_to_bytes: (a: number, b: number) => void;
  readonly block_from_bytes: (a: number, b: number) => number;
  readonly block_to_json: (a: number, b: number) => void;
  readonly block_to_js_value: (a: number) => number;
  readonly block_from_json: (a: number, b: number) => number;
  readonly block_header: (a: number) => number;
  readonly block_transaction_bodies: (a: number) => number;
  readonly block_transaction_witness_sets: (a: number) => number;
  readonly block_auxiliary_data_set: (a: number) => number;
  readonly block_invalid_transactions: (a: number) => number;
  readonly block_new: (
    a: number,
    b: number,
    c: number,
    d: number,
    e: number,
  ) => number;
  readonly __wbg_header_free: (a: number) => void;
  readonly header_to_bytes: (a: number, b: number) => void;
  readonly header_from_bytes: (a: number, b: number) => number;
  readonly header_to_json: (a: number, b: number) => void;
  readonly header_to_js_value: (a: number) => number;
  readonly header_from_json: (a: number, b: number) => number;
  readonly header_header_body: (a: number) => number;
  readonly header_body_signature: (a: number) => number;
  readonly header_new: (a: number, b: number) => number;
  readonly __wbg_operationalcert_free: (a: number) => void;
  readonly operationalcert_to_bytes: (a: number, b: number) => void;
  readonly operationalcert_from_bytes: (a: number, b: number) => number;
  readonly operationalcert_to_json: (a: number, b: number) => void;
  readonly operationalcert_to_js_value: (a: number) => number;
  readonly operationalcert_from_json: (a: number, b: number) => number;
  readonly operationalcert_hot_vkey: (a: number) => number;
  readonly operationalcert_sequence_number: (a: number) => number;
  readonly operationalcert_kes_period: (a: number) => number;
  readonly operationalcert_sigma: (a: number) => number;
  readonly operationalcert_new: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => number;
  readonly __wbg_headerbody_free: (a: number) => void;
  readonly headerbody_to_bytes: (a: number, b: number) => void;
  readonly headerbody_from_bytes: (a: number, b: number) => number;
  readonly headerbody_to_json: (a: number, b: number) => void;
  readonly headerbody_to_js_value: (a: number) => number;
  readonly headerbody_from_json: (a: number, b: number) => number;
  readonly headerbody_block_number: (a: number) => number;
  readonly headerbody_slot: (a: number) => number;
  readonly headerbody_prev_hash: (a: number) => number;
  readonly headerbody_issuer_vkey: (a: number) => number;
  readonly headerbody_vrf_vkey: (a: number) => number;
  readonly headerbody_nonce_vrf: (a: number) => number;
  readonly headerbody_leader_vrf: (a: number) => number;
  readonly headerbody_block_body_size: (a: number) => number;
  readonly headerbody_block_body_hash: (a: number) => number;
  readonly headerbody_operational_cert: (a: number) => number;
  readonly headerbody_protocol_version: (a: number) => number;
  readonly headerbody_new: (
    a: number,
    b: number,
    c: number,
    d: number,
    e: number,
    f: number,
    g: number,
    h: number,
    i: number,
    j: number,
    k: number,
  ) => number;
  readonly __wbg_assetname_free: (a: number) => void;
  readonly assetname_to_bytes: (a: number, b: number) => void;
  readonly assetname_from_bytes: (a: number, b: number) => number;
  readonly assetname_to_json: (a: number, b: number) => void;
  readonly assetname_to_js_value: (a: number) => number;
  readonly assetname_from_json: (a: number, b: number) => number;
  readonly assetname_new: (a: number, b: number) => number;
  readonly assetname_name: (a: number, b: number) => void;
  readonly __wbg_assetnames_free: (a: number) => void;
  readonly assetnames_to_bytes: (a: number, b: number) => void;
  readonly assetnames_from_bytes: (a: number, b: number) => number;
  readonly assetnames_to_json: (a: number, b: number) => void;
  readonly assetnames_to_js_value: (a: number) => number;
  readonly assetnames_from_json: (a: number, b: number) => number;
  readonly assetnames_new: () => number;
  readonly assetnames_len: (a: number) => number;
  readonly assetnames_get: (a: number, b: number) => number;
  readonly assetnames_add: (a: number, b: number) => void;
  readonly __wbg_assets_free: (a: number) => void;
  readonly assets_to_bytes: (a: number, b: number) => void;
  readonly assets_from_bytes: (a: number, b: number) => number;
  readonly assets_to_json: (a: number, b: number) => void;
  readonly assets_to_js_value: (a: number) => number;
  readonly assets_from_json: (a: number, b: number) => number;
  readonly assets_new: () => number;
  readonly assets_insert: (a: number, b: number, c: number) => number;
  readonly assets_get: (a: number, b: number) => number;
  readonly assets_keys: (a: number) => number;
  readonly __wbg_multiasset_free: (a: number) => void;
  readonly multiasset_to_bytes: (a: number, b: number) => void;
  readonly multiasset_from_bytes: (a: number, b: number) => number;
  readonly multiasset_to_json: (a: number, b: number) => void;
  readonly multiasset_to_js_value: (a: number) => number;
  readonly multiasset_from_json: (a: number, b: number) => number;
  readonly multiasset_insert: (a: number, b: number, c: number) => number;
  readonly multiasset_get: (a: number, b: number) => number;
  readonly multiasset_set_asset: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => number;
  readonly multiasset_get_asset: (a: number, b: number, c: number) => number;
  readonly multiasset_keys: (a: number) => number;
  readonly multiasset_sub: (a: number, b: number) => number;
  readonly __wbg_mintassets_free: (a: number) => void;
  readonly mintassets_new_from_entry: (a: number, b: number) => number;
  readonly mintassets_insert: (a: number, b: number, c: number) => number;
  readonly mintassets_get: (a: number, b: number) => number;
  readonly mintassets_keys: (a: number) => number;
  readonly __wbg_mint_free: (a: number) => void;
  readonly mint_to_bytes: (a: number, b: number) => void;
  readonly mint_from_bytes: (a: number, b: number) => number;
  readonly mint_to_json: (a: number, b: number) => void;
  readonly mint_to_js_value: (a: number) => number;
  readonly mint_from_json: (a: number, b: number) => number;
  readonly mint_new_from_entry: (a: number, b: number) => number;
  readonly mint_insert: (a: number, b: number, c: number) => number;
  readonly mint_get: (a: number, b: number) => number;
  readonly mint_keys: (a: number) => number;
  readonly mint_as_positive_multiasset: (a: number) => number;
  readonly mint_as_negative_multiasset: (a: number) => number;
  readonly __wbg_networkid_free: (a: number) => void;
  readonly networkid_to_bytes: (a: number, b: number) => void;
  readonly networkid_from_bytes: (a: number, b: number) => number;
  readonly networkid_to_json: (a: number, b: number) => void;
  readonly networkid_to_js_value: (a: number) => number;
  readonly networkid_from_json: (a: number, b: number) => number;
  readonly networkid_testnet: () => number;
  readonly networkid_mainnet: () => number;
  readonly networkid_kind: (a: number) => number;
  readonly transactionoutputs_len: (a: number) => number;
  readonly transactioninputs_len: (a: number) => number;
  readonly ed25519keyhashes_len: (a: number) => number;
  readonly relays_len: (a: number) => number;
  readonly stakecredentials_len: (a: number) => number;
  readonly rewardaddresses_len: (a: number) => number;
  readonly withdrawals_len: (a: number) => number;
  readonly scriptnofk_n: (a: number) => number;
  readonly nativescripts_len: (a: number) => number;
  readonly genesishashes_len: (a: number) => number;
  readonly scripthashes_len: (a: number) => number;
  readonly proposedprotocolparameterupdates_len: (a: number) => number;
  readonly protocolversion_major: (a: number) => number;
  readonly transactionbodies_len: (a: number) => number;
  readonly transactionwitnesssets_len: (a: number) => number;
  readonly transactionindexes_len: (a: number) => number;
  readonly mirtostakecredentials_len: (a: number) => number;
  readonly poolretirement_epoch: (a: number) => number;
  readonly protocolversion_minor: (a: number) => number;
  readonly certificates_len: (a: number) => number;
  readonly assets_len: (a: number) => number;
  readonly multiasset_len: (a: number) => number;
  readonly mintassets_len: (a: number) => number;
  readonly mint_len: (a: number) => number;
  readonly unitinterval_numerator: (a: number) => number;
  readonly transactioninput_index: (a: number) => number;
  readonly transactionbody_fee: (a: number) => number;
  readonly unitinterval_denominator: (a: number) => number;
  readonly timelockstart_slot: (a: number) => number;
  readonly timelockexpiry_slot: (a: number) => number;
  readonly poolparams_pledge: (a: number) => number;
  readonly stakeregistration_stake_credential: (a: number) => number;
  readonly stakederegistration_stake_credential: (a: number) => number;
  readonly url_url: (a: number, b: number) => void;
  readonly dnsrecordsrv_record: (a: number, b: number) => void;
  readonly scriptany_native_scripts: (a: number) => number;
  readonly proposedprotocolparameterupdates_new: () => number;
  readonly mirtostakecredentials_new: () => number;
  readonly __wbg_url_free: (a: number) => void;
  readonly __wbg_dnsrecordsrv_free: (a: number) => void;
  readonly __wbg_scriptany_free: (a: number) => void;
  readonly stakeregistration_new: (a: number) => number;
  readonly transactionoutputs_new: () => number;
  readonly transactioninputs_new: () => number;
  readonly stakecredentials_new: () => number;
  readonly rewardaddresses_new: () => number;
  readonly nativescripts_new: () => number;
  readonly genesishashes_new: () => number;
  readonly scripthashes_new: () => number;
  readonly transactionbodies_new: () => number;
  readonly transactionwitnesssets_new: () => number;
  readonly transactionindexes_new: () => number;
  readonly relays_new: () => number;
  readonly __wbg_timelockstart_free: (a: number) => void;
  readonly stakeregistration_to_json: (a: number, b: number) => void;
  readonly scriptany_to_json: (a: number, b: number) => void;
  readonly timelockstart_to_json: (a: number, b: number) => void;
  readonly timelockstart_new: (a: number) => number;
  readonly singlehostname_dns_name: (a: number) => number;
  readonly poolmetadata_url: (a: number) => number;
  readonly scriptany_new: (a: number) => number;
  readonly withdrawals_new: () => number;
  readonly multiasset_new: () => number;
  readonly mintassets_new: () => number;
  readonly mint_new: () => number;
  readonly __wbg_stakeregistration_free: (a: number) => void;
  readonly __wbg_linearfee_free: (a: number) => void;
  readonly linearfee_constant: (a: number) => number;
  readonly linearfee_coefficient: (a: number) => number;
  readonly linearfee_new: (a: number, b: number) => number;
  readonly min_fee: (a: number, b: number, c: number) => number;
  readonly __wbg_transactionoutputbuilder_free: (a: number) => void;
  readonly transactionoutputbuilder_new: () => number;
  readonly transactionoutputbuilder_with_address: (
    a: number,
    b: number,
  ) => number;
  readonly transactionoutputbuilder_with_datum: (
    a: number,
    b: number,
  ) => number;
  readonly transactionoutputbuilder_next: (a: number) => number;
  readonly __wbg_transactionoutputamountbuilder_free: (a: number) => void;
  readonly transactionoutputamountbuilder_with_value: (
    a: number,
    b: number,
  ) => number;
  readonly transactionoutputamountbuilder_with_coin: (
    a: number,
    b: number,
  ) => number;
  readonly transactionoutputamountbuilder_with_coin_and_asset: (
    a: number,
    b: number,
    c: number,
  ) => number;
  readonly transactionoutputamountbuilder_with_asset_and_min_required_coin: (
    a: number,
    b: number,
    c: number,
  ) => number;
  readonly transactionoutputamountbuilder_build: (a: number) => number;
  readonly __wbg_bip32privatekey_free: (a: number) => void;
  readonly bip32privatekey_derive: (a: number, b: number) => number;
  readonly bip32privatekey_from_128_xprv: (a: number, b: number) => number;
  readonly bip32privatekey_to_128_xprv: (a: number, b: number) => void;
  readonly bip32privatekey_generate_ed25519_bip32: () => number;
  readonly bip32privatekey_to_raw_key: (a: number) => number;
  readonly bip32privatekey_to_public: (a: number) => number;
  readonly bip32privatekey_from_bytes: (a: number, b: number) => number;
  readonly bip32privatekey_as_bytes: (a: number, b: number) => void;
  readonly bip32privatekey_from_bech32: (a: number, b: number) => number;
  readonly bip32privatekey_to_bech32: (a: number, b: number) => void;
  readonly bip32privatekey_from_bip39_entropy: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => number;
  readonly bip32privatekey_chaincode: (a: number, b: number) => void;
  readonly __wbg_bip32publickey_free: (a: number) => void;
  readonly bip32publickey_derive: (a: number, b: number) => number;
  readonly bip32publickey_to_raw_key: (a: number) => number;
  readonly bip32publickey_from_bytes: (a: number, b: number) => number;
  readonly bip32publickey_as_bytes: (a: number, b: number) => void;
  readonly bip32publickey_from_bech32: (a: number, b: number) => number;
  readonly bip32publickey_to_bech32: (a: number, b: number) => void;
  readonly bip32publickey_chaincode: (a: number, b: number) => void;
  readonly __wbg_privatekey_free: (a: number) => void;
  readonly privatekey_to_public: (a: number) => number;
  readonly privatekey_generate_ed25519: () => number;
  readonly privatekey_generate_ed25519extended: () => number;
  readonly privatekey_from_bech32: (a: number, b: number) => number;
  readonly privatekey_to_bech32: (a: number, b: number) => void;
  readonly privatekey_as_bytes: (a: number, b: number) => void;
  readonly privatekey_from_extended_bytes: (a: number, b: number) => number;
  readonly privatekey_from_normal_bytes: (a: number, b: number) => number;
  readonly privatekey_sign: (a: number, b: number, c: number) => number;
  readonly privatekey_from_bytes: (a: number, b: number) => number;
  readonly privatekey_to_bytes: (a: number, b: number) => void;
  readonly __wbg_publickey_free: (a: number) => void;
  readonly publickey_from_bech32: (a: number, b: number) => number;
  readonly publickey_to_bech32: (a: number, b: number) => void;
  readonly publickey_from_bytes: (a: number, b: number) => number;
  readonly publickey_verify: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => number;
  readonly publickey_hash: (a: number) => number;
  readonly __wbg_vkey_free: (a: number) => void;
  readonly vkey_to_bytes: (a: number, b: number) => void;
  readonly vkey_from_bytes: (a: number, b: number) => number;
  readonly vkey_new: (a: number) => number;
  readonly vkey_public_key: (a: number) => number;
  readonly __wbg_vkeys_free: (a: number) => void;
  readonly vkeys_get: (a: number, b: number) => number;
  readonly vkeys_add: (a: number, b: number) => void;
  readonly __wbg_vkeywitness_free: (a: number) => void;
  readonly vkeywitness_to_bytes: (a: number, b: number) => void;
  readonly vkeywitness_from_bytes: (a: number, b: number) => number;
  readonly vkeywitness_to_json: (a: number, b: number) => void;
  readonly vkeywitness_to_js_value: (a: number) => number;
  readonly vkeywitness_from_json: (a: number, b: number) => number;
  readonly vkeywitness_new: (a: number, b: number) => number;
  readonly vkeywitness_signature: (a: number) => number;
  readonly __wbg_vkeywitnesses_free: (a: number) => void;
  readonly vkeywitnesses_get: (a: number, b: number) => number;
  readonly vkeywitnesses_add: (a: number, b: number) => void;
  readonly __wbg_bootstrapwitness_free: (a: number) => void;
  readonly bootstrapwitness_to_bytes: (a: number, b: number) => void;
  readonly bootstrapwitness_from_bytes: (a: number, b: number) => number;
  readonly bootstrapwitness_to_json: (a: number, b: number) => void;
  readonly bootstrapwitness_to_js_value: (a: number) => number;
  readonly bootstrapwitness_from_json: (a: number, b: number) => number;
  readonly bootstrapwitness_vkey: (a: number) => number;
  readonly bootstrapwitness_signature: (a: number) => number;
  readonly bootstrapwitness_chain_code: (a: number, b: number) => void;
  readonly bootstrapwitness_attributes: (a: number, b: number) => void;
  readonly bootstrapwitness_new: (
    a: number,
    b: number,
    c: number,
    d: number,
    e: number,
    f: number,
  ) => number;
  readonly __wbg_bootstrapwitnesses_free: (a: number) => void;
  readonly bootstrapwitnesses_new: () => number;
  readonly bootstrapwitnesses_len: (a: number) => number;
  readonly bootstrapwitnesses_get: (a: number, b: number) => number;
  readonly bootstrapwitnesses_add: (a: number, b: number) => void;
  readonly __wbg_publickeys_free: (a: number) => void;
  readonly publickeys_new: () => number;
  readonly publickeys_get: (a: number, b: number) => number;
  readonly publickeys_add: (a: number, b: number) => void;
  readonly __wbg_ed25519signature_free: (a: number) => void;
  readonly ed25519signature_to_bytes: (a: number, b: number) => void;
  readonly ed25519signature_to_bech32: (a: number, b: number) => void;
  readonly ed25519signature_to_hex: (a: number, b: number) => void;
  readonly ed25519signature_from_bech32: (a: number, b: number) => number;
  readonly ed25519signature_from_hex: (a: number, b: number) => number;
  readonly ed25519signature_from_bytes: (a: number, b: number) => number;
  readonly __wbg_legacydaedalusprivatekey_free: (a: number) => void;
  readonly legacydaedalusprivatekey_from_bytes: (
    a: number,
    b: number,
  ) => number;
  readonly legacydaedalusprivatekey_as_bytes: (a: number, b: number) => void;
  readonly legacydaedalusprivatekey_chaincode: (a: number, b: number) => void;
  readonly __wbg_ed25519keyhash_free: (a: number) => void;
  readonly ed25519keyhash_from_bytes: (a: number, b: number) => number;
  readonly ed25519keyhash_to_bytes: (a: number, b: number) => void;
  readonly ed25519keyhash_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly ed25519keyhash_from_bech32: (a: number, b: number) => number;
  readonly ed25519keyhash_to_hex: (a: number, b: number) => void;
  readonly ed25519keyhash_from_hex: (a: number, b: number) => number;
  readonly scripthash_from_bytes: (a: number, b: number) => number;
  readonly scripthash_from_bech32: (a: number, b: number) => number;
  readonly scripthash_from_hex: (a: number, b: number) => number;
  readonly transactionhash_from_bytes: (a: number, b: number) => number;
  readonly transactionhash_from_bech32: (a: number, b: number) => number;
  readonly transactionhash_from_hex: (a: number, b: number) => number;
  readonly genesisdelegatehash_from_bytes: (a: number, b: number) => number;
  readonly genesisdelegatehash_from_bech32: (a: number, b: number) => number;
  readonly genesisdelegatehash_from_hex: (a: number, b: number) => number;
  readonly genesishash_from_bytes: (a: number, b: number) => number;
  readonly genesishash_from_bech32: (a: number, b: number) => number;
  readonly genesishash_from_hex: (a: number, b: number) => number;
  readonly __wbg_auxiliarydatahash_free: (a: number) => void;
  readonly auxiliarydatahash_from_bytes: (a: number, b: number) => number;
  readonly auxiliarydatahash_to_bytes: (a: number, b: number) => void;
  readonly auxiliarydatahash_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly auxiliarydatahash_from_bech32: (a: number, b: number) => number;
  readonly auxiliarydatahash_to_hex: (a: number, b: number) => void;
  readonly auxiliarydatahash_from_hex: (a: number, b: number) => number;
  readonly poolmetadatahash_from_bytes: (a: number, b: number) => number;
  readonly poolmetadatahash_from_bech32: (a: number, b: number) => number;
  readonly poolmetadatahash_from_hex: (a: number, b: number) => number;
  readonly vrfkeyhash_from_bytes: (a: number, b: number) => number;
  readonly vrfkeyhash_from_bech32: (a: number, b: number) => number;
  readonly vrfkeyhash_from_hex: (a: number, b: number) => number;
  readonly blockhash_from_bytes: (a: number, b: number) => number;
  readonly blockhash_from_bech32: (a: number, b: number) => number;
  readonly blockhash_from_hex: (a: number, b: number) => number;
  readonly datahash_from_bytes: (a: number, b: number) => number;
  readonly datahash_from_bech32: (a: number, b: number) => number;
  readonly datahash_from_hex: (a: number, b: number) => number;
  readonly scriptdatahash_from_bytes: (a: number, b: number) => number;
  readonly scriptdatahash_from_bech32: (a: number, b: number) => number;
  readonly scriptdatahash_from_hex: (a: number, b: number) => number;
  readonly kesvkey_from_bytes: (a: number, b: number) => number;
  readonly kesvkey_from_bech32: (a: number, b: number) => number;
  readonly kesvkey_from_hex: (a: number, b: number) => number;
  readonly vrfvkey_to_bytes: (a: number, b: number) => void;
  readonly vrfvkey_from_bytes: (a: number, b: number) => number;
  readonly vrfvkey_hash: (a: number) => number;
  readonly __wbg_kessignature_free: (a: number) => void;
  readonly kessignature_from_bytes: (a: number, b: number) => number;
  readonly __wbg_nonce_free: (a: number) => void;
  readonly nonce_to_bytes: (a: number, b: number) => void;
  readonly nonce_from_bytes: (a: number, b: number) => number;
  readonly nonce_new_identity: () => number;
  readonly nonce_new_from_hash: (a: number, b: number) => number;
  readonly nonce_get_hash: (a: number, b: number) => void;
  readonly __wbg_vrfcert_free: (a: number) => void;
  readonly vrfcert_to_bytes: (a: number, b: number) => void;
  readonly vrfcert_from_bytes: (a: number, b: number) => number;
  readonly vrfcert_to_json: (a: number, b: number) => void;
  readonly vrfcert_to_js_value: (a: number) => number;
  readonly vrfcert_from_json: (a: number, b: number) => number;
  readonly vrfcert_new: (a: number, b: number, c: number, d: number) => number;
  readonly vkeywitnesses_len: (a: number) => number;
  readonly vkeys_len: (a: number) => number;
  readonly publickeys_size: (a: number) => number;
  readonly scripthash_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly genesisdelegatehash_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly genesishash_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly transactionhash_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly poolmetadatahash_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly vrfkeyhash_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly blockhash_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly datahash_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly scriptdatahash_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly kesvkey_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly scripthash_to_bytes: (a: number, b: number) => void;
  readonly transactionhash_to_bytes: (a: number, b: number) => void;
  readonly genesisdelegatehash_to_bytes: (a: number, b: number) => void;
  readonly genesishash_to_bytes: (a: number, b: number) => void;
  readonly publickey_as_bytes: (a: number, b: number) => void;
  readonly poolmetadatahash_to_bytes: (a: number, b: number) => void;
  readonly vrfkeyhash_to_bytes: (a: number, b: number) => void;
  readonly blockhash_to_bytes: (a: number, b: number) => void;
  readonly datahash_to_bytes: (a: number, b: number) => void;
  readonly scriptdatahash_to_bytes: (a: number, b: number) => void;
  readonly kesvkey_to_bytes: (a: number, b: number) => void;
  readonly vrfvkey_to_raw_key: (a: number, b: number) => void;
  readonly kessignature_to_bytes: (a: number, b: number) => void;
  readonly vrfcert_output: (a: number, b: number) => void;
  readonly vrfcert_proof: (a: number, b: number) => void;
  readonly __wbg_vrfvkey_free: (a: number) => void;
  readonly scripthash_to_hex: (a: number, b: number) => void;
  readonly genesisdelegatehash_to_hex: (a: number, b: number) => void;
  readonly genesishash_to_hex: (a: number, b: number) => void;
  readonly transactionhash_to_hex: (a: number, b: number) => void;
  readonly poolmetadatahash_to_hex: (a: number, b: number) => void;
  readonly vrfkeyhash_to_hex: (a: number, b: number) => void;
  readonly blockhash_to_hex: (a: number, b: number) => void;
  readonly datahash_to_hex: (a: number, b: number) => void;
  readonly scriptdatahash_to_hex: (a: number, b: number) => void;
  readonly kesvkey_to_hex: (a: number, b: number) => void;
  readonly vkeywitnesses_new: () => number;
  readonly vkeys_new: () => number;
  readonly vkeywitness_vkey: (a: number) => number;
  readonly __wbg_scripthash_free: (a: number) => void;
  readonly __wbg_genesisdelegatehash_free: (a: number) => void;
  readonly __wbg_genesishash_free: (a: number) => void;
  readonly __wbg_kesvkey_free: (a: number) => void;
  readonly __wbg_poolmetadatahash_free: (a: number) => void;
  readonly __wbg_transactionhash_free: (a: number) => void;
  readonly __wbg_vrfkeyhash_free: (a: number) => void;
  readonly __wbg_blockhash_free: (a: number) => void;
  readonly __wbg_scriptdatahash_free: (a: number) => void;
  readonly __wbg_datahash_free: (a: number) => void;
  readonly __wbg_metadatamap_free: (a: number) => void;
  readonly metadatamap_to_bytes: (a: number, b: number) => void;
  readonly metadatamap_from_bytes: (a: number, b: number) => number;
  readonly metadatamap_insert: (a: number, b: number, c: number) => number;
  readonly metadatamap_insert_str: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => number;
  readonly metadatamap_insert_i32: (a: number, b: number, c: number) => number;
  readonly metadatamap_get: (a: number, b: number) => number;
  readonly metadatamap_get_str: (a: number, b: number, c: number) => number;
  readonly metadatamap_get_i32: (a: number, b: number) => number;
  readonly metadatamap_has: (a: number, b: number) => number;
  readonly metadatamap_keys: (a: number) => number;
  readonly __wbg_metadatalist_free: (a: number) => void;
  readonly metadatalist_to_bytes: (a: number, b: number) => void;
  readonly metadatalist_from_bytes: (a: number, b: number) => number;
  readonly metadatalist_new: () => number;
  readonly metadatalist_len: (a: number) => number;
  readonly metadatalist_get: (a: number, b: number) => number;
  readonly metadatalist_add: (a: number, b: number) => void;
  readonly __wbg_transactionmetadatum_free: (a: number) => void;
  readonly transactionmetadatum_to_bytes: (a: number, b: number) => void;
  readonly transactionmetadatum_from_bytes: (a: number, b: number) => number;
  readonly transactionmetadatum_new_map: (a: number) => number;
  readonly transactionmetadatum_new_list: (a: number) => number;
  readonly transactionmetadatum_new_int: (a: number) => number;
  readonly transactionmetadatum_new_bytes: (a: number, b: number) => number;
  readonly transactionmetadatum_new_text: (a: number, b: number) => number;
  readonly transactionmetadatum_kind: (a: number) => number;
  readonly transactionmetadatum_as_map: (a: number) => number;
  readonly transactionmetadatum_as_list: (a: number) => number;
  readonly transactionmetadatum_as_int: (a: number) => number;
  readonly transactionmetadatum_as_bytes: (a: number, b: number) => void;
  readonly transactionmetadatum_as_text: (a: number, b: number) => void;
  readonly __wbg_transactionmetadatumlabels_free: (a: number) => void;
  readonly transactionmetadatumlabels_to_bytes: (a: number, b: number) => void;
  readonly transactionmetadatumlabels_from_bytes: (
    a: number,
    b: number,
  ) => number;
  readonly transactionmetadatumlabels_get: (a: number, b: number) => number;
  readonly transactionmetadatumlabels_add: (a: number, b: number) => void;
  readonly __wbg_generaltransactionmetadata_free: (a: number) => void;
  readonly generaltransactionmetadata_to_bytes: (a: number, b: number) => void;
  readonly generaltransactionmetadata_from_bytes: (
    a: number,
    b: number,
  ) => number;
  readonly generaltransactionmetadata_to_json: (a: number, b: number) => void;
  readonly generaltransactionmetadata_to_js_value: (a: number) => number;
  readonly generaltransactionmetadata_from_json: (
    a: number,
    b: number,
  ) => number;
  readonly generaltransactionmetadata_new: () => number;
  readonly generaltransactionmetadata_len: (a: number) => number;
  readonly generaltransactionmetadata_insert: (
    a: number,
    b: number,
    c: number,
  ) => number;
  readonly generaltransactionmetadata_get: (a: number, b: number) => number;
  readonly generaltransactionmetadata_keys: (a: number) => number;
  readonly __wbg_auxiliarydata_free: (a: number) => void;
  readonly auxiliarydata_to_bytes: (a: number, b: number) => void;
  readonly auxiliarydata_from_bytes: (a: number, b: number) => number;
  readonly auxiliarydata_to_json: (a: number, b: number) => void;
  readonly auxiliarydata_to_js_value: (a: number) => number;
  readonly auxiliarydata_from_json: (a: number, b: number) => number;
  readonly auxiliarydata_new: () => number;
  readonly auxiliarydata_metadata: (a: number) => number;
  readonly auxiliarydata_set_metadata: (a: number, b: number) => void;
  readonly auxiliarydata_native_scripts: (a: number) => number;
  readonly auxiliarydata_set_native_scripts: (a: number, b: number) => void;
  readonly auxiliarydata_plutus_scripts: (a: number) => number;
  readonly auxiliarydata_set_plutus_scripts: (a: number, b: number) => void;
  readonly auxiliarydata_set_plutus_v2_scripts: (a: number, b: number) => void;
  readonly encode_arbitrary_bytes_as_metadatum: (
    a: number,
    b: number,
  ) => number;
  readonly decode_arbitrary_bytes_from_metadatum: (
    a: number,
    b: number,
  ) => void;
  readonly encode_json_str_to_metadatum: (
    a: number,
    b: number,
    c: number,
  ) => number;
  readonly decode_metadatum_to_json_str: (
    a: number,
    b: number,
    c: number,
  ) => void;
  readonly transactionmetadatumlabels_len: (a: number) => number;
  readonly metadatamap_len: (a: number) => number;
  readonly metadatamap_new: () => number;
  readonly transactionmetadatumlabels_new: () => number;
  readonly __wbg_networkinfo_free: (a: number) => void;
  readonly networkinfo_new: (a: number, b: number) => number;
  readonly networkinfo_network_id: (a: number) => number;
  readonly networkinfo_protocol_magic: (a: number) => number;
  readonly networkinfo_testnet: () => number;
  readonly networkinfo_mainnet: () => number;
  readonly __wbg_stakecredential_free: (a: number) => void;
  readonly stakecredential_from_keyhash: (a: number) => number;
  readonly stakecredential_from_scripthash: (a: number) => number;
  readonly stakecredential_to_keyhash: (a: number) => number;
  readonly stakecredential_to_scripthash: (a: number) => number;
  readonly stakecredential_kind: (a: number) => number;
  readonly stakecredential_to_bytes: (a: number, b: number) => void;
  readonly stakecredential_from_bytes: (a: number, b: number) => number;
  readonly stakecredential_to_json: (a: number, b: number) => void;
  readonly stakecredential_to_js_value: (a: number) => number;
  readonly stakecredential_from_json: (a: number, b: number) => number;
  readonly __wbg_byronaddress_free: (a: number) => void;
  readonly byronaddress_to_base58: (a: number, b: number) => void;
  readonly byronaddress_to_bytes: (a: number, b: number) => void;
  readonly byronaddress_from_bytes: (a: number, b: number) => number;
  readonly byronaddress_byron_protocol_magic: (a: number) => number;
  readonly byronaddress_attributes: (a: number, b: number) => void;
  readonly byronaddress_network_id: (a: number) => number;
  readonly byronaddress_from_base58: (a: number, b: number) => number;
  readonly byronaddress_icarus_from_key: (a: number, b: number) => number;
  readonly byronaddress_is_valid: (a: number, b: number) => number;
  readonly byronaddress_to_address: (a: number) => number;
  readonly __wbg_address_free: (a: number) => void;
  readonly address_from_bytes: (a: number, b: number) => number;
  readonly address_to_json: (a: number, b: number) => void;
  readonly address_to_js_value: (a: number) => number;
  readonly address_from_json: (a: number, b: number) => number;
  readonly address_to_bytes: (a: number, b: number) => void;
  readonly address_to_bech32: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly address_from_bech32: (a: number, b: number) => number;
  readonly address_network_id: (a: number) => number;
  readonly address_as_byron: (a: number) => number;
  readonly address_as_reward: (a: number) => number;
  readonly address_as_pointer: (a: number) => number;
  readonly address_as_enterprise: (a: number) => number;
  readonly address_as_base: (a: number) => number;
  readonly __wbg_baseaddress_free: (a: number) => void;
  readonly baseaddress_new: (a: number, b: number, c: number) => number;
  readonly baseaddress_payment_cred: (a: number) => number;
  readonly baseaddress_stake_cred: (a: number) => number;
  readonly baseaddress_to_address: (a: number) => number;
  readonly __wbg_enterpriseaddress_free: (a: number) => void;
  readonly enterpriseaddress_new: (a: number, b: number) => number;
  readonly enterpriseaddress_to_address: (a: number) => number;
  readonly rewardaddress_to_address: (a: number) => number;
  readonly __wbg_pointer_free: (a: number) => void;
  readonly pointer_new: (a: number, b: number, c: number) => number;
  readonly pointer_slot: (a: number) => number;
  readonly pointer_tx_index: (a: number) => number;
  readonly pointer_cert_index: (a: number) => number;
  readonly __wbg_pointeraddress_free: (a: number) => void;
  readonly pointeraddress_new: (a: number, b: number, c: number) => number;
  readonly pointeraddress_payment_cred: (a: number) => number;
  readonly pointeraddress_stake_pointer: (a: number) => number;
  readonly pointeraddress_to_address: (a: number) => number;
  readonly __wbg_transactionbuilderconfig_free: (a: number) => void;
  readonly __wbg_transactionbuilderconfigbuilder_free: (a: number) => void;
  readonly transactionbuilderconfigbuilder_new: () => number;
  readonly transactionbuilderconfigbuilder_fee_algo: (
    a: number,
    b: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_coins_per_utxo_byte: (
    a: number,
    b: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_pool_deposit: (
    a: number,
    b: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_key_deposit: (
    a: number,
    b: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_max_value_size: (
    a: number,
    b: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_max_tx_size: (
    a: number,
    b: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_ex_unit_prices: (
    a: number,
    b: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_max_tx_ex_units: (
    a: number,
    b: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_costmdls: (
    a: number,
    b: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_collateral_percentage: (
    a: number,
    b: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_max_collateral_inputs: (
    a: number,
    b: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_slot_config: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_blockfrost: (
    a: number,
    b: number,
  ) => number;
  readonly transactionbuilderconfigbuilder_build: (a: number) => number;
  readonly __wbg_transactionbuilder_free: (a: number) => void;
  readonly transactionbuilder_add_inputs_from: (
    a: number,
    b: number,
    c: number,
  ) => void;
  readonly transactionbuilder_add_input: (
    a: number,
    b: number,
    c: number,
  ) => void;
  readonly transactionbuilder_add_reference_input: (
    a: number,
    b: number,
  ) => void;
  readonly transactionbuilder_fee_for_input: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => number;
  readonly transactionbuilder_add_output: (a: number, b: number) => void;
  readonly transactionbuilder_add_plutus_script: (a: number, b: number) => void;
  readonly transactionbuilder_add_plutus_v2_script: (
    a: number,
    b: number,
  ) => void;
  readonly transactionbuilder_add_plutus_data: (a: number, b: number) => void;
  readonly transactionbuilder_add_native_script: (a: number, b: number) => void;
  readonly transactionbuilder_add_certificate: (
    a: number,
    b: number,
    c: number,
  ) => void;
  readonly transactionbuilder_fee_for_output: (a: number, b: number) => number;
  readonly transactionbuilder_set_ttl: (a: number, b: number) => void;
  readonly transactionbuilder_set_validity_start_interval: (
    a: number,
    b: number,
  ) => void;
  readonly transactionbuilder_add_withdrawal: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly transactionbuilder_auxiliary_data: (a: number) => number;
  readonly transactionbuilder_set_auxiliary_data: (
    a: number,
    b: number,
  ) => void;
  readonly transactionbuilder_set_metadata: (a: number, b: number) => void;
  readonly transactionbuilder_add_metadatum: (
    a: number,
    b: number,
    c: number,
  ) => void;
  readonly transactionbuilder_add_json_metadatum: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly transactionbuilder_add_json_metadatum_with_schema: (
    a: number,
    b: number,
    c: number,
    d: number,
    e: number,
  ) => void;
  readonly transactionbuilder_mint: (a: number) => number;
  readonly transactionbuilder_certificates: (a: number) => number;
  readonly transactionbuilder_withdrawals: (a: number) => number;
  readonly transactionbuilder_native_scripts: (a: number) => number;
  readonly transactionbuilder_add_mint: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
  readonly transactionbuilder_new: (a: number) => number;
  readonly transactionbuilder_script_data_hash: (a: number) => number;
  readonly transactionbuilder_add_collateral: (a: number, b: number) => void;
  readonly transactionbuilder_get_collateral: (a: number) => number;
  readonly transactionbuilder_add_required_signer: (
    a: number,
    b: number,
  ) => void;
  readonly transactionbuilder_required_signers: (a: number) => number;
  readonly transactionbuilder_set_network_id: (a: number, b: number) => void;
  readonly transactionbuilder_network_id: (a: number) => number;
  readonly transactionbuilder_redeemers: (a: number) => number;
  readonly transactionbuilder_get_explicit_input: (a: number) => number;
  readonly transactionbuilder_get_implicit_input: (a: number) => number;
  readonly transactionbuilder_get_total_input: (a: number) => number;
  readonly transactionbuilder_get_total_output: (a: number) => number;
  readonly transactionbuilder_get_explicit_output: (a: number) => number;
  readonly transactionbuilder_get_deposit: (a: number) => number;
  readonly transactionbuilder_get_fee_if_set: (a: number) => number;
  readonly transactionbuilder_balance: (
    a: number,
    b: number,
    c: number,
  ) => void;
  readonly transactionbuilder_to_bytes: (a: number, b: number) => void;
  readonly transactionbuilder_full_size: (a: number) => number;
  readonly transactionbuilder_output_sizes: (a: number, b: number) => void;
  readonly transactionbuilder_outputs: (a: number) => number;
  readonly transactionbuilder_construct: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => number;
  readonly transactionbuilder_build_tx: (a: number) => number;
  readonly transactionbuilder_min_fee: (a: number) => number;
  readonly __wbg_redeemerwitnesskey_free: (a: number) => void;
  readonly redeemerwitnesskey_tag: (a: number) => number;
  readonly redeemerwitnesskey_new: (a: number, b: number) => number;
  readonly __wbg_requiredwitnessset_free: (a: number) => void;
  readonly requiredwitnessset_add_vkey: (a: number, b: number) => void;
  readonly requiredwitnessset_add_vkey_key: (a: number, b: number) => void;
  readonly requiredwitnessset_add_vkey_key_hash: (a: number, b: number) => void;
  readonly requiredwitnessset_add_bootstrap: (a: number, b: number) => void;
  readonly requiredwitnessset_add_bootstrap_key: (a: number, b: number) => void;
  readonly requiredwitnessset_add_bootstrap_key_hash: (
    a: number,
    b: number,
  ) => void;
  readonly requiredwitnessset_add_native_script: (a: number, b: number) => void;
  readonly requiredwitnessset_add_native_script_hash: (
    a: number,
    b: number,
  ) => void;
  readonly requiredwitnessset_add_plutus_script: (a: number, b: number) => void;
  readonly requiredwitnessset_add_plutus_v2_script: (
    a: number,
    b: number,
  ) => void;
  readonly requiredwitnessset_add_plutus_hash: (a: number, b: number) => void;
  readonly requiredwitnessset_add_plutus_datum: (a: number, b: number) => void;
  readonly requiredwitnessset_add_plutus_datum_hash: (
    a: number,
    b: number,
  ) => void;
  readonly requiredwitnessset_add_redeemer: (a: number, b: number) => void;
  readonly requiredwitnessset_add_redeemer_tag: (a: number, b: number) => void;
  readonly requiredwitnessset_add_all: (a: number, b: number) => void;
  readonly requiredwitnessset_new: () => number;
  readonly __wbg_transactionwitnesssetbuilder_free: (a: number) => void;
  readonly transactionwitnesssetbuilder_add_vkey: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnesssetbuilder_add_bootstrap: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnesssetbuilder_add_native_script: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnesssetbuilder_add_plutus_script: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnesssetbuilder_add_plutus_v2_script: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnesssetbuilder_add_plutus_datum: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnesssetbuilder_add_redeemer: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnesssetbuilder_add_required_wits: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnesssetbuilder_new: () => number;
  readonly transactionwitnesssetbuilder_add_existing: (
    a: number,
    b: number,
  ) => void;
  readonly transactionwitnesssetbuilder_build: (a: number) => number;
  readonly baseaddress_from_address: (a: number) => number;
  readonly enterpriseaddress_from_address: (a: number) => number;
  readonly rewardaddress_from_address: (a: number) => number;
  readonly redeemerwitnesskey_index: (a: number) => number;
  readonly pointeraddress_from_address: (a: number) => number;
  readonly enterpriseaddress_payment_cred: (a: number) => number;
  readonly rewardaddress_payment_cred: (a: number) => number;
  readonly rewardaddress_new: (a: number, b: number) => number;
  readonly __wbg_rewardaddress_free: (a: number) => void;
  readonly byronaddress_from_address: (a: number) => number;
  readonly encrypt_with_password: (
    a: number,
    b: number,
    c: number,
    d: number,
    e: number,
    f: number,
    g: number,
    h: number,
    i: number,
  ) => void;
  readonly decrypt_with_password: (
    a: number,
    b: number,
    c: number,
    d: number,
    e: number,
  ) => void;
  readonly __wbg_blockfrost_free: (a: number) => void;
  readonly blockfrost_new: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => number;
  readonly blockfrost_url: (a: number, b: number) => void;
  readonly blockfrost_project_id: (a: number, b: number) => void;
  readonly apply_params_to_plutus_script: (a: number, b: number) => number;
  readonly __wbg_plutuswitness_free: (a: number) => void;
  readonly plutuswitness_new: (a: number, b: number, c: number) => number;
  readonly plutuswitness_new_plutus_v2: (
    a: number,
    b: number,
    c: number,
  ) => number;
  readonly plutuswitness_plutus_data: (a: number) => number;
  readonly plutuswitness_redeemer: (a: number) => number;
  readonly plutuswitness_script: (a: number) => number;
  readonly plutuswitness_version: (a: number) => number;
  readonly __wbg_scriptwitness_free: (a: number) => void;
  readonly scriptwitness_to_json: (a: number, b: number) => void;
  readonly scriptwitness_to_js_value: (a: number) => number;
  readonly scriptwitness_from_json: (a: number, b: number) => number;
  readonly scriptwitness_new_native_witness: (a: number) => number;
  readonly scriptwitness_new_plutus_witness: (a: number) => number;
  readonly scriptwitness_kind: (a: number) => number;
  readonly scriptwitness_as_native_witness: (a: number) => number;
  readonly scriptwitness_as_plutus_witness: (a: number) => number;
  readonly __wbg_transactionunspentoutput_free: (a: number) => void;
  readonly transactionunspentoutput_to_bytes: (a: number, b: number) => void;
  readonly transactionunspentoutput_from_bytes: (
    a: number,
    b: number,
  ) => number;
  readonly transactionunspentoutput_new: (a: number, b: number) => number;
  readonly transactionunspentoutput_input: (a: number) => number;
  readonly transactionunspentoutput_output: (a: number) => number;
  readonly transactionunspentoutput_to_legacy_bytes: (
    a: number,
    b: number,
  ) => void;
  readonly __wbg_transactionunspentoutputs_free: (a: number) => void;
  readonly transactionunspentoutputs_new: () => number;
  readonly transactionunspentoutputs_len: (a: number) => number;
  readonly transactionunspentoutputs_get: (a: number, b: number) => number;
  readonly transactionunspentoutputs_add: (a: number, b: number) => void;
  readonly __wbg_bignum_free: (a: number) => void;
  readonly bignum_to_bytes: (a: number, b: number) => void;
  readonly bignum_from_bytes: (a: number, b: number) => number;
  readonly bignum_from_str: (a: number, b: number) => number;
  readonly bignum_to_str: (a: number, b: number) => void;
  readonly bignum_zero: () => number;
  readonly bignum_is_zero: (a: number) => number;
  readonly bignum_checked_mul: (a: number, b: number) => number;
  readonly bignum_checked_add: (a: number, b: number) => number;
  readonly bignum_checked_sub: (a: number, b: number) => number;
  readonly bignum_checked_div: (a: number, b: number) => number;
  readonly bignum_checked_div_ceil: (a: number, b: number) => number;
  readonly bignum_clamped_sub: (a: number, b: number) => number;
  readonly bignum_compare: (a: number, b: number) => number;
  readonly __wbg_value_free: (a: number) => void;
  readonly value_to_bytes: (a: number, b: number) => void;
  readonly value_from_bytes: (a: number, b: number) => number;
  readonly value_to_json: (a: number, b: number) => void;
  readonly value_to_js_value: (a: number) => number;
  readonly value_from_json: (a: number, b: number) => number;
  readonly value_new: (a: number) => number;
  readonly value_new_from_assets: (a: number) => number;
  readonly value_zero: () => number;
  readonly value_is_zero: (a: number) => number;
  readonly value_coin: (a: number) => number;
  readonly value_set_coin: (a: number, b: number) => void;
  readonly value_multiasset: (a: number) => number;
  readonly value_set_multiasset: (a: number, b: number) => void;
  readonly value_checked_add: (a: number, b: number) => number;
  readonly value_checked_sub: (a: number, b: number) => number;
  readonly value_clamped_sub: (a: number, b: number) => number;
  readonly value_compare: (a: number, b: number) => number;
  readonly __wbg_int_free: (a: number) => void;
  readonly int_to_bytes: (a: number, b: number) => void;
  readonly int_from_bytes: (a: number, b: number) => number;
  readonly int_new: (a: number) => number;
  readonly int_new_negative: (a: number) => number;
  readonly int_new_i32: (a: number) => number;
  readonly int_is_positive: (a: number) => number;
  readonly int_as_positive: (a: number) => number;
  readonly int_as_negative: (a: number) => number;
  readonly int_as_i32: (a: number, b: number) => void;
  readonly int_as_i32_or_fail: (a: number) => number;
  readonly int_to_str: (a: number, b: number) => void;
  readonly int_from_str: (a: number, b: number) => number;
  readonly __wbg_bigint_free: (a: number) => void;
  readonly bigint_to_bytes: (a: number, b: number) => void;
  readonly bigint_from_bytes: (a: number, b: number) => number;
  readonly bigint_as_u64: (a: number) => number;
  readonly bigint_as_int: (a: number) => number;
  readonly bigint_from_str: (a: number, b: number) => number;
  readonly bigint_to_str: (a: number, b: number) => void;
  readonly make_daedalus_bootstrap_witness: (
    a: number,
    b: number,
    c: number,
  ) => number;
  readonly make_icarus_bootstrap_witness: (
    a: number,
    b: number,
    c: number,
  ) => number;
  readonly make_vkey_witness: (a: number, b: number) => number;
  readonly hash_auxiliary_data: (a: number) => number;
  readonly hash_transaction: (a: number) => number;
  readonly hash_plutus_data: (a: number) => number;
  readonly hash_blake2b256: (a: number, b: number, c: number) => void;
  readonly hash_blake2b224: (a: number, b: number, c: number) => void;
  readonly hash_script_data: (a: number, b: number, c: number) => number;
  readonly get_implicit_input: (a: number, b: number, c: number) => number;
  readonly get_deposit: (a: number, b: number, c: number) => number;
  readonly min_ada_required: (a: number, b: number) => number;
  readonly encode_json_str_to_native_script: (
    a: number,
    b: number,
    c: number,
    d: number,
    e: number,
  ) => number;
  readonly int_as_i32_or_nothing: (a: number, b: number) => void;
  readonly __wbg_plutusscript_free: (a: number) => void;
  readonly plutusscript_to_bytes: (a: number, b: number) => void;
  readonly plutusscript_from_bytes: (a: number, b: number) => number;
  readonly plutusscript_hash: (a: number, b: number) => number;
  readonly plutusscript_new: (a: number, b: number) => number;
  readonly plutusscript_bytes: (a: number, b: number) => void;
  readonly __wbg_plutusscripts_free: (a: number) => void;
  readonly plutusscripts_to_bytes: (a: number, b: number) => void;
  readonly plutusscripts_from_bytes: (a: number, b: number) => number;
  readonly plutusscripts_new: () => number;
  readonly plutusscripts_get: (a: number, b: number) => number;
  readonly plutusscripts_add: (a: number, b: number) => void;
  readonly __wbg_constrplutusdata_free: (a: number) => void;
  readonly constrplutusdata_to_bytes: (a: number, b: number) => void;
  readonly constrplutusdata_from_bytes: (a: number, b: number) => number;
  readonly constrplutusdata_alternative: (a: number) => number;
  readonly constrplutusdata_data: (a: number) => number;
  readonly constrplutusdata_new: (a: number, b: number) => number;
  readonly __wbg_costmodel_free: (a: number) => void;
  readonly costmodel_to_bytes: (a: number, b: number) => void;
  readonly costmodel_from_bytes: (a: number, b: number) => number;
  readonly costmodel_new: () => number;
  readonly costmodel_new_plutus_v2: () => number;
  readonly costmodel_set: (a: number, b: number, c: number) => number;
  readonly costmodel_get: (a: number, b: number) => number;
  readonly __wbg_costmdls_free: (a: number) => void;
  readonly costmdls_to_bytes: (a: number, b: number) => void;
  readonly costmdls_from_bytes: (a: number, b: number) => number;
  readonly costmdls_new: () => number;
  readonly costmdls_len: (a: number) => number;
  readonly costmdls_insert: (a: number, b: number, c: number) => number;
  readonly costmdls_get: (a: number, b: number) => number;
  readonly costmdls_keys: (a: number) => number;
  readonly __wbg_exunitprices_free: (a: number) => void;
  readonly exunitprices_to_bytes: (a: number, b: number) => void;
  readonly exunitprices_from_bytes: (a: number, b: number) => number;
  readonly exunitprices_mem_price: (a: number) => number;
  readonly exunitprices_step_price: (a: number) => number;
  readonly exunitprices_new: (a: number, b: number) => number;
  readonly exunitprices_from_float: (a: number, b: number) => number;
  readonly __wbg_exunits_free: (a: number) => void;
  readonly exunits_to_bytes: (a: number, b: number) => void;
  readonly exunits_from_bytes: (a: number, b: number) => number;
  readonly exunits_steps: (a: number) => number;
  readonly exunits_new: (a: number, b: number) => number;
  readonly __wbg_language_free: (a: number) => void;
  readonly language_to_bytes: (a: number, b: number) => void;
  readonly language_from_bytes: (a: number, b: number) => number;
  readonly language_new_plutus_v1: () => number;
  readonly language_new_plutus_v2: () => number;
  readonly language_kind: (a: number) => number;
  readonly __wbg_languages_free: (a: number) => void;
  readonly languages_new: () => number;
  readonly languages_get: (a: number, b: number) => number;
  readonly languages_add: (a: number, b: number) => void;
  readonly __wbg_plutusmap_free: (a: number) => void;
  readonly plutusmap_to_bytes: (a: number, b: number) => void;
  readonly plutusmap_from_bytes: (a: number, b: number) => number;
  readonly plutusmap_new: () => number;
  readonly plutusmap_insert: (a: number, b: number, c: number) => number;
  readonly plutusmap_get: (a: number, b: number) => number;
  readonly plutusmap_keys: (a: number) => number;
  readonly __wbg_plutusdata_free: (a: number) => void;
  readonly plutusdata_to_bytes: (a: number, b: number) => void;
  readonly plutusdata_from_bytes: (a: number, b: number) => number;
  readonly plutusdata_new_constr_plutus_data: (a: number) => number;
  readonly plutusdata_new_map: (a: number) => number;
  readonly plutusdata_new_list: (a: number) => number;
  readonly plutusdata_new_integer: (a: number) => number;
  readonly plutusdata_new_bytes: (a: number, b: number) => number;
  readonly plutusdata_kind: (a: number) => number;
  readonly plutusdata_as_constr_plutus_data: (a: number) => number;
  readonly plutusdata_as_map: (a: number) => number;
  readonly plutusdata_as_list: (a: number) => number;
  readonly plutusdata_as_integer: (a: number) => number;
  readonly plutusdata_as_bytes: (a: number, b: number) => void;
  readonly __wbg_plutuslist_free: (a: number) => void;
  readonly plutuslist_to_bytes: (a: number, b: number) => void;
  readonly plutuslist_from_bytes: (a: number, b: number) => number;
  readonly plutuslist_new: () => number;
  readonly plutuslist_get: (a: number, b: number) => number;
  readonly plutuslist_add: (a: number, b: number) => void;
  readonly __wbg_redeemer_free: (a: number) => void;
  readonly redeemer_to_bytes: (a: number, b: number) => void;
  readonly redeemer_from_bytes: (a: number, b: number) => number;
  readonly redeemer_tag: (a: number) => number;
  readonly redeemer_data: (a: number) => number;
  readonly redeemer_ex_units: (a: number) => number;
  readonly redeemer_new: (a: number, b: number, c: number, d: number) => number;
  readonly __wbg_redeemertag_free: (a: number) => void;
  readonly redeemertag_to_bytes: (a: number, b: number) => void;
  readonly redeemertag_from_bytes: (a: number, b: number) => number;
  readonly redeemertag_new_spend: () => number;
  readonly redeemertag_new_mint: () => number;
  readonly redeemertag_new_cert: () => number;
  readonly redeemertag_new_reward: () => number;
  readonly redeemertag_kind: (a: number) => number;
  readonly __wbg_redeemers_free: (a: number) => void;
  readonly redeemers_to_bytes: (a: number, b: number) => void;
  readonly redeemers_from_bytes: (a: number, b: number) => number;
  readonly redeemers_get: (a: number, b: number) => number;
  readonly redeemers_add: (a: number, b: number) => void;
  readonly __wbg_strings_free: (a: number) => void;
  readonly strings_get: (a: number, b: number, c: number) => void;
  readonly strings_add: (a: number, b: number, c: number) => void;
  readonly __wbg_data_free: (a: number) => void;
  readonly data_to_bytes: (a: number, b: number) => void;
  readonly data_from_bytes: (a: number, b: number) => number;
  readonly data_to_json: (a: number, b: number) => void;
  readonly data_to_js_value: (a: number) => number;
  readonly data_from_json: (a: number, b: number) => number;
  readonly data_new: (a: number) => number;
  readonly data_get: (a: number) => number;
  readonly __wbg_script_free: (a: number) => void;
  readonly script_to_bytes: (a: number, b: number) => void;
  readonly script_from_bytes: (a: number, b: number) => number;
  readonly script_to_json: (a: number, b: number) => void;
  readonly script_to_js_value: (a: number) => number;
  readonly script_from_json: (a: number, b: number) => number;
  readonly script_new_native: (a: number) => number;
  readonly script_new_plutus_v1: (a: number) => number;
  readonly script_new_plutus_v2: (a: number) => number;
  readonly script_kind: (a: number) => number;
  readonly script_as_native: (a: number) => number;
  readonly script_as_plutus_v1: (a: number) => number;
  readonly script_as_plutus_v2: (a: number) => number;
  readonly __wbg_scriptref_free: (a: number) => void;
  readonly scriptref_to_bytes: (a: number, b: number) => void;
  readonly scriptref_from_bytes: (a: number, b: number) => number;
  readonly scriptref_to_js_value: (a: number) => number;
  readonly scriptref_from_json: (a: number, b: number) => number;
  readonly scriptref_new: (a: number) => number;
  readonly scriptref_get: (a: number) => number;
  readonly __wbg_datum_free: (a: number) => void;
  readonly datum_to_bytes: (a: number, b: number) => void;
  readonly datum_from_bytes: (a: number, b: number) => number;
  readonly datum_to_json: (a: number, b: number) => void;
  readonly datum_to_js_value: (a: number) => number;
  readonly datum_from_json: (a: number, b: number) => number;
  readonly datum_new_data_hash: (a: number) => number;
  readonly datum_new_data: (a: number) => number;
  readonly datum_kind: (a: number) => number;
  readonly datum_as_data_hash: (a: number) => number;
  readonly datum_as_data: (a: number) => number;
  readonly encode_json_str_to_plutus_datum: (
    a: number,
    b: number,
    c: number,
  ) => number;
  readonly decode_plutus_datum_to_json_str: (
    a: number,
    b: number,
    c: number,
  ) => void;
  readonly plutusscripts_len: (a: number) => number;
  readonly costmodel_len: (a: number) => number;
  readonly languages_len: (a: number) => number;
  readonly plutusmap_len: (a: number) => number;
  readonly plutuslist_len: (a: number) => number;
  readonly redeemers_len: (a: number) => number;
  readonly strings_len: (a: number) => number;
  readonly exunits_mem: (a: number) => number;
  readonly redeemer_index: (a: number) => number;
  readonly redeemers_new: () => number;
  readonly strings_new: () => number;
  readonly scriptref_to_json: (a: number, b: number) => void;
  readonly __wbindgen_malloc: (a: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number) => number;
  readonly __wbindgen_export_2: WebAssembly.Table;
  readonly _dyn_core__ops__function__FnMut__A____Output___R_as_wasm_bindgen__closure__WasmClosure___describe__invoke__hf552e89fb1b782c2:
    (a: number, b: number, c: number) => void;
  readonly __wbindgen_add_to_stack_pointer: (a: number) => number;
  readonly __wbindgen_free: (a: number, b: number) => void;
  readonly __wbindgen_exn_store: (a: number) => void;
  readonly wasm_bindgen__convert__closures__invoke2_mut__h343db6eee225c129: (
    a: number,
    b: number,
    c: number,
    d: number,
  ) => void;
}

/**
 * If `module_or_path` is {RequestInfo} or {URL}, makes a request and
 * for everything else, calls `WebAssembly.instantiate` directly.
 *
 * @param {InitInput | Promise<InitInput>} module_or_path
 *
 * @returns {Promise<InitOutput>}
 */
export default function init(
  module_or_path?: InitInput | Promise<InitInput>,
): Promise<InitOutput>;

export type AddressJSON = string;
export type AssetNameJSON = string;
export type AssetNamesJSON = string[];
export interface AssetsJSON {
  [k: string]: string;
}
export interface AuxiliaryDataJSON {
  metadata?: {
    [k: string]: string;
  } | null;
  native_scripts?: NativeScriptsJSON | null;
  plutus_scripts?: PlutusScriptsJSON | null;
  plutus_v2_scripts?: PlutusScriptsJSON | null;
}
export type AuxiliaryDataHashJSON = string;
export interface AuxiliaryDataSetJSON {
  [k: string]: AuxiliaryDataJSON;
}
export type BigIntJSON = string;
export type BigNumJSON = string;
export interface BlockJSON {
  auxiliary_data_set: {
    [k: string]: AuxiliaryDataJSON;
  };
  header: HeaderJSON;
  invalid_transactions: TransactionIndexes;
  transaction_bodies: TransactionBodiesJSON;
  transaction_witness_sets: TransactionWitnessSetsJSON;
}
export type BlockHashJSON = string;
export interface BootstrapWitnessJSON {
  attributes: number[];
  chain_code: number[];
  signature: string;
  vkey: VkeyJSON;
}
export type BootstrapWitnessesJSON = BootstrapWitnessJSON[];
export type CertificateJSON = CertificateEnumJSON;
export type CertificateEnumJSON =
  | {
    StakeRegistrationJSON: StakeRegistration;
  }
  | {
    StakeDeregistrationJSON: StakeDeregistration;
  }
  | {
    StakeDelegationJSON: StakeDelegation;
  }
  | {
    PoolRegistrationJSON: PoolRegistration;
  }
  | {
    PoolRetirementJSON: PoolRetirement;
  }
  | {
    GenesisKeyDelegationJSON: GenesisKeyDelegation;
  }
  | {
    MoveInstantaneousRewardsCertJSON: MoveInstantaneousRewardsCert;
  };
export type CertificatesJSON = CertificateJSON[];
export interface ConstrPlutusDataJSON {
  alternative: string;
  data: PlutusListJSON;
}
export type CostModelJSON = string[];
export interface CostmdlsJSON {
  [k: string]: CostModelJSON;
}
export type DNSRecordAorAAAAJSON = string;
export type DNSRecordSRVJSON = string;
export type DataJSON = PlutusDataJSON;
export type DataHashJSON = string;
export type DatumJSON = DatumEnumJSON;
export type DatumEnumJSON =
  | {
    Hash: string;
  }
  | {
    DataJSON: Data;
  };
export type Ed25519KeyHashJSON = string;
export type Ed25519KeyHashesJSON = string[];
export type Ed25519SignatureJSON = string;
export interface ExUnitPricesJSON {
  mem_price: UnitIntervalJSON;
  step_price: UnitIntervalJSON;
}
export interface ExUnitsJSON {
  mem: string;
  steps: string;
}
export interface GeneralTransactionMetadataJSON {
  [k: string]: string;
}
export type GenesisDelegateHashJSON = string;
export type GenesisHashJSON = string;
export type GenesisHashesJSON = string[];
export interface GenesisKeyDelegationJSON {
  genesis_delegate_hash: string;
  genesishash: string;
  vrf_keyhash: string;
}
export interface HeaderJSON {
  body_signature: string;
  header_body: HeaderBodyJSON;
}
export interface HeaderBodyJSON {
  block_body_hash: string;
  block_body_size: number;
  block_number: number;
  issuer_vkey: VkeyJSON;
  leader_vrf: VRFCertJSON;
  nonce_vrf: VRFCertJSON;
  operational_cert: OperationalCertJSON;
  prev_hash?: string | null;
  protocol_version: ProtocolVersionJSON;
  slot: string;
  vrf_vkey: VRFVKeyJSON;
}
export type IntJSON = string;
export type Ipv4JSON = [number, number, number, number];
export type Ipv6JSON = [
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
  number,
];
export type KESVKeyJSON = string;
export type LanguageJSON = LanguageKindJSON;
export type LanguageKindJSON = "PlutusV1" | "PlutusV2";
export type LanguagesJSON = LanguageJSON[];
export type MIREnumJSON =
  | {
    ToOtherPot: string;
  }
  | {
    ToStakeCredentials: {
      [k: string]: ProtocolParamUpdateJSON;
    };
  };
export type MIRPotJSON = "Reserves" | "Treasury";
export interface MIRToStakeCredentialsJSON {
  [k: string]: ProtocolParamUpdateJSON;
}
export interface MintJSON {
  [k: string]: MintAssetsJSON;
}
export interface MintAssetsJSON {
  [k: string]: string;
}
export interface MoveInstantaneousRewardJSON {
  pot: MIRPotJSON;
  variant: MIREnumJSON;
}
export interface MoveInstantaneousRewardsCertJSON {
  move_instantaneous_reward: MoveInstantaneousRewardJSON;
}
export interface MultiAssetJSON {
  [k: string]: AssetsJSON;
}
export interface MultiHostNameJSON {
  dns_name: DNSRecordSRVJSON;
}
export type NativeScriptJSON = NativeScript1JSON;
export type NativeScript1JSON =
  | {
    ScriptPubkeyJSON: ScriptPubkey;
  }
  | {
    ScriptAllJSON: ScriptAll;
  }
  | {
    ScriptAnyJSON: ScriptAny;
  }
  | {
    ScriptNOfKJSON: ScriptNOfK;
  }
  | {
    TimelockStartJSON: TimelockStart;
  }
  | {
    TimelockExpiryJSON: TimelockExpiry;
  };
export type NativeScriptsJSON = NativeScriptJSON[];
export type NetworkIdJSON = NetworkIdKindJSON;
export type NetworkIdKindJSON = "Testnet" | "Mainnet";
export interface NonceJSON {
  hash?:
    | [
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
      number,
    ]
    | null;
}
export interface OperationalCertJSON {
  hot_vkey: string;
  kes_period: number;
  sequence_number: number;
  sigma: string;
}
export interface PlutusDataJSON {
  datum: PlutusDataEnumJSON;
  original_bytes?: number[] | null;
}
export type PlutusDataEnumJSON =
  | {
    ConstrPlutusDataJSON: ConstrPlutusData;
  }
  | {
    Map: PlutusMapJSON;
  }
  | {
    List: PlutusListJSON;
  }
  | {
    Integer: string;
  }
  | {
    Bytes: number[];
  };
export interface PlutusListJSON {
  definite_encoding?: boolean | null;
  elems: PlutusDataJSON[];
}
export type PlutusMapJSON = [PlutusDataJSON, PlutusData][];
export type PlutusScriptJSON = string;
export type PlutusScriptsJSON = string[];
export interface PoolMetadataJSON {
  pool_metadata_hash: string;
  url: URLJSON;
}
export type PoolMetadataHashJSON = string;
export interface PoolParamsJSON {
  cost: string;
  margin: UnitIntervalJSON;
  operator: string;
  pledge: string;
  pool_metadata?: PoolMetadataJSON | null;
  pool_owners: Ed25519KeyHashesJSON;
  relays: RelaysJSON;
  reward_account: string;
  vrf_keyhash: string;
}
export interface PoolRegistrationJSON {
  /**
   * We need this to figure out if someone registers a pool or only updates it. So that we know if we need to add the pool deposit or not.
   */
  is_update?: boolean | null;
  pool_params: PoolParamsJSON;
}
export interface PoolRetirementJSON {
  epoch: number;
  pool_keyhash: string;
}
export interface ProposedProtocolParameterUpdatesJSON {
  [k: string]: ProtocolParamUpdateJSON;
}
export interface ProtocolParamUpdateJSON {
  ada_per_utxo_byte?: string | null;
  collateral_percentage?: number | null;
  cost_models?: CostmdlsJSON | null;
  d?: UnitIntervalJSON | null;
  execution_costs?: ExUnitPricesJSON | null;
  expansion_rate?: UnitIntervalJSON | null;
  extra_entropy?: NonceJSON | null;
  key_deposit?: string | null;
  max_block_body_size?: number | null;
  max_block_ex_units?: ExUnitsJSON | null;
  max_block_header_size?: number | null;
  max_collateral_inputs?: number | null;
  max_epoch?: number | null;
  max_tx_ex_units?: ExUnitsJSON | null;
  max_tx_size?: number | null;
  max_value_size?: number | null;
  min_pool_cost?: string | null;
  minfee_a?: string | null;
  minfee_b?: string | null;
  n_opt?: number | null;
  pool_deposit?: string | null;
  pool_pledge_influence?: UnitIntervalJSON | null;
  protocol_version?: ProtocolVersionJSON | null;
  treasury_growth_rate?: UnitIntervalJSON | null;
}
export interface ProtocolVersionJSON {
  major: number;
  minor: number;
}
export type PublicKeyJSON = string;
export interface RedeemerJSON {
  data: PlutusDataJSON;
  ex_units: ExUnitsJSON;
  index: string;
  tag: RedeemerTagJSON;
}
export type RedeemerTagJSON = RedeemerTagKindJSON;
export type RedeemerTagKindJSON = "Spend" | "MintJSON" | "Cert" | "Reward";
export type RedeemersJSON = RedeemerJSON[];
export type RelayJSON = RelayEnumJSON;
export type RelayEnumJSON =
  | {
    SingleHostAddrJSON: SingleHostAddr;
  }
  | {
    SingleHostNameJSON: SingleHostName;
  }
  | {
    MultiHostNameJSON: MultiHostName;
  };
export type RelaysJSON = RelayJSON[];
export type RewardAddressJSON = string;
export type RewardAddressesJSON = string[];
export type ScriptJSON = ScriptEnumJSON;
export type ScriptEnumJSON =
  | {
    NativeScriptJSON: NativeScript;
  }
  | {
    PlutusScriptV1: string;
  }
  | {
    PlutusScriptV2: string;
  };
export interface ScriptAllJSON {
  native_scripts: NativeScriptsJSON;
}
export interface ScriptAnyJSON {
  native_scripts: NativeScriptsJSON;
}
export type ScriptDataHashJSON = string;
export type ScriptHashJSON = string;
export type ScriptHashesJSON = string[];
export interface ScriptNOfKJSON {
  n: number;
  native_scripts: NativeScriptsJSON;
}
export interface ScriptPubkeyJSON {
  addr_keyhash: string;
}
export type ScriptRefJSON = ScriptJSON;
export type ScriptWitnessJSON = ScriptWitnessEnumJSON;
export type ScriptWitnessEnumJSON =
  | {
    NativeWitness: NativeScriptJSON;
  }
  | {
    PlutusWitness: PlutusWitness;
  };
export interface SingleHostAddrJSON {
  ipv4?: Ipv4JSON | null;
  ipv6?: Ipv6JSON | null;
  port?: number | null;
}
export interface SingleHostNameJSON {
  dns_name: DNSRecordAorAAAAJSON;
  port?: number | null;
}
export type StakeCredTypeJSON =
  | {
    Key: string;
  }
  | {
    ScriptJSON: string;
  };
export type StakeCredentialJSON = StakeCredTypeJSON;
export type StakeCredentialsJSON = StakeCredTypeJSON[];
export interface StakeDelegationJSON {
  pool_keyhash: string;
  stake_credential: StakeCredTypeJSON;
}
export interface StakeDeregistrationJSON {
  stake_credential: StakeCredTypeJSON;
}
export interface StakeRegistrationJSON {
  stake_credential: StakeCredTypeJSON;
}
export interface TimelockExpiryJSON {
  slot: string;
}
export interface TimelockStartJSON {
  slot: string;
}
export interface TransactionJSON {
  auxiliary_data?: AuxiliaryDataJSON | null;
  body: TransactionBodyJSON;
  is_valid: boolean;
  witness_set: TransactionWitnessSetJSON;
}
export type TransactionBodiesJSON = TransactionBodyJSON[];
export interface TransactionBodyJSON {
  auxiliary_data_hash?: string | null;
  certs?: CertificatesJSON | null;
  collateral?: TransactionInputsJSON | null;
  collateral_return?: TransactionOutputJSON | null;
  fee: string;
  inputs: TransactionInputsJSON;
  mint?: MintJSON | null;
  network_id?: NetworkIdJSON | null;
  original_bytes?: number[] | null;
  outputs: TransactionOutputsJSON;
  reference_inputs?: TransactionInputsJSON | null;
  required_signers?: Ed25519KeyHashesJSON | null;
  script_data_hash?: string | null;
  total_collateral?: string | null;
  ttl?: string | null;
  update?: UpdateJSON | null;
  validity_start_interval?: string | null;
  withdrawals?: {
    [k: string]: ProtocolParamUpdateJSON;
  } | null;
}
export type TransactionHashJSON = string;
export interface TransactionInputJSON {
  index: string;
  transaction_id: string;
}
export type TransactionInputsJSON = TransactionInputJSON[];
export type TransactionMetadatumJSON = string;
export interface TransactionOutputJSON {
  address: string;
  amount: ValueJSON;
  datum?: DatumJSON | null;
  script_ref?: ScriptJSON | null;
}
export type TransactionOutputsJSON = TransactionOutputJSON[];
export interface TransactionWitnessSetJSON {
  bootstraps?: BootstrapWitnessesJSON | null;
  native_scripts?: NativeScriptsJSON | null;
  plutus_data?: PlutusListJSON | null;
  plutus_scripts?: PlutusScriptsJSON | null;
  plutus_v2_scripts?: PlutusScriptsJSON | null;
  redeemers?: RedeemersJSON | null;
  vkeys?: VkeywitnessesJSON | null;
}
export type TransactionWitnessSetsJSON = TransactionWitnessSetJSON[];
export type URLJSON = string;
export interface UnitIntervalJSON {
  denominator: string;
  numerator: string;
}
export interface UpdateJSON {
  epoch: number;
  proposed_protocol_parameter_updates: {
    [k: string]: ProtocolParamUpdateJSON;
  };
}
export interface VRFCertJSON {
  output: number[];
  proof: number[];
}
export type VRFKeyHashJSON = string;
export type VRFVKeyJSON = number[];
export interface ValueJSON {
  coin: string;
  multiasset?: MultiAssetJSON | null;
}
export type VkeyJSON = string;
export interface VkeywitnessJSON {
  signature: string;
  vkey: VkeyJSON;
}
export type VkeywitnessesJSON = VkeywitnessJSON[];
export interface WithdrawalsJSON {
  [k: string]: ProtocolParamUpdateJSON;
}
