/* tslint:disable */
/* eslint-disable */
export enum AlgorithmId {
  /**
   * r" EdDSA (Pure EdDSA, not HashedEdDSA) - the algorithm used for Cardano addresses
   */
  EdDSA = 0,
  /**
   * r" ChaCha20/Poly1305 w/ 256-bit key, 128-bit tag
   */
  ChaCha20Poly1305 = 1,
}
export enum CBORSpecialType {
  Bool = 0,
  Float = 1,
  Unassigned = 2,
  Break = 3,
  Undefined = 4,
  Null = 5,
}
export enum CBORValueKind {
  Int = 0,
  Bytes = 1,
  Text = 2,
  Array = 3,
  Object = 4,
  TaggedCBOR = 5,
  Special = 6,
}
export enum CurveType {
  P256 = 0,
  P384 = 1,
  P521 = 2,
  X25519 = 3,
  X448 = 4,
  Ed25519 = 5,
  Ed448 = 6,
}
export enum ECKey {
  CRV = 0,
  X = 1,
  Y = 2,
  D = 3,
}
export enum KeyOperation {
  Sign = 0,
  Verify = 1,
  Encrypt = 2,
  Decrypt = 3,
  WrapKey = 4,
  UnwrapKey = 5,
  DeriveKey = 6,
  DeriveBits = 7,
}
export enum KeyType {
  /**
   * r" octet key pair
   */
  OKP = 0,
  /**
   * r" 2-coord EC
   */
  EC2 = 1,
  Symmetric = 2,
}
export enum LabelKind {
  Int = 0,
  Text = 1,
}
export enum SigContext {
  Signature = 0,
  Signature1 = 1,
  CounterSignature = 2,
}
export enum SignedMessageKind {
  COSESIGN = 0,
  COSESIGN1 = 1,
}
export class BigNum {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): BigNum;
  static from_str(string: string): BigNum;
  to_str(): string;
  checked_mul(other: BigNum): BigNum;
  checked_add(other: BigNum): BigNum;
  checked_sub(other: BigNum): BigNum;
}
export class CBORArray {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): CBORArray;
  static new(): CBORArray;
  len(): number;
  get(index: number): CBORValue;
  add(elem: CBORValue): void;
  set_definite_encoding(use_definite: boolean): void;
  is_definite(): boolean;
}
export class CBORObject {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): CBORObject;
  static new(): CBORObject;
  len(): number;
  insert(key: CBORValue, value: CBORValue): CBORValue | undefined;
  get(key: CBORValue): CBORValue | undefined;
  keys(): CBORArray;
  set_definite_encoding(use_definite: boolean): void;
  is_definite(): boolean;
}
export class CBORSpecial {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): CBORSpecial;
  static new_bool(b: boolean): CBORSpecial;
  static new_unassigned(u: number): CBORSpecial;
  static new_break(): CBORSpecial;
  static new_null(): CBORSpecial;
  static new_undefined(): CBORSpecial;
  kind(): CBORSpecialType;
  as_bool(): boolean | undefined;
  as_float(): number | undefined;
  as_unassigned(): number | undefined;
}
export class CBORValue {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): CBORValue;
  static new_int(int: Int): CBORValue;
  static new_bytes(bytes: Uint8Array): CBORValue;
  static new_text(text: string): CBORValue;
  static new_array(arr: CBORArray): CBORValue;
  static new_object(obj: CBORObject): CBORValue;
  static new_tagged(tagged: TaggedCBOR): CBORValue;
  static new_special(special: CBORSpecial): CBORValue;
  static from_label(label: Label): CBORValue;
  kind(): CBORValueKind;
  as_int(): Int | undefined;
  as_bytes(): Uint8Array | undefined;
  as_text(): string | undefined;
  as_array(): CBORArray | undefined;
  as_object(): CBORObject | undefined;
  as_tagged(): TaggedCBOR | undefined;
  as_special(): CBORSpecial | undefined;
}
export class COSEEncrypt {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): COSEEncrypt;
  headers(): Headers;
  ciphertext(): Uint8Array | undefined;
  recipients(): COSERecipients;
  static new(headers: Headers, ciphertext: Uint8Array | null | undefined, recipients: COSERecipients): COSEEncrypt;
}
export class COSEEncrypt0 {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): COSEEncrypt0;
  headers(): Headers;
  ciphertext(): Uint8Array | undefined;
  static new(headers: Headers, ciphertext?: Uint8Array | null): COSEEncrypt0;
}
export class COSEKey {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): COSEKey;
  set_key_type(key_type: Label): void;
  key_type(): Label;
  set_key_id(key_id: Uint8Array): void;
  key_id(): Uint8Array | undefined;
  set_algorithm_id(algorithm_id: Label): void;
  algorithm_id(): Label | undefined;
  set_key_ops(key_ops: Labels): void;
  key_ops(): Labels | undefined;
  set_base_init_vector(base_init_vector: Uint8Array): void;
  base_init_vector(): Uint8Array | undefined;
  header(label: Label): CBORValue | undefined;
  set_header(label: Label, value: CBORValue): void;
  static new(key_type: Label): COSEKey;
}
export class COSERecipient {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): COSERecipient;
  headers(): Headers;
  ciphertext(): Uint8Array | undefined;
  static new(headers: Headers, ciphertext?: Uint8Array | null): COSERecipient;
}
export class COSERecipients {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): COSERecipients;
  static new(): COSERecipients;
  len(): number;
  get(index: number): COSERecipient;
  add(elem: COSERecipient): void;
}
export class COSESign {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): COSESign;
  headers(): Headers;
  payload(): Uint8Array | undefined;
  signatures(): COSESignatures;
  static new(headers: Headers, payload: Uint8Array | null | undefined, signatures: COSESignatures): COSESign;
}
export class COSESign1 {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): COSESign1;
  headers(): Headers;
  payload(): Uint8Array | undefined;
  signature(): Uint8Array;
  /**
   * For verifying, we will want to reverse-construct this SigStructure to check the signature against
   * # Arguments
   * * `external_aad` - External application data - see RFC 8152 section 4.3. Set to None if not using this.
   */
  signed_data(external_aad?: Uint8Array | null, external_payload?: Uint8Array | null): SigStructure;
  static new(headers: Headers, payload: Uint8Array | null | undefined, signature: Uint8Array): COSESign1;
}
export class COSESign1Builder {
  private constructor();
  free(): void;
  static new(headers: Headers, payload: Uint8Array, is_payload_external: boolean): COSESign1Builder;
  hash_payload(): void;
  set_external_aad(external_aad: Uint8Array): void;
  make_data_to_sign(): SigStructure;
  build(signed_sig_structure: Uint8Array): COSESign1;
}
export class COSESignBuilder {
  private constructor();
  free(): void;
  static new(headers: Headers, payload: Uint8Array, is_payload_external: boolean): COSESignBuilder;
  hash_payload(): void;
  set_external_aad(external_aad: Uint8Array): void;
  make_data_to_sign(): SigStructure;
  build(signed_sig_structure: COSESignatures): COSESign;
}
export class COSESignature {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): COSESignature;
  headers(): Headers;
  signature(): Uint8Array;
  static new(headers: Headers, signature: Uint8Array): COSESignature;
}
export class COSESignatures {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): COSESignatures;
  static new(): COSESignatures;
  len(): number;
  get(index: number): COSESignature;
  add(elem: COSESignature): void;
}
export class CounterSignature {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): CounterSignature;
  static new_single(cose_signature: COSESignature): CounterSignature;
  static new_multi(cose_signatures: COSESignatures): CounterSignature;
  signatures(): COSESignatures;
}
export class EdDSA25519Key {
  private constructor();
  free(): void;
  static new(pubkey_bytes: Uint8Array): EdDSA25519Key;
  set_private_key(private_key_bytes: Uint8Array): void;
  is_for_signing(): void;
  is_for_verifying(): void;
  build(): COSEKey;
}
export class HeaderMap {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): HeaderMap;
  set_algorithm_id(algorithm_id: Label): void;
  algorithm_id(): Label | undefined;
  set_criticality(criticality: Labels): void;
  criticality(): Labels | undefined;
  set_content_type(content_type: Label): void;
  content_type(): Label | undefined;
  set_key_id(key_id: Uint8Array): void;
  key_id(): Uint8Array | undefined;
  set_init_vector(init_vector: Uint8Array): void;
  init_vector(): Uint8Array | undefined;
  set_partial_init_vector(partial_init_vector: Uint8Array): void;
  partial_init_vector(): Uint8Array | undefined;
  set_counter_signature(counter_signature: CounterSignature): void;
  counter_signature(): CounterSignature | undefined;
  header(label: Label): CBORValue | undefined;
  set_header(label: Label, value: CBORValue): void;
  keys(): Labels;
  static new(): HeaderMap;
}
export class Headers {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): Headers;
  protected(): ProtectedHeaderMap;
  unprotected(): HeaderMap;
  static new(protected_: ProtectedHeaderMap, unprotected_: HeaderMap): Headers;
}
export class Int {
  private constructor();
  free(): void;
  static new(x: BigNum): Int;
  static new_negative(x: BigNum): Int;
  static new_i32(x: number): Int;
  is_positive(): boolean;
  as_positive(): BigNum | undefined;
  as_negative(): BigNum | undefined;
  as_i32(): number | undefined;
}
export class Label {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): Label;
  static new_int(int: Int): Label;
  static new_text(text: string): Label;
  kind(): LabelKind;
  as_int(): Int | undefined;
  as_text(): string | undefined;
  static from_algorithm_id(id: AlgorithmId): Label;
  static from_key_type(key_type: KeyType): Label;
  static from_ec_key(ec_key: ECKey): Label;
  static from_curve_type(curve_type: CurveType): Label;
  static from_key_operation(key_op: KeyOperation): Label;
}
export class Labels {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): Labels;
  static new(): Labels;
  len(): number;
  get(index: number): Label;
  add(elem: Label): void;
}
export class PasswordEncryption {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): PasswordEncryption;
  static new(data: COSEEncrypt0): PasswordEncryption;
}
export class ProtectedHeaderMap {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): ProtectedHeaderMap;
  static new_empty(): ProtectedHeaderMap;
  static new(header_map: HeaderMap): ProtectedHeaderMap;
  deserialized_headers(): HeaderMap;
}
export class PubKeyEncryption {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): PubKeyEncryption;
  static new(data: COSEEncrypt): PubKeyEncryption;
}
export class SigStructure {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): SigStructure;
  context(): SigContext;
  body_protected(): ProtectedHeaderMap;
  sign_protected(): ProtectedHeaderMap | undefined;
  external_aad(): Uint8Array;
  payload(): Uint8Array;
  set_sign_protected(sign_protected: ProtectedHeaderMap): void;
  static new(context: SigContext, body_protected: ProtectedHeaderMap, external_aad: Uint8Array, payload: Uint8Array): SigStructure;
}
export class SignedMessage {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): SignedMessage;
  static new_cose_sign(cose_sign: COSESign): SignedMessage;
  static new_cose_sign1(cose_sign1: COSESign1): SignedMessage;
  static from_user_facing_encoding(s: string): SignedMessage;
  to_user_facing_encoding(): string;
  kind(): SignedMessageKind;
  as_cose_sign(): COSESign | undefined;
  as_cose_sign1(): COSESign1 | undefined;
}
export class TaggedCBOR {
  private constructor();
  free(): void;
  to_bytes(): Uint8Array;
  static from_bytes(bytes: Uint8Array): TaggedCBOR;
  tag(): BigNum;
  value(): CBORValue;
  static new(tag: BigNum, value: CBORValue): TaggedCBOR;
}
