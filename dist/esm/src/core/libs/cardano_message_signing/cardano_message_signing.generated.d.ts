/**
 * Decompression callback
 *
 * @callback DecompressCallback
 * @param {Uint8Array} compressed
 * @return {Uint8Array} decompressed
 */
/**
 * Options for instantiating a Wasm instance.
 * @typedef {Object} InstantiateOptions
 * @property {URL=} url - Optional url to the Wasm file to instantiate.
 * @property {DecompressCallback=} decompress - Callback to decompress the
 * raw Wasm file bytes before instantiating.
 */
/** Instantiates an instance of the Wasm module returning its functions.
 * @remarks It is safe to call this multiple times and once successfully
 * loaded it will always return a reference to the same object.
 * @param {InstantiateOptions=} opts
 */
export function instantiate(opts?: InstantiateOptions | undefined): Promise<{
    BigNum: typeof BigNum;
    CBORArray: typeof CBORArray;
    CBORObject: typeof CBORObject;
    CBORSpecial: typeof CBORSpecial;
    CBORValue: typeof CBORValue;
    COSEEncrypt: typeof COSEEncrypt;
    COSEEncrypt0: typeof COSEEncrypt0;
    COSEKey: typeof COSEKey;
    COSERecipient: typeof COSERecipient;
    COSERecipients: typeof COSERecipients;
    COSESign: typeof COSESign;
    COSESign1: typeof COSESign1;
    COSESign1Builder: typeof COSESign1Builder;
    COSESignBuilder: typeof COSESignBuilder;
    COSESignature: typeof COSESignature;
    COSESignatures: typeof COSESignatures;
    CounterSignature: typeof CounterSignature;
    EdDSA25519Key: typeof EdDSA25519Key;
    HeaderMap: typeof HeaderMap;
    Headers: typeof Headers;
    Int: typeof Int;
    Label: typeof Label;
    Labels: typeof Labels;
    PasswordEncryption: typeof PasswordEncryption;
    ProtectedHeaderMap: typeof ProtectedHeaderMap;
    PubKeyEncryption: typeof PubKeyEncryption;
    SigStructure: typeof SigStructure;
    SignedMessage: typeof SignedMessage;
    TaggedCBOR: typeof TaggedCBOR;
}>;
/** Instantiates an instance of the Wasm module along with its exports.
 * @remarks It is safe to call this multiple times and once successfully
 * loaded it will always return a reference to the same object.
 * @param {InstantiateOptions=} opts
 * @returns {Promise<{
 *   instance: WebAssembly.Instance;
 *   exports: { BigNum : typeof BigNum ; CBORArray : typeof CBORArray ; CBORObject : typeof CBORObject ; CBORSpecial : typeof CBORSpecial ; CBORValue : typeof CBORValue ; COSEEncrypt : typeof COSEEncrypt ; COSEEncrypt0 : typeof COSEEncrypt0 ; COSEKey : typeof COSEKey ; COSERecipient : typeof COSERecipient ; COSERecipients : typeof COSERecipients ; COSESign : typeof COSESign ; COSESign1 : typeof COSESign1 ; COSESign1Builder : typeof COSESign1Builder ; COSESignBuilder : typeof COSESignBuilder ; COSESignature : typeof COSESignature ; COSESignatures : typeof COSESignatures ; CounterSignature : typeof CounterSignature ; EdDSA25519Key : typeof EdDSA25519Key ; HeaderMap : typeof HeaderMap ; Headers : typeof Headers ; Int : typeof Int ; Label : typeof Label ; Labels : typeof Labels ; PasswordEncryption : typeof PasswordEncryption ; ProtectedHeaderMap : typeof ProtectedHeaderMap ; PubKeyEncryption : typeof PubKeyEncryption ; SigStructure : typeof SigStructure ; SignedMessage : typeof SignedMessage ; TaggedCBOR : typeof TaggedCBOR  }
 * }>}
 */
export function instantiateWithInstance(opts?: InstantiateOptions | undefined): Promise<{
    instance: WebAssembly.Instance;
    exports: {
        BigNum: typeof BigNum;
        CBORArray: typeof CBORArray;
        CBORObject: typeof CBORObject;
        CBORSpecial: typeof CBORSpecial;
        CBORValue: typeof CBORValue;
        COSEEncrypt: typeof COSEEncrypt;
        COSEEncrypt0: typeof COSEEncrypt0;
        COSEKey: typeof COSEKey;
        COSERecipient: typeof COSERecipient;
        COSERecipients: typeof COSERecipients;
        COSESign: typeof COSESign;
        COSESign1: typeof COSESign1;
        COSESign1Builder: typeof COSESign1Builder;
        COSESignBuilder: typeof COSESignBuilder;
        COSESignature: typeof COSESignature;
        COSESignatures: typeof COSESignatures;
        CounterSignature: typeof CounterSignature;
        EdDSA25519Key: typeof EdDSA25519Key;
        HeaderMap: typeof HeaderMap;
        Headers: typeof Headers;
        Int: typeof Int;
        Label: typeof Label;
        Labels: typeof Labels;
        PasswordEncryption: typeof PasswordEncryption;
        ProtectedHeaderMap: typeof ProtectedHeaderMap;
        PubKeyEncryption: typeof PubKeyEncryption;
        SigStructure: typeof SigStructure;
        SignedMessage: typeof SignedMessage;
        TaggedCBOR: typeof TaggedCBOR;
    };
}>;
/** Gets if the Wasm module has been instantiated. */
export function isInstantiated(): boolean;
/** */
export const AlgorithmId: Readonly<{
    /**
     * r" EdDSA (Pure EdDSA, not HashedEdDSA) - the algorithm used for Cardano addresses
     */
    EdDSA: 0;
    "0": "EdDSA";
    /**
     * r" ChaCha20/Poly1305 w/ 256-bit key, 128-bit tag
     */
    ChaCha20Poly1305: 1;
    "1": "ChaCha20Poly1305";
}>;
/** */
export const KeyType: Readonly<{
    /**
     * r" octet key pair
     */
    OKP: 0;
    "0": "OKP";
    /**
     * r" 2-coord EC
     */
    EC2: 1;
    "1": "EC2";
    Symmetric: 2;
    "2": "Symmetric";
}>;
/** */
export const ECKey: Readonly<{
    CRV: 0;
    "0": "CRV";
    X: 1;
    "1": "X";
    Y: 2;
    "2": "Y";
    D: 3;
    "3": "D";
}>;
/** */
export const CurveType: Readonly<{
    P256: 0;
    "0": "P256";
    P384: 1;
    "1": "P384";
    P521: 2;
    "2": "P521";
    X25519: 3;
    "3": "X25519";
    X448: 4;
    "4": "X448";
    Ed25519: 5;
    "5": "Ed25519";
    Ed448: 6;
    "6": "Ed448";
}>;
/** */
export const KeyOperation: Readonly<{
    Sign: 0;
    "0": "Sign";
    Verify: 1;
    "1": "Verify";
    Encrypt: 2;
    "2": "Encrypt";
    Decrypt: 3;
    "3": "Decrypt";
    WrapKey: 4;
    "4": "WrapKey";
    UnwrapKey: 5;
    "5": "UnwrapKey";
    DeriveKey: 6;
    "6": "DeriveKey";
    DeriveBits: 7;
    "7": "DeriveBits";
}>;
/** */
export const CBORSpecialType: Readonly<{
    Bool: 0;
    "0": "Bool";
    Float: 1;
    "1": "Float";
    Unassigned: 2;
    "2": "Unassigned";
    Break: 3;
    "3": "Break";
    Undefined: 4;
    "4": "Undefined";
    Null: 5;
    "5": "Null";
}>;
/** */
export const CBORValueKind: Readonly<{
    Int: 0;
    "0": "Int";
    Bytes: 1;
    "1": "Bytes";
    Text: 2;
    "2": "Text";
    Array: 3;
    "3": "Array";
    Object: 4;
    "4": "Object";
    TaggedCBOR: 5;
    "5": "TaggedCBOR";
    Special: 6;
    "6": "Special";
}>;
/** */
export const LabelKind: Readonly<{
    Int: 0;
    "0": "Int";
    Text: 1;
    "1": "Text";
}>;
/** */
export const SignedMessageKind: Readonly<{
    COSESIGN: 0;
    "0": "COSESIGN";
    COSESIGN1: 1;
    "1": "COSESIGN1";
}>;
/** */
export const SigContext: Readonly<{
    Signature: 0;
    "0": "Signature";
    Signature1: 1;
    "1": "Signature1";
    CounterSignature: 2;
    "2": "CounterSignature";
}>;
/** */
export class BigNum {
    static __wrap(ptr: any): any;
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
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_str(): string;
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
}
/** */
export class CBORArray {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {CBORArray}
     */
    static from_bytes(bytes: Uint8Array): CBORArray;
    /**
     * @returns {CBORArray}
     */
    static new(): CBORArray;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {CBORValue}
     */
    get(index: number): CBORValue;
    /**
     * @param {CBORValue} elem
     */
    add(elem: CBORValue): void;
    /**
     * @param {boolean} use_definite
     */
    set_definite_encoding(use_definite: boolean): void;
    /**
     * @returns {boolean}
     */
    is_definite(): boolean;
}
/** */
export class CBORObject {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {CBORObject}
     */
    static from_bytes(bytes: Uint8Array): CBORObject;
    /**
     * @returns {CBORObject}
     */
    static new(): CBORObject;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {CBORValue} key
     * @param {CBORValue} value
     * @returns {CBORValue | undefined}
     */
    insert(key: CBORValue, value: CBORValue): CBORValue | undefined;
    /**
     * @param {CBORValue} key
     * @returns {CBORValue | undefined}
     */
    get(key: CBORValue): CBORValue | undefined;
    /**
     * @returns {CBORArray}
     */
    keys(): CBORArray;
    /**
     * @param {boolean} use_definite
     */
    set_definite_encoding(use_definite: boolean): void;
    /**
     * @returns {boolean}
     */
    is_definite(): boolean;
}
/** */
export class CBORSpecial {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {CBORSpecial}
     */
    static from_bytes(bytes: Uint8Array): CBORSpecial;
    /**
     * @param {boolean} b
     * @returns {CBORSpecial}
     */
    static new_bool(b: boolean): CBORSpecial;
    /**
     * @param {number} u
     * @returns {CBORSpecial}
     */
    static new_unassigned(u: number): CBORSpecial;
    /**
     * @returns {CBORSpecial}
     */
    static new_break(): CBORSpecial;
    /**
     * @returns {CBORSpecial}
     */
    static new_null(): CBORSpecial;
    /**
     * @returns {CBORSpecial}
     */
    static new_undefined(): CBORSpecial;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {boolean | undefined}
     */
    as_bool(): boolean | undefined;
    /**
     * @returns {number | undefined}
     */
    as_float(): number | undefined;
    /**
     * @returns {number | undefined}
     */
    as_unassigned(): number | undefined;
}
/** */
export class CBORValue {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {CBORValue}
     */
    static from_bytes(bytes: Uint8Array): CBORValue;
    /**
     * @param {Int} int
     * @returns {CBORValue}
     */
    static new_int(int: Int): CBORValue;
    /**
     * @param {Uint8Array} bytes
     * @returns {CBORValue}
     */
    static new_bytes(bytes: Uint8Array): CBORValue;
    /**
     * @param {string} text
     * @returns {CBORValue}
     */
    static new_text(text: string): CBORValue;
    /**
     * @param {CBORArray} arr
     * @returns {CBORValue}
     */
    static new_array(arr: CBORArray): CBORValue;
    /**
     * @param {CBORObject} obj
     * @returns {CBORValue}
     */
    static new_object(obj: CBORObject): CBORValue;
    /**
     * @param {TaggedCBOR} tagged
     * @returns {CBORValue}
     */
    static new_tagged(tagged: TaggedCBOR): CBORValue;
    /**
     * @param {CBORSpecial} special
     * @returns {CBORValue}
     */
    static new_special(special: CBORSpecial): CBORValue;
    /**
     * @param {Label} label
     * @returns {CBORValue}
     */
    static from_label(label: Label): CBORValue;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {Int | undefined}
     */
    as_int(): Int | undefined;
    /**
     * @returns {Uint8Array | undefined}
     */
    as_bytes(): Uint8Array | undefined;
    /**
     * @returns {string | undefined}
     */
    as_text(): string | undefined;
    /**
     * @returns {CBORArray | undefined}
     */
    as_array(): CBORArray | undefined;
    /**
     * @returns {CBORObject | undefined}
     */
    as_object(): CBORObject | undefined;
    /**
     * @returns {TaggedCBOR | undefined}
     */
    as_tagged(): TaggedCBOR | undefined;
    /**
     * @returns {CBORSpecial | undefined}
     */
    as_special(): CBORSpecial | undefined;
}
/** */
export class COSEEncrypt {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {COSEEncrypt}
     */
    static from_bytes(bytes: Uint8Array): COSEEncrypt;
    /**
     * @param {Headers} headers
     * @param {Uint8Array | undefined} ciphertext
     * @param {COSERecipients} recipients
     * @returns {COSEEncrypt}
     */
    static new(headers: Headers, ciphertext: Uint8Array | undefined, recipients: COSERecipients): COSEEncrypt;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {Headers}
     */
    headers(): Headers;
    /**
     * @returns {Uint8Array | undefined}
     */
    ciphertext(): Uint8Array | undefined;
    /**
     * @returns {COSERecipients}
     */
    recipients(): COSERecipients;
}
/** */
export class COSEEncrypt0 {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {COSEEncrypt0}
     */
    static from_bytes(bytes: Uint8Array): COSEEncrypt0;
    /**
     * @param {Headers} headers
     * @param {Uint8Array | undefined} ciphertext
     * @returns {COSEEncrypt0}
     */
    static new(headers: Headers, ciphertext: Uint8Array | undefined): COSEEncrypt0;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {Headers}
     */
    headers(): Headers;
    /**
     * @returns {Uint8Array | undefined}
     */
    ciphertext(): Uint8Array | undefined;
}
/** */
export class COSEKey {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {COSEKey}
     */
    static from_bytes(bytes: Uint8Array): COSEKey;
    /**
     * @param {Label} key_type
     * @returns {COSEKey}
     */
    static new(key_type: Label): COSEKey;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {Label} key_type
     */
    set_key_type(key_type: Label): void;
    /**
     * @returns {Label}
     */
    key_type(): Label;
    /**
     * @param {Uint8Array} key_id
     */
    set_key_id(key_id: Uint8Array): void;
    /**
     * @returns {Uint8Array | undefined}
     */
    key_id(): Uint8Array | undefined;
    /**
     * @param {Label} algorithm_id
     */
    set_algorithm_id(algorithm_id: Label): void;
    /**
     * @returns {Label | undefined}
     */
    algorithm_id(): Label | undefined;
    /**
     * @param {Labels} key_ops
     */
    set_key_ops(key_ops: Labels): void;
    /**
     * @returns {Labels | undefined}
     */
    key_ops(): Labels | undefined;
    /**
     * @param {Uint8Array} base_init_vector
     */
    set_base_init_vector(base_init_vector: Uint8Array): void;
    /**
     * @returns {Uint8Array | undefined}
     */
    base_init_vector(): Uint8Array | undefined;
    /**
     * @param {Label} label
     * @returns {CBORValue | undefined}
     */
    header(label: Label): CBORValue | undefined;
    /**
     * @param {Label} label
     * @param {CBORValue} value
     */
    set_header(label: Label, value: CBORValue): void;
}
/** */
export class COSERecipient {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {COSERecipient}
     */
    static from_bytes(bytes: Uint8Array): COSERecipient;
    /**
     * @param {Headers} headers
     * @param {Uint8Array | undefined} ciphertext
     * @returns {COSERecipient}
     */
    static new(headers: Headers, ciphertext: Uint8Array | undefined): COSERecipient;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {Headers}
     */
    headers(): Headers;
    /**
     * @returns {Uint8Array | undefined}
     */
    ciphertext(): Uint8Array | undefined;
}
/** */
export class COSERecipients {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {COSERecipients}
     */
    static from_bytes(bytes: Uint8Array): COSERecipients;
    /**
     * @returns {COSERecipients}
     */
    static new(): COSERecipients;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {COSERecipient}
     */
    get(index: number): COSERecipient;
    /**
     * @param {COSERecipient} elem
     */
    add(elem: COSERecipient): void;
}
/** */
export class COSESign {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {COSESign}
     */
    static from_bytes(bytes: Uint8Array): COSESign;
    /**
     * @param {Headers} headers
     * @param {Uint8Array | undefined} payload
     * @param {COSESignatures} signatures
     * @returns {COSESign}
     */
    static new(headers: Headers, payload: Uint8Array | undefined, signatures: COSESignatures): COSESign;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {Headers}
     */
    headers(): Headers;
    /**
     * @returns {Uint8Array | undefined}
     */
    payload(): Uint8Array | undefined;
    /**
     * @returns {COSESignatures}
     */
    signatures(): COSESignatures;
}
/** */
export class COSESign1 {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {COSESign1}
     */
    static from_bytes(bytes: Uint8Array): COSESign1;
    /**
     * @param {Headers} headers
     * @param {Uint8Array | undefined} payload
     * @param {Uint8Array} signature
     * @returns {COSESign1}
     */
    static new(headers: Headers, payload: Uint8Array | undefined, signature: Uint8Array): COSESign1;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {Headers}
     */
    headers(): Headers;
    /**
     * @returns {Uint8Array | undefined}
     */
    payload(): Uint8Array | undefined;
    /**
     * @returns {Uint8Array}
     */
    signature(): Uint8Array;
    /**
     * For verifying, we will want to reverse-construct this SigStructure to check the signature against
     * # Arguments
     * * `external_aad` - External application data - see RFC 8152 section 4.3. Set to None if not using this.
     * @param {Uint8Array | undefined} external_aad
     * @param {Uint8Array | undefined} external_payload
     * @returns {SigStructure}
     */
    signed_data(external_aad: Uint8Array | undefined, external_payload: Uint8Array | undefined): SigStructure;
}
/** */
export class COSESign1Builder {
    static __wrap(ptr: any): any;
    /**
     * @param {Headers} headers
     * @param {Uint8Array} payload
     * @param {boolean} is_payload_external
     * @returns {COSESign1Builder}
     */
    static new(headers: Headers, payload: Uint8Array, is_payload_external: boolean): COSESign1Builder;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /** */
    hash_payload(): void;
    /**
     * @param {Uint8Array} external_aad
     */
    set_external_aad(external_aad: Uint8Array): void;
    /**
     * @returns {SigStructure}
     */
    make_data_to_sign(): SigStructure;
    /**
     * @param {Uint8Array} signed_sig_structure
     * @returns {COSESign1}
     */
    build(signed_sig_structure: Uint8Array): COSESign1;
}
/** */
export class COSESignBuilder {
    static __wrap(ptr: any): any;
    /**
     * @param {Headers} headers
     * @param {Uint8Array} payload
     * @param {boolean} is_payload_external
     * @returns {COSESignBuilder}
     */
    static new(headers: Headers, payload: Uint8Array, is_payload_external: boolean): COSESignBuilder;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /** */
    hash_payload(): void;
    /**
     * @param {Uint8Array} external_aad
     */
    set_external_aad(external_aad: Uint8Array): void;
    /**
     * @returns {SigStructure}
     */
    make_data_to_sign(): SigStructure;
    /**
     * @param {COSESignatures} signed_sig_structure
     * @returns {COSESign}
     */
    build(signed_sig_structure: COSESignatures): COSESign;
}
/** */
export class COSESignature {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {COSESignature}
     */
    static from_bytes(bytes: Uint8Array): COSESignature;
    /**
     * @param {Headers} headers
     * @param {Uint8Array} signature
     * @returns {COSESignature}
     */
    static new(headers: Headers, signature: Uint8Array): COSESignature;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {Headers}
     */
    headers(): Headers;
    /**
     * @returns {Uint8Array}
     */
    signature(): Uint8Array;
}
/** */
export class COSESignatures {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {COSESignatures}
     */
    static from_bytes(bytes: Uint8Array): COSESignatures;
    /**
     * @returns {COSESignatures}
     */
    static new(): COSESignatures;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {COSESignature}
     */
    get(index: number): COSESignature;
    /**
     * @param {COSESignature} elem
     */
    add(elem: COSESignature): void;
}
/** */
export class CounterSignature {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {CounterSignature}
     */
    static from_bytes(bytes: Uint8Array): CounterSignature;
    /**
     * @param {COSESignature} cose_signature
     * @returns {CounterSignature}
     */
    static new_single(cose_signature: COSESignature): CounterSignature;
    /**
     * @param {COSESignatures} cose_signatures
     * @returns {CounterSignature}
     */
    static new_multi(cose_signatures: COSESignatures): CounterSignature;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {COSESignatures}
     */
    signatures(): COSESignatures;
}
/** */
export class EdDSA25519Key {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} pubkey_bytes
     * @returns {EdDSA25519Key}
     */
    static new(pubkey_bytes: Uint8Array): EdDSA25519Key;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @param {Uint8Array} private_key_bytes
     */
    set_private_key(private_key_bytes: Uint8Array): void;
    /** */
    is_for_signing(): void;
    /** */
    is_for_verifying(): void;
    /**
     * @returns {COSEKey}
     */
    build(): COSEKey;
}
/** */
export class HeaderMap {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {HeaderMap}
     */
    static from_bytes(bytes: Uint8Array): HeaderMap;
    /**
     * @returns {HeaderMap}
     */
    static new(): HeaderMap;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {Label} algorithm_id
     */
    set_algorithm_id(algorithm_id: Label): void;
    /**
     * @returns {Label | undefined}
     */
    algorithm_id(): Label | undefined;
    /**
     * @param {Labels} criticality
     */
    set_criticality(criticality: Labels): void;
    /**
     * @returns {Labels | undefined}
     */
    criticality(): Labels | undefined;
    /**
     * @param {Label} content_type
     */
    set_content_type(content_type: Label): void;
    /**
     * @returns {Label | undefined}
     */
    content_type(): Label | undefined;
    /**
     * @param {Uint8Array} key_id
     */
    set_key_id(key_id: Uint8Array): void;
    /**
     * @returns {Uint8Array | undefined}
     */
    key_id(): Uint8Array | undefined;
    /**
     * @param {Uint8Array} init_vector
     */
    set_init_vector(init_vector: Uint8Array): void;
    /**
     * @returns {Uint8Array | undefined}
     */
    init_vector(): Uint8Array | undefined;
    /**
     * @param {Uint8Array} partial_init_vector
     */
    set_partial_init_vector(partial_init_vector: Uint8Array): void;
    /**
     * @returns {Uint8Array | undefined}
     */
    partial_init_vector(): Uint8Array | undefined;
    /**
     * @param {CounterSignature} counter_signature
     */
    set_counter_signature(counter_signature: CounterSignature): void;
    /**
     * @returns {CounterSignature | undefined}
     */
    counter_signature(): CounterSignature | undefined;
    /**
     * @param {Label} label
     * @returns {CBORValue | undefined}
     */
    header(label: Label): CBORValue | undefined;
    /**
     * @param {Label} label
     * @param {CBORValue} value
     */
    set_header(label: Label, value: CBORValue): void;
    /**
     * @returns {Labels}
     */
    keys(): Labels;
}
/** */
export class Headers {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Headers}
     */
    static from_bytes(bytes: Uint8Array): Headers;
    /**
     * @param {ProtectedHeaderMap} protected_
     * @param {HeaderMap} unprotected_
     * @returns {Headers}
     */
    static new(protected_: ProtectedHeaderMap, unprotected_: HeaderMap): Headers;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {ProtectedHeaderMap}
     */
    protected(): ProtectedHeaderMap;
    /**
     * @returns {HeaderMap}
     */
    unprotected(): HeaderMap;
}
/** */
export class Int {
    static __wrap(ptr: any): any;
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
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {boolean}
     */
    is_positive(): boolean;
    /**
     * @returns {BigNum | undefined}
     */
    as_positive(): BigNum | undefined;
    /**
     * @returns {BigNum | undefined}
     */
    as_negative(): BigNum | undefined;
    /**
     * @returns {number | undefined}
     */
    as_i32(): number | undefined;
}
/** */
export class Label {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Label}
     */
    static from_bytes(bytes: Uint8Array): Label;
    /**
     * @param {Int} int
     * @returns {Label}
     */
    static new_int(int: Int): Label;
    /**
     * @param {string} text
     * @returns {Label}
     */
    static new_text(text: string): Label;
    /**
     * @param {number} id
     * @returns {Label}
     */
    static from_algorithm_id(id: number): Label;
    /**
     * @param {number} key_type
     * @returns {Label}
     */
    static from_key_type(key_type: number): Label;
    /**
     * @param {number} ec_key
     * @returns {Label}
     */
    static from_ec_key(ec_key: number): Label;
    /**
     * @param {number} curve_type
     * @returns {Label}
     */
    static from_curve_type(curve_type: number): Label;
    /**
     * @param {number} key_op
     * @returns {Label}
     */
    static from_key_operation(key_op: number): Label;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {Int | undefined}
     */
    as_int(): Int | undefined;
    /**
     * @returns {string | undefined}
     */
    as_text(): string | undefined;
}
/** */
export class Labels {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Labels}
     */
    static from_bytes(bytes: Uint8Array): Labels;
    /**
     * @returns {Labels}
     */
    static new(): Labels;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {Label}
     */
    get(index: number): Label;
    /**
     * @param {Label} elem
     */
    add(elem: Label): void;
}
/** */
export class PasswordEncryption {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PasswordEncryption}
     */
    static from_bytes(bytes: Uint8Array): PasswordEncryption;
    /**
     * @param {COSEEncrypt0} data
     * @returns {PasswordEncryption}
     */
    static new(data: COSEEncrypt0): PasswordEncryption;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
}
/** */
export class ProtectedHeaderMap {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ProtectedHeaderMap}
     */
    static from_bytes(bytes: Uint8Array): ProtectedHeaderMap;
    /**
     * @returns {ProtectedHeaderMap}
     */
    static new_empty(): ProtectedHeaderMap;
    /**
     * @param {HeaderMap} header_map
     * @returns {ProtectedHeaderMap}
     */
    static new(header_map: HeaderMap): ProtectedHeaderMap;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {HeaderMap}
     */
    deserialized_headers(): HeaderMap;
}
/** */
export class PubKeyEncryption {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PubKeyEncryption}
     */
    static from_bytes(bytes: Uint8Array): PubKeyEncryption;
    /**
     * @param {COSEEncrypt} data
     * @returns {PubKeyEncryption}
     */
    static new(data: COSEEncrypt): PubKeyEncryption;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
}
/** */
export class SigStructure {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {SigStructure}
     */
    static from_bytes(bytes: Uint8Array): SigStructure;
    /**
     * @param {number} context
     * @param {ProtectedHeaderMap} body_protected
     * @param {Uint8Array} external_aad
     * @param {Uint8Array} payload
     * @returns {SigStructure}
     */
    static new(context: number, body_protected: ProtectedHeaderMap, external_aad: Uint8Array, payload: Uint8Array): SigStructure;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    context(): number;
    /**
     * @returns {ProtectedHeaderMap}
     */
    body_protected(): ProtectedHeaderMap;
    /**
     * @returns {ProtectedHeaderMap | undefined}
     */
    sign_protected(): ProtectedHeaderMap | undefined;
    /**
     * @returns {Uint8Array}
     */
    external_aad(): Uint8Array;
    /**
     * @returns {Uint8Array}
     */
    payload(): Uint8Array;
    /**
     * @param {ProtectedHeaderMap} sign_protected
     */
    set_sign_protected(sign_protected: ProtectedHeaderMap): void;
}
/** */
export class SignedMessage {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {SignedMessage}
     */
    static from_bytes(bytes: Uint8Array): SignedMessage;
    /**
     * @param {COSESign} cose_sign
     * @returns {SignedMessage}
     */
    static new_cose_sign(cose_sign: COSESign): SignedMessage;
    /**
     * @param {COSESign1} cose_sign1
     * @returns {SignedMessage}
     */
    static new_cose_sign1(cose_sign1: COSESign1): SignedMessage;
    /**
     * @param {string} s
     * @returns {SignedMessage}
     */
    static from_user_facing_encoding(s: string): SignedMessage;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_user_facing_encoding(): string;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {COSESign | undefined}
     */
    as_cose_sign(): COSESign | undefined;
    /**
     * @returns {COSESign1 | undefined}
     */
    as_cose_sign1(): COSESign1 | undefined;
}
/** */
export class TaggedCBOR {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TaggedCBOR}
     */
    static from_bytes(bytes: Uint8Array): TaggedCBOR;
    /**
     * @param {BigNum} tag
     * @param {CBORValue} value
     * @returns {TaggedCBOR}
     */
    static new(tag: BigNum, value: CBORValue): TaggedCBOR;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {BigNum}
     */
    tag(): BigNum;
    /**
     * @returns {CBORValue}
     */
    value(): CBORValue;
}
/**
 * Decompression callback
 */
export type DecompressCallback = (compressed: Uint8Array) => Uint8Array;
/**
 * Options for instantiating a Wasm instance.
 */
export type InstantiateOptions = {
    /**
     * - Optional url to the Wasm file to instantiate.
     */
    url?: URL | undefined;
    /**
     * - Callback to decompress the
     * raw Wasm file bytes before instantiating.
     */
    decompress?: DecompressCallback | undefined;
};
