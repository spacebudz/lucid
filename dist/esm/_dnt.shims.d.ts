export { crypto, type Crypto, type SubtleCrypto, type AlgorithmIdentifier, type Algorithm, type RsaOaepParams, type BufferSource, type AesCtrParams, type AesCbcParams, type AesGcmParams, type CryptoKey, type KeyAlgorithm, type KeyType, type KeyUsage, type EcdhKeyDeriveParams, type HkdfParams, type HashAlgorithmIdentifier, type Pbkdf2Params, type AesDerivedKeyParams, type HmacImportParams, type JsonWebKey, type RsaOtherPrimesInfo, type KeyFormat, type RsaHashedKeyGenParams, type RsaKeyGenParams, type BigInteger, type EcKeyGenParams, type NamedCurve, type CryptoKeyPair, type AesKeyGenParams, type HmacKeyGenParams, type RsaHashedImportParams, type EcKeyImportParams, type AesKeyAlgorithm, type RsaPssParams, type EcdsaParams } from "@deno/shim-crypto";
export { fetch, File, FormData, Headers, Request, Response, type BodyInit, type HeadersInit, type RequestInit, type ResponseInit } from "undici";
export { default as WebSocket } from "ws";
export declare const dntGlobalThis: Omit<typeof globalThis, "crypto" | "fetch" | "File" | "FormData" | "Headers" | "Request" | "Response" | "WebSocket"> & {
    crypto: any;
    fetch: any;
    File: any;
    FormData: any;
    Headers: any;
    Request: any;
    Response: any;
    WebSocket: any;
};
