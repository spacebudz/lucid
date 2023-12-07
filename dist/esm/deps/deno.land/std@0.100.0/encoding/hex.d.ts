/**
 * ErrInvalidByte takes an invalid byte and returns an Error.
 * @param byte
 */
export declare function errInvalidByte(byte: number): Error;
/** ErrLength returns an error about odd string length. */
export declare function errLength(): Error;
/**
 * EncodedLen returns the length of an encoding of n source bytes. Specifically,
 * it returns n * 2.
 * @param n
 */
export declare function encodedLen(n: number): number;
/**
 * Encode encodes `src` into `encodedLen(src.length)` bytes.
 * @param src
 */
export declare function encode(src: Uint8Array): Uint8Array;
/**
 * EncodeToString returns the hexadecimal encoding of `src`.
 * @param src
 */
export declare function encodeToString(src: Uint8Array): string;
/**
 * Decode decodes `src` into `decodedLen(src.length)` bytes
 * If the input is malformed an error will be thrown
 * the error.
 * @param src
 */
export declare function decode(src: Uint8Array): Uint8Array;
/**
 * DecodedLen returns the length of decoding `x` source bytes.
 * Specifically, it returns `x / 2`.
 * @param x
 */
export declare function decodedLen(x: number): number;
/**
 * DecodeString returns the bytes represented by the hexadecimal string `s`.
 * DecodeString expects that src contains only hexadecimal characters and that
 * src has even length.
 * If the input is malformed, DecodeString will throw an error.
 * @param s the `string` to decode to `Uint8Array`
 */
export declare function decodeString(s: string): Uint8Array;
