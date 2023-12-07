import { KeyHash, Payload, PrivateKey, SignedMessage } from "../mod.js";
export declare function signData(addressHex: string, payload: Payload, privateKey: PrivateKey): SignedMessage;
export declare function verifyData(addressHex: string, keyHash: KeyHash, payload: Payload, signedMessage: SignedMessage): boolean;
