import { Lucid } from "./mod.js";
import { Address, Payload, PrivateKey, RewardAddress, SignedMessage } from "../types/mod.js";
export declare class Message {
    lucid: Lucid;
    address: Address | RewardAddress;
    payload: Payload;
    constructor(lucid: Lucid, address: Address | RewardAddress, payload: Payload);
    /** Sign message with selected wallet. */
    sign(): Promise<SignedMessage>;
    /** Sign message with a separate private key. */
    signWithPrivateKey(privateKey: PrivateKey): SignedMessage;
}
