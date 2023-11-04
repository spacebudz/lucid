import { Lucid } from "./mod.ts";
import {
  Address,
  Payload,
  PrivateKey,
  RewardAddress,
  SignedMessage,
} from "../types/mod.ts";
import { signData } from "../misc/sign_data.ts";
import { C } from "../mod.ts";
import { FreeableBucket, Freeables } from "../utils/freeable.ts";

export class Message {
  lucid: Lucid;
  address: Address | RewardAddress;
  payload: Payload;

  constructor(
    lucid: Lucid,
    address: Address | RewardAddress,
    payload: Payload
  ) {
    this.lucid = lucid;
    this.address = address;
    this.payload = payload;
  }

  /** Sign message with selected wallet. */
  sign(): Promise<SignedMessage> {
    return this.lucid.wallet.signMessage(this.address, this.payload);
  }

  /** Sign message with a separate private key. */
  signWithPrivateKey(privateKey: PrivateKey): SignedMessage {
    const bucket: FreeableBucket = [];
    try {
      const {
        paymentCredential,
        stakeCredential,
        address: { hex: hexAddress },
      } = this.lucid.utils.getAddressDetails(this.address);

      const keyHash = paymentCredential?.hash || stakeCredential?.hash;

      const skey = C.PrivateKey.from_bech32(privateKey);
      bucket.push(skey);
      const vkey = skey.to_public();
      bucket.push(vkey);
      const hash = vkey.hash();
      bucket.push(hash);
      const keyHashOriginal = hash.to_hex();

      if (!keyHash || keyHash !== keyHashOriginal) {
        throw new Error(`Cannot sign message for address: ${this.address}.`);
      }

      return signData(hexAddress, this.payload, privateKey);
    } finally {
      Freeables.free(...bucket);
    }
  }
}
