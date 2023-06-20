import { Lucid } from "./mod.js";
import {
  Address,
  Payload,
  PrivateKey,
  RewardAddress,
  SignedMessage,
} from "../types/mod.js";
import { signData } from "../misc/sign_data.js";
import { C } from "../mod.js";

export class Message {
  lucid: Lucid;
  address: Address | RewardAddress;
  payload: Payload;

  constructor(
    lucid: Lucid,
    address: Address | RewardAddress,
    payload: Payload,
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
    const { paymentCredential, stakeCredential, address: { hex: hexAddress } } =
      this.lucid.utils.getAddressDetails(this.address);

    const keyHash = paymentCredential?.hash || stakeCredential?.hash;

    const keyHashOriginal = C.PrivateKey.from_bech32(privateKey).to_public()
      .hash().to_hex();

    if (!keyHash || keyHash !== keyHashOriginal) {
      throw new Error(`Cannot sign message for address: ${this.address}.`);
    }

    return signData(hexAddress, this.payload, privateKey);
  }
}
