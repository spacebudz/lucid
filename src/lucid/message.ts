import { Addresses, Crypto, Lucid, SignedMessage } from "../mod.ts";
import { signMessage } from "../misc/sign_message.ts";

export class Message {
  lucid: Lucid;
  address: string;
  payload: string;

  constructor(
    lucid: Lucid,
    address: string,
    payload: string,
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
  signWithPrivateKey(privateKey: string): SignedMessage {
    const { payment, delegation } = Addresses.inspect(this.address);

    const keyHash = payment?.hash || delegation?.hash;

    const { credential: { hash } } = Crypto.privateKeyToDetails(privateKey);

    if (!keyHash || keyHash !== hash) {
      throw new Error(`Cannot sign message for address: ${this.address}.`);
    }

    return signMessage(this.address, this.payload, privateKey);
  }
}
