import { signData } from "../misc/sign_data.js";
import { C } from "../mod.js";
export class Message {
    constructor(lucid, address, payload) {
        Object.defineProperty(this, "lucid", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "address", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "payload", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        this.lucid = lucid;
        this.address = address;
        this.payload = payload;
    }
    /** Sign message with selected wallet. */
    sign() {
        return this.lucid.wallet.signMessage(this.address, this.payload);
    }
    /** Sign message with a separate private key. */
    signWithPrivateKey(privateKey) {
        const { paymentCredential, stakeCredential, address: { hex: hexAddress } } = this.lucid.utils.getAddressDetails(this.address);
        const keyHash = paymentCredential?.hash || stakeCredential?.hash;
        const keyHashOriginal = C.PrivateKey.from_bech32(privateKey).to_public()
            .hash().to_hex();
        if (!keyHash || keyHash !== keyHashOriginal) {
            throw new Error(`Cannot sign message for address: ${this.address}.`);
        }
        return signData(hexAddress, this.payload, privateKey);
    }
}
