import { C } from "../core/mod.js";
import { toHex } from "../utils/mod.js";
export class TxSigned {
    constructor(lucid, tx) {
        Object.defineProperty(this, "txSigned", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "lucid", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        this.lucid = lucid;
        this.txSigned = tx;
    }
    async submit() {
        return await (this.lucid.wallet || this.lucid.provider).submitTx(toHex(this.txSigned.to_bytes()));
    }
    /** Returns the transaction in Hex encoded Cbor. */
    toString() {
        return toHex(this.txSigned.to_bytes());
    }
    /** Return the transaction hash. */
    toHash() {
        return C.hash_transaction(this.txSigned.body()).to_hex();
    }
}
