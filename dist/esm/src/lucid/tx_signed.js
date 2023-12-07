import { C } from "../core/mod.js";
import { toHex } from "../utils/mod.js";
export class TxSigned {
    txSigned;
    lucid;
    constructor(lucid, tx) {
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
