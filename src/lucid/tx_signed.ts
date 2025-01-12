import { Hasher, Lucid } from "../mod.ts";

export class TxSigned {
  tx: string;
  private lucid: Lucid;
  constructor(lucid: Lucid, tx: string) {
    this.lucid = lucid;
    this.tx = tx;
  }

  async submit(): Promise<string> {
    const provider = this.lucid.wallet || this.lucid.provider;
    return await provider.submit(
      this.tx,
    );
  }

  toString(): string {
    return this.tx;
  }

  toHash(): string {
    return Hasher.hashTransaction(this.tx);
  }
}
