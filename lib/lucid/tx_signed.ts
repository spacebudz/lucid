import { Hasher, type Lucid, type SignerResult } from "../mod.ts";

export class TxSigned {
  private signerResult: SignerResult;
  private lucid: Lucid;
  constructor(lucid: Lucid, signerResult: SignerResult) {
    this.lucid = lucid;
    this.signerResult = signerResult;
  }

  async submit(): Promise<string> {
    const provider = this.lucid.wallet || this.lucid.provider;
    return await provider.submit(
      this.signerResult.tx,
    );
  }

  toWitnessSet(): string {
    return this.signerResult.witnessSet;
  }

  toString(): string {
    return this.signerResult.tx;
  }

  toHash(): string {
    return Hasher.hashTransaction(this.signerResult.tx);
  }
}
