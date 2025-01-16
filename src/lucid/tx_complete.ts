import { Hasher, InstructionSigner, Lucid, TxSigned } from "../mod.ts";

export class TxComplete {
  private instructionSigner: InstructionSigner;
  private tasks: (() => unknown)[];
  private lucid: Lucid;

  constructor(lucid: Lucid, instructionSigner: InstructionSigner) {
    this.lucid = lucid;
    this.instructionSigner = instructionSigner;
    this.tasks = [];
  }
  sign(): TxComplete {
    this.tasks.push(() => {
      return this.lucid.wallet.sign(this.instructionSigner);
    });
    return this;
  }

  signWithPrivateKey(privateKey: string): TxComplete {
    this.tasks.push(() => {
      this.instructionSigner.signWithKey(privateKey);
    });
    return this;
  }

  signWithSeed(seed: string, index?: number): TxComplete {
    this.tasks.push(() => {
      this.instructionSigner.signWithSeed(seed, index || 0);
    });
    return this;
  }

  /** vkey witness */
  signWithWitness(witness: string): TxComplete {
    this.tasks.push(() => {
      this.instructionSigner.signWithWitness(witness);
    });
    return this;
  }

  async partialSign(): Promise<string> {
    const witnessSet = await this.lucid.wallet.sign(this.instructionSigner);
    return witnessSet;
  }

  partialSignWithPrivateKey(privateKey: string): string {
    return this.instructionSigner
      .signWithKey(privateKey)
      .getPartialWitnessSet();
  }

  partialSignWithSeed(seed: string, index: number): string {
    return this.instructionSigner
      .signWithSeed(seed, index)
      .getPartialWitnessSet();
  }

  assemble(witnessSets: string[]): TxComplete {
    this.tasks.push(() => {
      for (const witnessSet of witnessSets) {
        this.instructionSigner.signWithWitnessSet(witnessSet);
      }
    });

    return this;
  }

  async commit(): Promise<TxSigned> {
    for (const task of this.tasks) {
      await task();
    }

    return new TxSigned(this.lucid, this.instructionSigner.commit());
  }

  toString(): string {
    return this.instructionSigner.commit();
  }

  toHash(): string {
    return Hasher.hashTransaction(this.instructionSigner.commit());
  }
}
