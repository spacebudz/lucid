import {
  applyParamsToScript,
  Codec,
  type Credential,
  type Data,
  type Exact,
  Hasher,
  type Lucid,
  type NativeScript,
  type Script,
} from "../mod.ts";

export class ScriptUtility<T extends unknown[] = Data[]> {
  private lucid: Lucid;
  script: Script;

  constructor(
    lucid: Lucid,
    script: Script | NativeScript,
    params?: Exact<[...T]>,
    type?: T,
  ) {
    this.lucid = lucid;

    if (script.type && "script" in script) {
      if (script.type !== "Native" && params) {
        const scriptWithParams = applyParamsToScript(
          params,
          script.script,
          type,
        );
        this.script = { type: script.type, script: scriptWithParams };
      } else {
        this.script = script;
      }
    } else {
      this.script = {
        type: "Native",
        script: Codec.encodeNativeScript(script),
      };
    }
  }

  toHash(): string {
    return Hasher.hashScript(this.script);
  }

  toAddress(delegation?: Credential): string {
    return this.lucid.utils.scriptToAddress(this.script, delegation);
  }

  toRewardAddress(): string {
    return this.lucid.utils.scriptToRewardAddress(this.script);
  }

  toString(): string {
    return this.script.script;
  }
}
