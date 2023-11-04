import {
  Address,
  Assets,
  Credential,
  Datum,
  DatumHash,
  Delegation,
  OutRef,
  ProtocolParameters,
  Provider,
  RewardAddress,
  Transaction,
  TxHash,
  Unit,
  UTxO,
} from "../types/mod.ts";
import { C } from "../core/mod.ts";
import { fromHex, fromUnit, toHex } from "../utils/mod.ts";

export class Kupmios implements Provider {
  kupoUrl: string;
  ogmiosUrl: string;

  /**
   * @param kupoUrl: http(s)://localhost:1442
   * @param ogmiosUrl: ws(s)://localhost:1337
   */
  constructor(kupoUrl: string, ogmiosUrl: string) {
    this.kupoUrl = kupoUrl;
    this.ogmiosUrl = ogmiosUrl;
  }

  async getProtocolParameters(): Promise<ProtocolParameters> {
    const client = await this.ogmiosWsp("Query", {
      query: "currentProtocolParameters",
    });

    return new Promise((res, rej) => {
      client.addEventListener(
        "message",
        (msg: MessageEvent<string>) => {
          try {
            const { result } = JSON.parse(msg.data);

            // deno-lint-ignore no-explicit-any
            const costModels: any = {};
            Object.keys(result.costModels).forEach((v) => {
              const version = v.split(":")[1].toUpperCase();
              const plutusVersion = "Plutus" + version;
              costModels[plutusVersion] = result.costModels[v];
            });
            const [memNum, memDenom] = result.prices.memory.split("/");
            const [stepsNum, stepsDenom] = result.prices.steps.split("/");

            res({
              minFeeA: parseInt(result.minFeeCoefficient),
              minFeeB: parseInt(result.minFeeConstant),
              maxTxSize: parseInt(result.maxTxSize),
              maxValSize: parseInt(result.maxValueSize),
              keyDeposit: BigInt(result.stakeKeyDeposit),
              poolDeposit: BigInt(result.poolDeposit),
              priceMem: parseInt(memNum) / parseInt(memDenom),
              priceStep: parseInt(stepsNum) / parseInt(stepsDenom),
              maxTxExMem: BigInt(result.maxExecutionUnitsPerTransaction.memory),
              maxTxExSteps: BigInt(
                result.maxExecutionUnitsPerTransaction.steps
              ),
              coinsPerUtxoByte: BigInt(result.coinsPerUtxoByte),
              collateralPercentage: parseInt(result.collateralPercentage),
              maxCollateralInputs: parseInt(result.maxCollateralInputs),
              costModels,
            });
            client.close();
          } catch (e) {
            rej(e);
          }
        },
        { once: true }
      );
    });
  }

  async getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]> {
    const isAddress = typeof addressOrCredential === "string";
    const queryPredicate = isAddress
      ? addressOrCredential
      : addressOrCredential.hash;
    const result = await fetch(
      `${this.kupoUrl}/matches/${queryPredicate}${
        isAddress ? "" : "/*"
      }?unspent`
    ).then((res) => res.json());
    return this.kupmiosUtxosToUtxos(result);
  }

  async getUtxosWithUnit(
    addressOrCredential: Address | Credential,
    unit: Unit
  ): Promise<UTxO[]> {
    const isAddress = typeof addressOrCredential === "string";
    const queryPredicate = isAddress
      ? addressOrCredential
      : addressOrCredential.hash;
    const { policyId, assetName } = fromUnit(unit);
    const result = await fetch(
      `${this.kupoUrl}/matches/${queryPredicate}${
        isAddress ? "" : "/*"
      }?unspent&policy_id=${policyId}${
        assetName ? `&asset_name=${assetName}` : ""
      }`
    ).then((res) => res.json());
    return this.kupmiosUtxosToUtxos(result);
  }

  async getUtxoByUnit(unit: Unit): Promise<UTxO> {
    const { policyId, assetName } = fromUnit(unit);
    const result = await fetch(
      `${this.kupoUrl}/matches/${policyId}.${
        assetName ? `${assetName}` : "*"
      }?unspent`
    ).then((res) => res.json());

    const utxos = await this.kupmiosUtxosToUtxos(result);

    if (utxos.length > 1) {
      throw new Error("Unit needs to be an NFT or only held by one address.");
    }

    return utxos[0];
  }

  async getUtxosByOutRef(outRefs: Array<OutRef>): Promise<UTxO[]> {
    const queryHashes = [...new Set(outRefs.map((outRef) => outRef.txHash))];

    const utxos = await Promise.all(
      queryHashes.map(async (txHash) => {
        const result = await fetch(
          `${this.kupoUrl}/matches/*@${txHash}?unspent`
        ).then((res) => res.json());
        return this.kupmiosUtxosToUtxos(result);
      })
    );

    return utxos
      .reduce((acc, utxos) => acc.concat(utxos), [])
      .filter((utxo) =>
        outRefs.some(
          (outRef) =>
            utxo.txHash === outRef.txHash &&
            utxo.outputIndex === outRef.outputIndex
        )
      );
  }

  async getDelegation(rewardAddress: RewardAddress): Promise<Delegation> {
    const client = await this.ogmiosWsp("Query", {
      query: { delegationsAndRewards: [rewardAddress] },
    });

    return new Promise((res, rej) => {
      client.addEventListener(
        "message",
        (msg: MessageEvent<string>) => {
          try {
            const { result } = JSON.parse(msg.data);
            const delegation = (result ? Object.values(result)[0] : {}) as {
              delegate: string;
              rewards: number;
            };
            res({
              poolId: delegation?.delegate || null,
              rewards: BigInt(delegation?.rewards || 0),
            });
            client.close();
          } catch (e) {
            rej(e);
          }
        },
        { once: true }
      );
    });
  }

  async getDatum(datumHash: DatumHash): Promise<Datum> {
    const result = await fetch(`${this.kupoUrl}/datums/${datumHash}`).then(
      (res) => res.json()
    );
    if (!result || !result.datum) {
      throw new Error(`No datum found for datum hash: ${datumHash}`);
    }
    return result.datum;
  }

  awaitTx(txHash: TxHash, checkInterval = 3000): Promise<boolean> {
    return new Promise((res) => {
      const confirmation = setInterval(async () => {
        const isConfirmed = await fetch(
          `${this.kupoUrl}/matches/*@${txHash}?unspent`
        ).then((res) => res.json());
        if (isConfirmed && isConfirmed.length > 0) {
          clearInterval(confirmation);
          await new Promise((res) => setTimeout(() => res(1), 1000));
          return res(true);
        }
      }, checkInterval);
    });
  }

  async submitTx(tx: Transaction): Promise<TxHash> {
    const client = await this.ogmiosWsp("SubmitTx", {
      submit: tx,
    });

    return new Promise((res, rej) => {
      client.addEventListener(
        "message",
        (msg: MessageEvent<string>) => {
          try {
            const { result } = JSON.parse(msg.data);

            if (result.SubmitSuccess) res(result.SubmitSuccess.txId);
            else rej(result.SubmitFail);
            client.close();
          } catch (e) {
            rej(e);
          }
        },
        { once: true }
      );
    });
  }

  private kupmiosUtxosToUtxos(utxos: unknown): Promise<UTxO[]> {
    return Promise.all(
      // deno-lint-ignore no-explicit-any
      (utxos as any).map(async (utxo: any) => {
        return {
          txHash: utxo.transaction_id,
          outputIndex: parseInt(utxo.output_index),
          address: utxo.address,
          assets: (() => {
            const a: Assets = { lovelace: BigInt(utxo.value.coins) };
            Object.keys(utxo.value.assets).forEach((unit) => {
              a[unit.replace(".", "")] = BigInt(utxo.value.assets[unit]);
            });
            return a;
          })(),
          datumHash: utxo?.datum_type === "hash" ? utxo.datum_hash : null,
          datum:
            utxo?.datum_type === "inline"
              ? await this.getDatum(utxo.datum_hash)
              : null,
          scriptRef:
            utxo.script_hash &&
            (await (async () => {
              const { script, language } = await fetch(
                `${this.kupoUrl}/scripts/${utxo.script_hash}`
              ).then((res) => res.json());

              if (language === "native") {
                return { type: "Native", script };
              } else if (language === "plutus:v1") {
                const plutusScript = C.PlutusScript.new(fromHex(script));
                const scriptBytes = plutusScript.to_bytes();
                plutusScript.free();
                return {
                  type: "PlutusV1",
                  script: toHex(scriptBytes),
                };
              } else if (language === "plutus:v2") {
                const plutusScript = C.PlutusScript.new(fromHex(script));
                const scriptBytes = plutusScript.to_bytes();
                plutusScript.free();

                return {
                  type: "PlutusV2",
                  script: toHex(scriptBytes),
                };
              }
            })()),
        } as UTxO;
      })
    );
  }

  private async ogmiosWsp(
    methodname: string,
    args: unknown
  ): Promise<WebSocket> {
    const client = new WebSocket(this.ogmiosUrl);
    await new Promise((res) => {
      client.addEventListener("open", () => res(1), { once: true });
    });
    client.send(
      JSON.stringify({
        type: "jsonwsp/request",
        version: "1.0",
        servicename: "ogmios",
        methodname,
        args,
      })
    );
    return client;
  }
}
