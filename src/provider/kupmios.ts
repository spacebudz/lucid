// deno-lint-ignore-file
import {
  Address,
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
import { getAddressDetails } from "../utils/mod.ts";

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
      client.addEventListener("message", (msg: MessageEvent<string>) => {
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

          res(
            {
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
                result.maxExecutionUnitsPerTransaction.steps,
              ),
              coinsPerUtxoByte: BigInt(result.coinsPerUtxoByte),
              collateralPercentage: parseInt(result.collateralPercentage),
              maxCollateralInputs: parseInt(result.maxCollateralInputs),
              costModels,
            },
          );
          client.close();
        } catch (e) {
          rej(e);
        }
      }, { once: true });
    });
  }

  async getUtxos(address: Address): Promise<UTxO[]> {
    throw new Error("Not implemented");
  }
  async getUtxosWithUnit(address: Address, unit: Unit): Promise<UTxO[]> {
    throw new Error("Not implemented");
  }
  async getUtxosByOutRef(outRefs: Array<OutRef>): Promise<UTxO[]> {
    throw new Error("Not implemented");
  }
  async getDelegation(rewardAddress: RewardAddress): Promise<Delegation> {
    const { stakeCredential } = getAddressDetails(rewardAddress);
    const client = await this.ogmiosWsp("Query", {
      query: { "delegationsAndRewards": [stakeCredential!.hash] },
    });

    return new Promise((res, rej) => {
      client.addEventListener("message", (msg: MessageEvent<string>) => {
        try {
          const { result } = JSON.parse(msg.data);
          res(
            {
              poolId: result.delegate || null,
              rewards: BigInt(result.rewards || 0),
            },
          );
          client.close();
        } catch (e) {
          rej(e);
        }
      }, { once: true });
    });
  }
  async getDatum(datumHash: DatumHash): Promise<Datum> {
    throw new Error("Not implemented");
  }
  async awaitTx(txHash: TxHash): Promise<boolean> {
    throw new Error("Not implemented");
  }
  async submitTx(tx: Transaction): Promise<TxHash> {
    throw new Error("Not implemented");
  }

  private async ogmiosWsp(
    methodname: string,
    args: unknown,
  ): Promise<WebSocket> {
    const client = new WebSocket(this.ogmiosUrl);
    await new Promise((res) => {
      client.addEventListener("open", () => res(1), { once: true });
    });
    client.send(JSON.stringify({
      type: "jsonwsp/request",
      version: "1.0",
      servicename: "ogmios",
      methodname,
      args,
    }));
    return client;
  }
}
