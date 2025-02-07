import {
  type ActiveDelegation,
  Addresses,
  type Assets,
  type Credential,
  fromUnit,
  type Network,
  type OutRef,
  type Provider,
  type RelevantProtocolParameters,
  Utils,
  type Utxo,
} from "../mod.ts";

export class Kupmios implements Provider {
  kupoUrl: string;
  ogmiosUrl: string;
  network?: Network;

  /**
   * @param kupoUrl: http(s)://localhost:1442
   * @param ogmiosUrl: ws(s)://localhost:1337
   */
  constructor(
    { kupoUrl, ogmiosUrl, network }: {
      kupoUrl: string;
      ogmiosUrl: string;
      network: Network;
    },
  ) {
    this.kupoUrl = kupoUrl;
    this.ogmiosUrl = ogmiosUrl;
    this.network = network;
  }

  async getProtocolParameters(): Promise<RelevantProtocolParameters> {
    const client = await this.rpc("queryLedgerState/protocolParameters");

    return new Promise((res, rej) => {
      client.addEventListener("message", (msg: MessageEvent<string>) => {
        try {
          const { result } = JSON.parse(msg.data);

          // deno-lint-ignore no-explicit-any
          const costModels: any = {};
          Object.keys(result.plutusCostModels).forEach((v) => {
            const version = v.split(":")[1].toUpperCase();
            const plutusVersion = "Plutus" + version;
            costModels[plutusVersion] = Object.values(
              result.plutusCostModels[v],
            );
          });
          const [memNum, memDenom] = result.scriptExecutionPrices.memory.split(
            "/",
          );
          const [stepsNum, stepsDenom] = result.scriptExecutionPrices.cpu.split(
            "/",
          );

          res(
            {
              minFeeA: parseInt(result.minFeeCoefficient),
              minFeeB: parseInt(result.minFeeConstant.ada.lovelace),
              maxTxSize: parseInt(result.maxTransactionSize.bytes),
              maxValSize: parseInt(result.maxValueSize.bytes),
              keyDeposit: parseInt(result.stakeCredentialDeposit.ada.lovelace),
              poolDeposit: parseInt(result.stakePoolDeposit.ada.lovelace),
              priceMem: parseInt(memNum) / parseInt(memDenom),
              priceStep: parseInt(stepsNum) / parseInt(stepsDenom),
              maxTxExMem: parseInt(
                result.maxExecutionUnitsPerTransaction.memory,
              ),
              maxTxExSteps: parseInt(
                result.maxExecutionUnitsPerTransaction.cpu,
              ),
              coinsPerUtxoByte: parseInt(result.minUtxoDepositCoefficient),
              collateralPercentage: parseInt(result.collateralPercentage),
              maxCollateralInputs: parseInt(result.maxCollateralInputs),
              costModels,
              minfeeRefscriptCostPerByte: parseInt(
                result.minFeeReferenceScripts.base,
              ),
            },
          );
          client.close();
        } catch (e) {
          rej(e);
        }
      }, { once: true });
    });
  }

  async getUtxos(addressOrCredential: string | Credential): Promise<Utxo[]> {
    const isAddress = typeof addressOrCredential === "string";
    const queryPredicate = isAddress
      ? addressOrCredential
      : addressOrCredential.hash;
    const result = await fetch(
      `${this.kupoUrl}/matches/${queryPredicate}${
        isAddress ? "" : "/*"
      }?unspent`,
    )
      .then((res) => res.json());
    return this.kupmiosUtxosToUtxos(result);
  }

  async getUtxosWithUnit(
    addressOrCredential: string | Credential,
    unit: string,
  ): Promise<Utxo[]> {
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
      }`,
    )
      .then((res) => res.json());
    return this.kupmiosUtxosToUtxos(result);
  }

  async getUtxoByUnit(unit: string): Promise<Utxo> {
    const { policyId, assetName } = fromUnit(unit);
    const result = await fetch(
      `${this.kupoUrl}/matches/${policyId}.${
        assetName ? `${assetName}` : "*"
      }?unspent`,
    )
      .then((res) => res.json());

    const utxos = await this.kupmiosUtxosToUtxos(result);

    if (utxos.length > 1) {
      throw new Error("Unit needs to be an NFT or only held by one address.");
    }

    return utxos[0];
  }

  async getUtxosByOutRef(outRefs: Array<OutRef>): Promise<Utxo[]> {
    const queryHashes = [...new Set(outRefs.map((outRef) => outRef.txHash))];

    const utxos = await Promise.all(queryHashes.map(async (txHash) => {
      const result = await fetch(
        `${this.kupoUrl}/matches/*@${txHash}?unspent`,
      ).then((res) => res.json());
      return this.kupmiosUtxosToUtxos(result);
    }));

    return utxos.reduce((acc, utxos) => acc.concat(utxos), []).filter((utxo) =>
      outRefs.some((outRef) =>
        utxo.txHash === outRef.txHash && utxo.outputIndex === outRef.outputIndex
      )
    );
  }

  async getDelegation(rewardAddress: string): Promise<ActiveDelegation> {
    const client = await this.rpc(
      "queryLedgerState/delegateRepresentative",
      Addresses.inspect(rewardAddress).delegation?.type === "Key"
        ? { keys: [rewardAddress] }
        : { scripts: [rewardAddress] },
    );

    return new Promise((res, rej) => {
      client.addEventListener("message", (msg: MessageEvent<string>) => {
        try {
          const { result } = JSON.parse(msg.data);
          const delegation = (result ? Object.values(result)[0] : {}) as {
            delegate: { id: string };
            rewards: { ada: { lovelace: number } };
          };
          res(
            {
              poolId: delegation?.delegate.id || null,
              drep: "- information not available -" as ActiveDelegation["drep"], // TODO
              rewards: BigInt(delegation?.rewards.ada.lovelace || 0),
            },
          );
          client.close();
        } catch (e) {
          rej(e);
        }
      }, { once: true });
    });
  }

  async getDatum(datumHash: string): Promise<string> {
    const result = await fetch(
      `${this.kupoUrl}/datums/${datumHash}`,
    ).then((res) => res.json());
    if (!result || !result.datum) {
      throw new Error(`No datum found for datum hash: ${datumHash}`);
    }
    return result.datum;
  }

  awaitTx(txHash: string, checkInterval = 3000): Promise<boolean> {
    return new Promise((res) => {
      const confirmation = setInterval(async () => {
        const isConfirmed = await fetch(
          `${this.kupoUrl}/matches/*@${txHash}?unspent`,
        ).then((res) => res.json());
        if (isConfirmed && isConfirmed.length > 0) {
          clearInterval(confirmation);
          await new Promise((res) => setTimeout(() => res(1), 1000));
          return res(true);
        }
      }, checkInterval);
    });
  }

  async submit(tx: string): Promise<string> {
    const client = await this.rpc("submitTransaction", {
      transaction: { cbor: tx },
    });

    return new Promise((res, rej) => {
      client.addEventListener("message", (msg: MessageEvent<string>) => {
        try {
          const { result, error } = JSON.parse(msg.data);

          if (result?.transaction) res(result.transaction.id);
          else rej(error);
          client.close();
        } catch (e) {
          rej(e);
        }
      }, { once: true });
    });
  }

  private kupmiosUtxosToUtxos(utxos: unknown): Promise<Utxo[]> {
    // deno-lint-ignore no-explicit-any
    return Promise.all((utxos as any).map(async (utxo: any) => {
      return ({
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
        datum: utxo?.datum_type === "inline"
          ? await this.getDatum(utxo.datum_hash)
          : null,
        scriptRef: utxo.script_hash &&
          (await (async () => {
            const {
              script,
              language,
            } = await fetch(
              `${this.kupoUrl}/scripts/${utxo.script_hash}`,
            ).then((res) => res.json());

            if (language === "native") {
              return { type: "Native", script };
            } else if (language === "plutus:v1") {
              return {
                type: "PlutusV1",
                script: Utils.applyDoubleCborEncoding(script),
              };
            } else if (language === "plutus:v2") {
              return {
                type: "PlutusV2",
                script: Utils.applyDoubleCborEncoding(script),
              };
            } else if (language === "plutus:v3") {
              return {
                type: "PlutusV3",
                script: Utils.applyDoubleCborEncoding(script),
              };
            }
          })()),
      }) as Utxo;
    }));
  }

  private async rpc(
    method: string,
    params?: unknown,
  ): Promise<WebSocket> {
    const client = new WebSocket(this.ogmiosUrl);
    await new Promise((res) => {
      client.addEventListener("open", () => res(1), { once: true });
    });
    client.send(JSON.stringify({
      "jsonrpc": "2.0",
      method,
      params,
    }));
    return client;
  }
}
