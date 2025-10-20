import {
  type ActiveDelegation,
  type Credential,
  fromHex,
  type Network,
  type OutRef,
  type Provider,
  type RelevantProtocolParameters,
  Utils,
  type Utxo,
} from "../mod.ts";
import denoJson from "../../deno.json" with { type: "json" };

export class Blockfrost implements Provider {
  url: string;
  projectId: string;
  network?: Network;

  constructor(url: string, projectId?: string) {
    this.url = url;
    this.projectId = projectId || "";
    if (url.includes("mainnet")) this.network = "Mainnet";
    else if (url.includes("preprod")) this.network = "Preprod";
    else if (url.includes("preview")) this.network = "Preview";
  }

  async getProtocolParameters(): Promise<RelevantProtocolParameters> {
    const result = await fetch(`${this.url}/epochs/latest/parameters`, {
      headers: { project_id: this.projectId, lucid },
    }).then((res) => res.json());

    if (!result) throw new Error("Failed to fetch protocal parameters");

    if (result.error) {
      throw new Error(
        `${result.status_code} ${result.error}:  ${result.message}`,
      );
    }

    return {
      minFeeA: parseInt(result.min_fee_a),
      minFeeB: parseInt(result.min_fee_b),
      maxTxSize: parseInt(result.max_tx_size),
      maxValSize: parseInt(result.max_val_size),
      keyDeposit: parseInt(result.key_deposit),
      poolDeposit: parseInt(result.pool_deposit),
      priceMem: parseFloat(result.price_mem),
      priceStep: parseFloat(result.price_step),
      maxTxExMem: parseInt(result.max_tx_ex_mem),
      maxTxExSteps: parseInt(result.max_tx_ex_steps),
      coinsPerUtxoByte: parseInt(result.coins_per_utxo_size),
      collateralPercentage: parseInt(result.collateral_percent),
      maxCollateralInputs: parseInt(result.max_collateral_inputs),
      costModels: Object.fromEntries(
        Object.entries(result.cost_models).map((
          [plutus, costModel],
        ) => [plutus, Object.values(costModel as Record<string, number>)]),
      ),
      minfeeRefscriptCostPerByte: parseInt(
        result.min_fee_ref_script_cost_per_byte,
      ),
    };
  }

  async getUtxos(addressOrCredential: string | Credential): Promise<Utxo[]> {
    const queryPredicate = (() => {
      if (typeof addressOrCredential === "string") return addressOrCredential;
      const credentialBech32 = addressOrCredential.type === "Key"
        ? Utils.encodeBech32("addr_vkh", addressOrCredential.hash)
        : Utils.encodeBech32("addr_vkh", addressOrCredential.hash); // should be 'script' (CIP-0005)
      return credentialBech32;
    })();
    let result: BlockfrostUtxoResult = [];
    let page = 1;
    while (true) {
      const pageResult: BlockfrostUtxoResult | BlockfrostUtxoError =
        await fetch(
          `${this.url}/addresses/${queryPredicate}/utxos?page=${page}`,
          { headers: { project_id: this.projectId, lucid } },
        ).then((res) => res.json());
      if ((pageResult as BlockfrostUtxoError).error) {
        if ((pageResult as BlockfrostUtxoError).status_code === 404) {
          return [];
        } else {
          throw new Error("Could not fetch UTxOs from Blockfrost. Try again.");
        }
      }
      result = result.concat(pageResult as BlockfrostUtxoResult);
      if ((pageResult as BlockfrostUtxoResult).length <= 0) break;
      page++;
    }

    return this.blockfrostUtxosToUtxos(result);
  }

  async getUtxosWithUnit(
    addressOrCredential: string | Credential,
    unit: string,
  ): Promise<Utxo[]> {
    const queryPredicate = (() => {
      if (typeof addressOrCredential === "string") return addressOrCredential;
      const credentialBech32 = addressOrCredential.type === "Key"
        ? Utils.encodeBech32("addr_vkh", addressOrCredential.hash)
        : Utils.encodeBech32("addr_vkh", addressOrCredential.hash); // should be 'script' (CIP-0005)
      return credentialBech32;
    })();
    let result: BlockfrostUtxoResult = [];
    let page = 1;
    while (true) {
      const pageResult: BlockfrostUtxoResult | BlockfrostUtxoError =
        await fetch(
          `${this.url}/addresses/${queryPredicate}/utxos/${unit}?page=${page}`,
          { headers: { project_id: this.projectId, lucid } },
        ).then((res) => res.json());
      if ((pageResult as BlockfrostUtxoError).error) {
        if ((pageResult as BlockfrostUtxoError).status_code === 404) {
          return [];
        } else {
          throw new Error("Could not fetch UTxOs from Blockfrost. Try again.");
        }
      }
      result = result.concat(pageResult as BlockfrostUtxoResult);
      if ((pageResult as BlockfrostUtxoResult).length <= 0) break;
      page++;
    }

    return this.blockfrostUtxosToUtxos(result);
  }

  async getUtxoByUnit(unit: string): Promise<Utxo> {
    const addresses = await fetch(
      `${this.url}/assets/${unit}/addresses?count=2`,
      { headers: { project_id: this.projectId, lucid } },
    ).then((res) => res.json());

    if (!addresses || addresses.error) {
      throw new Error("Unit not found.");
    }
    if (addresses.length > 1) {
      throw new Error("Unit needs to be an NFT or only held by one address.");
    }

    const address = addresses[0].address;

    const utxos = await this.getUtxosWithUnit(address, unit);

    if (utxos.length > 1) {
      throw new Error("Unit needs to be an NFT or only held by one address.");
    }

    return utxos[0];
  }

  async getUtxosByOutRef(outRefs: OutRef[]): Promise<Utxo[]> {
    // TODO: Make sure old already spent UTxOs are not retrievable.
    const queryHashes = [...new Set(outRefs.map((outRef) => outRef.txHash))];
    const utxos = await Promise.all(queryHashes.map(async (txHash) => {
      const result = await fetch(
        `${this.url}/txs/${txHash}/utxos`,
        { headers: { project_id: this.projectId, lucid } },
      ).then((res) => res.json());
      if (!result || result.error) {
        return [];
      }
      const utxosResult: BlockfrostUtxoResult = result.outputs.map((
        // deno-lint-ignore no-explicit-any
        r: any,
      ) => ({
        ...r,
        tx_hash: txHash,
      }));
      return this.blockfrostUtxosToUtxos(utxosResult);
    }));

    return utxos.reduce((acc, utxos) => acc.concat(utxos), []).filter((utxo) =>
      outRefs.some((outRef) =>
        utxo.txHash === outRef.txHash && utxo.outputIndex === outRef.outputIndex
      )
    );
  }

  async getDelegation(rewardAddress: string): Promise<ActiveDelegation> {
    const result = await fetch(
      `${this.url}/accounts/${rewardAddress}`,
      { headers: { project_id: this.projectId, lucid } },
    ).then((res) => res.json());
    if (!result || result.error) {
      return { poolId: null, drep: null, rewards: 0n };
    }

    const drep: ActiveDelegation["drep"] = result.drep_id
      ? result.drep_id.includes("abstain")
        ? "Abstain"
        : result.drep_id.includes("confidence")
        ? "NoConfidence"
        : { Id: result.drep_id }
      : null;

    return {
      poolId: result.pool_id || null,
      drep,
      rewards: BigInt(result.withdrawable_amount),
    };
  }

  async getDatum(datumHash: string): Promise<string> {
    const datum = await fetch(
      `${this.url}/scripts/datum/${datumHash}/cbor`,
      {
        headers: { project_id: this.projectId, lucid },
      },
    )
      .then((res) => res.json())
      .then((res) => res.cbor);
    if (!datum || datum.error) {
      throw new Error(`No datum found for datum hash: ${datumHash}`);
    }
    return datum;
  }

  awaitTx(txHash: string, checkInterval = 3000): Promise<boolean> {
    return new Promise((res) => {
      const confirmation = setInterval(async () => {
        const isConfirmed = await fetch(`${this.url}/txs/${txHash}`, {
          headers: { project_id: this.projectId, lucid },
        }).then((res) => res.json());
        if (isConfirmed && !isConfirmed.error) {
          clearInterval(confirmation);
          await new Promise((res) => setTimeout(() => res(1), 1000));
          return res(true);
        }
      }, checkInterval);
    });
  }

  async submit(tx: string): Promise<string> {
    const result = await fetch(`${this.url}/tx/submit`, {
      method: "POST",
      headers: {
        "Content-Type": "application/cbor",
        project_id: this.projectId,
        lucid,
      },
      body: fromHex(tx) as BufferSource,
    }).then((res) => res.json());
    if (!result || result.error) {
      if (result?.status_code === 400) throw new Error(result.message);
      else throw new Error("Could not submit transaction.");
    }
    return result;
  }

  private async blockfrostUtxosToUtxos(
    result: BlockfrostUtxoResult,
  ): Promise<Utxo[]> {
    return (await Promise.all(
      result.map(async (r) => ({
        txHash: r.tx_hash,
        outputIndex: r.output_index,
        assets: Object.fromEntries(
          r.amount.map(({ unit, quantity }) => [unit, BigInt(quantity)]),
        ),
        address: r.address,
        datumHash: (!r.inline_datum && r.data_hash) || undefined,
        datum: r.inline_datum || undefined,
        scriptRef: r.reference_script_hash
          ? (await (async () => {
            const {
              type,
            } = await fetch(
              `${this.url}/scripts/${r.reference_script_hash}`,
              {
                headers: { project_id: this.projectId, lucid },
              },
            ).then((res) => res.json());
            // TODO: support native scripts
            if (type === "Native" || type === "native") {
              throw new Error("Native script ref not implemented!");
            }

            const { cbor: script } = await fetch(
              `${this.url}/scripts/${r.reference_script_hash}/cbor`,
              { headers: { project_id: this.projectId, lucid } },
            ).then((res) => res.json());
            return {
              type: type[0].toUpperCase() + type.slice(1),
              script: Utils.applyDoubleCborEncoding(script),
            };
          })())
          : undefined,
      })),
    )) as Utxo[];
  }
}

type BlockfrostUtxoResult = Array<{
  tx_hash: string;
  output_index: number;
  address: string;
  amount: Array<{ unit: string; quantity: string }>;
  data_hash?: string;
  inline_datum?: string;
  reference_script_hash?: string;
}>;

type BlockfrostUtxoError = {
  status_code: number;
  error: unknown;
};

const lucid = denoJson.version; // Lucid version
