import { C } from "../core/mod.ts";
import { applyDoubleCborEncoding, fromHex, toHex } from "../utils/mod.ts";
import {
  Address,
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
import packageJson from "../../package.json" assert { type: "json" };
import { FreeableBucket, Freeables } from "../utils/freeable.ts";

export class Blockfrost implements Provider {
  url: string;
  projectId: string;

  constructor(url: string, projectId?: string) {
    this.url = url;
    this.projectId = projectId || "";
  }

  async getProtocolParameters(): Promise<ProtocolParameters> {
    const result = await fetch(`${this.url}/epochs/latest/parameters`, {
      headers: { project_id: this.projectId, lucid },
    }).then((res) => res.json());

    return {
      minFeeA: parseInt(result.min_fee_a),
      minFeeB: parseInt(result.min_fee_b),
      maxTxSize: parseInt(result.max_tx_size),
      maxValSize: parseInt(result.max_val_size),
      keyDeposit: BigInt(result.key_deposit),
      poolDeposit: BigInt(result.pool_deposit),
      priceMem: parseFloat(result.price_mem),
      priceStep: parseFloat(result.price_step),
      maxTxExMem: BigInt(result.max_tx_ex_mem),
      maxTxExSteps: BigInt(result.max_tx_ex_steps),
      coinsPerUtxoByte: BigInt(result.coins_per_utxo_size),
      collateralPercentage: parseInt(result.collateral_percent),
      maxCollateralInputs: parseInt(result.max_collateral_inputs),
      costModels: result.cost_models,
    };
  }

  async getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]> {
    const queryPredicate = (() => {
      if (typeof addressOrCredential === "string") return addressOrCredential;
      const hash =
        addressOrCredential.type === "Key"
          ? C.Ed25519KeyHash.from_hex(addressOrCredential.hash)
          : C.ScriptHash.from_hex(addressOrCredential.hash);

      const credentialBech32 = hash.to_bech32("addr_vkh"); // should be 'script' according to CIP-0005, but to maintain bakcwards compatabiltiy I am not changing this
      hash.free();
      return credentialBech32;
    })();
    let result: BlockfrostUtxoResult = [];
    let page = 1;
    while (true) {
      const pageResult: BlockfrostUtxoResult | BlockfrostUtxoError =
        await fetch(
          `${this.url}/addresses/${queryPredicate}/utxos?page=${page}`,
          { headers: { project_id: this.projectId, lucid } }
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
    addressOrCredential: Address | Credential,
    unit: Unit
  ): Promise<UTxO[]> {
    const queryPredicate = (() => {
      if (typeof addressOrCredential === "string") return addressOrCredential;
      const hash =
        addressOrCredential.type === "Key"
          ? C.Ed25519KeyHash.from_hex(addressOrCredential.hash)
          : C.ScriptHash.from_hex(addressOrCredential.hash);

      const credentialBech32 = hash.to_bech32("addr_vkh"); // should be 'script' according to CIP-0005, but to maintain bakcwards compatabiltiy I am not changing this
      hash.free();
      return credentialBech32;
    })();
    let result: BlockfrostUtxoResult = [];
    let page = 1;
    while (true) {
      const pageResult: BlockfrostUtxoResult | BlockfrostUtxoError =
        await fetch(
          `${this.url}/addresses/${queryPredicate}/utxos/${unit}?page=${page}`,
          { headers: { project_id: this.projectId, lucid } }
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

  async getUtxoByUnit(unit: Unit): Promise<UTxO> {
    const addresses = await fetch(
      `${this.url}/assets/${unit}/addresses?count=2`,
      { headers: { project_id: this.projectId, lucid } }
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

  async getUtxosByOutRef(outRefs: OutRef[]): Promise<UTxO[]> {
    // TODO: Make sure old already spent UTxOs are not retrievable.
    const queryHashes = [...new Set(outRefs.map((outRef) => outRef.txHash))];
    const utxos = await Promise.all(
      queryHashes.map(async (txHash) => {
        const result = await fetch(`${this.url}/txs/${txHash}/utxos`, {
          headers: { project_id: this.projectId, lucid },
        }).then((res) => res.json());
        if (!result || result.error) {
          return [];
        }
        const utxosResult: BlockfrostUtxoResult = result.outputs.map(
          (
            // deno-lint-ignore no-explicit-any
            r: any
          ) => ({
            ...r,
            tx_hash: txHash,
          })
        );
        return this.blockfrostUtxosToUtxos(utxosResult);
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
    const result = await fetch(`${this.url}/accounts/${rewardAddress}`, {
      headers: { project_id: this.projectId, lucid },
    }).then((res) => res.json());
    if (!result || result.error) {
      return { poolId: null, rewards: 0n };
    }
    return {
      poolId: result.pool_id || null,
      rewards: BigInt(result.withdrawable_amount),
    };
  }

  async getDatum(datumHash: DatumHash): Promise<Datum> {
    const datum = await fetch(`${this.url}/scripts/datum/${datumHash}/cbor`, {
      headers: { project_id: this.projectId, lucid },
    })
      .then((res) => res.json())
      .then((res) => res.cbor);
    if (!datum || datum.error) {
      throw new Error(`No datum found for datum hash: ${datumHash}`);
    }
    return datum;
  }

  awaitTx(txHash: TxHash, checkInterval = 3000): Promise<boolean> {
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

  async submitTx(tx: Transaction): Promise<TxHash> {
    const result = await fetch(`${this.url}/tx/submit`, {
      method: "POST",
      headers: {
        "Content-Type": "application/cbor",
        project_id: this.projectId,
        lucid,
      },
      body: fromHex(tx),
    }).then((res) => res.json());
    if (!result || result.error) {
      if (result?.status_code === 400) throw new Error(result.message);
      else throw new Error("Could not submit transaction.");
    }
    return result;
  }

  private async blockfrostUtxosToUtxos(
    result: BlockfrostUtxoResult
  ): Promise<UTxO[]> {
    return (await Promise.all(
      result.map(async (r) => ({
        txHash: r.tx_hash,
        outputIndex: r.output_index,
        assets: Object.fromEntries(
          r.amount.map(({ unit, quantity }) => [unit, BigInt(quantity)])
        ),
        address: r.address,
        datumHash: (!r.inline_datum && r.data_hash) || undefined,
        datum: r.inline_datum || undefined,
        scriptRef: r.reference_script_hash
          ? await (async () => {
              const { type } = await fetch(
                `${this.url}/scripts/${r.reference_script_hash}`,
                {
                  headers: { project_id: this.projectId, lucid },
                }
              ).then((res) => res.json());
              // TODO: support native scripts
              if (type === "Native" || type === "native") {
                throw new Error("Native script ref not implemented!");
              }
              const { cbor: script } = await fetch(
                `${this.url}/scripts/${r.reference_script_hash}/cbor`,
                { headers: { project_id: this.projectId, lucid } }
              ).then((res) => res.json());
              return {
                type: type === "plutusV1" ? "PlutusV1" : "PlutusV2",
                script: applyDoubleCborEncoding(script),
              };
            })()
          : undefined,
      }))
    )) as UTxO[];
  }
}

/**
 * This function is temporarily needed only, until Blockfrost returns the datum natively in Cbor.
 * The conversion is ambigious, that's why it's better to get the datum directly in Cbor.
 */
export function datumJsonToCbor(json: DatumJson): Datum {
  const convert = (json: DatumJson): C.PlutusData => {
    const bucket: FreeableBucket = [];
    try {
      if (!isNaN(json.int!)) {
        const int = C.BigInt.from_str(json.int!.toString());
        bucket.push(int);
        return C.PlutusData.new_integer(int);
      } else if (json.bytes || !isNaN(Number(json.bytes))) {
        return C.PlutusData.new_bytes(fromHex(json.bytes!));
      } else if (json.map) {
        const m = C.PlutusMap.new();
        bucket.push(m);
        json.map.forEach(({ k, v }: { k: unknown; v: unknown }) => {
          const key = convert(k as DatumJson);
          bucket.push(key);
          const value = convert(v as DatumJson);
          bucket.push(value);

          m.insert(key, value);
        });
        return C.PlutusData.new_map(m);
      } else if (json.list) {
        const l = C.PlutusList.new();
        bucket.push(l);
        json.list.forEach((v: DatumJson) => {
          const value = convert(v);
          bucket.push(value);
          l.add(value);
        });
        return C.PlutusData.new_list(l);
      } else if (!isNaN(json.constructor! as unknown as number)) {
        const l = C.PlutusList.new();
        bucket.push(l);
        json.fields!.forEach((v: DatumJson) => {
          const value = convert(v);
          bucket.push(value);
          l.add(value);
        });
        const constructorIndex = C.BigNum.from_str(
          json.constructor!.toString()
        );
        bucket.push(constructorIndex);
        const plutusData = C.ConstrPlutusData.new(constructorIndex, l);
        bucket.push(plutusData);
        return C.PlutusData.new_constr_plutus_data(plutusData);
      }
      throw new Error("Unsupported type");
    } finally {
      Freeables.free(...bucket);
    }
  };

  const convertedJson = convert(json);
  const cbor = convertedJson.to_bytes();
  convertedJson.free();
  return toHex(cbor);
}

type DatumJson = {
  int?: number;
  bytes?: string;
  list?: Array<DatumJson>;
  map?: Array<{ k: unknown; v: unknown }>;
  fields?: Array<DatumJson>;
  [constructor: string]: unknown; // number; constructor needs to be simulated like this as optional argument
};

type BlockfrostUtxoResult = Array<{
  tx_hash: string;
  output_index: number;
  address: Address;
  amount: Array<{ unit: string; quantity: string }>;
  data_hash?: string;
  inline_datum?: string;
  reference_script_hash?: string;
}>;

type BlockfrostUtxoError = {
  status_code: number;
  error: unknown;
};

const lucid = packageJson.version; // Lucid version
