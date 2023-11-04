import { C } from "../core/mod.ts";
import { applyDoubleCborEncoding, fromHex } from "../utils/mod.ts";
import {
  Address,
  Assets,
  Credential,
  Datum,
  DatumHash,
  Delegation,
  Json,
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

export type MaestroSupportedNetworks = "Mainnet" | "Preprod" | "Preview";

export interface MaestroConfig {
  network: MaestroSupportedNetworks;
  apiKey: string;
  turboSubmit?: boolean; // Read about paid turbo transaction submission feature at https://docs-v1.gomaestro.org/docs/Dapp%20Platform/Turbo%20Transaction.
}

export class Maestro implements Provider {
  url: string;
  apiKey: string;
  turboSubmit: boolean;

  constructor({ network, apiKey, turboSubmit = false }: MaestroConfig) {
    this.url = `https://${network}.gomaestro-api.org/v1`;
    this.apiKey = apiKey;
    this.turboSubmit = turboSubmit;
  }

  async getProtocolParameters(): Promise<ProtocolParameters> {
    const timestampedResult = await fetch(`${this.url}/protocol-params`, {
      headers: this.commonHeaders(),
    }).then((res) => res.json());
    const result = timestampedResult.data;
    // Decimal numbers in Maestro are given as ratio of two numbers represented by string of format "firstNumber/secondNumber".
    const decimalFromRationalString = (str: string): number => {
      const forwardSlashIndex = str.indexOf("/");
      return (
        parseInt(str.slice(0, forwardSlashIndex)) /
        parseInt(str.slice(forwardSlashIndex + 1))
      );
    };
    // To rename keys in an object by the given key-map.
    // deno-lint-ignore no-explicit-any
    const renameKeysAndSort = (obj: any, newKeys: any) => {
      const entries = Object.keys(obj)
        .sort()
        .map((key) => {
          const newKey = newKeys[key] || key;
          return {
            [newKey]: Object.fromEntries(
              Object.entries(obj[key]).sort(([k, _v], [k2, _v2]) =>
                k.localeCompare(k2)
              )
            ),
          };
        });
      return Object.assign({}, ...entries);
    };
    return {
      minFeeA: parseInt(result.min_fee_coefficient),
      minFeeB: parseInt(result.min_fee_constant),
      maxTxSize: parseInt(result.max_tx_size),
      maxValSize: parseInt(result.max_value_size),
      keyDeposit: BigInt(result.stake_key_deposit),
      poolDeposit: BigInt(result.pool_deposit),
      priceMem: decimalFromRationalString(result.prices.memory),
      priceStep: decimalFromRationalString(result.prices.steps),
      maxTxExMem: BigInt(result.max_execution_units_per_transaction.memory),
      maxTxExSteps: BigInt(result.max_execution_units_per_transaction.steps),
      coinsPerUtxoByte: BigInt(result.coins_per_utxo_byte),
      collateralPercentage: parseInt(result.collateral_percentage),
      maxCollateralInputs: parseInt(result.max_collateral_inputs),
      costModels: renameKeysAndSort(result.cost_models, {
        "plutus:v1": "PlutusV1",
        "plutus:v2": "PlutusV2",
      }),
    };
  }

  private async getUtxosInternal(
    addressOrCredential: Address | Credential,
    unit?: Unit
  ): Promise<UTxO[]> {
    const queryPredicate = (() => {
      if (typeof addressOrCredential === "string") {
        return "/addresses/" + addressOrCredential;
      }
      const hash =
        addressOrCredential.type == "Key"
          ? C.Ed25519KeyHash.from_hex(addressOrCredential.hash)
          : C.ScriptHash.from_hex(addressOrCredential.hash);
      let credentialBech32Query = "/addresses/cred/";
      credentialBech32Query +=
        addressOrCredential.type === "Key"
          ? hash.to_bech32("addr_vkh")
          : hash.to_bech32("addr_shared_vkh");

      hash.free();

      return credentialBech32Query;
    })();
    const qparams = new URLSearchParams({
      count: "100",
      ...(unit && { asset: unit }),
    });
    const result: MaestroUtxos = await this.getAllPagesData(
      async (qry: string) =>
        await fetch(qry, { headers: this.commonHeaders() }),
      `${this.url}${queryPredicate}/utxos`,
      qparams,
      "Location: getUtxosInternal. Error: Could not fetch UTxOs from Maestro"
    );
    return result.map(this.maestroUtxoToUtxo);
  }

  getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]> {
    return this.getUtxosInternal(addressOrCredential);
  }

  getUtxosWithUnit(
    addressOrCredential: Address | Credential,
    unit: Unit
  ): Promise<UTxO[]> {
    return this.getUtxosInternal(addressOrCredential, unit);
  }

  async getUtxoByUnit(unit: Unit): Promise<UTxO> {
    const timestampedAddressesResponse = await fetch(
      `${this.url}/assets/${unit}/addresses?count=2`,
      { headers: this.commonHeaders() }
    );
    const timestampedAddresses = await timestampedAddressesResponse.json();
    if (!timestampedAddressesResponse.ok) {
      if (timestampedAddresses.message) {
        throw new Error(timestampedAddresses.message);
      }
      throw new Error(
        "Location: getUtxoByUnit. Error: Couldn't perform query. Received status code: " +
          timestampedAddressesResponse.status
      );
    }
    const addressesWithAmount = timestampedAddresses.data;
    if (addressesWithAmount.length === 0) {
      throw new Error("Location: getUtxoByUnit. Error: Unit not found.");
    }
    if (addressesWithAmount.length > 1) {
      throw new Error(
        "Location: getUtxoByUnit. Error: Unit needs to be an NFT or only held by one address."
      );
    }

    const address = addressesWithAmount[0].address;

    const utxos = await this.getUtxosWithUnit(address, unit);

    if (utxos.length > 1) {
      throw new Error(
        "Location: getUtxoByUnit. Error: Unit needs to be an NFT or only held by one address."
      );
    }

    return utxos[0];
  }

  async getUtxosByOutRef(outRefs: OutRef[]): Promise<UTxO[]> {
    const qry = `${this.url}/transactions/outputs`;
    const body = JSON.stringify(
      outRefs.map(({ txHash, outputIndex }) => `${txHash}#${outputIndex}`)
    );
    const utxos = await this.getAllPagesData<MaestroUtxo>(
      async (qry: string) =>
        await fetch(qry, {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
            ...this.commonHeaders(),
          },
          body: body,
        }),
      qry,
      new URLSearchParams({}),
      "Location: getUtxosByOutRef. Error: Could not fetch UTxOs by references from Maestro"
    );
    return utxos.map(this.maestroUtxoToUtxo);
  }

  async getDelegation(rewardAddress: RewardAddress): Promise<Delegation> {
    const timestampedResultResponse = await fetch(
      `${this.url}/accounts/${rewardAddress}`,
      { headers: this.commonHeaders() }
    );
    if (!timestampedResultResponse.ok) {
      return { poolId: null, rewards: 0n };
    }
    const timestampedResult = await timestampedResultResponse.json();
    const result = timestampedResult.data;
    return {
      poolId: result.delegated_pool || null,
      rewards: BigInt(result.rewards_available),
    };
  }

  async getDatum(datumHash: DatumHash): Promise<Datum> {
    const timestampedResultResponse = await fetch(
      `${this.url}/datum/${datumHash}`,
      {
        headers: this.commonHeaders(),
      }
    );
    if (!timestampedResultResponse.ok) {
      if (timestampedResultResponse.status === 404) {
        throw new Error(`No datum found for datum hash: ${datumHash}`);
      } else {
        throw new Error(
          "Location: getDatum. Error: Couldn't successfully perform query. Received status code: " +
            timestampedResultResponse.status
        );
      }
    }
    const timestampedResult = await timestampedResultResponse.json();
    return timestampedResult.data.bytes;
  }

  awaitTx(txHash: TxHash, checkInterval = 3000): Promise<boolean> {
    return new Promise((res) => {
      const confirmation = setInterval(async () => {
        const isConfirmedResponse = await fetch(
          `${this.url}/transactions/${txHash}/cbor`,
          {
            headers: this.commonHeaders(),
          }
        );
        if (isConfirmedResponse.ok) {
          await isConfirmedResponse.json();
          clearInterval(confirmation);
          await new Promise((res) => setTimeout(() => res(1), 1000));
          return res(true);
        }
      }, checkInterval);
    });
  }

  async submitTx(tx: Transaction): Promise<TxHash> {
    let queryUrl = `${this.url}/txmanager`;
    queryUrl += this.turboSubmit ? "/turbosubmit" : "";
    const response = await fetch(queryUrl, {
      method: "POST",
      headers: {
        "Content-Type": "application/cbor",
        Accept: "text/plain",
        ...this.commonHeaders(),
      },
      body: fromHex(tx),
    });
    const result = await response.text();
    if (!response.ok) {
      if (response.status === 400) throw new Error(result);
      else {
        throw new Error(
          "Could not submit transaction. Received status code: " +
            response.status
        );
      }
    }
    return result;
  }

  private commonHeaders() {
    return { "api-key": this.apiKey, lucid };
  }

  private maestroUtxoToUtxo(result: MaestroUtxo): UTxO {
    return {
      txHash: result.tx_hash,
      outputIndex: result.index,
      assets: (() => {
        const a: Assets = {};
        result.assets.forEach((am) => {
          a[am.unit] = BigInt(am.amount);
        });
        return a;
      })(),
      address: result.address,
      datumHash: result.datum
        ? result.datum.type == "inline"
          ? undefined
          : result.datum.hash
        : undefined,
      datum: result.datum?.bytes,
      scriptRef: result.reference_script
        ? result.reference_script.type == "native"
          ? undefined
          : {
              type:
                result.reference_script.type == "plutusv1"
                  ? "PlutusV1"
                  : "PlutusV2",
              script: applyDoubleCborEncoding(result.reference_script.bytes!),
            }
        : undefined,
    };
  }
  private async getAllPagesData<T>(
    getResponse: (qry: string) => Promise<Response>,
    qry: string,
    paramsGiven: URLSearchParams,
    errorMsg: string
  ): Promise<Array<T>> {
    let nextCursor = null;
    let result: Array<T> = [];
    while (true) {
      if (nextCursor !== null) {
        paramsGiven.set("cursor", nextCursor);
      }
      const response = await getResponse(`${qry}?` + paramsGiven);
      const pageResult = await response.json();
      if (!response.ok) {
        throw new Error(
          `${errorMsg}. Received status code: ${response.status}`
        );
      }
      nextCursor = pageResult.next_cursor;
      result = result.concat(pageResult.data as Array<T>);
      if (nextCursor == null) break;
    }
    return result;
  }
}

type MaestroDatumOptionType = "hash" | "inline";

type MaestroDatumOption = {
  type: MaestroDatumOptionType;
  hash: string;
  bytes?: string; // Hex encoded datum CBOR bytes (`null` if datum type is `hash` and corresponding datum bytes have not been seen on-chain).
  json?: Json;
};

type MaestroScriptType = "native" | "plutusv1" | "plutusv2";

type MaestroScript = {
  hash: string;
  type: MaestroScriptType;
  bytes?: string; // Script bytes (`null` if `native` script).
  json?: Json;
};

type MaestroAsset = {
  unit: string;
  amount: number;
};

type MaestroUtxo = {
  tx_hash: TxHash;
  index: number;
  assets: Array<MaestroAsset>;
  address: Address;
  datum?: MaestroDatumOption;
  reference_script?: MaestroScript;
  // Other fields such as `slot` & `txout_cbor` are ignored.
};

type MaestroUtxos = Array<MaestroUtxo>;

const lucid = packageJson.version; // Lucid version
