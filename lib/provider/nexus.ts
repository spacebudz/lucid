import {
  type ActiveDelegation,
  type Assets,
  type Credential,
  type Network,
  type OutRef,
  type Provider,
  type RelevantProtocolParameters,
  type Script,
  Utils,
  type Utxo,
} from "../mod.ts";
import denoJson from "../../deno.json" with { type: "json" };

export type NexusSupportedNetworks = "Mainnet" | "Preprod" | "Preview";

type NexusNetworkQuery =
  | "CARDANO_MAINNET"
  | "CARDANO_PREPROD"
  | "CARDANO_PREVIEW";

const NETWORK_QUERY: Record<NexusSupportedNetworks, NexusNetworkQuery> = {
  Mainnet: "CARDANO_MAINNET",
  Preprod: "CARDANO_PREPROD",
  Preview: "CARDANO_PREVIEW",
};

export interface NexusConfig {
  network: NexusSupportedNetworks;
  /** API key issued for your application, sent as the `X-Api-Key` header. */
  apiKey: string;
  /** Defaults to `https://nexus.gerowallet.io`. */
  url?: string;
}

const PAGE_SIZE = 100;
const OUT_REF_BATCH = 100;

/**
 * [Nexus](https://nexus.gerowallet.io) provider — Gero's hosted, multi-chain
 * Cardano data API. One base URL and an `X-Api-Key` header; the network is
 * derived from the key's scope (also sent as `?network=`).
 */
export class Nexus implements Provider {
  url: string;
  apiKey: string;
  network?: Network;
  private networkQuery: NexusNetworkQuery;

  constructor(
    { network, apiKey, url = "https://nexus.gerowallet.io" }: NexusConfig,
  ) {
    this.url = url.replace(/\/+$/, "");
    this.apiKey = apiKey;
    this.network = network;
    this.networkQuery = NETWORK_QUERY[network];
  }

  private headers(): Record<string, string> {
    return { "X-Api-Key": this.apiKey, lucid };
  }

  private endpoint(
    path: string,
    query: Record<string, string | number> = {},
  ): string {
    const url = new URL(`${this.url}${path}`);
    url.searchParams.set("network", this.networkQuery);
    for (const [key, value] of Object.entries(query)) {
      url.searchParams.set(key, String(value));
    }
    return url.toString();
  }

  /** Fetch + parse JSON, throwing the API's error envelope message on any non-2xx. */
  private async fetchJson<T>(
    url: string,
    init: { method?: string; headers?: Record<string, string>; body?: string } =
      {},
  ): Promise<T> {
    const response = await fetch(url, {
      method: init.method,
      headers: { ...this.headers(), ...(init.headers ?? {}) },
      body: init.body,
    });
    if (!response.ok) {
      let message = `Nexus request failed with status ${response.status}`;
      try {
        const body = await response.json();
        if (typeof body?.message === "string") message = body.message;
        else if (typeof body?.error === "string") message = body.error;
      } catch {
        // Non-JSON error body: keep the status-based message.
      }
      throw new Error(message);
    }
    return await response.json() as T;
  }

  async getProtocolParameters(): Promise<RelevantProtocolParameters> {
    const result = await this.fetchJson<NexusProtocolParameters>(
      this.endpoint("/api/epoch/latest/parameters"),
    );

    const costModels: Record<string, number[]> = {};
    for (const [key, model] of Object.entries(result.costModels ?? {})) {
      const version = normalizeCostModelKey(key);
      if (version) costModels[version] = costModelToArray(model);
    }

    return {
      minFeeA: result.minFeeA,
      minFeeB: result.minFeeB,
      maxTxSize: result.maxTxSize,
      maxValSize: Number(result.maxValSize),
      keyDeposit: Number(result.keyDeposit),
      poolDeposit: Number(result.poolDeposit),
      priceMem: result.priceMem,
      priceStep: result.priceStep,
      maxTxExMem: Number(result.maxTxExMem),
      maxTxExSteps: Number(result.maxTxExSteps),
      coinsPerUtxoByte: Number(result.coinsPerUtxoSize),
      collateralPercentage: result.collateralPercent,
      maxCollateralInputs: result.maxCollateralInputs,
      costModels,
      minfeeRefscriptCostPerByte: result.minFeeRefScriptCostPerByte ?? 0,
    };
  }

  private async paginateUtxos(
    pathFor: (page: number) => string,
  ): Promise<Utxo[]> {
    const utxos: Utxo[] = [];
    for (let page = 1;; page++) {
      const batch = await this.fetchJson<NexusAddressUtxo[]>(pathFor(page));
      if (!Array.isArray(batch) || batch.length === 0) break;
      for (const dto of batch) utxos.push(addressUtxoToUtxo(dto));
      if (batch.length < PAGE_SIZE) break;
    }
    return utxos;
  }

  getUtxos(addressOrCredential: string | Credential): Promise<Utxo[]> {
    const base = typeof addressOrCredential === "string"
      ? `/api/addresses/${encodeURIComponent(addressOrCredential)}/utxos`
      : `/api/addresses/cred/${
        encodeURIComponent(addressOrCredential.hash)
      }/utxos`;
    return this.paginateUtxos((page) =>
      this.endpoint(base, { page, pageSize: PAGE_SIZE })
    );
  }

  async getUtxosWithUnit(
    addressOrCredential: string | Credential,
    unit: string,
  ): Promise<Utxo[]> {
    if (typeof addressOrCredential === "string") {
      const base = `/api/addresses/${
        encodeURIComponent(addressOrCredential)
      }/utxos/${encodeURIComponent(unit)}`;
      return this.paginateUtxos((page) =>
        this.endpoint(base, { page, pageSize: PAGE_SIZE })
      );
    }
    // Nexus has no credential+asset endpoint; filter the credential's UTxOs client-side.
    const utxos = await this.getUtxos(addressOrCredential);
    return utxos.filter((utxo) => utxo.assets[unit] !== undefined);
  }

  async getUtxoByUnit(unit: string): Promise<Utxo> {
    const located = await this.fetchJson<NexusAddressUtxo[]>(
      this.endpoint(`/api/assets/${encodeURIComponent(unit)}/utxos`, {
        page: 1,
        pageSize: PAGE_SIZE,
      }),
    );
    const unspent = located.filter((dto) => dto.spent !== true);
    if (unspent.length === 0) throw new Error("Unit not found.");
    if (unspent.length > 1) {
      throw new Error("Unit needs to be an NFT or only held by one address.");
    }
    // The asset endpoint locates the out-ref but may omit datum/script enrichment;
    // the out-ref endpoint always carries the full output, so resolve through it.
    const [full] = await this.getUtxosByOutRef([
      { txHash: unspent[0].txHash, outputIndex: unspent[0].txIndex },
    ]);
    return full ?? addressUtxoToUtxo(unspent[0]);
  }

  async getUtxosByOutRef(outRefs: OutRef[]): Promise<Utxo[]> {
    const utxos: Utxo[] = [];
    for (let i = 0; i < outRefs.length; i += OUT_REF_BATCH) {
      const chunk = outRefs.slice(i, i + OUT_REF_BATCH).map((
        { txHash, outputIndex },
      ) => ({ txHash, outputIndex }));
      const dtos = await this.fetchJson<NexusOutRefUtxo[]>(
        this.endpoint("/api/transactions/utxos"),
        {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(chunk),
        },
      );
      for (const dto of dtos) utxos.push(outRefUtxoToUtxo(dto));
    }
    return utxos.filter((utxo) =>
      outRefs.some((outRef) =>
        utxo.txHash === outRef.txHash && utxo.outputIndex === outRef.outputIndex
      )
    );
  }

  async getDelegation(rewardAddress: string): Promise<ActiveDelegation> {
    const response = await fetch(
      this.endpoint(`/api/account/${encodeURIComponent(rewardAddress)}/info`),
      { headers: this.headers() },
    );
    if (!response.ok) return { poolId: null, drep: null, rewards: 0n };
    const result: NexusAccountInfo = await response.json();
    return {
      poolId: result.poolId ?? null,
      drep: null,
      rewards: BigInt(result.withdrawableAmount ?? 0),
    };
  }

  async getDatum(datumHash: string): Promise<string> {
    const response = await fetch(
      this.endpoint(`/api/scripts/datum/${encodeURIComponent(datumHash)}`),
      { headers: this.headers() },
    );
    if (!response.ok) {
      throw new Error(`No datum found for datum hash: ${datumHash}`);
    }
    const result: NexusDatum = await response.json();
    if (!result.cbor) {
      throw new Error(`No datum found for datum hash: ${datumHash}`);
    }
    return result.cbor;
  }

  awaitTx(txHash: string, checkInterval = 3000): Promise<boolean> {
    return new Promise((res) => {
      const confirmation = setInterval(async () => {
        // Poll the transaction-details endpoint (not `/cbor`, which is not served on
        // every deployment); a 200 means the tx has been indexed.
        const response = await fetch(
          this.endpoint(`/api/transactions/${encodeURIComponent(txHash)}`),
          { headers: this.headers() },
        );
        if (response.ok) {
          await response.body?.cancel();
          clearInterval(confirmation);
          await new Promise((r) => setTimeout(() => r(1), 1000));
          return res(true);
        }
      }, checkInterval);
    });
  }

  async submit(tx: string): Promise<string> {
    const response = await fetch(this.endpoint("/api/transactions/submit"), {
      method: "POST",
      headers: { "Content-Type": "text/plain", ...this.headers() },
      body: tx,
    });
    const result = await response.text();
    if (!response.ok) throw new Error(result);
    // Nexus returns the tx hash as a possibly quoted/padded text body.
    return result.trim().replace(/^"|"$/g, "");
  }
}

// ---------------------------------------------------------------------------
// Mappers
// ---------------------------------------------------------------------------

const COST_MODEL_KEY = /plutus[:_\s-]?v?(1|2|3)/i;

function normalizeCostModelKey(key: string): string | undefined {
  const match = COST_MODEL_KEY.exec(key);
  return match ? `PlutusV${match[1]}` : undefined;
}

function costModelToArray(model: Record<string, number>): number[] {
  const keys = Object.keys(model);
  const allNumeric = keys.length > 0 && keys.every((k) => /^\d+$/.test(k));
  const ordered = allNumeric
    ? [...keys].sort((a, b) => Number(a) - Number(b))
    : keys;
  return ordered.map((k) => model[k]);
}

// Reference-script CBOR is language-tagged (`82 0X <script>`): 00 native, 01/02/03
// Plutus V1/V2/V3. The tag selects the type and is stripped from the script bytes.
const SCRIPT_REF_TAG: Record<string, Script["type"]> = {
  "8200": "Native",
  "8201": "PlutusV1",
  "8202": "PlutusV2",
  "8203": "PlutusV3",
};

function unwrapScriptRef(cborHex: string): Script {
  const type = SCRIPT_REF_TAG[cborHex.slice(0, 4).toLowerCase()];
  if (type) {
    const script = cborHex.slice(4);
    return type === "Native"
      ? { type, script }
      : { type, script: Utils.applyDoubleCborEncoding(script) };
  }
  return { type: "PlutusV2", script: Utils.applyDoubleCborEncoding(cborHex) };
}

function normalizeScriptType(type: string | null | undefined): Script["type"] {
  const t = (type ?? "").toLowerCase();
  if (t.includes("v1")) return "PlutusV1";
  if (t.includes("v3")) return "PlutusV3";
  if (t.includes("native") || t.includes("timelock")) return "Native";
  return "PlutusV2";
}

function addressUtxoToUtxo(dto: NexusAddressUtxo): Utxo {
  const assets: Assets = { lovelace: BigInt(dto.value) };
  for (const asset of dto.assets ?? []) {
    assets[asset.unit] = BigInt(asset.quantity);
  }
  const datum = dto.inlineDatum?.bytes || undefined;
  let scriptRef: Script | undefined;
  if (dto.referenceScript?.bytes) {
    scriptRef =
      SCRIPT_REF_TAG[dto.referenceScript.bytes.slice(0, 4).toLowerCase()]
        ? unwrapScriptRef(dto.referenceScript.bytes)
        : {
          type: normalizeScriptType(dto.referenceScript.type),
          script: Utils.applyDoubleCborEncoding(dto.referenceScript.bytes),
        };
  }
  return {
    txHash: dto.txHash,
    outputIndex: dto.txIndex,
    address: dto.address,
    assets,
    datumHash: datum ? undefined : dto.datumHash || undefined,
    datum,
    scriptRef,
  };
}

function outRefUtxoToUtxo(dto: NexusOutRefUtxo): Utxo {
  const assets: Assets = {};
  for (const amount of dto.amounts ?? []) {
    assets[amount.unit] = BigInt(amount.quantity);
  }
  if (assets.lovelace === undefined) {
    assets.lovelace = BigInt(dto.lovelace_amount ?? 0);
  }
  const datum = dto.inline_datum || undefined;
  return {
    txHash: dto.tx_hash,
    outputIndex: dto.output_index,
    address: dto.owner_addr,
    assets,
    datumHash: datum ? undefined : dto.data_hash || undefined,
    datum,
    scriptRef: dto.script_ref ? unwrapScriptRef(dto.script_ref) : undefined,
  };
}

// ---------------------------------------------------------------------------
// Nexus wire types (only the fields consumed here)
// ---------------------------------------------------------------------------

type NexusProtocolParameters = {
  minFeeA: number;
  minFeeB: number;
  maxTxSize: number;
  maxValSize: string;
  keyDeposit: string;
  poolDeposit: string;
  priceMem: number;
  priceStep: number;
  maxTxExMem: string;
  maxTxExSteps: string;
  coinsPerUtxoSize: string;
  collateralPercent: number;
  maxCollateralInputs: number;
  minFeeRefScriptCostPerByte?: number;
  costModels: Record<string, Record<string, number>>;
};

type NexusAssetBalance = { unit: string; quantity: string };

type NexusReferenceScript = { type?: string; bytes?: string };

/** `GET /api/addresses/.../utxos` (camelCase). */
type NexusAddressUtxo = {
  txHash: string;
  txIndex: number;
  address: string;
  value: string;
  datumHash?: string;
  inlineDatum?: { bytes?: string };
  referenceScript?: NexusReferenceScript;
  assets?: NexusAssetBalance[];
  spent?: boolean;
};

/** `POST /api/transactions/utxos` (snake_case). */
type NexusOutRefUtxo = {
  tx_hash: string;
  output_index: number;
  owner_addr: string;
  amounts?: NexusAssetBalance[];
  lovelace_amount?: number;
  data_hash?: string;
  inline_datum?: string;
  script_ref?: string;
};

type NexusAccountInfo = { poolId?: string; withdrawableAmount?: string };

type NexusDatum = { cbor?: string };

const lucid = denoJson.version; // Lucid version, sent as a header.
