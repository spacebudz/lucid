import { C } from "../core/mod.js";
import { applyDoubleCborEncoding, fromHex } from "../utils/mod.js";
import packageJson from "../../package.js";
export class Maestro {
    constructor({ network, apiKey, turboSubmit = false }) {
        Object.defineProperty(this, "url", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "apiKey", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "turboSubmit", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        this.url = `https://${network}.gomaestro-api.org/v1`;
        this.apiKey = apiKey;
        this.turboSubmit = turboSubmit;
    }
    async getProtocolParameters() {
        const timestampedResult = await fetch(`${this.url}/protocol-params`, {
            headers: this.commonHeaders(),
        }).then((res) => res.json());
        const result = timestampedResult.data;
        // Decimal numbers in Maestro are given as ratio of two numbers represented by string of format "firstNumber/secondNumber".
        const decimalFromRationalString = (str) => {
            const forwardSlashIndex = str.indexOf("/");
            return parseInt(str.slice(0, forwardSlashIndex)) / parseInt(str.slice(forwardSlashIndex + 1));
        };
        // To rename keys in an object by the given key-map.
        // deno-lint-ignore no-explicit-any
        const renameKeysAndSort = (obj, newKeys) => {
            const entries = Object.keys(obj).sort().map((key) => {
                const newKey = newKeys[key] || key;
                return {
                    [newKey]: Object.fromEntries(Object.entries(obj[key]).sort(([k, _v], [k2, _v2]) => k.localeCompare(k2))),
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
    async getUtxos(addressOrCredential) {
        const queryPredicate = (() => {
            if (typeof addressOrCredential === "string")
                return "/addresses/" + addressOrCredential;
            let credentialBech32Query = "/addresses/cred/";
            credentialBech32Query += addressOrCredential.type === "Key"
                ? C.Ed25519KeyHash.from_hex(addressOrCredential.hash).to_bech32("addr_vkh")
                : C.ScriptHash.from_hex(addressOrCredential.hash).to_bech32("addr_shared_vkh");
            return credentialBech32Query;
        })();
        let result = [];
        let nextCursor = null;
        while (true) {
            const appendCursorString = nextCursor === null ? "" : `&cursor=${nextCursor}`;
            const response = await fetch(`${this.url}${queryPredicate}/utxos?count=100${appendCursorString}`, { headers: this.commonHeaders() });
            const pageResult = await response.json();
            if (!response.ok) {
                throw new Error("Could not fetch UTxOs from Maestro. Received status code: " + response.status);
            }
            nextCursor = pageResult.next_cursor;
            result = result.concat(pageResult.data);
            if (nextCursor == null)
                break;
        }
        return result.map(this.maestroUtxoToUtxo);
    }
    async getUtxosWithUnit(addressOrCredential, unit) {
        const utxos = await this.getUtxos(addressOrCredential);
        return utxos.filter((utxo) => utxo.assets[unit]);
    }
    async getUtxoByUnit(unit) {
        const timestampedAddressesResponse = await fetch(`${this.url}/assets/${unit}/addresses?count=2`, { headers: this.commonHeaders() });
        const timestampedAddresses = await timestampedAddressesResponse.json();
        if (!timestampedAddressesResponse.ok) {
            if (timestampedAddresses.message)
                throw new Error(timestampedAddresses.message);
            throw new Error("Couldn't perform query. Received status code: " + timestampedAddressesResponse.status);
        }
        const addressesWithAmount = timestampedAddresses.data;
        if (addressesWithAmount.length === 0) {
            throw new Error("Unit not found.");
        }
        if (addressesWithAmount.length > 1) {
            throw new Error("Unit needs to be an NFT or only held by one address.");
        }
        const address = addressesWithAmount[0].address;
        const utxos = await this.getUtxosWithUnit(address, unit);
        if (utxos.length > 1) {
            throw new Error("Unit needs to be an NFT or only held by one address.");
        }
        return utxos[0];
    }
    async getUtxosByOutRef(outRefs) {
        const response = await fetch(`${this.url}/transactions/outputs`, {
            method: "POST",
            headers: {
                'Content-Type': 'application/json',
                ...this.commonHeaders()
            },
            body: JSON.stringify(outRefs.map(({ txHash, outputIndex }) => `${txHash}#${outputIndex}`)),
        });
        if (!response.ok)
            return [];
        const utxos = (await response.json()).data;
        return utxos.map(this.maestroUtxoToUtxo);
    }
    async getDelegation(rewardAddress) {
        const timestampedResultResponse = await fetch(`${this.url}/accounts/${rewardAddress}`, { headers: this.commonHeaders() });
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
    async getDatum(datumHash) {
        const timestampedResultResponse = await fetch(`${this.url}/datum/${datumHash}`, {
            headers: this.commonHeaders(),
        });
        if (!timestampedResultResponse.ok) {
            if (timestampedResultResponse.status === 404)
                throw new Error(`No datum found for datum hash: ${datumHash}`);
            else
                throw new Error("Couldn't successfully perform query. Received status code: " + timestampedResultResponse.status);
        }
        const timestampedResult = await timestampedResultResponse.json();
        return timestampedResult.data.bytes;
    }
    awaitTx(txHash, checkInterval = 3000) {
        return new Promise((res) => {
            const confirmation = setInterval(async () => {
                const isConfirmedResponse = await fetch(`${this.url}/transactions/${txHash}/cbor`, {
                    headers: this.commonHeaders(),
                });
                if (isConfirmedResponse.ok) {
                    await isConfirmedResponse.json();
                    clearInterval(confirmation);
                    await new Promise((res) => setTimeout(() => res(1), 1000));
                    return res(true);
                }
            }, checkInterval);
        });
    }
    async submitTx(tx) {
        let queryUrl = `${this.url}/txmanager`;
        queryUrl += this.turboSubmit ? '/turbosubmit' : '';
        const response = await fetch(queryUrl, {
            method: "POST",
            headers: {
                "Content-Type": "application/cbor",
                "Accept": "text/plain",
                ...this.commonHeaders(),
            },
            body: fromHex(tx),
        });
        const result = await response.text();
        if (!response.ok) {
            if (response.status === 400)
                throw new Error(result);
            else
                throw new Error("Could not submit transaction. Received status code: " + response.status);
        }
        return result;
    }
    commonHeaders() {
        return { "api-key": this.apiKey, lucid };
    }
    maestroUtxoToUtxo(result) {
        return {
            txHash: result.tx_hash,
            outputIndex: result.index,
            assets: (() => {
                const a = {};
                result.assets.forEach((am) => {
                    a[am.unit] = BigInt(am.amount);
                });
                return a;
            })(),
            address: result.address,
            datumHash: result.datum
                ? result.datum.type == "inline" ? undefined : result.datum.hash
                : undefined,
            datum: result.datum?.bytes,
            scriptRef: result.reference_script
                ? result.reference_script.type == "native" ? undefined : {
                    type: result.reference_script.type == "plutusv1"
                        ? "PlutusV1"
                        : "PlutusV2",
                    script: applyDoubleCborEncoding(result.reference_script.bytes),
                }
                : undefined,
        };
    }
}
const lucid = packageJson.version; // Lucid version
