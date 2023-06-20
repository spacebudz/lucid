import { C } from "../core/mod.js";
import { applyDoubleCborEncoding, fromHex } from "../utils/mod.js";
import packageJson from "../../package.js";
export class Maestro {
    constructor(url, apiKey = "") {
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
        // Return the given `url` without the last '/' if it exists.
        this.url =
            // Safe even if `url` is an empty string.
            url.slice(-1) === "/" ? url.slice(0, -1) : url;
        this.apiKey = apiKey;
    }
    async getProtocolParameters() {
        const result = await fetch(`${this.url}/protocol-params`, {
            headers: this.commonHeaders(),
        }).then((res) => res.json());
        // Decimal numbers in Maestro are given as ratio of two numbers represented by string of format "firstNumber/secondNumber".
        const decimalFromRationalString = (str) => {
            const forwardSlashIndex = str.indexOf("/");
            return parseInt(str.slice(0, forwardSlashIndex)) / parseInt(str.slice(forwardSlashIndex + 1));
        };
        // To rename keys in an object by the given key-map.
        // deno-lint-ignore no-explicit-any
        const renameKeysAndSort = (obj, newKeys) => {
            const entries = Object.keys(obj).map((key) => {
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
                return addressOrCredential;
            const credentialBech32 = addressOrCredential.type === "Key"
                ? C.Ed25519KeyHash.from_hex(addressOrCredential.hash).to_bech32("addr_vkh")
                : C.ScriptHash.from_hex(addressOrCredential.hash).to_bech32("script");
            return credentialBech32;
        })();
        let result = [];
        let page = 1;
        while (true) {
            const response = await fetch(`${this.url}/addresses/${queryPredicate}/utxos?page=${page}`, { headers: this.commonHeaders() });
            const pageResult = await response.json();
            if (!response.ok) {
                throw new Error("Could not fetch UTxOs from Maestro. Try again.");
            }
            result = result.concat(pageResult);
            if (pageResult.length <= 0)
                break;
            page++;
        }
        return result.map(this.maestroUtxoToUtxo);
    }
    async getUtxosWithUnit(addressOrCredential, unit) {
        const utxos = await this.getUtxos(addressOrCredential);
        return utxos.filter((utxo) => utxo.assets[unit]);
    }
    async getUtxoByUnit(unit) {
        const addresses = await fetch(`${this.url}/assets/${unit}/addresses?count=2`, { headers: this.commonHeaders() }).then((res) => res.json());
        if (addresses.length === 0) { // In case of invalid parameters also we get an empty list.
            throw new Error("Unit not found.");
        }
        if (addresses.length > 1) {
            throw new Error("Unit needs to be an NFT or only held by one address.");
        }
        const address = addresses[0];
        const utxos = await this.getUtxosWithUnit(address, unit);
        if (utxos.length > 1) {
            throw new Error("Unit needs to be an NFT or only held by one address.");
        }
        return utxos[0];
    }
    async getUtxosByOutRef(outRefs) {
        const utxos = await Promise.all(outRefs.map(async (outRef) => {
            const result = await fetch(`${this.url}/transactions/${outRef.txHash}/outputs/${outRef.outputIndex}/utxo`, { headers: this.commonHeaders() }).then((res) => res.json());
            if (!result || result.message) {
                return [];
            }
            return [this.maestroUtxoToUtxo(result)];
        }));
        return utxos.reduce((acc, utxo) => acc.concat(utxo), []);
    }
    async getDelegation(rewardAddress) {
        const result = await fetch(`${this.url}/accounts/${rewardAddress}`, { headers: this.commonHeaders() }).then((res) => res.json());
        if (!result || result.message) {
            return { poolId: null, rewards: 0n };
        }
        return {
            poolId: result.delegated_pool || null,
            rewards: BigInt(result.rewards_available),
        };
    }
    async getDatum(datumHash) {
        const result = await fetch(`${this.url}/datum/${datumHash}`, {
            headers: this.commonHeaders(),
        })
            .then((res) => res.json());
        if (!result || result.message) {
            throw new Error(`No datum found for datum hash: ${datumHash}`);
        }
        return result.bytes;
    }
    awaitTx(txHash, checkInterval = 3000) {
        return new Promise((res) => {
            const confirmation = setInterval(async () => {
                const isConfirmed = await fetch(`${this.url}/transactions/${txHash}/cbor`, {
                    headers: this.commonHeaders(),
                }).then((res) => res.json());
                if (isConfirmed && !isConfirmed.message) {
                    clearInterval(confirmation);
                    await new Promise((res) => setTimeout(() => res(1), 1000));
                    return res(true);
                }
            }, checkInterval);
        });
    }
    async submitTx(tx) {
        const response = await fetch(`${this.url}/txmanager`, {
            method: "POST",
            headers: {
                "Content-Type": "application/cbor",
                ...this.commonHeaders(),
            },
            body: fromHex(tx),
        });
        const result = await response.text();
        if (!response.ok) {
            if (response.status === 400)
                throw new Error(result);
            else
                throw new Error("Could not submit transaction.");
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
                    a[am.unit.replace("#", "")] = BigInt(am.quantity);
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
