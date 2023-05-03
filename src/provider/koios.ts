import {
    Address,
    Assets,
    Credential,
    Datum,
    DatumHash,
    Delegation,
    Network,
    OutRef,
    ProtocolParameters,
    Provider,
    RewardAddress,
    Transaction,
    TxHash,
    Unit,
    UTxO,
} from "../types/mod.ts";
import {BackendFactory, KoiosHttpError, KoiosTimeoutError} from "@adabox/koios-ts-client/dist/index.js"
import {C} from "../core/core.ts";
import {applyDoubleCborEncoding, fromHex, fromUnit} from "../utils/utils.ts";

export class KoiosProvider implements Provider {

    private readonly backendService

    constructor(network: Network) {
        if (network === 'Mainnet') {
            this.backendService = BackendFactory.getKoiosMainnetService()
        } else if (network === 'Preview') {
            this.backendService = BackendFactory.getKoiosPreviewService()
        } else if (network === 'Preprod') {
            this.backendService = BackendFactory.getKoiosPreprodService()
        } else {
            throw Error("Unsupported Network Type")
        }
    }

    async getProtocolParameters(): Promise<ProtocolParameters> {
        const result = await this.backendService.getEpochService().getEpochProtocolParameters()

        return {
            minFeeA: parseInt(result[0].min_fee_a),
            minFeeB: parseInt(result[0].min_fee_b),
            maxTxSize: parseInt(result[0].max_tx_size),
            maxValSize: parseInt(result[0].max_val_size),
            keyDeposit: BigInt(result[0].key_deposit),
            poolDeposit: BigInt(result[0].pool_deposit),
            priceMem: parseFloat(result[0].price_mem),
            priceStep: parseFloat(result[0].price_step),
            maxTxExMem: BigInt(result[0].max_tx_ex_mem),
            maxTxExSteps: BigInt(result[0].max_tx_ex_steps),
            coinsPerUtxoByte: BigInt(result[0].coins_per_utxo_size),
            collateralPercentage: parseInt(result[0].collateral_percent),
            maxCollateralInputs: parseInt(result[0].max_collateral_inputs),
            costModels: JSON.parse(result[0].cost_models),
        };
    }

    async getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]> {
        const queryPredicate = (() => {
            if (typeof addressOrCredential === "string") return addressOrCredential;
             // should be 'script' (CIP-0005)
            return addressOrCredential.type === "Key"
                ? C.Ed25519KeyHash.from_hex(addressOrCredential.hash).to_bech32("addr_vkh")
                : C.ScriptHash.from_hex(addressOrCredential.hash).to_bech32("addr_vkh");
        })();
        try {
            const result = await this.backendService.getAddressService().getAddressInformation([queryPredicate])
            if (Array.isArray(result) && result.length > 0 && result[0].utxo_set && result[0].utxo_set.length > 0) {
                return this.koiosUtxosToUtxos(result[0].utxo_set, result[0].address)
            } else {
                return []
            }
        } catch (e) {
            throw new Error("Could not fetch UTxOs from Koios. Try again.");
        }
    }

    private async koiosUtxosToUtxos(result: any, address?: string): Promise<UTxO[]> {
        return (await Promise.all(
            result.map(async (r: any) => ({
                txHash: r.tx_hash,
                outputIndex: r.tx_index,
                assets: (() => {
                    const a: Assets = {};
                    r.asset_list.forEach((am: any) => {
                        a[am.policy_id + am.asset_name] = BigInt(am.quantity);
                    });
                    return a;
                })(),
                address: address ? address : r.payment_addr.bech32,
                datumHash: !r.inline_datum ? r.datum_hash : undefined,
                datum: r.inline_datum,
                scriptRef: {
                    type: r.reference_script ? r.reference_script.type : null,
                    script: r.reference_script ? applyDoubleCborEncoding(r.reference_script.bytes) : null
                },
            })),
        )) as UTxO[];
    }

    async getUtxosWithUnit(addressOrCredential: Address | Credential, unit: Unit): Promise<UTxO[]> {
        const queryPredicate = (() => {
            if (typeof addressOrCredential === "string") return addressOrCredential;
            // should be 'script' (CIP-0005)
            return addressOrCredential.type === "Key"
                ? C.Ed25519KeyHash.from_hex(addressOrCredential.hash).to_bech32("addr_vkh")
                : C.ScriptHash.from_hex(addressOrCredential.hash).to_bech32("addr_vkh");
        })();
        try {
            const result = await this.backendService.getAddressService().getAddressInformation([queryPredicate])
            if (Array.isArray(result) && result.length > 0 && result[0].utxo_set && result[0].utxo_set.length > 0) {
                return (await this.koiosUtxosToUtxos(result[0].utxo_set, result[0].address)).filter((utxo): utxo is UTxO => {
                    const keys = Object.keys(utxo.assets)
                    return keys.length > 0 && keys.includes(unit)
                })
            } else {
                return []
            }
        } catch (e) {
            throw new Error("Could not fetch UTxOs from Koios. Try again.");
        }
    }

    async getUtxoByUnit(unit: Unit): Promise<UTxO> {
        let assetAddresses
        try {
            let { policyId, assetName } = fromUnit(unit)
            assetName = String(assetName)
            assetAddresses = await this.backendService.getAssetService().getAssetAddresses(policyId, assetName)
        } catch (e) {
            throw new Error("Could not fetch UTxO from Koios. Try again.");
        }
        if (Array.isArray(assetAddresses) && assetAddresses.length > 0) {
            if (assetAddresses.length > 1) {
                throw new Error("Unit needs to be an NFT or only held by one address.");
            }
            const address = assetAddresses[0].payment_address
            try {
                const utxos: UTxO[] = await this.getUtxos(address)
                const result = utxos.find<UTxO>((utxo): utxo is UTxO => {
                    const keys = Object.keys(utxo.assets)
                    return keys.length > 0 && keys.includes(unit)
                })
                if (result) {
                    return result
                }
            } catch (e) {
                throw new Error("Could not fetch UTxO from Koios. Try again.");
            }
        }
        throw new Error("Unit not found.");
    }

    async getUtxosByOutRef(outRefs: OutRef[]): Promise<UTxO[]> {
        try {
            const utxos = []
            const queryHashes = [...new Set(outRefs.map((outRef) => outRef.txHash))];
            const result = await this.backendService.getTransactionsService().getTransactionUTxOs(queryHashes)
            if (Array.isArray(result) && result.length > 0) {
                for (const utxo of result) {
                    if (utxo.outputs && utxo.outputs.length > 0) {
                        utxos.push(await this.koiosUtxosToUtxos(utxo.outputs))
                    }
                }
                return utxos.reduce((acc, utxos) => acc.concat(utxos), []).filter((utxo) =>
                    outRefs.some((outRef) =>
                        utxo.txHash === outRef.txHash && utxo.outputIndex === outRef.outputIndex
                    )
                );
            } else {
                return []
            }
        } catch (e) {
            throw new Error("Could not fetch UTxOs from Koios. Try again.");
        }
    }

    async getDelegation(rewardAddress: RewardAddress): Promise<Delegation> {
        try {
            const result = await this.backendService.getAccountService().getAccountInformation([rewardAddress])
            if (Array.isArray(result) && result.length > 0) {
                return {
                    poolId: result[0].delegated_pool || null,
                    rewards: BigInt(result[0].rewards_available),
                }
            }
        } catch (e) {
            throw new Error("Could not fetch Account Information from Koios. Try again.");
        }
        throw new Error("No Delegation Found by Reward Address");
    }

    async getDatum(datumHash: DatumHash): Promise<Datum> {
        try {
            const result = await this.backendService.getScriptService().getDatumInformation([datumHash])
            if (Array.isArray(result) && result.length > 0) {
                return result[0].bytes
            }
        } catch (e) {
            throw new Error("Could not fetch Datum Information from Koios. Try again.");
        }
        throw new Error("No Datum Found by Datum Hash");
    }

    awaitTx(txHash: TxHash, checkInterval = 3000): Promise<boolean> {
        return new Promise((res) => {
            const confirmation = setInterval(async () => {
                try {
                    const result = await this.backendService.getTransactionsService().getTransactionInformation([txHash])
                    if (Array.isArray(result) && result.length > 0) {
                        clearInterval(confirmation);
                        await new Promise((res) => setTimeout(() => res(1), 1000));
                        return res(true)
                    }
                } catch (e) {
                    throw new Error("Could not fetch Datum Information from Koios. Try again.");
                }
            }, checkInterval);
        });
    }

    async submitTx(tx: Transaction): Promise<TxHash> {
        try {
            return await this.backendService.getTransactionsService().submitTransaction(fromHex(tx))
        } catch (e) {
            if (e instanceof KoiosHttpError) {
                throw new Error(`Transaction Submission Error: ${e.message}`);
            } else if (e instanceof KoiosTimeoutError) {
                throw new Error("Timeout Error.");
            } else {
                throw new Error("Could not submit transaction.");
            }
        }
    }
}