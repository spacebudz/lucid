import { decode, decodeString, encodeToString, } from "../../deps/deno.land/std@0.100.0/encoding/hex.js";
import { C } from "../core/mod.js";
import { generateMnemonic } from "../misc/bip39.js";
import { crc8 } from "../misc/crc8.js";
import { SLOT_CONFIG_NETWORK, slotToBeginUnixTime, unixTimeToEnclosingSlot, } from "../plutus/time.js";
import { Data } from "../plutus/data.js";
export class Utils {
    lucid;
    constructor(lucid) {
        this.lucid = lucid;
    }
    validatorToAddress(validator, stakeCredential) {
        const validatorHash = this.validatorToScriptHash(validator);
        if (stakeCredential) {
            return C.BaseAddress.new(networkToId(this.lucid.network), C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(validatorHash)), stakeCredential.type === "Key"
                ? C.StakeCredential.from_keyhash(C.Ed25519KeyHash.from_hex(stakeCredential.hash))
                : C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(stakeCredential.hash)))
                .to_address()
                .to_bech32(undefined);
        }
        else {
            return C.EnterpriseAddress.new(networkToId(this.lucid.network), C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(validatorHash)))
                .to_address()
                .to_bech32(undefined);
        }
    }
    credentialToAddress(paymentCredential, stakeCredential) {
        if (stakeCredential) {
            return C.BaseAddress.new(networkToId(this.lucid.network), paymentCredential.type === "Key"
                ? C.StakeCredential.from_keyhash(C.Ed25519KeyHash.from_hex(paymentCredential.hash))
                : C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(paymentCredential.hash)), stakeCredential.type === "Key"
                ? C.StakeCredential.from_keyhash(C.Ed25519KeyHash.from_hex(stakeCredential.hash))
                : C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(stakeCredential.hash)))
                .to_address()
                .to_bech32(undefined);
        }
        else {
            return C.EnterpriseAddress.new(networkToId(this.lucid.network), paymentCredential.type === "Key"
                ? C.StakeCredential.from_keyhash(C.Ed25519KeyHash.from_hex(paymentCredential.hash))
                : C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(paymentCredential.hash)))
                .to_address()
                .to_bech32(undefined);
        }
    }
    validatorToRewardAddress(validator) {
        const validatorHash = this.validatorToScriptHash(validator);
        return C.RewardAddress.new(networkToId(this.lucid.network), C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(validatorHash)))
            .to_address()
            .to_bech32(undefined);
    }
    credentialToRewardAddress(stakeCredential) {
        return C.RewardAddress.new(networkToId(this.lucid.network), stakeCredential.type === "Key"
            ? C.StakeCredential.from_keyhash(C.Ed25519KeyHash.from_hex(stakeCredential.hash))
            : C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(stakeCredential.hash)))
            .to_address()
            .to_bech32(undefined);
    }
    validatorToScriptHash(validator) {
        switch (validator.type) {
            case "Native":
                return C.NativeScript.from_bytes(fromHex(validator.script))
                    .hash(C.ScriptHashNamespace.NativeScript)
                    .to_hex();
            case "PlutusV1":
                return C.PlutusScript.from_bytes(fromHex(applyDoubleCborEncoding(validator.script)))
                    .hash(C.ScriptHashNamespace.PlutusV1)
                    .to_hex();
            case "PlutusV2":
                return C.PlutusScript.from_bytes(fromHex(applyDoubleCborEncoding(validator.script)))
                    .hash(C.ScriptHashNamespace.PlutusV2)
                    .to_hex();
            default:
                throw new Error("No variant matched");
        }
    }
    mintingPolicyToId(mintingPolicy) {
        return this.validatorToScriptHash(mintingPolicy);
    }
    datumToHash(datum) {
        return C.hash_plutus_data(C.PlutusData.from_bytes(fromHex(datum))).to_hex();
    }
    scriptHashToCredential(scriptHash) {
        return {
            type: "Script",
            hash: scriptHash,
        };
    }
    keyHashToCredential(keyHash) {
        return {
            type: "Key",
            hash: keyHash,
        };
    }
    generatePrivateKey() {
        return generatePrivateKey();
    }
    generateSeedPhrase() {
        return generateSeedPhrase();
    }
    unixTimeToSlot(unixTime) {
        return unixTimeToEnclosingSlot(unixTime, SLOT_CONFIG_NETWORK[this.lucid.network]);
    }
    slotToUnixTime(slot) {
        return slotToBeginUnixTime(slot, SLOT_CONFIG_NETWORK[this.lucid.network]);
    }
    /** Address can be in Bech32 or Hex. */
    getAddressDetails(address) {
        return getAddressDetails(address);
    }
    /**
     * Convert a native script from Json to the Hex representation.
     * It follows this Json format: https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md
     */
    nativeScriptFromJson(nativeScript) {
        return nativeScriptFromJson(nativeScript);
    }
    paymentCredentialOf(address) {
        return paymentCredentialOf(address);
    }
    stakeCredentialOf(rewardAddress) {
        return stakeCredentialOf(rewardAddress);
    }
    getMinAdaForOutput(output) {
        const minAda = C.min_ada_required(output, C.BigNum.from_str(this.lucid.protocolParameters.coinsPerUtxoByte.toString()));
        return BigInt(minAda.to_str()).valueOf();
    }
}
function addressFromHexOrBech32(address) {
    try {
        return C.Address.from_bytes(fromHex(address));
    }
    catch (_e) {
        try {
            return C.Address.from_bech32(address);
        }
        catch (_e) {
            throw new Error("Could not deserialize address.");
        }
    }
}
/** Address can be in Bech32 or Hex. */
export function getAddressDetails(address) {
    // Base Address
    try {
        const parsedAddress = C.BaseAddress.from_address(addressFromHexOrBech32(address));
        const paymentCredential = parsedAddress.payment_cred().kind() === 0
            ? {
                type: "Key",
                hash: toHex(parsedAddress.payment_cred().to_keyhash().to_bytes()),
            }
            : {
                type: "Script",
                hash: toHex(parsedAddress.payment_cred().to_scripthash().to_bytes()),
            };
        const stakeCredential = parsedAddress.stake_cred().kind() === 0
            ? {
                type: "Key",
                hash: toHex(parsedAddress.stake_cred().to_keyhash().to_bytes()),
            }
            : {
                type: "Script",
                hash: toHex(parsedAddress.stake_cred().to_scripthash().to_bytes()),
            };
        return {
            type: "Base",
            networkId: parsedAddress.to_address().network_id(),
            address: {
                bech32: parsedAddress.to_address().to_bech32(undefined),
                hex: toHex(parsedAddress.to_address().to_bytes()),
            },
            paymentCredential,
            stakeCredential,
        };
    }
    catch (_e) {
        /* pass */
    }
    // Enterprise Address
    try {
        const parsedAddress = C.EnterpriseAddress.from_address(addressFromHexOrBech32(address));
        const paymentCredential = parsedAddress.payment_cred().kind() === 0
            ? {
                type: "Key",
                hash: toHex(parsedAddress.payment_cred().to_keyhash().to_bytes()),
            }
            : {
                type: "Script",
                hash: toHex(parsedAddress.payment_cred().to_scripthash().to_bytes()),
            };
        return {
            type: "Enterprise",
            networkId: parsedAddress.to_address().network_id(),
            address: {
                bech32: parsedAddress.to_address().to_bech32(undefined),
                hex: toHex(parsedAddress.to_address().to_bytes()),
            },
            paymentCredential,
        };
    }
    catch (_e) {
        /* pass */
    }
    // Pointer Address
    try {
        const parsedAddress = C.PointerAddress.from_address(addressFromHexOrBech32(address));
        const paymentCredential = parsedAddress.payment_cred().kind() === 0
            ? {
                type: "Key",
                hash: toHex(parsedAddress.payment_cred().to_keyhash().to_bytes()),
            }
            : {
                type: "Script",
                hash: toHex(parsedAddress.payment_cred().to_scripthash().to_bytes()),
            };
        return {
            type: "Pointer",
            networkId: parsedAddress.to_address().network_id(),
            address: {
                bech32: parsedAddress.to_address().to_bech32(undefined),
                hex: toHex(parsedAddress.to_address().to_bytes()),
            },
            paymentCredential,
        };
    }
    catch (_e) {
        /* pass */
    }
    // Reward Address
    try {
        const parsedAddress = C.RewardAddress.from_address(addressFromHexOrBech32(address));
        const stakeCredential = parsedAddress.payment_cred().kind() === 0
            ? {
                type: "Key",
                hash: toHex(parsedAddress.payment_cred().to_keyhash().to_bytes()),
            }
            : {
                type: "Script",
                hash: toHex(parsedAddress.payment_cred().to_scripthash().to_bytes()),
            };
        return {
            type: "Reward",
            networkId: parsedAddress.to_address().network_id(),
            address: {
                bech32: parsedAddress.to_address().to_bech32(undefined),
                hex: toHex(parsedAddress.to_address().to_bytes()),
            },
            stakeCredential,
        };
    }
    catch (_e) {
        /* pass */
    }
    // Limited support for Byron addresses
    try {
        const parsedAddress = ((address) => {
            try {
                return C.ByronAddress.from_bytes(fromHex(address));
            }
            catch (_e) {
                try {
                    return C.ByronAddress.from_base58(address);
                }
                catch (_e) {
                    throw new Error("Could not deserialize address.");
                }
            }
        })(address);
        return {
            type: "Byron",
            networkId: parsedAddress.network_id(),
            address: {
                bech32: "",
                hex: toHex(parsedAddress.to_address().to_bytes()),
            },
        };
    }
    catch (_e) { /* pass */ }
    throw new Error("No address type matched for: " + address);
}
export function paymentCredentialOf(address) {
    const { paymentCredential } = getAddressDetails(address);
    if (!paymentCredential) {
        throw new Error("The specified address does not contain a payment credential.");
    }
    return paymentCredential;
}
export function stakeCredentialOf(rewardAddress) {
    const { stakeCredential } = getAddressDetails(rewardAddress);
    if (!stakeCredential) {
        throw new Error("The specified address does not contain a stake credential.");
    }
    return stakeCredential;
}
export function generatePrivateKey() {
    return C.PrivateKey.generate_ed25519().to_bech32();
}
export function generateSeedPhrase() {
    return generateMnemonic(256);
}
export function valueToAssets(value) {
    const assets = {};
    assets["lovelace"] = BigInt(value.coin().to_str());
    const ma = value.multiasset();
    if (ma) {
        const multiAssets = ma.keys();
        for (let j = 0; j < multiAssets.len(); j++) {
            const policy = multiAssets.get(j);
            const policyAssets = ma.get(policy);
            const assetNames = policyAssets.keys();
            for (let k = 0; k < assetNames.len(); k++) {
                const policyAsset = assetNames.get(k);
                const quantity = policyAssets.get(policyAsset);
                const unit = toHex(policy.to_bytes()) + toHex(policyAsset.name());
                assets[unit] = BigInt(quantity.to_str());
            }
        }
    }
    return assets;
}
export function assetsToValue(assets) {
    const multiAsset = C.MultiAsset.new();
    const lovelace = assets["lovelace"];
    const units = Object.keys(assets);
    const policies = Array.from(new Set(units
        .filter((unit) => unit !== "lovelace")
        .map((unit) => unit.slice(0, 56))));
    policies.forEach((policy) => {
        const policyUnits = units.filter((unit) => unit.slice(0, 56) === policy);
        const assetsValue = C.Assets.new();
        policyUnits.forEach((unit) => {
            assetsValue.insert(C.AssetName.new(fromHex(unit.slice(56))), C.BigNum.from_str(assets[unit].toString()));
        });
        multiAsset.insert(C.ScriptHash.from_bytes(fromHex(policy)), assetsValue);
    });
    const value = C.Value.new(C.BigNum.from_str(lovelace ? lovelace.toString() : "0"));
    if (units.length > 1 || !lovelace)
        value.set_multiasset(multiAsset);
    return value;
}
export function fromScriptRef(scriptRef) {
    const kind = scriptRef.get().kind();
    switch (kind) {
        case 0:
            return {
                type: "Native",
                script: toHex(scriptRef.get().as_native().to_bytes()),
            };
        case 1:
            return {
                type: "PlutusV1",
                script: toHex(scriptRef.get().as_plutus_v1().to_bytes()),
            };
        case 2:
            return {
                type: "PlutusV2",
                script: toHex(scriptRef.get().as_plutus_v2().to_bytes()),
            };
        default:
            throw new Error("No variant matched.");
    }
}
export function toScriptRef(script) {
    switch (script.type) {
        case "Native":
            return C.ScriptRef.new(C.Script.new_native(C.NativeScript.from_bytes(fromHex(script.script))));
        case "PlutusV1":
            return C.ScriptRef.new(C.Script.new_plutus_v1(C.PlutusScript.from_bytes(fromHex(applyDoubleCborEncoding(script.script)))));
        case "PlutusV2":
            return C.ScriptRef.new(C.Script.new_plutus_v2(C.PlutusScript.from_bytes(fromHex(applyDoubleCborEncoding(script.script)))));
        default:
            throw new Error("No variant matched.");
    }
}
export function utxoToCore(utxo) {
    const address = (() => {
        try {
            return C.Address.from_bech32(utxo.address);
        }
        catch (_e) {
            return C.ByronAddress.from_base58(utxo.address).to_address();
        }
    })();
    const output = C.TransactionOutput.new(address, assetsToValue(utxo.assets));
    if (utxo.datumHash) {
        output.set_datum(C.Datum.new_data_hash(C.DataHash.from_bytes(fromHex(utxo.datumHash))));
    }
    // inline datum
    if (!utxo.datumHash && utxo.datum) {
        output.set_datum(C.Datum.new_data(C.Data.new(C.PlutusData.from_bytes(fromHex(utxo.datum)))));
    }
    if (utxo.scriptRef) {
        output.set_script_ref(toScriptRef(utxo.scriptRef));
    }
    return C.TransactionUnspentOutput.new(C.TransactionInput.new(C.TransactionHash.from_bytes(fromHex(utxo.txHash)), C.BigNum.from_str(utxo.outputIndex.toString())), output);
}
export function coreToUtxo(coreUtxo) {
    const datum = coreUtxo.output()?.datum()?.as_data()?.get();
    const datumHash = coreUtxo.output()?.datum()?.as_data_hash()?.to_hex();
    return {
        txHash: toHex(coreUtxo.input().transaction_id().to_bytes()),
        outputIndex: parseInt(coreUtxo.input().index().to_str()),
        assets: valueToAssets(coreUtxo.output().amount()),
        address: coreUtxo.output().address().as_byron()
            ? coreUtxo.output().address().as_byron()?.to_base58()
            : coreUtxo.output().address().to_bech32(undefined),
        datumHash: datum ? C.hash_plutus_data(datum).to_hex() : datumHash,
        datum: datum && toHex(datum.to_bytes()),
        scriptRef: coreUtxo.output()?.script_ref() &&
            fromScriptRef(coreUtxo.output().script_ref()),
    };
}
export function networkToId(network) {
    switch (network) {
        case "Preview":
            return 0;
        case "Preprod":
            return 0;
        case "Custom":
            return 0;
        case "Mainnet":
            return 1;
        default:
            throw new Error("Network not found");
    }
}
export function fromHex(hex) {
    return decodeString(hex);
}
export function toHex(bytes) {
    return encodeToString(bytes);
}
/** Convert a Hex encoded string to a Utf-8 encoded string. */
export function toText(hex) {
    return new TextDecoder().decode(decode(new TextEncoder().encode(hex)));
}
/** Convert a Utf-8 encoded string to a Hex encoded string. */
export function fromText(text) {
    return toHex(new TextEncoder().encode(text));
}
export function toPublicKey(privateKey) {
    return C.PrivateKey.from_bech32(privateKey).to_public().to_bech32();
}
/** Padded number in Hex. */
function checksum(num) {
    return crc8(fromHex(num)).toString(16).padStart(2, "0");
}
export function toLabel(num) {
    if (num < 0 || num > 65535) {
        throw new Error(`Label ${num} out of range: min label 1 - max label 65535.`);
    }
    const numHex = num.toString(16).padStart(4, "0");
    return "0" + numHex + checksum(numHex) + "0";
}
export function fromLabel(label) {
    if (label.length !== 8 || !(label[0] === "0" && label[7] === "0")) {
        return null;
    }
    const numHex = label.slice(1, 5);
    const num = parseInt(numHex, 16);
    const check = label.slice(5, 7);
    return check === checksum(numHex) ? num : null;
}
/**
 * @param name Hex encoded
 */
export function toUnit(policyId, name, label) {
    const hexLabel = Number.isInteger(label) ? toLabel(label) : "";
    const n = name ? name : "";
    if ((n + hexLabel).length > 64) {
        throw new Error("Asset name size exceeds 32 bytes.");
    }
    if (policyId.length !== 56) {
        throw new Error(`Policy id invalid: ${policyId}.`);
    }
    return policyId + hexLabel + n;
}
/**
 * Splits unit into policy id, asset name (entire asset name), name (asset name without label) and label if applicable.
 * name will be returned in Hex.
 */
export function fromUnit(unit) {
    const policyId = unit.slice(0, 56);
    const assetName = unit.slice(56) || null;
    const label = fromLabel(unit.slice(56, 64));
    const name = (() => {
        const hexName = Number.isInteger(label) ? unit.slice(64) : unit.slice(56);
        return hexName || null;
    })();
    return { policyId, assetName, name, label };
}
/**
 * Convert a native script from Json to the Hex representation.
 * It follows this Json format: https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md
 */
export function nativeScriptFromJson(nativeScript) {
    return {
        type: "Native",
        script: toHex(C.encode_json_str_to_native_script(JSON.stringify(nativeScript), "", C.ScriptSchema.Node).to_bytes()),
    };
}
export function applyParamsToScript(plutusScript, params, type) {
    const p = (type ? Data.castTo(params, type) : params);
    return toHex(C.apply_params_to_plutus_script(C.PlutusList.from_bytes(fromHex(Data.to(p))), C.PlutusScript.from_bytes(fromHex(applyDoubleCborEncoding(plutusScript)))).to_bytes());
}
export const chunk = (array, size) => {
    const chunks = [];
    const n = array.length;
    let i = 0;
    while (i < n) {
        chunks.push(array.slice(i, (i += size)));
    }
    return chunks;
};
/** Returns double cbor encoded script. If script is already double cbor encoded it's returned as it is. */
export function applyDoubleCborEncoding(script) {
    try {
        C.PlutusScript.from_bytes(C.PlutusScript.from_bytes(fromHex(script)).bytes());
        return script;
    }
    catch (_e) {
        return toHex(C.PlutusScript.new(fromHex(script)).to_bytes());
    }
}
export function addAssets(...assets) {
    return assets.reduce((a, b) => {
        for (const k in b) {
            if (Object.hasOwn(b, k)) {
                a[k] = (a[k] || 0n) + b[k];
            }
        }
        return a;
    }, {});
}
