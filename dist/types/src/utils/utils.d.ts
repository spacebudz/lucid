import { C } from "../core/mod.js";
import { Address, AddressDetails, Assets, CertificateValidator, Credential, Datum, DatumHash, Exact, KeyHash, MintingPolicy, NativeScript, Network, PolicyId, PrivateKey, PublicKey, RewardAddress, Script, ScriptHash, Slot, SpendingValidator, Unit, UnixTime, UTxO, Validator, WithdrawalValidator } from "../types/mod.js";
import { Lucid } from "../lucid/mod.js";
import { Data } from "../plutus/data.js";
export declare class Utils {
    private lucid;
    constructor(lucid: Lucid);
    validatorToAddress(validator: SpendingValidator, stakeCredential?: Credential): Address;
    credentialToAddress(paymentCredential: Credential, stakeCredential?: Credential): Address;
    validatorToRewardAddress(validator: CertificateValidator | WithdrawalValidator): RewardAddress;
    credentialToRewardAddress(stakeCredential: Credential): RewardAddress;
    validatorToScriptHash(validator: Validator): ScriptHash;
    mintingPolicyToId(mintingPolicy: MintingPolicy): PolicyId;
    datumToHash(datum: Datum): DatumHash;
    scriptHashToCredential(scriptHash: ScriptHash): Credential;
    keyHashToCredential(keyHash: KeyHash): Credential;
    generatePrivateKey(): PrivateKey;
    generateSeedPhrase(): string;
    unixTimeToSlot(unixTime: UnixTime): Slot;
    slotToUnixTime(slot: Slot): UnixTime;
    /** Address can be in Bech32 or Hex. */
    getAddressDetails(address: string): AddressDetails;
    /**
     * Convert a native script from Json to the Hex representation.
     * It follows this Json format: https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md
     */
    nativeScriptFromJson(nativeScript: NativeScript): Script;
    paymentCredentialOf(address: Address): Credential;
    stakeCredentialOf(rewardAddress: RewardAddress): Credential;
    getMinAdaForOutput(output: C.TransactionOutput): bigint;
}
/** Address can be in Bech32 or Hex. */
export declare function getAddressDetails(address: string): AddressDetails;
export declare function paymentCredentialOf(address: Address): Credential;
export declare function stakeCredentialOf(rewardAddress: RewardAddress): Credential;
export declare function generatePrivateKey(): PrivateKey;
export declare function generateSeedPhrase(): string;
export declare function valueToAssets(value: C.Value): Assets;
export declare function assetsToValue(assets: Assets): C.Value;
export declare function fromScriptRef(scriptRef: C.ScriptRef): Script;
export declare function toScriptRef(script: Script): C.ScriptRef;
export declare function utxoToCore(utxo: UTxO): C.TransactionUnspentOutput;
export declare function coreToUtxo(coreUtxo: C.TransactionUnspentOutput): UTxO;
export declare function networkToId(network: Network): number;
export declare function fromHex(hex: string): Uint8Array;
export declare function toHex(bytes: Uint8Array): string;
/** Convert a Hex encoded string to a Utf-8 encoded string. */
export declare function toText(hex: string): string;
/** Convert a Utf-8 encoded string to a Hex encoded string. */
export declare function fromText(text: string): string;
export declare function toPublicKey(privateKey: PrivateKey): PublicKey;
export declare function toLabel(num: number): string;
export declare function fromLabel(label: string): number | null;
/**
 * @param name Hex encoded
 */
export declare function toUnit(policyId: PolicyId, name?: string | null, label?: number | null): Unit;
/**
 * Splits unit into policy id, asset name (entire asset name), name (asset name without label) and label if applicable.
 * name will be returned in Hex.
 */
export declare function fromUnit(unit: Unit): {
    policyId: PolicyId;
    assetName: string | null;
    name: string | null;
    label: number | null;
};
/**
 * Convert a native script from Json to the Hex representation.
 * It follows this Json format: https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md
 */
export declare function nativeScriptFromJson(nativeScript: NativeScript): Script;
export declare function applyParamsToScript<T extends unknown[] = Data[]>(plutusScript: string, params: Exact<[...T]>, type?: T): string;
export declare const chunk: <T>(array: T[], size: number) => T[][];
/** Returns double cbor encoded script. If script is already double cbor encoded it's returned as it is. */
export declare function applyDoubleCborEncoding(script: string): string;
export declare function addAssets(...assets: Assets[]): Assets;
