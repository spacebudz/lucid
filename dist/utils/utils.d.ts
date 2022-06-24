/// <reference types="node" />
import { Core } from '../core';
import { Address, AddressDetails, Assets, Credential, KeyHash, PrivateKey, ScriptHash, Slot, SpendingValidator, UnixTime, UTxO, Validator } from '../types';
import { Lucid } from '..';
export declare class Utils {
    private lucid;
    constructor(lucid: Lucid);
    validatorToAddress(validator: SpendingValidator, stakeCredential?: Credential): Address;
    validatorToScriptHash(validator: Validator): ScriptHash;
    scriptHashToCredential(scriptHash: ScriptHash): Credential;
    keyHashToCredential(keyHash: KeyHash): Credential;
    generatePrivateKey(): PrivateKey;
    unixTimeToSlot(unixTime: UnixTime): Slot;
    slotToUnixTime(slot: Slot): UnixTime;
    /** Address can be in bech32 or hex */
    getAddressDetails(address: string): AddressDetails;
}
export declare const valueToAssets: (value: Core.Value) => Assets;
export declare const assetsToValue: (assets: Assets) => Core.Value | import("../../custom_modules/cardano-multiplatform-lib-web/cardano_multiplatform_lib").Value;
export declare const utxoToCore: (utxo: UTxO) => Core.TransactionUnspentOutput;
export declare const coreToUtxo: (coreUtxo: Core.TransactionUnspentOutput) => UTxO;
export declare const fromHex: (hex: string) => Buffer;
export declare const toHex: (bytes: Buffer | Uint8Array) => string;
