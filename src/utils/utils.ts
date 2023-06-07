import {
  decode,
  decodeString,
  encodeToString,
} from "https://deno.land/std@0.100.0/encoding/hex.ts";
import { C } from "../core/mod.ts";
import {
  Address,
  AddressDetails,
  Assets,
  CertificateValidator,
  Credential,
  Datum,
  DatumHash,
  Exact,
  KeyHash,
  MintingPolicy,
  NativeScript,
  Network,
  PolicyId,
  PrivateKey,
  PublicKey,
  RewardAddress,
  Script,
  ScriptHash,
  Slot,
  SpendingValidator,
  Unit,
  UnixTime,
  UTxO,
  Validator,
  WithdrawalValidator,
} from "../types/mod.ts";
import { Lucid } from "../lucid/mod.ts";
import { generateMnemonic } from "../misc/bip39.ts";
import { crc8 } from "../misc/crc8.ts";
import {
  SLOT_CONFIG_NETWORK,
  slotToBeginUnixTime,
  unixTimeToEnclosingSlot,
} from "../plutus/time.ts";
import { Data } from "../plutus/data.ts";

export class Utils {
  private lucid: Lucid;
  constructor(lucid: Lucid) {
    this.lucid = lucid;
  }

  validatorToAddress(
    validator: SpendingValidator,
    stakeCredential?: Credential,
  ): Address {
    const validatorHash = this.validatorToScriptHash(validator);
    if (stakeCredential) {
      return C.BaseAddress.new(
        networkToId(this.lucid.network),
        C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(validatorHash)),
        stakeCredential.type === "Key"
          ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_hex(stakeCredential.hash),
          )
          : C.StakeCredential.from_scripthash(
            C.ScriptHash.from_hex(stakeCredential.hash),
          ),
      )
        .to_address()
        .to_bech32(undefined);
    } else {
      return C.EnterpriseAddress.new(
        networkToId(this.lucid.network),
        C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(validatorHash)),
      )
        .to_address()
        .to_bech32(undefined);
    }
  }

  credentialToAddress(
    paymentCredential: Credential,
    stakeCredential?: Credential,
  ): Address {
    if (stakeCredential) {
      return C.BaseAddress.new(
        networkToId(this.lucid.network),
        paymentCredential.type === "Key"
          ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_hex(paymentCredential.hash),
          )
          : C.StakeCredential.from_scripthash(
            C.ScriptHash.from_hex(paymentCredential.hash),
          ),
        stakeCredential.type === "Key"
          ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_hex(stakeCredential.hash),
          )
          : C.StakeCredential.from_scripthash(
            C.ScriptHash.from_hex(stakeCredential.hash),
          ),
      )
        .to_address()
        .to_bech32(undefined);
    } else {
      return C.EnterpriseAddress.new(
        networkToId(this.lucid.network),
        paymentCredential.type === "Key"
          ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_hex(paymentCredential.hash),
          )
          : C.StakeCredential.from_scripthash(
            C.ScriptHash.from_hex(paymentCredential.hash),
          ),
      )
        .to_address()
        .to_bech32(undefined);
    }
  }

  validatorToRewardAddress(
    validator: CertificateValidator | WithdrawalValidator,
  ): RewardAddress {
    const validatorHash = this.validatorToScriptHash(validator);
    return C.RewardAddress.new(
      networkToId(this.lucid.network),
      C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(validatorHash)),
    )
      .to_address()
      .to_bech32(undefined);
  }

  credentialToRewardAddress(stakeCredential: Credential): RewardAddress {
    return C.RewardAddress.new(
      networkToId(this.lucid.network),
      stakeCredential.type === "Key"
        ? C.StakeCredential.from_keyhash(
          C.Ed25519KeyHash.from_hex(stakeCredential.hash),
        )
        : C.StakeCredential.from_scripthash(
          C.ScriptHash.from_hex(stakeCredential.hash),
        ),
    )
      .to_address()
      .to_bech32(undefined);
  }

  validatorToScriptHash(validator: Validator): ScriptHash {
    switch (validator.type) {
      case "Native":
        return C.NativeScript.from_bytes(fromHex(validator.script))
          .hash(C.ScriptHashNamespace.NativeScript)
          .to_hex();
      case "PlutusV1":
        return C.PlutusScript.from_bytes(
          fromHex(applyDoubleCborEncoding(validator.script)),
        )
          .hash(C.ScriptHashNamespace.PlutusV1)
          .to_hex();
      case "PlutusV2":
        return C.PlutusScript.from_bytes(
          fromHex(applyDoubleCborEncoding(validator.script)),
        )
          .hash(C.ScriptHashNamespace.PlutusV2)
          .to_hex();
      default:
        throw new Error("No variant matched");
    }
  }

  mintingPolicyToId(mintingPolicy: MintingPolicy): PolicyId {
    return this.validatorToScriptHash(mintingPolicy);
  }

  datumToHash(datum: Datum): DatumHash {
    return C.hash_plutus_data(C.PlutusData.from_bytes(fromHex(datum))).to_hex();
  }

  scriptHashToCredential(scriptHash: ScriptHash): Credential {
    return {
      type: "Script",
      hash: scriptHash,
    };
  }

  keyHashToCredential(keyHash: KeyHash): Credential {
    return {
      type: "Key",
      hash: keyHash,
    };
  }

  generatePrivateKey(): PrivateKey {
    return generatePrivateKey();
  }

  generateSeedPhrase(): string {
    return generateSeedPhrase();
  }

  unixTimeToSlot(unixTime: UnixTime): Slot {
    return unixTimeToEnclosingSlot(
      unixTime,
      SLOT_CONFIG_NETWORK[this.lucid.network],
    );
  }

  slotToUnixTime(slot: Slot): UnixTime {
    return slotToBeginUnixTime(slot, SLOT_CONFIG_NETWORK[this.lucid.network]);
  }

  /** Address can be in Bech32 or Hex. */
  getAddressDetails(address: string): AddressDetails {
    return getAddressDetails(address);
  }

  /**
   * Convert a native script from Json to the Hex representation.
   * It follows this Json format: https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md
   */
  nativeScriptFromJson(nativeScript: NativeScript): Script {
    return nativeScriptFromJson(nativeScript);
  }

  paymentCredentialOf(address: Address): Credential {
    return paymentCredentialOf(address);
  }

  stakeCredentialOf(rewardAddress: RewardAddress): Credential {
    return stakeCredentialOf(rewardAddress);
  }
}

function addressFromHexOrBech32(address: string): C.Address {
  try {
    return C.Address.from_bytes(fromHex(address));
  } catch (_e) {
    try {
      return C.Address.from_bech32(address);
    } catch (_e) {
      throw new Error("Could not deserialize address.");
    }
  }
}

/** Address can be in Bech32 or Hex. */
export function getAddressDetails(address: string): AddressDetails {
  // Base Address
  try {
    const parsedAddress = C.BaseAddress.from_address(
      addressFromHexOrBech32(address),
    )!;
    const paymentCredential: Credential =
      parsedAddress.payment_cred().kind() === 0
        ? {
          type: "Key",
          hash: toHex(
            parsedAddress.payment_cred().to_keyhash()!.to_bytes(),
          ),
        }
        : {
          type: "Script",
          hash: toHex(
            parsedAddress.payment_cred().to_scripthash()!.to_bytes(),
          ),
        };
    const stakeCredential: Credential = parsedAddress.stake_cred().kind() === 0
      ? {
        type: "Key",
        hash: toHex(parsedAddress.stake_cred().to_keyhash()!.to_bytes()),
      }
      : {
        type: "Script",
        hash: toHex(
          parsedAddress.stake_cred().to_scripthash()!.to_bytes(),
        ),
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
  } catch (_e) { /* pass */ }

  // Enterprise Address
  try {
    const parsedAddress = C.EnterpriseAddress.from_address(
      addressFromHexOrBech32(address),
    )!;
    const paymentCredential: Credential =
      parsedAddress.payment_cred().kind() === 0
        ? {
          type: "Key",
          hash: toHex(
            parsedAddress.payment_cred().to_keyhash()!.to_bytes(),
          ),
        }
        : {
          type: "Script",
          hash: toHex(
            parsedAddress.payment_cred().to_scripthash()!.to_bytes(),
          ),
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
  } catch (_e) { /* pass */ }

  // Pointer Address
  try {
    const parsedAddress = C.PointerAddress.from_address(
      addressFromHexOrBech32(address),
    )!;
    const paymentCredential: Credential =
      parsedAddress.payment_cred().kind() === 0
        ? {
          type: "Key",
          hash: toHex(
            parsedAddress.payment_cred().to_keyhash()!.to_bytes(),
          ),
        }
        : {
          type: "Script",
          hash: toHex(
            parsedAddress.payment_cred().to_scripthash()!.to_bytes(),
          ),
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
  } catch (_e) { /* pass */ }

  // Reward Address
  try {
    const parsedAddress = C.RewardAddress.from_address(
      addressFromHexOrBech32(address),
    )!;
    const stakeCredential: Credential =
      parsedAddress.payment_cred().kind() === 0
        ? {
          type: "Key",
          hash: toHex(
            parsedAddress.payment_cred().to_keyhash()!.to_bytes(),
          ),
        }
        : {
          type: "Script",
          hash: toHex(
            parsedAddress.payment_cred().to_scripthash()!.to_bytes(),
          ),
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
  } catch (_e) { /* pass */ }

  // Limited support for Byron addresses
  try {
    const parsedAddress = ((address: string): C.ByronAddress => {
      try {
        return C.ByronAddress.from_bytes(fromHex(address));
      } catch (_e) {
        try {
          return C.ByronAddress.from_base58(address);
        } catch (_e) {
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
  } catch (_e) { /* pass */ }

  throw new Error("No address type matched for: " + address);
}

export function paymentCredentialOf(address: Address): Credential {
  const { paymentCredential } = getAddressDetails(address);
  if (!paymentCredential) {
    throw new Error(
      "The specified address does not contain a payment credential.",
    );
  }
  return paymentCredential;
}

export function stakeCredentialOf(rewardAddress: RewardAddress): Credential {
  const { stakeCredential } = getAddressDetails(rewardAddress);
  if (!stakeCredential) {
    throw new Error(
      "The specified address does not contain a stake credential.",
    );
  }
  return stakeCredential;
}

export function generatePrivateKey(): PrivateKey {
  return C.PrivateKey.generate_ed25519().to_bech32();
}

export function generateSeedPhrase(): string {
  return generateMnemonic(256);
}

export function valueToAssets(value: C.Value): Assets {
  const assets: Assets = {};
  assets["lovelace"] = BigInt(value.coin().to_str());
  const ma = value.multiasset();
  if (ma) {
    const multiAssets = ma.keys();
    for (let j = 0; j < multiAssets.len(); j++) {
      const policy = multiAssets.get(j);
      const policyAssets = ma.get(policy)!;
      const assetNames = policyAssets.keys();
      for (let k = 0; k < assetNames.len(); k++) {
        const policyAsset = assetNames.get(k);
        const quantity = policyAssets.get(policyAsset)!;
        const unit = toHex(policy.to_bytes()) + toHex(policyAsset.name());
        assets[unit] = BigInt(quantity.to_str());
      }
    }
  }
  return assets;
}

export function assetsToValue(assets: Assets): C.Value {
  const multiAsset = C.MultiAsset.new();
  const lovelace = assets["lovelace"];
  const units = Object.keys(assets);
  const policies = Array.from(
    new Set(
      units
        .filter((unit) => unit !== "lovelace")
        .map((unit) => unit.slice(0, 56)),
    ),
  );
  policies.forEach((policy) => {
    const policyUnits = units.filter((unit) => unit.slice(0, 56) === policy);
    const assetsValue = C.Assets.new();
    policyUnits.forEach((unit) => {
      assetsValue.insert(
        C.AssetName.new(fromHex(unit.slice(56))),
        C.BigNum.from_str(assets[unit].toString()),
      );
    });
    multiAsset.insert(C.ScriptHash.from_bytes(fromHex(policy)), assetsValue);
  });
  const value = C.Value.new(
    C.BigNum.from_str(lovelace ? lovelace.toString() : "0"),
  );
  if (units.length > 1 || !lovelace) value.set_multiasset(multiAsset);
  return value;
}

export function fromScriptRef(scriptRef: C.ScriptRef): Script {
  const kind = scriptRef.get().kind();
  switch (kind) {
    case 0:
      return {
        type: "Native",
        script: toHex(scriptRef.get().as_native()!.to_bytes()),
      };
    case 1:
      return {
        type: "PlutusV1",
        script: toHex(scriptRef.get().as_plutus_v1()!.to_bytes()),
      };
    case 2:
      return {
        type: "PlutusV2",
        script: toHex(scriptRef.get().as_plutus_v2()!.to_bytes()),
      };
    default:
      throw new Error("No variant matched.");
  }
}

export function toScriptRef(script: Script): C.ScriptRef {
  switch (script.type) {
    case "Native":
      return C.ScriptRef.new(
        C.Script.new_native(C.NativeScript.from_bytes(fromHex(script.script))),
      );
    case "PlutusV1":
      return C.ScriptRef.new(
        C.Script.new_plutus_v1(
          C.PlutusScript.from_bytes(
            fromHex(applyDoubleCborEncoding(script.script)),
          ),
        ),
      );
    case "PlutusV2":
      return C.ScriptRef.new(
        C.Script.new_plutus_v2(
          C.PlutusScript.from_bytes(
            fromHex(applyDoubleCborEncoding(script.script)),
          ),
        ),
      );
    default:
      throw new Error("No variant matched.");
  }
}

export function utxoToCore(utxo: UTxO): C.TransactionUnspentOutput {
  const address: C.Address = (() => {
    try {
      return C.Address.from_bech32(utxo.address);
    } catch (_e) {
      return C.ByronAddress.from_base58(utxo.address).to_address();
    }
  })();
  const output = C.TransactionOutput.new(address, assetsToValue(utxo.assets));
  if (utxo.datumHash) {
    output.set_datum(
      C.Datum.new_data_hash(C.DataHash.from_bytes(fromHex(utxo.datumHash))),
    );
  }
  // inline datum
  if (!utxo.datumHash && utxo.datum) {
    output.set_datum(
      C.Datum.new_data(
        C.Data.new(C.PlutusData.from_bytes(fromHex(utxo.datum))),
      ),
    );
  }

  if (utxo.scriptRef) {
    output.set_script_ref(toScriptRef(utxo.scriptRef));
  }

  return C.TransactionUnspentOutput.new(
    C.TransactionInput.new(
      C.TransactionHash.from_bytes(fromHex(utxo.txHash)),
      C.BigNum.from_str(utxo.outputIndex.toString()),
    ),
    output,
  );
}

export function coreToUtxo(coreUtxo: C.TransactionUnspentOutput): UTxO {
  return {
    txHash: toHex(coreUtxo.input().transaction_id().to_bytes()),
    outputIndex: parseInt(coreUtxo.input().index().to_str()),
    assets: valueToAssets(coreUtxo.output().amount()),
    address: coreUtxo.output().address().as_byron()
      ? coreUtxo.output().address().as_byron()?.to_base58()!
      : coreUtxo.output().address().to_bech32(undefined),
    datumHash: coreUtxo.output()?.datum()?.as_data_hash()?.to_hex(),
    datum: coreUtxo.output()?.datum()?.as_data() &&
      toHex(coreUtxo.output().datum()!.as_data()!.get().to_bytes()),
    scriptRef: coreUtxo.output()?.script_ref() &&
      fromScriptRef(coreUtxo.output().script_ref()!),
  };
}

export function networkToId(network: Network): number {
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

export function fromHex(hex: string): Uint8Array {
  return decodeString(hex);
}

export function toHex(bytes: Uint8Array): string {
  return encodeToString(bytes);
}

/** Convert a Hex encoded string to a Utf-8 encoded string. */
export function toText(hex: string): string {
  return new TextDecoder().decode(decode(new TextEncoder().encode(hex)));
}

/** Convert a Utf-8 encoded string to a Hex encoded string. */
export function fromText(text: string): string {
  return toHex(new TextEncoder().encode(text));
}

export function toPublicKey(privateKey: PrivateKey): PublicKey {
  return C.PrivateKey.from_bech32(privateKey).to_public().to_bech32();
}

/** Padded number in Hex. */
function checksum(num: string): string {
  return crc8(fromHex(num)).toString(16).padStart(2, "0");
}

export function toLabel(num: number): string {
  if (num < 0 || num > 65535) {
    throw new Error(
      `Label ${num} out of range: min label 1 - max label 65535.`,
    );
  }
  const numHex = num.toString(16).padStart(4, "0");
  return "0" + numHex + checksum(numHex) + "0";
}

export function fromLabel(label: string): number | null {
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
export function toUnit(
  policyId: PolicyId,
  name?: string | null,
  label?: number | null,
): Unit {
  const hexLabel = Number.isInteger(label) ? toLabel(label!) : "";
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
export function fromUnit(
  unit: Unit,
): {
  policyId: PolicyId;
  assetName: string | null;
  name: string | null;
  label: number | null;
} {
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
export function nativeScriptFromJson(nativeScript: NativeScript): Script {
  return {
    type: "Native",
    script: toHex(
      C.encode_json_str_to_native_script(
        JSON.stringify(nativeScript),
        "",
        C.ScriptSchema.Node,
      ).to_bytes(),
    ),
  };
}

export function applyParamsToScript<T extends unknown[] = Data[]>(
  plutusScript: string,
  params: Exact<[...T]>,
  type?: T,
): string {
  const p = (type ? Data.castTo<T>(params, type) : params) as Data[];
  return toHex(
    C.apply_params_to_plutus_script(
      C.PlutusList.from_bytes(fromHex(Data.to<Data[]>(p))),
      C.PlutusScript.from_bytes(fromHex(applyDoubleCborEncoding(plutusScript))),
    ).to_bytes(),
  );
}

/** Returns double cbor encoded script. If script is already double cbor encoded it's returned as it is. */
export function applyDoubleCborEncoding(script: string): string {
  try {
    C.PlutusScript.from_bytes(
      C.PlutusScript.from_bytes(fromHex(script)).bytes(),
    );
    return script;
  } catch (_e) {
    return toHex(C.PlutusScript.new(fromHex(script)).to_bytes());
  }
}

export function addAssets(...assets: Assets[]): Assets {
  return assets.reduce((a, b) => {
    for (const k in b) {
      if (Object.hasOwn(b, k)) {
        a[k] = (a[k] || 0n) + b[k];
      }
    }
    return a;
  }, {});
}
