import {
  decode,
  decodeString,
  encodeToString,
} from "https://deno.land/std@0.100.0/encoding/hex.ts";
import { C, Core } from "../core/mod.ts";
import {
  Address,
  AddressDetails,
  Assets,
  Credential,
  Datum,
  DatumHash,
  KeyHash,
  MintingPolicy,
  Network,
  PolicyId,
  PrivateKey,
  ScriptHash,
  Slot,
  SpendingValidator,
  Unit,
  UnixTime,
  UTxO,
  Validator,
} from "../types/mod.ts";
import { Lucid } from "../lucid/mod.ts";
import { generateMnemonic } from "./bip39.ts";

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

  validatorToScriptHash(validator: Validator): ScriptHash {
    if (validator.type === "Native") {
      return C.NativeScript.from_bytes(fromHex(validator.script))
        .hash(C.ScriptHashNamespace.NativeScript)
        .to_hex();
    } else if (validator.type === "PlutusV1") {
      return C.PlutusScript.from_bytes(fromHex(validator.script))
        .hash(C.ScriptHashNamespace.PlutusV1)
        .to_hex();
    } else if (validator.type === "PlutusV2") {
      return C.PlutusScript.from_bytes(fromHex(validator.script))
        .hash(C.ScriptHashNamespace.PlutusV2)
        .to_hex();
    }
    throw new Error("No variant matched");
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
    return C.PrivateKey.generate_ed25519().to_bech32();
  }

  generateSeedPhrase(): string {
    return generateMnemonic(256);
  }

  unixTimeToSlot(unixTime: UnixTime): Slot {
    return unixTimeToSlot(unixTime, this.lucid.network);
  }

  slotToUnixTime(slot: Slot): UnixTime {
    return slotToUnixTime(slot, this.lucid.network);
  }

  /** Address can be in Bech32 or Hex */
  getAddressDetails(address: string): AddressDetails {
    return getAddressDetails(address);
  }
}

/** Address can be in Bech32 or Hex */
export const getAddressDetails = (address: string): AddressDetails => {
  // Base Address
  try {
    const parsedAddress = C.BaseAddress.from_address(
      C.Address.from_bytes(fromHex(address)),
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
      address: {
        type: "Base",
        address: parsedAddress.to_address().to_bech32(undefined),
      },
      paymentCredential,
      stakeCredential,
    };
  } catch (_e) { /* pass */ }
  try {
    const parsedAddress = C.BaseAddress.from_address(
      C.Address.from_bech32(address),
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
      address: {
        type: "Base",
        address: parsedAddress.to_address().to_bech32(undefined),
      },
      paymentCredential,
      stakeCredential,
    };
  } catch (_e) { /* pass */ }

  // Enterprise Address
  try {
    const parsedAddress = C.EnterpriseAddress.from_address(
      C.Address.from_bytes(fromHex(address)),
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
      address: {
        type: "Enterprise",
        address: parsedAddress.to_address().to_bech32(undefined),
      },
      paymentCredential,
    };
  } catch (_e) { /* pass */ }

  try {
    const parsedAddress = C.EnterpriseAddress.from_address(
      C.Address.from_bech32(address),
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
      address: {
        type: "Enterprise",
        address: parsedAddress.to_address().to_bech32(undefined),
      },
      paymentCredential,
    };
  } catch (_e) { /* pass */ }

  // Pointer Address
  try {
    const parsedAddress = C.PointerAddress.from_address(
      C.Address.from_bytes(fromHex(address)),
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
      address: {
        type: "Pointer",
        address: parsedAddress.to_address().to_bech32(undefined),
      },
      paymentCredential,
    };
  } catch (_e) { /* pass */ }

  try {
    const parsedAddress = C.PointerAddress.from_address(
      C.Address.from_bech32(address),
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
      address: {
        type: "Pointer",
        address: parsedAddress.to_address().to_bech32(undefined),
      },
      paymentCredential,
    };
  } catch (_e) { /* pass */ }

  // Reward Address
  try {
    const parsedAddress = C.RewardAddress.from_address(
      C.Address.from_bytes(fromHex(address)),
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
      address: {
        type: "Reward",
        address: parsedAddress.to_address().to_bech32(undefined),
      },
      stakeCredential,
    };
  } catch (_e) { /* pass */ }

  try {
    const parsedAddress = C.RewardAddress.from_address(
      C.Address.from_bech32(address),
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
      address: {
        type: "Reward",
        address: parsedAddress.to_address().to_bech32(undefined),
      },
      stakeCredential,
    };
  } catch (_e) { /* pass */ }
  throw new Error("No address type matched for: " + address);
};

export const valueToAssets = (value: Core.Value): Assets => {
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
};

export const assetsToValue = (assets: Assets) => {
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
};

export const utxoToCore = (utxo: UTxO): Core.TransactionUnspentOutput => {
  const address: Core.Address = (() => {
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
    output.set_script_ref(C.ScriptRef.from_bytes(fromHex(utxo.scriptRef)));
  }

  return C.TransactionUnspentOutput.new(
    C.TransactionInput.new(
      C.TransactionHash.from_bytes(fromHex(utxo.txHash)),
      C.BigNum.from_str(utxo.outputIndex.toString()),
    ),
    output,
  );
};

export const coreToUtxo = (coreUtxo: Core.TransactionUnspentOutput): UTxO => {
  return {
    txHash: toHex(coreUtxo.input().transaction_id().to_bytes()),
    outputIndex: parseInt(coreUtxo.input().index().to_str()),
    assets: valueToAssets(coreUtxo.output().amount()),
    address: coreUtxo.output().address().as_byron()
      ? coreUtxo.output().address().as_byron()?.to_base58()!
      : coreUtxo.output().address().to_bech32(undefined),
    datumHash: coreUtxo.output()?.datum()?.as_data_hash()?.to_hex(),
    datum: coreUtxo.output()?.datum()?.as_data() &&
      toHex(coreUtxo.output().datum()!.as_data()!.to_bytes()),
    scriptRef: coreUtxo.output()?.script_ref() &&
      toHex(coreUtxo.output().script_ref()!.to_bytes()),
  };
};

export const networkToId = (network: Network): number => {
  switch (network) {
    case "Testnet":
      return 0;
    case "Preview":
      return 0;
    case "Preprod":
      return 0;
    case "Mainnet":
      return 1;
    default:
      throw new Error("Network not found");
  }
};

export const fromHex = (hex: string): Uint8Array => decodeString(hex);

export const toHex = (bytes: Uint8Array): string => encodeToString(bytes);

/*
It's okay to assume for now the slot duration is 1s for every era (Byron excluded).
To convert slot to unix time and vice versa in a specific network we simply need one absolute slot and the belonging unix timestamp.
*/

const slotToUnixTime = (slot: Slot, network: Network): UnixTime => {
  switch (network) {
    case "Testnet":
      return 1595967616000 + (slot * 1000 - 1598400000);
    case "Preview":
      return 1661369430000 + (slot * 1000 - 1366230000);
    case "Preprod":
      return 1661369504000 + (slot * 1000 - 5686304000);
    case "Mainnet":
      return 1596491091000 + (slot * 1000 - 4924800000);
    default:
      throw new Error("Network not found");
  }
};

const unixTimeToSlot = (unixTime: UnixTime, network: Network): Slot => {
  switch (network) {
    case "Testnet":
      return Math.floor((unixTime - 1595967616000 + 1598400000) / 1000);
    case "Preview":
      return Math.floor((unixTime - 1661369430000 + 1366230000) / 1000);
    case "Preprod":
      return Math.floor((unixTime - 1661369504000 + 5686304000) / 1000);
    case "Mainnet":
      return Math.floor((unixTime - 1596491091000 + 4924800000) / 1000);
    default:
      throw new Error("Network not found");
  }
};

export const hexToUtf8 = (hex: string): string =>
  new TextDecoder().decode(decode(new TextEncoder().encode(hex)));

export const utf8ToHex = (utf8: string): string =>
  toHex(new TextEncoder().encode(utf8));

// WIP!! This is not finalized yet until CIP-0067 and CIP-0068 are merged

const checksum = (num: number): string =>
  num.toString(16).split("").reduce(
    (acc, curr) => acc + parseInt(curr, 16),
    0x0,
  )
    .toString(16).padStart(2, "0");

export const toLabel = (num: number): string => {
  if (num < 0 || num > 65535) {
    throw new Error(
      `Label ${num} out of range: min label 0 - max label 65535.`,
    );
  }
  return "0" + num.toString(16).padStart(4, "0") + checksum(num) +
    "0";
};

export const fromLabel = (label: string): number | null => {
  if (label.length !== 8 || !(label[0] === "0" && label[7] === "0")) {
    return null;
  }
  const num = parseInt(label.slice(1, 5), 16);
  const check = label.slice(5, 7);
  return check === checksum(num) ? num : null;
};

/**
 * @param name UTF-8 encoded
 */
export const toUnit = (
  policyId: PolicyId,
  name?: string | null,
  label?: number | null,
): Unit => {
  const hexLabel = Number.isInteger(label) ? toLabel(label!) : "";
  const hexName = name ? toHex(new TextEncoder().encode(name)) : "";
  if ((hexName + hexLabel).length > 64) {
    throw new Error("Asset name size exceeds 32 bytes.");
  }
  if (policyId.length !== 56) {
    throw new Error(`Policy Id invalid: ${policyId}.`);
  }
  return policyId + hexLabel + hexName;
};

/**
 * Splits unit into policy id, name and label if applicable.
 * name will be returned in UTF-8 if possible, otherwise in HEX.
 */
export const fromUnit = (
  unit: Unit,
): { policyId: PolicyId; name: string | null; label: number | null } => {
  const policyId = unit.slice(0, 56);
  const label = fromLabel(unit.slice(56, 64));
  const name = (() => {
    const hexName = Number.isInteger(label) ? unit.slice(64) : unit.slice(56);
    if (!hexName) return null;
    try {
      return hexToUtf8(hexName);
    } catch (_e) {
      return hexName;
    }
  })();
  return { policyId, name, label };
};
