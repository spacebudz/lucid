import {
  decode,
  decodeString,
  encodeToString,
} from "https://deno.land/std@0.100.0/encoding/hex.ts";
import {
  Addresses,
  Assets,
  Credential,
  Data,
  Exact,
  Json,
  Utils,
} from "../mod.ts";
import { crc8 } from "../misc/crc8.ts";

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
  policyId: string,
  name?: string | null,
  label?: number | null,
): string {
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
  unit: string,
): {
  policyId: string;
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

export function applyParamsToScript<T extends unknown[] = Data[]>(
  params: Exact<[...T]>,
  plutusScript: string,
  type?: T,
): string {
  const p = Data.to(
    type ? Data.castTo<T>(params, Data.Tuple(type as Json)) : params as Data[],
  );

  return Utils.applyParamsToScript(p, plutusScript);
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

export function paymentCredentialOf(address: string): Credential {
  const { payment } = Addresses.inspect(address);
  if (!payment) {
    throw new Error(
      "The specified address does not contain a payment credential.",
    );
  }
  return payment;
}

export function stakeCredentialOf(address: string): Credential {
  const { delegation } = Addresses.inspect(address);
  if (!delegation) {
    throw new Error(
      "The specified address does not contain a stake credential.",
    );
  }
  return delegation;
}
