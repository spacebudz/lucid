import { C, Core } from "../core/mod.ts";
import { Datum, Json, PlutusData, Redeemer } from "../types/mod.ts";
import { fromHex, toHex } from "./utils.ts";

export class Construct {
  index: number;
  args: PlutusData[];

  constructor(index: number, args: PlutusData[]) {
    this.index = index;
    this.args = args;
  }
}

export class Data {
  /**
   * Convert PlutusData to CBOR encoded data
   */
  static to(plutusData: PlutusData): Datum | Redeemer {
    const serialize = (data: PlutusData) => {
      try {
        if (
          typeof data === "bigint" ||
          typeof data === "number" ||
          (typeof data === "string" &&
            !isNaN(parseInt(data)) &&
            data.slice(-1) === "n")
        ) {
          const bigint = typeof data === "string"
            ? BigInt(data.slice(0, -1))
            : data;
          return C.PlutusData.new_integer(C.BigInt.from_str(bigint.toString()));
        } else if (typeof data === "string") {
          return C.PlutusData.new_bytes(fromHex(data));
        } else if (data instanceof Uint8Array) {
          return C.PlutusData.new_bytes(data);
        } else if (data instanceof Construct) {
          const { index, args } = data;
          const plutusList = C.PlutusList.new();

          args.forEach((arg) => plutusList.add(serialize(arg)));

          return C.PlutusData.new_constr_plutus_data(
            C.ConstrPlutusData.new(
              C.BigNum.from_str(index.toString()),
              plutusList,
            ),
          );
        } else if (data instanceof Array) {
          const plutusList = C.PlutusList.new();

          data.forEach((arg) => plutusList.add(serialize(arg)));

          return C.PlutusData.new_list(plutusList);
        } else if (data instanceof Map) {
          const plutusMap = C.PlutusMap.new();

          for (const [key, value] of data.entries()) {
            plutusMap.insert(serialize(key), serialize(value));
          }

          return C.PlutusData.new_map(plutusMap);
        }
        throw new Error("Unsupported type");
      } catch (error) {
        throw new Error("Could not serialize the data: " + error);
      }
    };
    return toHex(serialize(plutusData).to_bytes()) as Datum | Redeemer;
  }

  /**
   * Convert CBOR encoded data to PlutusData
   */
  static from(data: Datum | Redeemer): PlutusData {
    const plutusData = C.PlutusData.from_bytes(fromHex(data));
    const deserialize = (data: Core.PlutusData): PlutusData => {
      if (data.kind() === 0) {
        const constr = data.as_constr_plutus_data()!;
        const l = constr.data();
        const desL = [];
        for (let i = 0; i < l.len(); i++) {
          desL.push(deserialize(l.get(i)));
        }
        return new Construct(parseInt(constr.alternative().to_str()), desL);
      } else if (data.kind() === 1) {
        const m = data.as_map()!;
        const desM: Map<PlutusData, PlutusData> = new Map();
        const keys = m.keys();
        for (let i = 0; i < keys.len(); i++) {
          desM.set(deserialize(keys.get(i)), deserialize(m.get(keys.get(i))!));
        }
        return desM;
      } else if (data.kind() === 2) {
        const l = data.as_list()!;
        const desL = [];
        for (let i = 0; i < l.len(); i++) {
          desL.push(deserialize(l.get(i)));
        }
        return desL;
      } else if (data.kind() === 3) {
        return BigInt(data.as_integer()!.to_str());
      } else if (data.kind() === 4) {
        return toHex(data.as_bytes()!);
      }
      throw new Error("Unsupported type");
    };
    return deserialize(plutusData);
  }

  /** Convert conveniently a JSON object (e.g. Metadata) to PlutusData
   *
   * Note: Constructor cannot be used here obviously
   */
  static fromJson(json: Json): PlutusData {
    const toPlutusData = (json: Json): PlutusData => {
      if (typeof json === "string") {
        return toHex(new TextEncoder().encode(json));
      }
      if (typeof json === "number") return BigInt(json);
      if (typeof json === "bigint") return json;
      if (json instanceof Array) return json.map((v) => toPlutusData(v));
      if (json instanceof Object) {
        const tempMap: Map<PlutusData, PlutusData> = new Map();
        Object.entries(json).forEach(([key, value]) => {
          tempMap.set(toPlutusData(key), toPlutusData(value));
        });
        return tempMap as PlutusData;
      }
      throw new Error("Unsupported type");
    };
    return toPlutusData(json);
  }

  /**
   * Convert plutusData to a JSON object
   *
   * Note: Constructor cannot be used here, also only bytes/integers as JSON keys
   */
  static toJson(plutusData: PlutusData): Json {
    const fromPlutusData = (data: PlutusData): Json => {
      if (
        typeof data === "bigint" ||
        typeof data === "number" ||
        (typeof data === "string" &&
          !isNaN(parseInt(data)) &&
          data.slice(-1) === "n")
      ) {
        const bigint = typeof data === "string"
          ? BigInt(data.slice(0, -1))
          : data;
        return parseInt(bigint.toString());
      }
      if (typeof data === "string") {
        return new TextDecoder().decode(fromHex(data));
      }
      if (data instanceof Array) return data.map((v) => fromPlutusData(v));
      if (data instanceof Map) {
        const tempJson: Json = {};
        data.forEach((value, key) => {
          const convertedKey = fromPlutusData(key);
          console.log(convertedKey);
          if (
            typeof convertedKey !== "string" &&
            typeof convertedKey !== "number"
          ) {
            throw new Error(
              "Unsupported type (Note: Only bytes or integers can be keys of a JSON object",
            );
          }
          tempJson[convertedKey] = fromPlutusData(value);
        });
        return tempJson;
      }
      throw new Error(
        "Unsupported type (Note: Constructor cannot be converted to JSON)",
      );
    };
    return fromPlutusData(plutusData);
  }

  static empty(): Datum | Redeemer {
    return toHex(
      C.PlutusData.new_constr_plutus_data(
        C.ConstrPlutusData.new(C.BigNum.from_str("0"), C.PlutusList.new()),
      ).to_bytes(),
    );
  }
}
