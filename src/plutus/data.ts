import { C, Core } from "../core/mod.ts";
import { Datum, Json, PlutusData, Redeemer, Bytes } from "../types/mod.ts";
import { fromHex, toHex } from "../utils/utils.ts";
import { assert } from "https://deno.land/std@0.167.0/testing/asserts.ts";

export class Constr<T> {
  index: number;
  fields: T[];

  constructor(index: number, fields: T[]) {
    this.index = index;
    this.fields = fields;
  }
}

export class Field<T> {
  key: string;
  value: T;

  constructor(key: string, value: T) {
    this.key = key;
    this.value = value;
  }

  static unpack<T>(data: T | Field<T>): T {
    return data instanceof Field ? data.value : data
  }

  static forEach<T>(l: Array<T> | Array<Field<T>>, fn: (arg0: T) => void): void {
    if (l.length === 0) {
      return;
    }
    if (l[0] instanceof Field) {
      l.forEach((arg) => {
        assert(arg instanceof Field, "expected only Fields in Array");
        fn(arg.value);
      })
    } else {
      l.forEach((arg) => {
        assert(!(arg instanceof Field), "expected no Fields in Array");
        fn(arg);
      })
    }
  }
}

export type Shape = PlutusData

export class Data {
  /** Convert PlutusData to Cbor encoded data */
  static to(plutusData: PlutusData): Datum | Redeemer {
    function serialize(data: PlutusData): Core.PlutusData {
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
        } else if (data instanceof Constr) {
          const { index, fields } = data;
          const plutusList = C.PlutusList.new();

          fields.forEach((field) => plutusList.add(serialize(field)));

          return C.PlutusData.new_constr_plutus_data(
            C.ConstrPlutusData.new(
              C.BigNum.from_str(index.toString()),
              plutusList,
            ),
          );
        } else if (data instanceof Array) {
          const plutusList = C.PlutusList.new();
          Field.forEach(data, (arg) => plutusList.add(serialize(arg)))
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
    }
    return toHex(serialize(plutusData).to_bytes()) as Datum | Redeemer;
  }

  /** Convert Cbor encoded data to PlutusData */
  static from(data: Datum | Redeemer, shape?: Shape): PlutusData {
    const plutusData = C.PlutusData.from_bytes(fromHex(data));

    function deserializeConstr(data: Core.PlutusData, shape?: Shape): Constr<PlutusData> {
      const constr = data.as_constr_plutus_data()!;
      const index = parseInt(constr.alternative().to_str());
      const l = constr.data();
      if (shape) {
        assert(shape instanceof Constr<Shape>, "expected Constr");
        assert((shape as Constr<Shape>).index === index, "wrong Constr index");
        assert((shape as Constr<Shape>).fields.length === l.len(), "wrong Constr size");
      }
      const desL: PlutusData[] = [];
      for (let i = 0; i < l.len(); i++) {
        desL.push(deserialize(
            l.get(i),
            shape ? (shape as Constr<Shape>).fields[i] : undefined
          ));
      }
      return new Constr(index, desL);
    }

    function deserializeMap(data: Core.PlutusData, shape?: Shape): Map<PlutusData, PlutusData> {
      assert(shape === undefined || shape instanceof Map<Shape, Shape>, "expected Map");
      const m = data.as_map()!;
      assert(shape === undefined || (shape as Map<Shape, Shape>).keys.length === m.len(), "wrong Map size");
      const desM: Map<PlutusData, PlutusData> = new Map();
      const keys = m.keys();
      const shapeKeys = shape ? (shape as Map<Shape, Shape>).keys() : undefined
      for (let i = 0; i < keys.len(); i++) {
        const kShape = shapeKeys?.next().value
        const vShape = shape ? (shape as Map<Shape, Shape>).get(kShape) : undefined
        const k = deserialize(
            keys.get(i),
            kShape
          );
        const v = deserialize(
            m.get(keys.get(i))!,
            vShape
          );
        desM.set(k, v);
      }
      return desM;
    }

    function deserializeList(data: Core.PlutusData, shape?: Shape): Array<PlutusData> | Array<Field<PlutusData>> {
      const l = data.as_list()!;
      if (shape) {
        assert(shape instanceof Array, "expected List");
        assert(shape.length === l.len(), "wrong List size");
        if (shape.length > 0 && shape[0] instanceof Field) {
          const desR: Array<Field<Shape>> = [];
          for (let i = 0; i < l.len(); i++) {
            const field = shape[i];
            assert(field instanceof Field<Shape>, "expected only Fields in Array")
            desR.push(new Field(
              field.key,
              deserialize(l.get(i), field.value)
            ));
          }
          return desR;
        }
      }
      const desL: PlutusData[] = [];
      for (let i = 0; i < l.len(); i++) {
        desL.push(deserialize(
            l.get(i),
            shape ? (shape as Array<PlutusData>)[i] : undefined
          ));
      }
      return desL;
    }

    function deserializeInteger(data: Core.PlutusData, shape?: Shape): bigint {
      assert(shape === undefined || typeof shape === "bigint", "expected Integer");
      return BigInt(data.as_integer()!.to_str());
    }

    function deserializeBytes(data: Core.PlutusData, shape?: Shape): Bytes {
      assert(shape === undefined || typeof shape === "string" || shape instanceof Uint8Array, "expected ByteString");
      return toHex(data.as_bytes()!);
    }

    function deserialize(data: Core.PlutusData, shape?: Shape): PlutusData {
      switch (data.kind()) {
        case 0:
          return deserializeConstr(data, shape);
        case 1:
          return deserializeMap(data, shape);
        case 2:
          return deserializeList(data, shape);
        case 3:
          return deserializeInteger(data, shape);
        case 4:
          return deserializeBytes(data, shape);
        default:
          throw new Error("Unsupported type");
      }
    }

    return deserialize(plutusData, shape);
  }

  /**
   * Convert conveniently a Json object (e.g. Metadata) to PlutusData.
   * Note: Constructor cannot be used here.
   */
  static fromJson(json: Json): PlutusData {
    function toPlutusData(json: Json): PlutusData {
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
    }
    return toPlutusData(json);
  }

  /**
   * Convert PlutusData to a Json object.
   * Note: Constructor cannot be used here, also only bytes/integers as Json keys.
   */
  static toJson(plutusData: PlutusData): Json {
    function fromPlutusData(data: PlutusData): Json {
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
      if (data instanceof Array) return data.map((v) => fromPlutusData(Field.unpack(v)));
      if (data instanceof Map) {
        const tempJson: Json = {};
        data.forEach((value, key) => {
          const convertedKey = fromPlutusData(key);
          if (
            typeof convertedKey !== "string" &&
            typeof convertedKey !== "number"
          ) {
            throw new Error(
              "Unsupported type (Note: Only bytes or integers can be keys of a JSON object)",
            );
          }
          tempJson[convertedKey] = fromPlutusData(value);
        });
        return tempJson;
      }
      throw new Error(
        "Unsupported type (Note: Constructor cannot be converted to JSON)",
      );
    }
    return fromPlutusData(plutusData);
  }

  static empty(): Datum | Redeemer {
    return "d87980";
  }
}
