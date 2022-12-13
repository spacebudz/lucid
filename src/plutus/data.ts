import { C, Core } from "../core/mod.ts";
import { Datum, Json, PlutusData, Redeemer, Bytes, RecordType, List } from "../types/mod.ts";
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

export class Alternative<T> {
  index: number;
  fields: RecordType<T>;

  constructor(index: number, fields: RecordType<T>) {
    this.index = index;
    this.fields = fields;
  }
}

export class UnsizedConstr<T> {
  index: number;
  fieldShape: T;

  constructor(index: number, fieldShape: T) {
    this.index = index;
    this.fieldShape = fieldShape;
  }
}

export class UnsizedList<T> {
  elemShape: T;

  constructor(elemShape: T) {
    this.elemShape = elemShape;
  }
}


export class UnsizedMap<Tk, Tv> {
  keyShape: Tk;
  valueShape: Tv;

  constructor(keyShape: Tk, valueShape: Tv) {
    this.keyShape = keyShape;
    this.valueShape = valueShape;
  }
}

// Seems we need Unsized* to represent containers of arbitrary size yet uniform content types.
// Heterogeneous containers seem possible ochain, as it seems Plutarch records represent as Lists.

// @ts-ignore: TODO fix circular reference
export type Shape =
  | undefined
  | bigint
  | string
  | Uint8Array
  | List<Shape>
  | Map<Shape, Shape>
  | Constr<Shape> // We emulate the constr like this
  | RecordType<Shape> // To represent record syntax. Represented as lists onchain.
  | UnsizedConstr<Shape>
  | UnsizedMap<Shape, Shape>
  | UnsizedList<Shape>
  | SumType<Shape>;

export type ListLike<T> = List<T> | RecordType<T> | UnsizedList<T>
export type MapLike<Tk, Tv> = Map<Tk, Tv> | UnsizedMap<Tk, Tv>
export type SumType<T> = Array<Alternative<T>>

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
          data.forEach((arg) => plutusList.add(serialize(arg)))
          return C.PlutusData.new_list(plutusList);
        } else if (data instanceof Map) {
          const plutusMap = C.PlutusMap.new();

          for (const [key, value] of data.entries()) {
            plutusMap.insert(serialize(key), serialize(value));
          }

          return C.PlutusData.new_map(plutusMap);
        } else if (data instanceof Object) { // interpreting as record types
          return serialize(Object.values(data))
        }
        throw new Error("Unsupported type: " + JSON.stringify(data));
      } catch (error) {
        throw new Error(`Could not serialize ${JSON.stringify(data)}: ${error}`);
      }
    }
    return toHex(serialize(plutusData).to_bytes()) as Datum | Redeemer;
  }

  private static deserializeRecord(l: Core.PlutusList, shape: RecordType<Shape>): RecordType<PlutusData> {
    const fields: string[] = Object.keys(shape);
    const shapes: Shape[] = Object.values(shape);
    assert(l.len() === fields.length, "unexpected Record size");
    const desR: RecordType<PlutusData> = {};
    for (let i = 0; i < l.len(); i++) {
      desR[fields[i]] = Data.deserialize(l.get(i), shapes[i]);
    }
    return desR;
  }

  private static deserializeSum(index: number, fields: Core.PlutusList, shape: SumType<Shape>): RecordType<PlutusData> {
    assert(index < shape.length, "sum type Constr index out of bounds");
    return Data.deserializeRecord(fields, shape[index]);
  }
  
  private static deserializeConstr(data: Core.PlutusData, shape?: Shape): Constr<PlutusData> | RecordType<PlutusData> {
    const constr = data.as_constr_plutus_data()!;
    const index = parseInt(constr.alternative().to_str());
    const fields = constr.data();

    let getInnerShape: (i: number) => Shape = () => undefined;
    if (shape) {
      if (shape instanceof Constr<Shape>) {
        assert(shape.index === index, "wrong Constr index");
        assert(shape.fields.length === fields.len(), "wrong Constr size");
        getInnerShape = (i) => shape.fields[i];
      } else if (shape instanceof UnsizedConstr<Shape>) {
        assert(shape.index === index, "wrong Constr index");
        getInnerShape = () => shape.fieldShape;
      } else if (shape instanceof Array<Alternative<Shape>>) {
        return Data.deserializeSum(index, fields, shape);
      } else {
        throw "expected Constr"
      }
    }

    const desL: PlutusData[] = [];
    for (let i = 0; i < fields.len(); i++) {
      desL.push(Data.deserialize(
          fields.get(i),
          getInnerShape(i)
        ));
    }
    return new Constr(index, desL);
  }

  private static deserializeMap(data: Core.PlutusData, shape?: MapLike<Shape, Shape>): Map<PlutusData, PlutusData> {
    const m = data.as_map()!;

    let getKeyShape: () => Shape = () => undefined;
    let getValueShape: (keyShape: Shape) => Shape = () => undefined;
    if (shape) {
      if (shape instanceof Map<Shape, Shape>) {
        assert(shape.size === m.len(), "wrong Map size");
        const keyIterator = shape.keys();
        getKeyShape = () => keyIterator.next().value;
        getValueShape = (keyShape: Shape) => shape.get(keyShape);
      } else if (shape instanceof UnsizedMap<Shape, Shape>) {
        getKeyShape = () => shape.keyShape;
        getValueShape = () => shape.valueShape;
      } else {
        throw "expected Map";
      }
    }

    const desM: Map<PlutusData, PlutusData> = new Map();
    const keys = m.keys();
    for (let i = 0; i < keys.len(); i++) {
      const kShape = getKeyShape()
      const vShape = getValueShape(kShape)
      const k = Data.deserialize(
          keys.get(i),
          kShape
        );
      const v = Data.deserialize(
          m.get(keys.get(i))!,
          vShape
        );
      desM.set(k, v);
    }
    return desM;
  }

  private static deserializeList(data: Core.PlutusData, shape?: ListLike<Shape>): List<PlutusData> | RecordType<PlutusData> {
    const l = data.as_list()!;

    let getInnerShape: (i: number) => Shape = () => undefined;
    if (shape) {
      if (shape instanceof Array) {
        assert(shape.length === l.len(), "wrong List size");
        getInnerShape = (i) => shape[i];
      } else if (shape instanceof UnsizedList<Shape>) {
        getInnerShape = () => shape.elemShape;
      } else if (shape instanceof Object) {
        return this.deserializeRecord(l, shape);
      } else {
        throw "expected List";
      }
    }
    
    const desL: PlutusData[] = [];
    for (let i = 0; i < l.len(); i++) {
      desL.push(Data.deserialize(
          l.get(i),
          getInnerShape(i)
        ));
    }
    return desL;
  }

  private static deserializeInteger(data: Core.PlutusData, shape: Shape): bigint {
    assert(shape === undefined || typeof shape === "bigint", "expected Integer, got " + JSON.stringify(data));
    return BigInt(data.as_integer()!.to_str());
  }

  private static deserializeBytes(data: Core.PlutusData, shape: Shape): Bytes {
    if (shape instanceof Uint8Array) return data.as_bytes()!;
    assert(shape === undefined || typeof shape === "string", "expected ByteString, got " + JSON.stringify(data));
    return toHex(data.as_bytes()!);
  }

  private static deserialize(data: Core.PlutusData, shape: Shape): PlutusData {
    switch (data.kind()) {
      case 0:
        return Data.deserializeConstr(data, shape);
      case 1:
        return Data.deserializeMap(data, shape);
      case 2:
        return Data.deserializeList(data, shape);
      case 3:
        return Data.deserializeInteger(data, shape);
      case 4:
        return Data.deserializeBytes(data, shape);
      default:
        throw new Error("Unsupported type");
    }
  }

  /** Convert Cbor encoded data to PlutusData */
  static from(data: Datum | Redeemer, shape?: Shape): PlutusData {
    const plutusData = C.PlutusData.from_bytes(fromHex(data));
    return Data.deserialize(plutusData, shape);
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
      if (data instanceof Array) return data.map((v) => fromPlutusData(v));
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
      if (data instanceof Object) return fromPlutusData(Object.values(data)) // interpreting as records
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
