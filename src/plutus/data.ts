import {
  Static as _Static,
  TEnum,
  TLiteral,
  TLiteralValue,
  TObject,
  TProperties,
  TSchema,
  Type,
} from "https://deno.land/x/typebox@0.25.13/src/typebox.ts";
import { C, Core } from "../core/mod.ts";
import { Datum, Json, Redeemer } from "../types/mod.ts";
import { fromHex, toHex } from "../utils/utils.ts";

export class Constr<T> {
  index: number;
  fields: T[];

  constructor(index: number, fields: T[]) {
    this.index = index;
    this.fields = fields;
  }
}

export declare namespace Data {
  export type Static<T extends TSchema, P extends unknown[] = []> = _Static<
    T,
    P
  >;
}

export type Data =
  | bigint // Integer
  | string // Bytes in hex
  | Array<Data>
  | Map<Data, Data> // AssocList
  | Constr<Data>;

export const Data = {
  // Types
  // Note: Recursive types are not supported (yet)
  BigInt: Type.Unsafe<bigint>({ type: "bigint" }),
  String: Type.String(), // Bytes in hex
  Boolean: Type.Boolean(),
  Any: Type.Unsafe<Data>(),
  Array: function <T extends TSchema>(schema: T) {
    return Type.Array(schema);
  },
  Map: function <T extends TSchema, U extends TSchema>(
    keySchema: T,
    valueSchema: U,
  ) {
    return Type.Unsafe<Map<Data.Static<T>, Data.Static<U>>>({
      key: keySchema,
      value: valueSchema,
      type: "map",
    });
  },
  /**
   * Object applies by default a PlutusData Constr with index 0.\
   * Set 'hasConstr' to false to serialize Object as PlutusData List.
   */
  Object: function <T extends TProperties>(
    properties: T,
    hasConstr = true,
  ) {
    return Type.Object(properties, { hasConstr });
  },
  Enum: function <T extends TSchema>(items: T[]) {
    return Type.Union(items);
  },
  /**
   * Tuple is by default a PlutusData List.\
   * Set 'hasConstr' to true to apply a PlutusData Constr with index 0.
   */
  Tuple: function <T extends TSchema[]>(items: [...T], hasConstr = false) {
    return Type.Tuple(items, { hasConstr });
  },
  Literal: function <T extends TLiteralValue>(literal: T): TLiteral<T> {
    return Type.Literal(literal);
  },
  Nullable: function <T extends TSchema>(schema: T) {
    return Type.Unsafe<Data.Static<T> | null>({ ...schema, nullable: true });
  },

  /**
   * Convert PlutusData to Cbor encoded data.\
   * Or apply a shape and convert the provided data struct to Cbor encoded data.
   */
  to,
  /** Convert Cbor encoded data to PlutusData */
  from,
  /**
   *  Convert Cbor encoded data to Data.\
   *  Or apply a shape and cast the cbor encoded data to a certain type.
   */
  fromJson,
  /**
   * Convert PlutusData to a Json object.
   * Note: Constructor cannot be used here, also only bytes/integers as Json keys.
   */
  toJson,
  void: function (): Datum | Redeemer {
    return "d87980";
  },
  castFrom,
  castTo,
};

/**
 * Convert PlutusData to Cbor encoded data.\
 * Or apply a shape and convert the provided data struct to Cbor encoded data.
 */
function to<T = Data>(data: T, shape?: TSchema): Datum | Redeemer {
  function serialize(data: Data): Core.PlutusData {
    try {
      if (
        typeof data === "bigint"
      ) {
        return C.PlutusData.new_integer(C.BigInt.from_str(data.toString()));
      } else if (typeof data === "string") {
        return C.PlutusData.new_bytes(fromHex(data));
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
  }
  const d = shape ? castTo<T>(data, shape) : data as Data;
  return toHex(serialize(d).to_bytes()) as Datum | Redeemer;
}

/**
 *  Convert Cbor encoded data to Data.\
 *  Or apply a shape and cast the cbor encoded data to a certain type.
 */
function from<T = Data>(raw: Datum | Redeemer, shape?: TSchema): T {
  function deserialize(data: Core.PlutusData): Data {
    if (data.kind() === 0) {
      const constr = data.as_constr_plutus_data()!;
      const l = constr.data();
      const desL = [];
      for (let i = 0; i < l.len(); i++) {
        desL.push(deserialize(l.get(i)));
      }
      return new Constr(parseInt(constr.alternative().to_str()), desL);
    } else if (data.kind() === 1) {
      const m = data.as_map()!;
      const desM: Map<Data, Data> = new Map();
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
  }
  const data = deserialize(C.PlutusData.from_bytes(fromHex(raw)));

  return shape ? castFrom<T>(data, shape) : data as T;
}

/**
 * Convert conveniently a Json object (e.g. Metadata) to PlutusData.
 * Note: Constructor cannot be used here.
 */
function fromJson(json: Json): Data {
  function toData(json: Json): Data {
    if (typeof json === "string") {
      return json.startsWith("0x")
        ? json.slice(2)
        : toHex(new TextEncoder().encode(json));
    }
    if (typeof json === "number") return BigInt(json);
    if (typeof json === "bigint") return json;
    if (json instanceof Array) return json.map((v) => toData(v));
    if (json instanceof Object) {
      const tempMap: Map<Data, Data> = new Map();
      Object.entries(json).forEach(([key, value]) => {
        tempMap.set(toData(key), toData(value));
      });
      return tempMap as Data;
    }
    throw new Error("Unsupported type");
  }
  return toData(json);
}

/**
 * Convert PlutusData to a Json object.
 * Note: Constructor cannot be used here, also only bytes/integers as Json keys.
 */
function toJson(plutusData: Data): Json {
  function fromData(data: Data): Json {
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
      try {
        return new TextDecoder(undefined, { fatal: true }).decode(
          fromHex(data),
        );
      } catch (_) {
        return "0x" + data;
      }
    }
    if (data instanceof Array) return data.map((v) => fromData(v));
    if (data instanceof Map) {
      const tempJson: Json = {};
      data.forEach((value, key) => {
        const convertedKey = fromData(key);
        if (
          typeof convertedKey !== "string" &&
          typeof convertedKey !== "number"
        ) {
          throw new Error(
            "Unsupported type (Note: Only bytes or integers can be keys of a JSON object)",
          );
        }
        tempJson[convertedKey] = fromData(value);
      });
      return tempJson;
    }
    throw new Error(
      "Unsupported type (Note: Constructor cannot be converted to JSON)",
    );
  }
  return fromData(plutusData);
}

function castFrom<T>(data: Data, shape: TSchema): T {
  if (!shape) throw new Error("Could not type cast data.");
  const shapeType = (shape.anyOf ? "enum" : "") || shape.type;

  if (shape.nullable) {
    if (!(data instanceof Constr)) {
      throw new Error("Could not type cast to nullable.");
    }
    if (data.index === 0) {
      const noNullableShape = { ...shape };
      noNullableShape.nullable = false;
      return castFrom<T>(data.fields[0], noNullableShape);
    } else if (data.index === 1 && data.fields.length === 0) return null as T;
    throw new Error("Could not type cast to nullable.");
  }

  switch (shapeType) {
    case "bigint": {
      if (typeof data !== "bigint") {
        throw new Error("Could not type cast to bigint.");
      }

      return data as T;
    }
    case "string": {
      if (typeof data !== "string") {
        throw new Error("Could not type cast to string/bytes.");
      }

      return data as T;
    }
    case "boolean": {
      if (!(data instanceof Constr)) {
        throw new Error("Could not type cast to boolean.");
      }
      if (data.index === 0 && data.fields.length === 0) return false as T;
      else if (data.index === 1 && data.fields.length === 0) return true as T;

      throw new Error("Could not type cast to boolean.");
    }
    case "enum": {
      if (!(data instanceof Constr)) {
        throw new Error("Could not type cast to enum.");
      }
      const enumSchema = shape.anyOf[data.index];
      if (!enumSchema) throw new Error("Could not type cast to enum.");
      switch (enumSchema.type) {
        case "string": {
          if (
            typeof enumSchema.const === "string" &&
            /[A-Z]/.test(enumSchema.const[0]) && data.fields.length === 0
          ) {
            return enumSchema.const as T;
          }
          throw new Error("Could not type cast to enum.");
        }
        case "object": {
          const objectSchema: TObject = enumSchema.properties;
          const key = Object.keys(objectSchema)[0];

          if (!(/[A-Z]/.test(key[0]))) {
            throw new Error(
              "Could not type cast to enum. Enums need to start with an uppercase letter.",
            );
          }

          return {
            [key]: castFrom<T>(data.fields, objectSchema[key]),
          } as T;
        }
      }
      throw new Error("Could not type cast to enum.");
    }
    case "object": {
      if (data instanceof Constr && data.index === 0 && shape.hasConstr) {
        const fields: Record<string, T> = {};
        Object.entries(shape.properties as TSchema).forEach(
          ([name, schema]: [string, TSchema], index: number) => {
            if ((/[A-Z]/.test(name[0]))) {
              throw new Error(
                "Could not type cast to object. Object properties need to start with a lowercase letter.",
              );
            }
            fields[name] = castFrom<T>(
              data.fields[index],
              schema,
            );
          },
        );
        return fields as T;
      } else if (data instanceof Array && !shape.hasConstr) {
        const fields: Record<string, T> = {};
        Object.entries(shape.properties as TSchema).forEach(
          ([name, schema]: [string, TSchema], index: number) => {
            if ((/[A-Z]/.test(name[0]))) {
              throw new Error(
                "Could not type cast to object. Object properties need to start with a lowercase letter.",
              );
            }
            fields[name] = castFrom<T>(
              data[index],
              schema,
            );
          },
        );
        return fields as T;
      }
      throw new Error("Could not type cast to object.");
    }
    case "array": {
      if (shape.items instanceof Array) { // tuple
        if (data instanceof Constr && data.index === 0 && shape.hasConstr) {
          return data.fields.map((field, index) =>
            castFrom<T>(field, shape.items[index])
          ) as T;
        } else if (data instanceof Array && !shape.hasConstr) {
          return data.map((field, index) =>
            castFrom<T>(field, shape.items[index])
          ) as T;
        }

        throw new Error("Could not type cast to tuple.");
      } else { // array
        if (!(data instanceof Array)) {
          throw new Error("Could not type cast to array.");
        }

        return data.map((field) => castFrom<T>(field, shape.items)) as T;
      }
    }
    case "map": {
      if (!(data instanceof Map)) {
        throw new Error("Could not type cast to map.");
      }

      const map = new Map();
      for (
        const [key, value] of (data)
          .entries()
      ) {
        map.set(castFrom<T>(key, shape.key), castFrom<T>(value, shape.value));
      }
      return map as T;
    }
    case undefined:
      return data as T;
  }
  throw new Error("Could not type cast data.");
}

function castTo<T>(struct: T, shape: TSchema): Data {
  if (!shape) throw new Error("Could not type cast struct.");
  const shapeType = (shape.anyOf ? "enum" : "") || shape.type;

  if (shape.nullable) {
    if (struct !== null) {
      const noNullableShape = { ...shape };
      noNullableShape.nullable = false;
      return new Constr(0, [castTo<T>(struct, noNullableShape)]);
    }
    return new Constr(1, []);
  }

  switch (shapeType) {
    case "bigint": {
      if (typeof struct !== "bigint") {
        throw new Error("Could not type cast to bigint.");
      }

      return struct as bigint;
    }
    case "string": {
      if (typeof struct !== "string") {
        throw new Error("Could not type cast to string/bytes.");
      }

      return struct as string;
    }
    case "boolean": {
      if (typeof struct !== "boolean") {
        throw new Error("Could not type cast to boolean.");
      }

      return new Constr(struct ? 1 : 0, []);
    }
    case "enum": {
      switch (typeof struct) {
        case "string": {
          if (!(/[A-Z]/.test(struct[0]))) {
            throw new Error(
              "Could not type cast to enum. Enum needs to start with a uppercase letter.",
            );
          }
          const enumIndex = (shape as TEnum).anyOf.findIndex((
            schema: TLiteral,
          ) => schema.type === "string" && schema.const === struct);
          if (enumIndex < 0) throw "Could not type cast to enum.";
          return new Constr(enumIndex, []);
        }
        case "object": {
          if (struct === null) throw "Could not type cast to enum.";
          const enumKey = Object.keys(struct)[0];
          if (!(/[A-Z]/.test(enumKey[0]))) {
            throw new Error(
              "Could not type cast to enum. Enum needs to start with a uppercase letter.",
            );
          }
          const enumIndex = (shape as TEnum).anyOf.findIndex((
            schema: TSchema,
          ) =>
            schema.type === "object" &&
            Object.keys(schema.properties)[0] === enumKey
          );
          const enumSchema = shape.anyOf[enumIndex].properties[enumKey];

          return new Constr(
            enumIndex,
            (struct as Record<string, T[]>)[enumKey].map((item, index) =>
              castTo<T>(item, enumSchema.items[index])
            ),
          );
        }
      }
      throw new Error("Could not type cast to enum.");
    }
    case "object": {
      if (typeof struct !== "object" || struct === null) {
        throw new Error("Could not type cast to object.");
      }
      const fields = Object.keys(shape.properties).map((name) =>
        castTo<T>((struct as Record<string, T>)[name], shape.properties[name])
      );
      return shape.hasConstr ? new Constr(0, fields) : fields;
    }
    case "array": {
      if (!(struct instanceof Array)) {
        throw new Error("Could not type cast to array.");
      }
      if (shape.items instanceof Array) { // tuple
        const fields = struct.map((item, index) =>
          castTo<T>(item, shape.items[index])
        );
        return shape.hasConstr ? new Constr(0, fields) : fields;
      } else { // array
        return struct.map((item) => castTo<T>(item, shape.items));
      }
    }
    case "map": {
      if (!(struct instanceof Map)) {
        throw new Error("Could not type cast to map.");
      }
      const map = new Map<Data, Data>();
      for (
        const [key, value] of (struct)
          .entries()
      ) {
        map.set(castTo<T>(key, shape.key), castTo<T>(value, shape.value));
      }
      return map;
    }
    case undefined: {
      return struct as Data;
    }
  }
  throw new Error("Could not type cast struct.");
}
