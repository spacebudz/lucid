import {
  Static as _Static,
  TEnum,
  TLiteral,
  TLiteralValue,
  TProperties,
  TSchema,
  Type,
} from "../../deps/deno.land/x/typebox@0.25.13/src/typebox.js";
import { C } from "../core/mod.js";
import { Datum, Exact, Json, Redeemer } from "../types/mod.js";
import { fromHex, fromText, toHex } from "../utils/utils.js";

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
  Integer: function (
    options?: {
      minimum?: number;
      maximum?: number;
      exclusiveMinimum?: number;
      exclusiveMaximum?: number;
    },
  ) {
    const integer = Type.Unsafe<bigint>({ dataType: "integer" });
    if (options) {
      Object.entries(options).forEach(([key, value]) => {
        integer[key] = value;
      });
    }
    return integer;
  },
  Bytes: function (
    options?: { minLength?: number; maxLength?: number; enum?: string[] },
  ) {
    const bytes = Type.Unsafe<string>({ dataType: "bytes" });
    if (options) {
      Object.entries(options).forEach(([key, value]) => {
        bytes[key] = value;
      });
    }
    return bytes;
  },
  Boolean: function () {
    return Type.Unsafe<boolean>({
      anyOf: [
        {
          title: "False",
          dataType: "constructor",
          index: 0,
          fields: [],
        },
        {
          title: "True",
          dataType: "constructor",
          index: 1,
          fields: [],
        },
      ],
    });
  },
  Any: function () {
    return Type.Unsafe<Data>({ description: "Any Data." });
  },
  Array: function <T extends TSchema>(
    items: T,
    options?: { minItems?: number; maxItems?: number; uniqueItems?: boolean },
  ) {
    const array = Type.Array(items);
    replaceProperties(array, { dataType: "list", items });
    if (options) {
      Object.entries(options).forEach(([key, value]) => {
        array[key] = value;
      });
    }
    return array;
  },
  Map: function <T extends TSchema, U extends TSchema>(
    keys: T,
    values: U,
    options?: { minItems?: number; maxItems?: number },
  ) {
    const map = Type.Unsafe<Map<Data.Static<T>, Data.Static<U>>>({
      dataType: "map",
      keys,
      values,
    });
    if (options) {
      Object.entries(options).forEach(([key, value]) => {
        map[key] = value;
      });
    }
    return map;
  },
  /**
   * Object applies by default a PlutusData Constr with index 0.\
   * Set 'hasConstr' to false to serialize Object as PlutusData List.
   */
  Object: function <T extends TProperties>(
    properties: T,
    options?: { hasConstr?: boolean },
  ) {
    const object = Type.Object(properties);
    replaceProperties(object, {
      anyOf: [{
        dataType: "constructor",
        index: 0, // Will be replaced when using Data.Enum
        fields: Object.entries(properties).map(([title, p]) => ({
          ...p,
          title,
        })),
      }],
    });
    object.anyOf[0].hasConstr = typeof options?.hasConstr === "undefined" ||
      options.hasConstr;
    return object;
  },
  Enum: function <T extends TSchema>(items: T[]) {
    const union = Type.Union(items);
    replaceProperties(
      union,
      {
        anyOf: items.map((item, index) =>
          item.anyOf[0].fields.length === 0
            ? ({
              ...item.anyOf[0],
              index,
            })
            : ({
              dataType: "constructor",
              title: (() => {
                const title = item.anyOf[0].fields[0].title;
                if (
                  (title as string).charAt(0) !==
                    (title as string).charAt(0).toUpperCase()
                ) {
                  throw new Error(
                    `Enum '${title}' needs to start with an uppercase letter.`,
                  );
                }
                return item.anyOf[0].fields[0].title;
              })(),
              index,
              fields: item.anyOf[0].fields[0].items ||
                item.anyOf[0].fields[0].anyOf[0].fields,
            })
        ),
      },
    );
    return union;
  },
  /**
   * Tuple is by default a PlutusData List.\
   * Set 'hasConstr' to true to apply a PlutusData Constr with index 0.
   */
  Tuple: function <T extends TSchema[]>(
    items: [...T],
    options?: { hasConstr?: boolean },
  ) {
    const tuple = Type.Tuple(items);
    replaceProperties(tuple, {
      dataType: "list",
      items,
    });
    if (options) {
      Object.entries(options).forEach(([key, value]) => {
        tuple[key] = value;
      });
    }
    return tuple;
  },
  Literal: function <T extends TLiteralValue>(title: T): TLiteral<T> {
    if (
      (title as string).charAt(0) !== (title as string).charAt(0).toUpperCase()
    ) {
      throw new Error(
        `Enum '${title}' needs to start with an uppercase letter.`,
      );
    }
    const literal = Type.Literal(title);
    replaceProperties(literal, {
      anyOf: [{
        dataType: "constructor",
        title,
        index: 0, // Will be replaced in Data.Enum
        fields: [],
      }],
    });
    return literal;
  },
  Nullable: function <T extends TSchema>(item: T) {
    return Type.Unsafe<Data.Static<T> | null>({
      anyOf: [
        {
          title: "Some",
          description: "An optional value.",
          dataType: "constructor",
          index: 0,
          fields: [
            item,
          ],
        },
        {
          title: "None",
          description: "Nothing.",
          dataType: "constructor",
          index: 1,
          fields: [],
        },
      ],
    });
  },

  /**
   * Convert PlutusData to Cbor encoded data.\
   * Or apply a shape and convert the provided data struct to Cbor encoded data.
   */
  to,
  /** Convert Cbor encoded data to PlutusData */
  from,
  /**
   * Note Constr cannot be used here.\
   * Strings prefixed with '0x' are not UTF-8 encoded.
   */
  fromJson,
  /**
   * Note Constr cannot be used here, also only bytes/integers as Json keys.\
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
function to<T = Data>(data: Exact<T>, type?: T): Datum | Redeemer {
  function serialize(data: Data): C.PlutusData {
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
  const d = type ? castTo<T>(data, type) : data as Data;
  return toHex(serialize(d).to_bytes()) as Datum | Redeemer;
}

/**
 *  Convert Cbor encoded data to Data.\
 *  Or apply a shape and cast the cbor encoded data to a certain type.
 */
function from<T = Data>(raw: Datum | Redeemer, type?: T): T {
  function deserialize(data: C.PlutusData): Data {
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

  return type ? castFrom<T>(data, type) : data as T;
}

/**
 * Note Constr cannot be used here.\
 * Strings prefixed with '0x' are not UTF-8 encoded.
 */
function fromJson(json: Json): Data {
  function toData(json: Json): Data {
    if (typeof json === "string") {
      return json.startsWith("0x")
        ? toHex(fromHex(json.slice(2)))
        : fromText(json);
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
 * Note Constr cannot be used here, also only bytes/integers as Json keys.\
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
        return "0x" + toHex(fromHex(data));
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

function castFrom<T = Data>(data: Data, type: T): T {
  const shape = type as Json;
  if (!shape) throw new Error("Could not type cast data.");
  const shapeType = (shape.anyOf ? "enum" : "") || shape.dataType;

  switch (shapeType) {
    case "integer": {
      if (typeof data !== "bigint") {
        throw new Error("Could not type cast to integer.");
      }
      integerConstraints(data, shape);
      return data as T;
    }
    case "bytes": {
      if (typeof data !== "string") {
        throw new Error("Could not type cast to bytes.");
      }
      bytesConstraints(data, shape);
      return data as T;
    }
    case "constructor": {
      if (isVoid(shape)) {
        if (
          !(data instanceof Constr) || data.index !== 0 ||
          data.fields.length !== 0
        ) {
          throw new Error("Could not type cast to void.");
        }
        return undefined as T;
      } else if (
        data instanceof Constr && data.index === shape.index &&
        (shape.hasConstr || shape.hasConstr === undefined)
      ) {
        const fields: Record<string, T> = {};
        if (shape.fields.length !== data.fields.length) {
          throw new Error(
            "Could not type cast to object. Fields do not match.",
          );
        }
        shape.fields.forEach(
          (field: Json, fieldIndex: number) => {
            const title = field.title || "wrapper";
            if ((/[A-Z]/.test(title[0]))) {
              throw new Error(
                "Could not type cast to object. Object properties need to start with a lowercase letter.",
              );
            }
            fields[title] = castFrom<T>(
              data.fields[fieldIndex],
              field,
            );
          },
        );
        return fields as T;
      } else if (
        data instanceof Array && !shape.hasConstr &&
        shape.hasConstr !== undefined
      ) {
        const fields: Record<string, T> = {};
        if (shape.fields.length !== data.length) {
          throw new Error("Could not ype cast to object. Fields do not match.");
        }
        shape.fields.forEach(
          (field: Json, fieldIndex: number) => {
            const title = field.title || "wrapper";
            if ((/[A-Z]/.test(title[0]))) {
              throw new Error(
                "Could not type cast to object. Object properties need to start with a lowercase letter.",
              );
            }
            fields[title] = castFrom<T>(
              data[fieldIndex],
              field,
            );
          },
        );
        return fields as T;
      }
      throw new Error("Could not type cast to object.");
    }
    case "enum": {
      // When enum has only one entry it's a single constructor/record object
      if (shape.anyOf.length === 1) {
        return castFrom<T>(data, shape.anyOf[0]);
      }

      if (!(data instanceof Constr)) {
        throw new Error("Could not type cast to enum.");
      }

      const enumShape = shape.anyOf.find((entry: Json) =>
        entry.index === data.index
      );
      if (!enumShape || enumShape.fields.length !== data.fields.length) {
        throw new Error("Could not type cast to enum.");
      }
      if (isBoolean(shape)) {
        if (data.fields.length !== 0) {
          throw new Error("Could not type cast to boolean.");
        }
        switch (data.index) {
          case 0:
            return false as T;
          case 1:
            return true as T;
        }
        throw new Error("Could not type cast to boolean.");
      } else if (isNullable(shape)) {
        switch (data.index) {
          case 0: {
            if (
              data.fields.length !== 1
            ) {
              throw new Error("Could not type cast to nullable object.");
            }
            return castFrom<T>(data.fields[0], shape.anyOf[0].fields[0]);
          }
          case 1: {
            if (
              data.fields.length !== 0
            ) {
              throw new Error("Could not type cast to nullable object.");
            }
            return null as T;
          }
        }
        throw new Error("Could not type cast to nullable object.");
      }
      switch (enumShape.dataType) {
        case "constructor": {
          if (enumShape.fields.length === 0) {
            if (
              /[A-Z]/.test(enumShape.title[0])
            ) {
              return enumShape.title as T;
            }
            throw new Error("Could not type cast to enum.");
          } else {
            if (!(/[A-Z]/.test(enumShape.title))) {
              throw new Error(
                "Could not type cast to enum. Enums need to start with an uppercase letter.",
              );
            }

            if (enumShape.fields.length !== data.fields.length) {
              throw new Error("Could not type cast to enum.");
            }

            // check if named args
            const args = enumShape.fields[0].title
              ? Object.fromEntries(enumShape.fields.map((
                field: Json,
                index: number,
              ) => [field.title, castFrom<T>(data.fields[index], field)]))
              : enumShape.fields.map((
                field: Json,
                index: number,
              ) => castFrom<T>(data.fields[index], field));

            return {
              [enumShape.title]: args,
            } as T;
          }
        }
      }
      throw new Error("Could not type cast to enum.");
    }
    case "list": {
      if (shape.items instanceof Array) {
        // tuple
        if (
          data instanceof Constr &&
          data.index === 0 &&
          shape.hasConstr
        ) {
          return data.fields.map((field, index) =>
            castFrom<T>(field, shape.items[index])
          ) as T;
        } else if (data instanceof Array && !shape.hasConstr) {
          return data.map((field, index) =>
            castFrom<T>(field, shape.items[index])
          ) as T;
        }

        throw new Error("Could not type cast to tuple.");
      } else {
        // array
        if (!(data instanceof Array)) {
          throw new Error("Could not type cast to array.");
        }
        listConstraints(data, shape);

        return data.map((field) => castFrom<T>(field, shape.items)) as T;
      }
    }
    case "map": {
      if (!(data instanceof Map)) {
        throw new Error("Could not type cast to map.");
      }
      mapConstraints(data, shape);
      const map = new Map();
      for (
        const [key, value] of (data)
          .entries()
      ) {
        map.set(castFrom<T>(key, shape.keys), castFrom<T>(value, shape.values));
      }
      return map as T;
    }
    case undefined: {
      return data as T;
    }
  }
  throw new Error("Could not type cast data.");
}

function castTo<T>(struct: Exact<T>, type: T): Data {
  const shape = type as Json;
  if (!shape) throw new Error("Could not type cast struct.");
  const shapeType = (shape.anyOf ? "enum" : "") || shape.dataType;

  switch (shapeType) {
    case "integer": {
      if (typeof struct !== "bigint") {
        throw new Error("Could not type cast to integer.");
      }
      integerConstraints(struct, shape);
      return struct as bigint;
    }
    case "bytes": {
      if (typeof struct !== "string") {
        throw new Error("Could not type cast to bytes.");
      }
      bytesConstraints(struct, shape);
      return struct as string;
    }
    case "constructor": {
      if (isVoid(shape)) {
        if (struct !== undefined) {
          throw new Error("Could not type cast to void.");
        }
        return new Constr(0, []);
      } else if (
        typeof struct !== "object" || struct === null ||
        shape.fields.length !== Object.keys(struct).length
      ) {
        throw new Error("Could not type cast to constructor.");
      }
      const fields = shape.fields.map((field: Json) =>
        castTo<T>(
          (struct as Record<string, Json>)[field.title || "wrapper"],
          field,
        )
      );
      return (shape.hasConstr || shape.hasConstr === undefined)
        ? new Constr(shape.index, fields)
        : fields;
    }
    case "enum": {
      // When enum has only one entry it's a single constructor/record object
      if (shape.anyOf.length === 1) {
        return castTo<T>(struct, shape.anyOf[0]);
      }

      if (isBoolean(shape)) {
        if (typeof struct !== "boolean") {
          throw new Error(
            "Could not type cast to boolean.",
          );
        }
        return new Constr(struct ? 1 : 0, []);
      } else if (isNullable(shape)) {
        if (struct === null) return new Constr(1, []);
        else {
          const fields = shape.anyOf[0].fields;
          if (fields.length !== 1) {
            throw new Error("Could not type cast to nullable object.");
          }
          return new Constr(0, [
            castTo<T>(struct, fields[0]),
          ]);
        }
      }
      switch (typeof struct) {
        case "string": {
          if (!(/[A-Z]/.test(struct[0]))) {
            throw new Error(
              "Could not type cast to enum. Enum needs to start with an uppercase letter.",
            );
          }
          const enumIndex = (shape as TEnum).anyOf.findIndex((
            s: TLiteral,
          ) =>
            s.dataType === "constructor" &&
            s.fields.length === 0 &&
            s.title === struct
          );
          if (enumIndex === -1) throw new Error("Could not type cast to enum.");
          return new Constr(enumIndex, []);
        }
        case "object": {
          if (struct === null) throw new Error("Could not type cast to enum.");
          const structTitle = Object.keys(struct)[0];

          if (!(/[A-Z]/.test(structTitle))) {
            throw new Error(
              "Could not type cast to enum. Enum needs to start with an uppercase letter.",
            );
          }
          const enumEntry = shape.anyOf.find((s: Json) =>
            s.dataType === "constructor" &&
            s.title === structTitle
          );

          if (!enumEntry) throw new Error("Could not type cast to enum.");

          const args = (struct as Record<string, T[] | Json>)[structTitle];

          return new Constr(
            enumEntry.index,
            // check if named args
            args instanceof Array
              ? args.map((item, index) =>
                castTo<T>(item, enumEntry.fields[index])
              )
              : enumEntry.fields.map(
                (entry: Json) => {
                  const [_, item]: [string, Json] = Object.entries(args).find((
                    [title],
                  ) => title === entry.title)!;
                  return castTo<T>(item, entry);
                },
              ),
          );
        }
      }
      throw new Error("Could not type cast to enum.");
    }
    case "list": {
      if (!(struct instanceof Array)) {
        throw new Error("Could not type cast to array/tuple.");
      }
      if (shape.items instanceof Array) {
        // tuple
        const fields = struct.map((item, index) =>
          castTo<T>(item, shape.items[index])
        );
        return shape.hasConstr ? new Constr(0, fields) : fields;
      } else {
        // array
        listConstraints(struct, shape);
        return struct.map((item) => castTo<T>(item, shape.items));
      }
    }
    case "map": {
      if (!(struct instanceof Map)) {
        throw new Error("Could not type cast to map.");
      }

      mapConstraints(struct, shape);

      const map = new Map<Data, Data>();
      for (
        const [key, value] of (struct)
          .entries()
      ) {
        map.set(castTo<T>(key, shape.keys), castTo<T>(value, shape.values));
      }
      return map;
    }
    case undefined: {
      return struct as Data;
    }
  }
  throw new Error("Could not type cast struct.");
}

function integerConstraints(integer: bigint, shape: TSchema) {
  if (shape.minimum && integer < BigInt(shape.minimum)) {
    throw new Error(
      `Integer ${integer} is below the minimum ${shape.minimum}.`,
    );
  }
  if (shape.maximum && integer > BigInt(shape.maximum)) {
    throw new Error(
      `Integer ${integer} is above the maxiumum ${shape.maximum}.`,
    );
  }
  if (shape.exclusiveMinimum && integer <= BigInt(shape.exclusiveMinimum)) {
    throw new Error(
      `Integer ${integer} is below the exclusive minimum ${shape.exclusiveMinimum}.`,
    );
  }
  if (shape.exclusiveMaximum && integer >= BigInt(shape.exclusiveMaximum)) {
    throw new Error(
      `Integer ${integer} is above the exclusive maximum ${shape.exclusiveMaximum}.`,
    );
  }
}

function bytesConstraints(bytes: string, shape: TSchema) {
  if (
    shape.enum && !shape.enum.some((keyword: string) => keyword === bytes)
  ) throw new Error(`None of the keywords match with '${bytes}'.`);
  if (shape.minLength && bytes.length / 2 < shape.minLength) {
    throw new Error(
      `Bytes need to have a length of at least ${shape.minLength} bytes.`,
    );
  }

  if (shape.maxLength && bytes.length / 2 > shape.maxLength) {
    throw new Error(
      `Bytes can have a length of at most ${shape.minLength} bytes.`,
    );
  }
}

function listConstraints(list: Array<unknown>, shape: TSchema) {
  if (shape.minItems && list.length < shape.minItems) {
    throw new Error(
      `Array needs to contain at least ${shape.minItems} items.`,
    );
  }
  if (shape.maxItems && list.length > shape.maxItems) {
    throw new Error(
      `Array can contain at most ${shape.maxItems} items.`,
    );
  }
  if (shape.uniqueItems && (new Set(list)).size !== list.length) {
    // Note this only works for primitive types like string and bigint.
    throw new Error(
      "Array constains duplicates.",
    );
  }
}

function mapConstraints(map: Map<unknown, unknown>, shape: TSchema) {
  if (shape.minItems && map.size < shape.minItems) {
    throw new Error(
      `Map needs to contain at least ${shape.minItems} items.`,
    );
  }

  if (shape.maxItems && map.size > shape.maxItems) {
    throw new Error(
      `Map can contain at most ${shape.maxItems} items.`,
    );
  }
}

function isBoolean(shape: TSchema): boolean {
  return shape.anyOf && shape.anyOf[0]?.title === "False" &&
    shape.anyOf[1]?.title === "True";
}

function isVoid(shape: TSchema): boolean {
  return shape.index === 0 && shape.fields.length === 0;
}

function isNullable(shape: TSchema): boolean {
  return shape.anyOf && shape.anyOf[0]?.title === "Some" &&
    shape.anyOf[1]?.title === "None";
}

function replaceProperties(object: Json, properties: Json) {
  Object.keys(object).forEach((key) => {
    delete object[key];
  });
  Object.assign(object, properties);
}
