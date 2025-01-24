import {
  Codec,
  type DataJson,
  type Exact,
  fromHex,
  fromText,
  type Json,
  toHex,
} from "../mod.ts";

export class Constr<T> {
  index: number;
  fields: T[];

  constructor(index: number, fields: T[]) {
    this.index = index;
    this.fields = fields;
  }
}

export type Data =
  | bigint
  | string
  | Array<Data>
  | Map<Data, Data>
  | Constr<Data>;

export const Data = {
  Bytes: (
    options?:
      | { minLength?: number; maxLength?: number; enum?: string[] }
      | number,
  ) => {
    const bytes: Record<string, unknown> = { dataType: "bytes" };
    if (typeof options === "number") {
      bytes.minLength = options;
      bytes.maxLength = options;
    } else if (options) {
      Object.entries(options).forEach(([key, value]) => {
        bytes[key] = value;
      });
    }
    return bytes as unknown as string;
  },
  Integer: () => ({ dataType: "integer" } as unknown as bigint),
  Boolean: () => ({
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
  } as unknown as boolean),
  Any: () => ({ description: "Any Data." } as unknown as Data),
  Array: <T>(
    items: T,
    options?:
      | { minItems?: number; maxItems?: number; uniqueItems?: boolean }
      | number,
  ) => {
    const array: Record<string, unknown> = { dataType: "list", items };
    if (typeof options === "number") {
      array.minItems = options;
      array.maxItems = options;
    } else if (options) {
      Object.entries(options).forEach(([key, value]) => {
        array[key] = value;
      });
    }
    return array as unknown as Array<T>;
  },
  Map: <K, V>(
    keys: K,
    values: V,
    options?: { minItems?: number; maxItems?: number } | number,
  ) => {
    const map: Record<string, unknown> = {
      dataType: "map",
      keys,
      values,
    };
    if (typeof options === "number") {
      map.minItems = options;
      map.maxItems = options;
    } else if (options) {
      Object.entries(options).forEach(([key, value]) => {
        map[key] = value;
      });
    }
    return map as unknown as Map<K, V>;
  },
  Object: <T extends object>(
    properties: T,
    options?: { hasConstr?: boolean },
  ) => {
    const object = {
      anyOf: [{
        dataType: "constructor",
        index: 0,
        fields: Object.entries(properties).map(([title, p]) => {
          if (title[0] !== title[0].toLowerCase()) {
            throw new Error(
              `Object requires lower case properties: found ${title}, expected ${
                title[0].toUpperCase() + title.slice(1)
              }`,
            );
          }
          return { ...p, title };
        }),
      }] as Array<Record<string, unknown>>,
    };
    object.anyOf[0].hasConstr = typeof options?.hasConstr === "undefined" ||
      options.hasConstr;
    return object as unknown as T;
  },
  Enum: <const T extends unknown[]>(...items: T) => {
    const union = {
      anyOf: items.flatMap((item, index) => {
        if (typeof item === "string") {
          if (item[0] !== item[0].toUpperCase()) {
            throw new Error(
              `Enum requires upper case: found ${item}, expected ${
                item[0].toUpperCase() + item.slice(1)
              }`,
            );
          }
          return { dataType: "constructor", title: item, index, fields: [] };
        } else {
          return Object.entries(item as object).map(
            ([title, fields], subIndex) => {
              if (title[0] !== title[0].toUpperCase()) {
                throw new Error(
                  `Enum requires upper case: found ${title}, expected ${
                    title[0].toUpperCase() + title.slice(1)
                  }`,
                );
              }
              return {
                dataType: "constructor",
                title,
                index: index + subIndex,
                fields: (fields instanceof Array)
                  ? fields
                  : Object.entries(fields).map(([title, value]) => {
                    if (title[0] !== title[0].toLowerCase()) {
                      throw new Error(
                        `Enum requires lower case args: found ${title}, expected ${
                          title[0].toUpperCase() + title.slice(1)
                        }`,
                      );
                    }
                    return { ...value as object, title };
                  }),
              };
            },
          );
        }
      }),
    };

    type DeepRemoveReadonly<T> = T extends object
      ? { -readonly [K in keyof T]: DeepRemoveReadonly<T[K]> }
      : T;

    return union as unknown as DeepRemoveReadonly<T[number]>;
  },
  Tuple: <T extends unknown[]>(
    items: [...T],
    options?: { hasConstr?: boolean },
  ) => {
    const tuple: Record<string, unknown> = {
      dataType: "list",
      items,
    };
    if (options) {
      Object.entries(options).forEach(([key, value]) => {
        tuple[key] = value;
      });
    }
    return tuple as unknown as T;
  },
  Nullable: <T>(item: T) => {
    return {
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
    } as unknown as T | null;
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
  fromMetadata,
  /**
   * Note Constr cannot be used here, also only bytes/integers as Json keys.\
   */
  toMetadata,
  void: function (): string {
    return "d87980";
  },
  castFrom,
  castTo,
};

/**
 * Convert PlutusData to Cbor encoded data.\
 * Or apply a shape and convert the provided data struct to Cbor encoded data.
 */
function to<T = Data>(data: Exact<T>, type?: T): string {
  function dataToJson(data: Data): DataJson {
    if (typeof data === "bigint") return { int: data };
    if (typeof data === "string") return { bytes: data };
    if (data instanceof Array) return { list: data.map(dataToJson) };
    if (data instanceof Map) {
      return {
        map: (() => {
          const map = [];
          for (const [key, value] of data.entries()) {
            map.push({ k: dataToJson(key), v: dataToJson(value) });
          }
          return map;
        })(),
      };
    }
    return { constructor: data.index, fields: data.fields.map(dataToJson) };
  }
  const d = type ? castTo<T>(data, type) : data as Data;
  return Codec.encodeData(dataToJson(d));
}

/**
 *  Convert Cbor encoded data to Data.\
 *  Or apply a shape and cast the cbor encoded data to a certain type.
 */
function from<T = Data>(raw: string, type?: T): T {
  function jsonToData(json: DataJson): Data {
    if ("int" in json) return json.int;
    if ("bytes" in json) return json.bytes;
    if ("list" in json) return json.list.map(jsonToData);
    if ("map" in json) {
      return new Map(
        json.map.map(({ k, v }) => [jsonToData(k), jsonToData(v)]),
      );
    }
    return new Constr(json.constructor, json.fields.map(jsonToData));
  }

  const data = jsonToData(Codec.decodeData(raw));
  return type ? castFrom<T>(data, type) : data as T;
}

/**
 * Note Constr cannot be used here.\
 * Strings prefixed with '0x' are not UTF-8 encoded.
 */
function fromMetadata(json: Json): Data {
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
 * Note Constr cannot be used here, also only bytes/integers as Json keys.
 */
function toMetadata(plutusData: Data): Json {
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
  const schema = type as Json;
  if (!schema) throw new Error("Could not type cast data.");
  const { shape, definitions } = schema.definitions
    ? schema
    : { shape: schema, definitions: {} };

  function castFromHelper<T = Data>(data: Data, type: T): T {
    const shape = type as Json;
    if (!shape) throw new Error("Could not type cast data.");
    const shapeType = (shape.anyOf ? "enum" : shape["$ref"] ? "$ref" : "") ||
      shape.dataType;

    switch (shapeType) {
      case "$ref": {
        const definition =
          definitions[shape["$ref"].split("#/definitions/")[1]];
        return castFromHelper(data, definition);
      }
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
              fields[title] = castFromHelper<T>(
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
            throw new Error(
              "Could not ype cast to object. Fields do not match.",
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
              fields[title] = castFromHelper<T>(
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
          return castFromHelper<T>(data, shape.anyOf[0]);
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
              return castFromHelper<T>(
                data.fields[0],
                shape.anyOf[0].fields[0],
              );
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
                ) => [
                  field.title,
                  castFromHelper<T>(data.fields[index], field),
                ]))
                : enumShape.fields.map((
                  field: Json,
                  index: number,
                ) => castFromHelper<T>(data.fields[index], field));

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
              castFromHelper<T>(field, shape.items[index])
            ) as T;
          } else if (data instanceof Array && !shape.hasConstr) {
            return data.map((field, index) =>
              castFromHelper<T>(field, shape.items[index])
            ) as T;
          }

          throw new Error("Could not type cast to tuple.");
        } else {
          // array
          if (!(data instanceof Array)) {
            throw new Error("Could not type cast to array.");
          }
          listConstraints(data, shape);

          return data.map((field) =>
            castFromHelper<T>(field, shape.items)
          ) as T;
        }
      }
      case "map": {
        if (!(data instanceof Map)) {
          throw new Error("Could not type cast to map.");
        }
        mapConstraints(data, shape);
        const map = new Map();
        for (
          const [key, value] of data
            .entries()
        ) {
          map.set(
            castFromHelper<T>(key, shape.keys),
            castFromHelper<T>(value, shape.values),
          );
        }
        return map as T;
      }
      case undefined: {
        return data as T;
      }
    }
    throw new Error("Could not type cast data.");
  }

  return castFromHelper(data, shape);
}

function castTo<T>(struct: Exact<T>, type: T): Data {
  const schema = type as Json;
  if (!schema) throw new Error("Could not type cast data.");
  const { shape, definitions } = schema.definitions
    ? schema
    : { shape: schema, definitions: {} };

  function castToHelper<T>(struct: Exact<T>, type: T): Data {
    const shape = type as Json;
    if (!shape) throw new Error("Could not type cast struct.");
    const shapeType = (shape.anyOf ? "enum" : shape["$ref"] ? "$ref" : "") ||
      shape.dataType;

    switch (shapeType) {
      case "$ref": {
        const definition =
          definitions[shape["$ref"].split("#/definitions/")[1]];
        return castToHelper(struct, definition);
      }
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
          castToHelper<T>(
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
          return castToHelper<T>(struct, shape.anyOf[0]);
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
              castToHelper<T>(struct, fields[0]),
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
            const enumIndex = shape.anyOf.findIndex((
              s: Json,
            ) =>
              s.dataType === "constructor" &&
              s.fields.length === 0 &&
              s.title === struct
            );
            if (enumIndex === -1) {
              throw new Error("Could not type cast to enum.");
            }
            return new Constr(enumIndex, []);
          }
          case "object": {
            if (struct === null) {
              throw new Error("Could not type cast to enum.");
            }
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
                  castToHelper<T>(item, enumEntry.fields[index])
                )
                : enumEntry.fields.map(
                  (entry: Json) => {
                    const [_, item]: [string, Json] = Object.entries(args).find(
                      (
                        [title],
                      ) => title === entry.title,
                    )!;
                    return castToHelper<T>(item, entry);
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
            castToHelper<T>(item, shape.items[index])
          );
          return shape.hasConstr ? new Constr(0, fields) : fields;
        } else {
          // array
          listConstraints(struct, shape);
          return struct.map((item) => castToHelper<T>(item, shape.items));
        }
      }
      case "map": {
        if (!(struct instanceof Map)) {
          throw new Error("Could not type cast to map.");
        }

        mapConstraints(struct, shape);

        const map = new Map<Data, Data>();
        for (
          const [key, value] of struct
            .entries()
        ) {
          map.set(
            castToHelper<T>(key, shape.keys),
            castToHelper<T>(value, shape.values),
          );
        }
        return map;
      }
      case undefined: {
        return struct as Data;
      }
    }
    throw new Error("Could not type cast struct.");
  }
  return castToHelper(struct, shape);
}

function integerConstraints(integer: bigint, shape: Json) {
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

function bytesConstraints(bytes: string, shape: Json) {
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

function listConstraints(list: Array<unknown>, shape: Json) {
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

function mapConstraints(map: Map<unknown, unknown>, shape: Json) {
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

function isBoolean(shape: Json): boolean {
  return shape.anyOf && shape.anyOf[0]?.title === "False" &&
    shape.anyOf[1]?.title === "True";
}

function isVoid(shape: Json): boolean {
  return shape.index === 0 && shape.fields.length === 0;
}

function isNullable(shape: Json): boolean {
  return shape.anyOf && shape.anyOf[0]?.title === "Some" &&
    shape.anyOf[1]?.title === "None";
}
