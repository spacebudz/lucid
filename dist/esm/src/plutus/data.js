import { Type, } from "../../deps/deno.land/x/typebox@0.25.13/src/typebox.js";
import { C } from "../core/mod.js";
import { fromHex, fromText, toHex } from "../utils/utils.js";
export class Constr {
    constructor(index, fields) {
        Object.defineProperty(this, "index", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "fields", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        this.index = index;
        this.fields = fields;
    }
}
export const Data = {
    // Types
    // Note: Recursive types are not supported (yet)
    Integer: function (options) {
        const integer = Type.Unsafe({ dataType: "integer" });
        if (options) {
            Object.entries(options).forEach(([key, value]) => {
                integer[key] = value;
            });
        }
        return integer;
    },
    Bytes: function (options) {
        const bytes = Type.Unsafe({ dataType: "bytes" });
        if (options) {
            Object.entries(options).forEach(([key, value]) => {
                bytes[key] = value;
            });
        }
        return bytes;
    },
    Boolean: function () {
        return Type.Unsafe({
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
        return Type.Unsafe({ description: "Any Data." });
    },
    Array: function (items, options) {
        const array = Type.Array(items);
        replaceProperties(array, { dataType: "list", items });
        if (options) {
            Object.entries(options).forEach(([key, value]) => {
                array[key] = value;
            });
        }
        return array;
    },
    Map: function (keys, values, options) {
        const map = Type.Unsafe({
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
    Object: function (properties, options) {
        const object = Type.Object(properties);
        replaceProperties(object, {
            anyOf: [{
                    dataType: "constructor",
                    index: 0,
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
    Enum: function (items) {
        const union = Type.Union(items);
        replaceProperties(union, {
            anyOf: items.map((item, index) => item.anyOf[0].fields.length === 0
                ? ({
                    ...item.anyOf[0],
                    index,
                })
                : ({
                    dataType: "constructor",
                    title: (() => {
                        const title = item.anyOf[0].fields[0].title;
                        if (title.charAt(0) !==
                            title.charAt(0).toUpperCase()) {
                            throw new Error(`Enum '${title}' needs to start with an uppercase letter.`);
                        }
                        return item.anyOf[0].fields[0].title;
                    })(),
                    index,
                    fields: item.anyOf[0].fields[0].items ||
                        item.anyOf[0].fields[0].anyOf[0].fields,
                })),
        });
        return union;
    },
    /**
     * Tuple is by default a PlutusData List.\
     * Set 'hasConstr' to true to apply a PlutusData Constr with index 0.
     */
    Tuple: function (items, options) {
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
    Literal: function (title) {
        if (title.charAt(0) !== title.charAt(0).toUpperCase()) {
            throw new Error(`Enum '${title}' needs to start with an uppercase letter.`);
        }
        const literal = Type.Literal(title);
        replaceProperties(literal, {
            anyOf: [{
                    dataType: "constructor",
                    title,
                    index: 0,
                    fields: [],
                }],
        });
        return literal;
    },
    Nullable: function (item) {
        return Type.Unsafe({
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
    void: function () {
        return "d87980";
    },
    castFrom,
    castTo,
};
/**
 * Convert PlutusData to Cbor encoded data.\
 * Or apply a shape and convert the provided data struct to Cbor encoded data.
 */
function to(data, type) {
    function serialize(data) {
        try {
            if (typeof data === "bigint") {
                return C.PlutusData.new_integer(C.BigInt.from_str(data.toString()));
            }
            else if (typeof data === "string") {
                return C.PlutusData.new_bytes(fromHex(data));
            }
            else if (data instanceof Constr) {
                const { index, fields } = data;
                const plutusList = C.PlutusList.new();
                fields.forEach((field) => plutusList.add(serialize(field)));
                return C.PlutusData.new_constr_plutus_data(C.ConstrPlutusData.new(C.BigNum.from_str(index.toString()), plutusList));
            }
            else if (data instanceof Array) {
                const plutusList = C.PlutusList.new();
                data.forEach((arg) => plutusList.add(serialize(arg)));
                return C.PlutusData.new_list(plutusList);
            }
            else if (data instanceof Map) {
                const plutusMap = C.PlutusMap.new();
                for (const [key, value] of data.entries()) {
                    plutusMap.insert(serialize(key), serialize(value));
                }
                return C.PlutusData.new_map(plutusMap);
            }
            throw new Error("Unsupported type");
        }
        catch (error) {
            throw new Error("Could not serialize the data: " + error);
        }
    }
    const d = type ? castTo(data, type) : data;
    return toHex(serialize(d).to_bytes());
}
/**
 *  Convert Cbor encoded data to Data.\
 *  Or apply a shape and cast the cbor encoded data to a certain type.
 */
function from(raw, type) {
    function deserialize(data) {
        if (data.kind() === 0) {
            const constr = data.as_constr_plutus_data();
            const l = constr.data();
            const desL = [];
            for (let i = 0; i < l.len(); i++) {
                desL.push(deserialize(l.get(i)));
            }
            return new Constr(parseInt(constr.alternative().to_str()), desL);
        }
        else if (data.kind() === 1) {
            const m = data.as_map();
            const desM = new Map();
            const keys = m.keys();
            for (let i = 0; i < keys.len(); i++) {
                desM.set(deserialize(keys.get(i)), deserialize(m.get(keys.get(i))));
            }
            return desM;
        }
        else if (data.kind() === 2) {
            const l = data.as_list();
            const desL = [];
            for (let i = 0; i < l.len(); i++) {
                desL.push(deserialize(l.get(i)));
            }
            return desL;
        }
        else if (data.kind() === 3) {
            return BigInt(data.as_integer().to_str());
        }
        else if (data.kind() === 4) {
            return toHex(data.as_bytes());
        }
        throw new Error("Unsupported type");
    }
    const data = deserialize(C.PlutusData.from_bytes(fromHex(raw)));
    return type ? castFrom(data, type) : data;
}
/**
 * Note Constr cannot be used here.\
 * Strings prefixed with '0x' are not UTF-8 encoded.
 */
function fromJson(json) {
    function toData(json) {
        if (typeof json === "string") {
            return json.startsWith("0x")
                ? toHex(fromHex(json.slice(2)))
                : fromText(json);
        }
        if (typeof json === "number")
            return BigInt(json);
        if (typeof json === "bigint")
            return json;
        if (json instanceof Array)
            return json.map((v) => toData(v));
        if (json instanceof Object) {
            const tempMap = new Map();
            Object.entries(json).forEach(([key, value]) => {
                tempMap.set(toData(key), toData(value));
            });
            return tempMap;
        }
        throw new Error("Unsupported type");
    }
    return toData(json);
}
/**
 * Note Constr cannot be used here, also only bytes/integers as Json keys.\
 */
function toJson(plutusData) {
    function fromData(data) {
        if (typeof data === "bigint" ||
            typeof data === "number" ||
            (typeof data === "string" &&
                !isNaN(parseInt(data)) &&
                data.slice(-1) === "n")) {
            const bigint = typeof data === "string"
                ? BigInt(data.slice(0, -1))
                : data;
            return parseInt(bigint.toString());
        }
        if (typeof data === "string") {
            try {
                return new TextDecoder(undefined, { fatal: true }).decode(fromHex(data));
            }
            catch (_) {
                return "0x" + toHex(fromHex(data));
            }
        }
        if (data instanceof Array)
            return data.map((v) => fromData(v));
        if (data instanceof Map) {
            const tempJson = {};
            data.forEach((value, key) => {
                const convertedKey = fromData(key);
                if (typeof convertedKey !== "string" &&
                    typeof convertedKey !== "number") {
                    throw new Error("Unsupported type (Note: Only bytes or integers can be keys of a JSON object)");
                }
                tempJson[convertedKey] = fromData(value);
            });
            return tempJson;
        }
        throw new Error("Unsupported type (Note: Constructor cannot be converted to JSON)");
    }
    return fromData(plutusData);
}
function castFrom(data, type) {
    const shape = type;
    if (!shape)
        throw new Error("Could not type cast data.");
    const shapeType = (shape.anyOf ? "enum" : "") || shape.dataType;
    switch (shapeType) {
        case "integer": {
            if (typeof data !== "bigint") {
                throw new Error("Could not type cast to integer.");
            }
            integerConstraints(data, shape);
            return data;
        }
        case "bytes": {
            if (typeof data !== "string") {
                throw new Error("Could not type cast to bytes.");
            }
            bytesConstraints(data, shape);
            return data;
        }
        case "constructor": {
            if (isVoid(shape)) {
                if (!(data instanceof Constr) || data.index !== 0 ||
                    data.fields.length !== 0) {
                    throw new Error("Could not type cast to void.");
                }
                return undefined;
            }
            else if (data instanceof Constr && data.index === shape.index &&
                (shape.hasConstr || shape.hasConstr === undefined)) {
                const fields = {};
                if (shape.fields.length !== data.fields.length) {
                    throw new Error("Could not type cast to object. Fields do not match.");
                }
                shape.fields.forEach((field, fieldIndex) => {
                    const title = field.title || "wrapper";
                    if ((/[A-Z]/.test(title[0]))) {
                        throw new Error("Could not type cast to object. Object properties need to start with a lowercase letter.");
                    }
                    fields[title] = castFrom(data.fields[fieldIndex], field);
                });
                return fields;
            }
            else if (data instanceof Array && !shape.hasConstr &&
                shape.hasConstr !== undefined) {
                const fields = {};
                if (shape.fields.length !== data.length) {
                    throw new Error("Could not ype cast to object. Fields do not match.");
                }
                shape.fields.forEach((field, fieldIndex) => {
                    const title = field.title || "wrapper";
                    if ((/[A-Z]/.test(title[0]))) {
                        throw new Error("Could not type cast to object. Object properties need to start with a lowercase letter.");
                    }
                    fields[title] = castFrom(data[fieldIndex], field);
                });
                return fields;
            }
            throw new Error("Could not type cast to object.");
        }
        case "enum": {
            // When enum has only one entry it's a single constructor/record object
            if (shape.anyOf.length === 1) {
                return castFrom(data, shape.anyOf[0]);
            }
            if (!(data instanceof Constr)) {
                throw new Error("Could not type cast to enum.");
            }
            const enumShape = shape.anyOf.find((entry) => entry.index === data.index);
            if (!enumShape || enumShape.fields.length !== data.fields.length) {
                throw new Error("Could not type cast to enum.");
            }
            if (isBoolean(shape)) {
                if (data.fields.length !== 0) {
                    throw new Error("Could not type cast to boolean.");
                }
                switch (data.index) {
                    case 0:
                        return false;
                    case 1:
                        return true;
                }
                throw new Error("Could not type cast to boolean.");
            }
            else if (isNullable(shape)) {
                switch (data.index) {
                    case 0: {
                        if (data.fields.length !== 1) {
                            throw new Error("Could not type cast to nullable object.");
                        }
                        return castFrom(data.fields[0], shape.anyOf[0].fields[0]);
                    }
                    case 1: {
                        if (data.fields.length !== 0) {
                            throw new Error("Could not type cast to nullable object.");
                        }
                        return null;
                    }
                }
                throw new Error("Could not type cast to nullable object.");
            }
            switch (enumShape.dataType) {
                case "constructor": {
                    if (enumShape.fields.length === 0) {
                        if (/[A-Z]/.test(enumShape.title[0])) {
                            return enumShape.title;
                        }
                        throw new Error("Could not type cast to enum.");
                    }
                    else {
                        if (!(/[A-Z]/.test(enumShape.title))) {
                            throw new Error("Could not type cast to enum. Enums need to start with an uppercase letter.");
                        }
                        if (enumShape.fields.length !== data.fields.length) {
                            throw new Error("Could not type cast to enum.");
                        }
                        // check if named args
                        const args = enumShape.fields[0].title
                            ? Object.fromEntries(enumShape.fields.map((field, index) => [field.title, castFrom(data.fields[index], field)]))
                            : enumShape.fields.map((field, index) => castFrom(data.fields[index], field));
                        return {
                            [enumShape.title]: args,
                        };
                    }
                }
            }
            throw new Error("Could not type cast to enum.");
        }
        case "list": {
            if (shape.items instanceof Array) {
                // tuple
                if (data instanceof Constr &&
                    data.index === 0 &&
                    shape.hasConstr) {
                    return data.fields.map((field, index) => castFrom(field, shape.items[index]));
                }
                else if (data instanceof Array && !shape.hasConstr) {
                    return data.map((field, index) => castFrom(field, shape.items[index]));
                }
                throw new Error("Could not type cast to tuple.");
            }
            else {
                // array
                if (!(data instanceof Array)) {
                    throw new Error("Could not type cast to array.");
                }
                listConstraints(data, shape);
                return data.map((field) => castFrom(field, shape.items));
            }
        }
        case "map": {
            if (!(data instanceof Map)) {
                throw new Error("Could not type cast to map.");
            }
            mapConstraints(data, shape);
            const map = new Map();
            for (const [key, value] of (data)
                .entries()) {
                map.set(castFrom(key, shape.keys), castFrom(value, shape.values));
            }
            return map;
        }
        case undefined: {
            return data;
        }
    }
    throw new Error("Could not type cast data.");
}
function castTo(struct, type) {
    const shape = type;
    if (!shape)
        throw new Error("Could not type cast struct.");
    const shapeType = (shape.anyOf ? "enum" : "") || shape.dataType;
    switch (shapeType) {
        case "integer": {
            if (typeof struct !== "bigint") {
                throw new Error("Could not type cast to integer.");
            }
            integerConstraints(struct, shape);
            return struct;
        }
        case "bytes": {
            if (typeof struct !== "string") {
                throw new Error("Could not type cast to bytes.");
            }
            bytesConstraints(struct, shape);
            return struct;
        }
        case "constructor": {
            if (isVoid(shape)) {
                if (struct !== undefined) {
                    throw new Error("Could not type cast to void.");
                }
                return new Constr(0, []);
            }
            else if (typeof struct !== "object" || struct === null ||
                shape.fields.length !== Object.keys(struct).length) {
                throw new Error("Could not type cast to constructor.");
            }
            const fields = shape.fields.map((field) => castTo(struct[field.title || "wrapper"], field));
            return (shape.hasConstr || shape.hasConstr === undefined)
                ? new Constr(shape.index, fields)
                : fields;
        }
        case "enum": {
            // When enum has only one entry it's a single constructor/record object
            if (shape.anyOf.length === 1) {
                return castTo(struct, shape.anyOf[0]);
            }
            if (isBoolean(shape)) {
                if (typeof struct !== "boolean") {
                    throw new Error("Could not type cast to boolean.");
                }
                return new Constr(struct ? 1 : 0, []);
            }
            else if (isNullable(shape)) {
                if (struct === null)
                    return new Constr(1, []);
                else {
                    const fields = shape.anyOf[0].fields;
                    if (fields.length !== 1) {
                        throw new Error("Could not type cast to nullable object.");
                    }
                    return new Constr(0, [
                        castTo(struct, fields[0]),
                    ]);
                }
            }
            switch (typeof struct) {
                case "string": {
                    if (!(/[A-Z]/.test(struct[0]))) {
                        throw new Error("Could not type cast to enum. Enum needs to start with an uppercase letter.");
                    }
                    const enumIndex = shape.anyOf.findIndex((s) => s.dataType === "constructor" &&
                        s.fields.length === 0 &&
                        s.title === struct);
                    if (enumIndex === -1)
                        throw new Error("Could not type cast to enum.");
                    return new Constr(enumIndex, []);
                }
                case "object": {
                    if (struct === null)
                        throw new Error("Could not type cast to enum.");
                    const structTitle = Object.keys(struct)[0];
                    if (!(/[A-Z]/.test(structTitle))) {
                        throw new Error("Could not type cast to enum. Enum needs to start with an uppercase letter.");
                    }
                    const enumEntry = shape.anyOf.find((s) => s.dataType === "constructor" &&
                        s.title === structTitle);
                    if (!enumEntry)
                        throw new Error("Could not type cast to enum.");
                    const args = struct[structTitle];
                    return new Constr(enumEntry.index, 
                    // check if named args
                    args instanceof Array
                        ? args.map((item, index) => castTo(item, enumEntry.fields[index]))
                        : enumEntry.fields.map((entry) => {
                            const [_, item] = Object.entries(args).find(([title]) => title === entry.title);
                            return castTo(item, entry);
                        }));
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
                const fields = struct.map((item, index) => castTo(item, shape.items[index]));
                return shape.hasConstr ? new Constr(0, fields) : fields;
            }
            else {
                // array
                listConstraints(struct, shape);
                return struct.map((item) => castTo(item, shape.items));
            }
        }
        case "map": {
            if (!(struct instanceof Map)) {
                throw new Error("Could not type cast to map.");
            }
            mapConstraints(struct, shape);
            const map = new Map();
            for (const [key, value] of (struct)
                .entries()) {
                map.set(castTo(key, shape.keys), castTo(value, shape.values));
            }
            return map;
        }
        case undefined: {
            return struct;
        }
    }
    throw new Error("Could not type cast struct.");
}
function integerConstraints(integer, shape) {
    if (shape.minimum && integer < BigInt(shape.minimum)) {
        throw new Error(`Integer ${integer} is below the minimum ${shape.minimum}.`);
    }
    if (shape.maximum && integer > BigInt(shape.maximum)) {
        throw new Error(`Integer ${integer} is above the maxiumum ${shape.maximum}.`);
    }
    if (shape.exclusiveMinimum && integer <= BigInt(shape.exclusiveMinimum)) {
        throw new Error(`Integer ${integer} is below the exclusive minimum ${shape.exclusiveMinimum}.`);
    }
    if (shape.exclusiveMaximum && integer >= BigInt(shape.exclusiveMaximum)) {
        throw new Error(`Integer ${integer} is above the exclusive maximum ${shape.exclusiveMaximum}.`);
    }
}
function bytesConstraints(bytes, shape) {
    if (shape.enum && !shape.enum.some((keyword) => keyword === bytes))
        throw new Error(`None of the keywords match with '${bytes}'.`);
    if (shape.minLength && bytes.length / 2 < shape.minLength) {
        throw new Error(`Bytes need to have a length of at least ${shape.minLength} bytes.`);
    }
    if (shape.maxLength && bytes.length / 2 > shape.maxLength) {
        throw new Error(`Bytes can have a length of at most ${shape.minLength} bytes.`);
    }
}
function listConstraints(list, shape) {
    if (shape.minItems && list.length < shape.minItems) {
        throw new Error(`Array needs to contain at least ${shape.minItems} items.`);
    }
    if (shape.maxItems && list.length > shape.maxItems) {
        throw new Error(`Array can contain at most ${shape.maxItems} items.`);
    }
    if (shape.uniqueItems && (new Set(list)).size !== list.length) {
        // Note this only works for primitive types like string and bigint.
        throw new Error("Array constains duplicates.");
    }
}
function mapConstraints(map, shape) {
    if (shape.minItems && map.size < shape.minItems) {
        throw new Error(`Map needs to contain at least ${shape.minItems} items.`);
    }
    if (shape.maxItems && map.size > shape.maxItems) {
        throw new Error(`Map can contain at most ${shape.maxItems} items.`);
    }
}
function isBoolean(shape) {
    return shape.anyOf && shape.anyOf[0]?.title === "False" &&
        shape.anyOf[1]?.title === "True";
}
function isVoid(shape) {
    return shape.index === 0 && shape.fields.length === 0;
}
function isNullable(shape) {
    return shape.anyOf && shape.anyOf[0]?.title === "Some" &&
        shape.anyOf[1]?.title === "None";
}
function replaceProperties(object, properties) {
    Object.keys(object).forEach((key) => {
        delete object[key];
    });
    Object.assign(object, properties);
}
