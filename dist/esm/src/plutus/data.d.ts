import { Static as _Static, TLiteral, TLiteralValue, TProperties, TSchema } from "../../deps/deno.land/x/typebox@0.25.13/src/typebox.js";
import { Datum, Exact, Json, Redeemer } from "../types/mod.js";
export declare class Constr<T> {
    index: number;
    fields: T[];
    constructor(index: number, fields: T[]);
}
export declare namespace Data {
    type Static<T extends TSchema, P extends unknown[] = []> = _Static<T, P>;
}
export type Data = bigint | string | Array<Data> | Map<Data, Data> | Constr<Data>;
export declare const Data: {
    Integer: (options?: {
        minimum?: number;
        maximum?: number;
        exclusiveMinimum?: number;
        exclusiveMaximum?: number;
    }) => import("../../deps/deno.land/x/typebox@0.25.13/src/typebox.js").TUnsafe<bigint>;
    Bytes: (options?: {
        minLength?: number;
        maxLength?: number;
        enum?: string[];
    }) => import("../../deps/deno.land/x/typebox@0.25.13/src/typebox.js").TUnsafe<string>;
    Boolean: () => import("../../deps/deno.land/x/typebox@0.25.13/src/typebox.js").TUnsafe<boolean>;
    Any: () => import("../../deps/deno.land/x/typebox@0.25.13/src/typebox.js").TUnsafe<Data>;
    Array: <T extends TSchema>(items: T, options?: {
        minItems?: number;
        maxItems?: number;
        uniqueItems?: boolean;
    }) => import("../../deps/deno.land/x/typebox@0.25.13/src/typebox.js").TArray<T>;
    Map: <T_1 extends TSchema, U extends TSchema>(keys: T_1, values: U, options?: {
        minItems?: number;
        maxItems?: number;
    }) => import("../../deps/deno.land/x/typebox@0.25.13/src/typebox.js").TUnsafe<Map<Data.Static<T_1, []>, Data.Static<U, []>>>;
    /**
     * Object applies by default a PlutusData Constr with index 0.\
     * Set 'hasConstr' to false to serialize Object as PlutusData List.
     */
    Object: <T_2 extends TProperties>(properties: T_2, options?: {
        hasConstr?: boolean;
    }) => import("../../deps/deno.land/x/typebox@0.25.13/src/typebox.js").TObject<T_2>;
    Enum: <T_3 extends TSchema>(items: T_3[]) => import("../../deps/deno.land/x/typebox@0.25.13/src/typebox.js").TUnion<T_3[]>;
    /**
     * Tuple is by default a PlutusData List.\
     * Set 'hasConstr' to true to apply a PlutusData Constr with index 0.
     */
    Tuple: <T_4 extends TSchema[]>(items: [...T_4], options?: {
        hasConstr?: boolean;
    }) => import("../../deps/deno.land/x/typebox@0.25.13/src/typebox.js").TTuple<T_4>;
    Literal: <T_5 extends TLiteralValue>(title: T_5) => TLiteral<T_5>;
    Nullable: <T_6 extends TSchema>(item: T_6) => import("../../deps/deno.land/x/typebox@0.25.13/src/typebox.js").TUnsafe<Data.Static<T_6, []> | null>;
    /**
     * Convert PlutusData to Cbor encoded data.\
     * Or apply a shape and convert the provided data struct to Cbor encoded data.
     */
    to: typeof to;
    /** Convert Cbor encoded data to PlutusData */
    from: typeof from;
    /**
     * Note Constr cannot be used here.\
     * Strings prefixed with '0x' are not UTF-8 encoded.
     */
    fromJson: typeof fromJson;
    /**
     * Note Constr cannot be used here, also only bytes/integers as Json keys.\
     */
    toJson: typeof toJson;
    void: () => Datum | Redeemer;
    castFrom: typeof castFrom;
    castTo: typeof castTo;
};
/**
 * Convert PlutusData to Cbor encoded data.\
 * Or apply a shape and convert the provided data struct to Cbor encoded data.
 */
declare function to<T = Data>(data: Exact<T>, type?: T): Datum | Redeemer;
/**
 *  Convert Cbor encoded data to Data.\
 *  Or apply a shape and cast the cbor encoded data to a certain type.
 */
declare function from<T = Data>(raw: Datum | Redeemer, type?: T): T;
/**
 * Note Constr cannot be used here.\
 * Strings prefixed with '0x' are not UTF-8 encoded.
 */
declare function fromJson(json: Json): Data;
/**
 * Note Constr cannot be used here, also only bytes/integers as Json keys.\
 */
declare function toJson(plutusData: Data): Json;
declare function castFrom<T = Data>(data: Data, type: T): T;
declare function castTo<T>(struct: Exact<T>, type: T): Data;
export {};
