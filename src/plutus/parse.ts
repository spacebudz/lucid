import { assert } from "https://deno.land/std@0.167.0/testing/asserts.ts";
import { PlutusData } from "../types/mod.ts";
import { Constr } from "./data.ts";

/*
PType - for parser-type. Also a nod to Plutarch.
It's basically a crude runtime type system for data parsing.
Each class represents a mechanism to create the corresponding
non-P-type, not actual data.
plift parses, pconstant composes.
*/

export interface PType {
  //@ts-ignore TODO fix circular reference
  plift(data: PlutusData): PLifted<PType>;
  pconstant(data: PLifted<PType>): PlutusData;
}

export type PLifted<T extends PType> = ReturnType<T[`plift`]>;
// export type PData = PLifted<PType>

// export interface PConstanted {
//   new (name: string): MyClass;
// }

/** the most general type. Similar to any or undefined.
 * TODO consider type checks in the functions still.
 */
export class PData implements PType {
  public plift(data: PlutusData): PLifted<PType> {
    return data;
  }
  public pconstant(data: PLifted<PType>): PlutusData {
    return data;
  }
}

export class PInteger implements PType {
  public plift = (i: bigint): bigint => {
    assert(
      typeof i === `bigint`,
      `plift: expected Integer: ${i}`,
    );
    return i;
  };
  public pconstant = (data: bigint): bigint => {
    assert(typeof data === `bigint`, `pconstant: expected Integer`);
    return data;
  };
}

export class PByteString implements PType {
  public plift = (s: string): string => {
    assert(
      typeof s === `string`,
      `plift: expected String: ${s}`,
    );
    return s;
  };

  public pconstant = (data: string): string => {
    assert(typeof data === `string`, `pconstant: expected String: ${data}`);
    return data;
  };
}

export class PList<T extends PType> implements PType {
  constructor(
    public pelem: T,
    public length?: number,
  ) {}

  public plift = (l: Array<PlutusData>): Array<PLifted<T>> => {
    assert(l instanceof Array, `List.plift: expected List: ${l}`);
    assert(!this.length || this.length === l.length, `plift: wrong length`);
    return l.map((elem) => this.pelem.plift(elem));
  };

  public pconstant = (data: Array<PLifted<T>>): Array<PlutusData> => {
    assert(data instanceof Array, `pconstant: expected Array`);
    assert(
      !this.length || this.length === data.length,
      `pconstant: wrong length`,
    );
    return data.map(this.pelem.pconstant);
  };
}

export class PMap<K extends PType, V extends PType> implements PType {
  constructor(
    public pkey: K,
    public pvalue: V,
    public size?: number,
  ) {}

  public plift = (
    m: Map<PlutusData, PlutusData>,
  ): Map<PLifted<K>, PLifted<V>> => {
    assert(m instanceof Map, `plift: expected Map`);
    assert(
      !this.size || this.size === m.size,
      `plift: wrong size: ${JSON.stringify(this)} vs. ${JSON.stringify(m)}`,
    );
    const p = new Map<PLifted<K>, PLifted<V>>();
    m.forEach((value: PlutusData, key: PlutusData) => {
      p.set(this.pkey.plift(key), this.pvalue.plift(value));
    });
    return p;
  };

  public pconstant = (
    data: Map<PLifted<K>, PLifted<V>>,
  ): Map<PlutusData, PlutusData> => {
    assert(data instanceof Map, `pconstant: expected Map`);
    assert(!this.size || this.size === data.size, `pconstant: wrong size`);
    const m = new Map<PLifted<K>, PLifted<V>>();
    data.forEach((value, key) => {
      m.set(key, value);
    });
    return m;
  };
}

export class PConstr implements PType {
  constructor(
    public index: number,
    public pfields: PRecord,
  ) {}

  public plift = (
    c: Constr<PlutusData>,
  ): Record<string, PLifted<PType>> => {
    assert(c instanceof Constr, `plift: expected Constr`);
    assert(
      this.index === c.index,
      `plift: wrong constr index: ${this} vs. ${c}`,
    );
    return this.pfields.plift(c.fields);
  };

  public pconstant = (
    data: Record<string, PLifted<PType>>,
  ): Constr<PlutusData> => {
    assert(data instanceof Object, `PConstr.pconstant: expected Object`);
    assert(
      !(data instanceof Array),
      `PConstr.pconstant: unexpected Array: ${data}`,
    );
    return new Constr(this.index, this.pfields.pconstant(data));
  };
}

export class PSum implements PType {
  constructor(
    public pconstrs: Array<PRecord>,
  ) {}

  public plift = (
    c: Constr<PlutusData>,
  ): PObject<Record<string, PType>> | Record<string, PType> => { // TODO the return type
    assert(c instanceof Constr, `plift: expected Constr`);
    assert(c.index < this.pconstrs.length, `plift: constr index out of bounds`);
    return this.pconstrs[c.index].plift(c.fields);
  };

  public pconstant = (data: Object): Constr<PlutusData> => {
    assert(data instanceof Object, `PSum.pconstant: expected Object`);
    assert(
      !(data instanceof Array),
      `PSum.pconstant: unexpected Array: ${data}`,
    );
    throw new Error(`pconstant: not implemented`); // TODO something about matching maybe
  };
}

export class PRecord implements PType {
  constructor(
    public pfields: Record<string, PType>,
    public plifted?: { new (...params: any): any },
  ) {}

  public plift = (l: Array<PlutusData>): Record<string, PLifted<PType>> => {
    assert(
      l instanceof Array,
      `Record.plift: expected List: ${l}`,
    );
    let obj: Record<string, PLifted<PType>> = {};

    const pfields = Object.entries(this.pfields);
    l.forEach((value, i) => {
      const key = pfields[i][0];
      const pvalue = pfields[i][1];
      obj[key] = pvalue.plift(value);
    });

    if (this.plifted) {
      return new this.plifted(...Object.values(obj)); // TODO ensure this works
    } else {
      return obj;
    }
  };

  public pconstant = (data: Object): Array<PlutusData> => {
    assert(data instanceof Object, `PRecord.pconstant: expected Object`);
    assert(
      !(data instanceof Array),
      `PRecord.pconstant: unexpected Array: ${data}`,
    );
    const l = new Array<PlutusData>();
    Object.entries(data).forEach(([key, value]) => {
      const pfield = this.pfields[key];
      assert(pfield, `field not found: ${key}`);
      l.push(pfield.pconstant(value));
    });
    return l;
  };
}

type PObject<T extends Record<string, PType>> = {
  [Key in keyof T]: PLifted<T[Key]>;
};

// examples

const PValue = new PMap(
  new PByteString(),
  new PMap(new PByteString(), new PInteger()),
);

type Value = Map<string, Map<string, bigint>>;
const valueData: Value = new Map<string, Map<string, bigint>>();
