// TODO consider generating wrong cases as well

import {
  Constr,
  Data,
  PByteString,
  PConstr,
  PData,
  PInteger,
  PLifted,
  PList,
  PlutusData,
  PMap,
  PRecord,
  PSum,
  PType,
} from "../mod.ts";

const zeroChance = 0.1;
const ndefChance = 0.1;
const maxInteger = Number.MAX_SAFE_INTEGER;
const maxStringBytes = 100; // TODO higher
export const gMaxLength = 12;
export const gMaxDepth = 2;

export function randomChoice<T>(alternatives: T[]): T {
  return randomIndexedChoice(alternatives)[0];
}

export function randomIndexedChoice<T>(alternatives: T[]): [T, number] {
  const choice = Math.floor(Math.random() * alternatives.length);
  return [alternatives[choice], choice];
}

export class ExtConstr<T> {
  index: number;
  fields: Record<string, T>;

  constructor(index: number, fields: Record<string, T>) {
    this.index = index;
    this.fields = fields;
  }
}
// @ts-ignore TODO fix recursive type declaration
export type ExtPlutusData =
  | bigint
  | string
  | Array<ExtPlutusData>
  | Map<ExtPlutusData, ExtPlutusData>
  | ExtConstr<ExtPlutusData>
  | Record<string, ExtPlutusData>
  | Example;

// generate ExtPlutusData from PTypes

export function genPTypeData(ptype: PType): ExtPlutusData {
  if (ptype instanceof PData) {
    const actual = genPType(
      Math.max(1, gMaxDepth / 2),
      Math.max(1, gMaxLength / 2),
    ); // note: might result in up to double depth
    return genPTypeData(actual);
  } else if (ptype instanceof PInteger) {
    return genInteger();
  } else if (ptype instanceof PByteString) {
    return genByteString(); // TODO Uint8Arrays
  } else if (ptype instanceof PList) {
    return genList(ptype);
  } else if (ptype instanceof PMap) {
    return genMap(ptype);
  } else if (ptype instanceof PConstr) {
    return genExtConstr(ptype);
  } else if (ptype instanceof PSum) {
    return genSum(ptype);
  } else if (ptype instanceof PRecord) {
    return genRecord(ptype);
  } else {
    throw new Error("unknown PType during data generation");
  }
}

export function stripExt(data: ExtPlutusData): PlutusData {
  if (data instanceof Array) {
    return data.map(stripExt);
  } else if (data instanceof Map) {
    const m = new Map<PlutusData, PlutusData>();
    data.forEach(
      (value: ExtPlutusData, key: ExtPlutusData) => {
        m.set(stripExt(key), stripExt(value));
      },
    );
    return m;
  } else if (data instanceof ExtConstr) {
    return new Constr(data.index, Object.values(data.fields).map(stripExt));
  } else if (data instanceof Object) {
    return stripExt(Object.values(data));
  } else {
    return data;
  }
}

export function stripConstr(data: ExtPlutusData): ExtPlutusData {
  if (data instanceof Array) {
    return data.map(stripConstr);
  } else if (data instanceof Map) {
    const m = new Map<ExtPlutusData, ExtPlutusData>();
    data.forEach(
      (value: ExtPlutusData, key: ExtPlutusData) => {
        m.set(stripConstr(key), stripConstr(value));
      },
    );
    return m;
  } else if (data instanceof ExtConstr) {
    return stripConstr(data.fields);
  } else if (data instanceof Example) {
    return data;
  } else if (data instanceof Object) {
    const r: Record<string, ExtPlutusData> = {};
    Object.entries(data).forEach(([key, value]) => {
      r[key] = stripConstr(value);
    });
    return r;
  } else {
    return data;
  }
}

export function maybeNdef<T>(value: T) {
  if (Math.random() > ndefChance) {
    return value;
  } else {
    return undefined;
  }
}

export function genNumber(maxValue: number): number {
  if (Math.random() > zeroChance) {
    return Math.floor(Math.random() * maxValue);
  } else {
    return 0;
  }
}

export function genInteger(): bigint {
  return BigInt(genNumber(maxInteger));
}

export function genString(alph: string): string {
  function genChar(): string {
    const choice = Math.floor(Math.random() * (alph.length + 10));
    if (choice < alph.length) {
      return alph.charAt(choice);
    } else {
      return Math.floor(Math.random() * 10).toString();
    }
  }
  const l: string[] = [];
  const maxi = 8 * genNumber(maxStringBytes);
  for (let i = 0; i < maxi; i++) {
    l.push(genChar());
  }
  const s = l.join("");
  return s;
}

export function genName(): string {
  const lower = "abcdefghijklmnopqrstuvwxyz";
  const upper = lower.toUpperCase();
  const alph = lower + upper; // TODO special characters
  return genString(alph);
}

export function genByteString(): string {
  return genString("abcdef");
}

export function genList(plist: PList<PType>): Array<ExtPlutusData> {
  const length = plist.length ? plist.length : genNumber(gMaxLength);
  const l = new Array<ExtPlutusData>();
  for (let i = 0; i < length; i++) {
    l.push(genPTypeData(plist.pelem));
  }
  return l;
}

export function genMap(
  pmap: PMap<PType, PType>,
): Map<ExtPlutusData, ExtPlutusData> {
  const size = pmap.size ? pmap.size : genNumber(gMaxLength);
  const m = new Map<ExtPlutusData, ExtPlutusData>();
  const keyStrings = new Array<string>();
  while (m.size < size) {
    const key = genPTypeData(pmap.pkey);
    const keyString = Data.to(stripExt(key));
    if (!keyStrings.includes(keyString)) {
      keyStrings.push(keyString);
      const value = genPTypeData(pmap.pvalue);
      m.set(key, value);
    }
  }
  return m;
}

export function genExtConstr<T extends Record<string, PLifted<PType>>>(
  pconstr: PConstr<T>,
): ExtConstr<ExtPlutusData> {
  const record = genRecord(pconstr.pfields);
  //   const fields = Object.values(record);
  return new ExtConstr(pconstr.index, record);
}

export function genSum<T extends Record<string, PLifted<PType>>>(
  psum: PSum<T>,
): Constr<ExtPlutusData> {
  const [precord, index] = randomIndexedChoice(psum.pconstrs);
  const record = genRecord<T>(precord);
  const fields = Object.values(record);
  return new Constr(index, fields);
}

export function genRecord<T extends Record<string, PLifted<PType>>>(
  precord: PRecord<T>,
): Example | Record<string, ExtPlutusData> {
  if (precord.plifted) {
    return new Example(
      genByteString(),
      genByteString(),
      BigInt(genNumber(maxInteger)),
    );
  } else {
    const r: Record<string, ExtPlutusData> = {};
    Object.entries(precord.pfields).forEach(([key, pfield]) => {
      r[key] = genPTypeData(pfield);
    });
    return r;
  }
}

// generate PTypes

const primitiveGenerators = [
  genPData,
  genPInteger,
  genPByteString,
];

const containerGenerators = [
  genPList,
  genPMap,
  genPConstr,
  // genPSum, // TODO test sums after pconstant is implemented
  genPRecord,
  genPExample,
];

export function genPType(maxDepth: number, maxLength: number): PType {
  const generator = maxDepth > 0
    ? randomChoice([
      ...primitiveGenerators,
      ...containerGenerators,
    ])
    : randomChoice(primitiveGenerators);
  return generator(maxDepth - 1, maxLength / 2);
}

export function genPPrimitive(): PType {
  const generator = randomChoice(primitiveGenerators);
  return generator();
}

export function genPData(): PData {
  return new PData();
}
export function genPInteger(): PInteger {
  return new PInteger();
}

export function genPByteString(): PByteString {
  return new PByteString();
}

export function genPList(
  maxDepth: number,
  maxLength: number,
): PList<PLifted<PType>> {
  const pelem = genPType(maxDepth, maxLength);
  const length = maybeNdef(genNumber(maxLength));
  return new PList(pelem, length);
}

export function genPMap(
  maxDepth: number,
  maxLength: number,
): PMap<PLifted<PType>, PLifted<PType>> {
  const pkey = genPType(maxDepth - 1, maxLength / 2);
  const pvalue = genPType(maxDepth - 1, maxLength / 2);
  const size = maybeNdef(genNumber(maxLength));
  return new PMap(pkey, pvalue, size);
}

export function genPConstr<T extends Record<string, PLifted<PType>>>(
  maxDepth: number,
  maxLength: number,
): PConstr<T> {
  const index = genNumber(maxInteger);
  const pfields = genPRecord<T>(maxDepth, maxLength);
  return new PConstr<T>(index, pfields);
}

export function genPSum<T extends Record<string, PLifted<PType>>>(
  maxDepth: number,
  maxLength: number,
): PSum<T> {
  const pconstrs = new Array<PRecord<T>>();
  const maxi = genNumber(maxLength);
  for (let i = 0; i < maxi; i++) {
    pconstrs.push(genPRecord(maxDepth, maxLength));
  }
  return new PSum(pconstrs);
}

export function genPRecord<T extends Record<string, PLifted<PType>>>(
  maxDepth: number,
  maxLength: number,
): PRecord<T> {
  const pfields: Record<string, PType> = {};
  const maxi = genNumber(maxLength);
  for (let i = 0; i < maxi; i++) {
    const key = genName();
    const pvalue = genPType(maxDepth, maxLength);
    pfields[key] = pvalue;
  }
  return new PRecord(pfields);
}

// sample named record

export class Example {
  constructor(
    public ccy: string,
    public tkn: string,
    public amnt: bigint,
  ) {}
}

export function genPExample() {
  return new PRecord(
    {
      "ccy": new PByteString(),
      "tkn": new PByteString(),
      "amnt": new PInteger(),
    },
    Example,
  );
}
