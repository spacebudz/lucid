import {
  Constr,
  Data,
  Shape,
  UnsizedConstr,
  UnsizedList,
  UnsizedMap,
} from "../plutus/data.ts";
import { List, PlutusData, RecordType } from "../types/types.ts";
import { fromHex, toHex } from "./utils.ts";

function randomChoice<T>(alternatives: T[]): T {
  const choice = Math.floor(Math.random() * alternatives.length);
  return alternatives[choice];
}

function genNumber(maxLength: number): number {
  if (Math.random() > 0.9) {
    return 0;
  } else {
    return Math.floor(Math.random() * (maxLength));
  }
}

function genInteger(maxLength: number): [bigint, bigint] {
  const i = BigInt(genNumber(maxLength));
  return [i, i];
}

function genChar(): string {
  const alph = "abcdef";
  const choice = Math.floor(Math.random() * (alph.length + 10));
  if (choice < alph.length) {
    return alph.charAt(choice);
  } else {
    return Math.floor(Math.random() * 10).toString();
  }
}

function genString(maxLength: number): [string, string] {
  const l: string[] = [];
  const maxi = 8 * Math.floor(maxLength * Math.random());
  for (let i = 0; i < maxi; i++) {
    l.push(genChar());
  }
  const s = l.join("");
  return [s, s];
}

function genUint8Array(maxLength: number): [Uint8Array, Uint8Array] {
  const u = fromHex(genString(maxLength)[1]);
  return [u, u];
}

function genList(
  maxLength: number,
  maxDepth: number,
): [Array<Shape>, Array<PlutusData>] {
  const l: Array<PlutusData> = [];
  const shape: Array<Shape> = [];
  const maxi = maxLength * Math.random();
  for (let i = 0; i < maxi; i++) {
    const [elemShape, elem] = genPlutusData(maxLength, maxDepth);
    shape.push(elemShape);
    l.push(elem);
  }
  return [shape, l];
}

function genUnsizedList(
  maxLength: number,
): [UnsizedList<Shape>, List<PlutusData>] {
  const generator = choosePrimitiveGenerator();
  const l: Array<PlutusData> = [];
  let elemShape;
  let elem;
  const maxi = maxLength * Math.random();
  for (let i = 0; i < maxi; i++) {
    [elemShape, elem] = generator(maxLength);
    l.push(elem);
  }
  const shape = new UnsizedList(elemShape);
  return [shape, l];
}

function genMap(
  maxLength: number,
  maxDepth: number,
): [Map<Shape, Shape>, Map<PlutusData, PlutusData>] {
  const m = new Map<PlutusData, PlutusData>();
  const shape = new Map<Shape, Shape>();
  const keyStrings: string[] = [];
  const maxi = maxLength * Math.random();
  for (let i = 0; i < maxi; i++) {
    const [keyShape, key] = genPlutusData(maxLength, maxDepth);
    const keyString = Data.to(key);
    if (!keyStrings.includes(keyString)) {
      keyStrings.push(keyString);
      const [valueShape, value] = genPlutusData(maxLength, maxDepth);
      shape.set(keyShape, valueShape);
      m.set(key, value);
    }
  }
  return [shape, m];
}

function genUnsizedMap(
  maxLength: number,
): [UnsizedMap<Shape, Shape>, Map<PlutusData, PlutusData>] {
  const keyGenerator = choosePrimitiveGenerator();
  const valueGenerator = choosePrimitiveGenerator();
  const m = new Map<PlutusData, PlutusData>();
  let keyShape;
  let key;
  let valueShape;
  let value;
  const keyStrings: string[] = [];
  const maxi = maxLength * Math.random();
  for (let i = 0; i < maxi; i++) {
    [keyShape, key] = keyGenerator(maxLength);
    const keyString = Data.to(key);
    if (!keyStrings.includes(keyString)) {
      keyStrings.push(keyString);
      const v = valueGenerator(maxLength);
      valueShape = v[0];
      value = v[1];
      m.set(key, value);
    }
  }

  const shape = new UnsizedMap(keyShape, valueShape);
  return [shape, m];
}

function genConstr(
  maxLength: number,
  maxDepth: number,
): [Constr<Shape>, Constr<PlutusData>] {
  const [fieldsShape, fields] = genList(maxLength, maxDepth);
  const index = genNumber(maxLength);
  const shape = new Constr(index, fieldsShape);
  const c = new Constr(index, fields);
  return [shape, c];
}

function genUnsizedConstr(
  maxLength: number,
): [UnsizedConstr<Shape>, Constr<PlutusData>] {
  const [fieldsShape, fields] = genUnsizedList(maxLength);
  const index = genNumber(maxLength);
  const shape = new UnsizedConstr(index, fieldsShape.elemShape);
  const c = new Constr(index, fields);
  return [shape, c];
}

function genRecord(
  maxLength: number,
  maxDepth: number,
): [RecordType<Shape>, RecordType<PlutusData>] {
  const r: RecordType<PlutusData> = {};
  const shape: RecordType<Shape> = {};
  const maxi = maxLength * Math.random();
  for (let i = 0; i < maxi; i++) {
    const [fieldShape, field] = genPlutusData(maxLength, maxDepth);
    const [nameShape, name] = genString(maxLength);
    shape[nameShape] = fieldShape;
    r[name] = field;
  }
  return [shape, r];
}

function genUnsizedValue(
  maxLength: number,
): [
  UnsizedMap<string, UnsizedMap<string, bigint>>,
  Map<string, Map<string, bigint>>,
] {
  const v = new Map<string, Map<string, bigint>>();
  const ccys: string[] = [];
  const maxi = maxLength * Math.random();
  for (let i = 0; i < maxi; i++) {
    const ccy = genString(maxLength)[1];
    if (!ccys.includes(ccy)) {
      ccys.push(ccy);
      const tknMap = new Map<string, bigint>();
      const tkns: string[] = [];
      const maxj = maxLength * Math.random();
      for (let j = 0; j < maxj; j++) {
        const tkn = genString(maxLength)[1];
        const amnt = genInteger(maxLength)[1];
        if (!tkns.includes(tkn)) {
          tkns.push(tkn);
          tknMap.set(tkn, amnt);
        }
      }
      v.set(ccy, tknMap);
    }
  }

  const shape = new UnsizedMap("ccy", new UnsizedMap("tkn", 0n));
  return [shape, v];
}

function choosePrimitiveGenerator() {
  return randomChoice([
    //genUint8Array,
    genString,
    genInteger,
    genUnsizedValue,
  ]);
}

function chooseGenerator() {
  return randomChoice([
    //genUint8Array,
    genString,
    genInteger,
    genList,
    genUnsizedList,
    genMap,
    genUnsizedMap,
    genConstr,
    genUnsizedConstr,
    genRecord,
    genUnsizedValue,
  ]);
}

export function genPlutusData(
  maxLength: number,
  maxDepth: number,
): [Shape, PlutusData] {
  const newMaxLength = Math.max(0, maxLength - 1);
  const newMaxDepth = maxDepth - 1;
  const generator = maxDepth > 0
    ? chooseGenerator()
    : choosePrimitiveGenerator();

  return generator(newMaxLength, newMaxDepth);
}

export function strip(data: PlutusData): PlutusData {
  if (data instanceof Constr) {
    return new Constr(data.index, strip(data.fields) as PlutusData[]);
  } else if (data instanceof Array) {
    const l: PlutusData[] = [];
    data.forEach((elem) => l.push(strip(elem)));
    return l;
  } else if (data instanceof Map) {
    const m = new Map<PlutusData, PlutusData>();
    data.forEach((v, k) => m.set(strip(k), strip(v)));
    return m;
  } else if (data instanceof Uint8Array) {
    return toHex(data);
  } else if (data instanceof Object) { // interpreting as records
    const l: PlutusData[] = [];
    Object.values(data).forEach((elem) => l.push(strip(elem)));
    return l;
  } else {
    return data;
  }
}
