import { Constr, Data, Shape, UnsizedConstr, UnsizedList, UnsizedMap } from "../plutus/data.ts"
import { List, PlutusData, RecordType } from "../types/types.ts"
import { toHex, fromHex } from "./utils.ts"

function randomChoice<T>(alternatives: T[]): T {
    const choice = Math.floor(Math.random() * alternatives.length)
    return alternatives[choice]
}

function genNumber(maxLength: number): number {
    if (Math.random() > 0.9) {
        return 0
    } else {
        return Math.floor(Math.random() * (maxLength ** maxLength))
    }
}

function genInteger(maxLength: number): [bigint, bigint] {
    const i = BigInt(genNumber(maxLength))
    return [i, i]
}

function genChar(): string {
    const alph = "abcdef"
    const choice = Math.floor(Math.random() * (alph.length + 10))
    if (choice < alph.length) {
        return alph.charAt(choice)
    } else {
        return Math.floor(Math.random() * 10).toString()
    }
}

function genString(maxLength: number): [string, string] {
    const l: string[] = []
    const length =  8 * Math.floor(maxLength * Math.random())
    for (let i = 0; i < length; i++) {
        l.push(genChar())
    }
    const s = l.join("")
    return [s, s]
}

function genUint8Array(maxLength: number): [Uint8Array, Uint8Array] {
    const u = fromHex(genString(maxLength)[1])
    return [u, u]
} 

function genList(maxDepth: number, maxLength: number): [Array<Shape>, Array<PlutusData>] {
    const l: Array<PlutusData> = []
    const shape: Array<Shape> = []
    for (let i = 0; i < maxLength * Math.random(); i++) {
        const [elemShape, elem] = genPlutusData(maxDepth , maxLength)
        shape.push(elemShape)
        l.push(elem)
    }
    return [shape, l]
}

function genUnsizedList(maxDepth: number, maxLength: number): [UnsizedList<Shape>, List<PlutusData>] {
    const generator = choosePrimitiveGenerator()
    const l: Array<PlutusData> = []
    let elemShape
    let elem
    for (let i = 0; i < maxLength * Math.random(); i++) {
        [elemShape, elem] = generator(maxDepth) 
        l.push(elem)
    }
    const shape = new UnsizedList(elemShape)
    return [shape, l]
}

function genMap(maxDepth: number, maxLength: number): [Map<Shape, Shape>, Map<PlutusData, PlutusData>] {
    const m = new Map<PlutusData, PlutusData>()
    const shape = new Map<Shape, Shape>()
    const keyStrings: string[] = []
    for (let i = 0; i < maxLength * Math.random(); i++) {
        const [keyShape, key] = genPlutusData(maxDepth , maxLength)
        const keyString = Data.to(key)
        if (!keyStrings.includes(keyString)) {
            keyStrings.push(keyString)
            const [valueShape, value] = genPlutusData(maxDepth, maxLength)
            shape.set(keyShape, valueShape)
            m.set(key, (value))

        }
    }
    return [shape, m]
}

function genUnsizedMap(maxDepth: number, maxLength: number): [UnsizedMap<Shape, Shape>, Map<PlutusData, PlutusData>] {
    const keyGenerator = choosePrimitiveGenerator()
    const valueGenerator = choosePrimitiveGenerator()
    const m = new Map<PlutusData, PlutusData>()
    let keyShape
    let key
    let valueShape
    let value
    const keyStrings: string[] = []
    for (let i = 0; i < maxLength * Math.random(); i++) {
        [keyShape, key] = keyGenerator(maxDepth)
        const keyString = Data.to(key)
        if (!keyStrings.includes(keyString)) {
            keyStrings.push(keyString)
            const v = valueGenerator(maxDepth)
            valueShape = v[0]
            value = v[1]
            m.set(key, value)
        }
    }

    const shape = new UnsizedMap(keyShape, valueShape)
    return [shape, m]
}

function genConstr(maxDepth: number, maxLength: number): [Constr<Shape>, Constr<PlutusData>] {
    const [fieldsShape, fields] = genList(maxDepth , maxLength)
    const index = genNumber(maxLength)
    const shape = new Constr(index, fieldsShape)
    const c = new Constr(index, fields)
    return [shape, c]
}

function genUnsizedConstr(maxDepth: number, maxLength: number): [UnsizedConstr<Shape>, Constr<PlutusData>] {
    const [fieldsShape, fields] = genUnsizedList(maxDepth, maxLength)
    const index = genNumber(maxLength)
    const shape = new UnsizedConstr(index, fieldsShape.elemShape)
    const c = new Constr(index, fields)
    return [shape, c]
}

function genRecord(maxDepth: number, maxLength: number): [RecordType<Shape>, RecordType<PlutusData>] {
    const r: RecordType<PlutusData> = {}
    const shape: RecordType<Shape> = {}
    for (let i = 0; i < maxLength * Math.random(); i++) {
        const [fieldShape, field] = genPlutusData(maxDepth, maxLength)
        const [nameShape, name] = genString(maxLength)
        shape[nameShape] = fieldShape
        r[name] = field
    }
    return [shape, r]
}

function choosePrimitiveGenerator() {
    return randomChoice([
        //genUint8Array,
        genString,
        genInteger
    ])
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
        genRecord
    ])
}

export function genPlutusData(maxDepth: number, maxLength: number): [Shape, PlutusData] {
    const newMaxLength = maxLength / 2
    const newMaxDepth = maxDepth - 1
    const generator = maxDepth > 0 ? chooseGenerator() : choosePrimitiveGenerator()

    return generator(newMaxDepth, newMaxLength)

    // const choice = Math.floor(Math.random() * numChoices)
    // switch (choice) {
    //     case 1:
    //         return genString(maxLength)
    //     case 2:
    //         return genInteger(maxLength)
    //     case 3:
    //         return genList(newMaxDepth, newMaxLength)
    //     case 4:
    //         return genMap(newMaxDepth, newMaxLength)
    //     case 5:
    //         return genConstr(newMaxDepth, newMaxLength)
    //     case 6:
    //         return genRecord(newMaxDepth, newMaxLength)
    //     default:
    //         return genUint8Array(maxLength)
    // }
}

export function strip(data: PlutusData): PlutusData {
    if (data instanceof Constr) {
        return new Constr(data.index, strip(data.fields) as PlutusData[])
    } else if (data instanceof Array) {
        const l: PlutusData[] = []
        data.forEach((elem) => l.push(strip(elem)))
        return l
    } else if (data instanceof Map) {
        const m = new Map<PlutusData, PlutusData>()
        data.forEach((v, k) => m.set(strip(k), strip(v)))
        return m
    } else if (data instanceof Uint8Array) {
        return toHex(data)
    } else if (data instanceof Object) { // interpreting as records
        const l: PlutusData[] = []
        Object.values(data).forEach((elem) => l.push(strip(elem)))
        return l
    } else {
        return data
    }
}