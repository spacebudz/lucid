import { PlutusData, C, Datum, Redeemer } from '..';
import Core from 'core/types';
import { fromHex, toHex } from './utils';

export class Construct {
  index: number;
  args: PlutusData[];

  constructor(index: number, args: PlutusData[]) {
    this.index = index;
    this.args = args;
  }
}

export class Data {
  static to(data: PlutusData): Datum | Redeemer {
    const serialize = (data: PlutusData) => {
      try {
        if (
          typeof data === 'bigint' ||
          typeof data === 'number' ||
          (typeof data === 'string' &&
            !isNaN(parseInt(data)) &&
            data.slice(-1) === 'n')
        ) {
          const bigint =
            typeof data === 'string' ? BigInt(data.slice(0, -1)) : data;
          return C.PlutusData.new_integer(C.BigInt.from_str(bigint.toString()));
        } else if (typeof data === 'string') {
          return C.PlutusData.new_bytes(fromHex(data));
        } else if (data instanceof Construct) {
          const { index, args } = data;
          const plutusList = C.PlutusList.new();

          args.forEach(arg => plutusList.add(serialize(arg)));

          return C.PlutusData.new_constr_plutus_data(
            C.ConstrPlutusData.new(
              C.BigNum.from_str(index.toString()),
              plutusList
            )
          );
        } else if (data instanceof Array) {
          const plutusList = C.PlutusList.new();

          data.forEach(arg => plutusList.add(serialize(arg)));

          return C.PlutusData.new_list(plutusList);
        } else if (data instanceof Map) {
          const plutusMap = C.PlutusMap.new();

          for (const [key, value] of data.entries()) {
            plutusMap.insert(serialize(key), serialize(value));
          }

          return C.PlutusData.new_map(plutusMap);
        }
        throw new Error('Unsupported type');
      } catch (error) {
        throw new Error('Could not serialize the data: ' + error);
      }
    };
    return toHex(serialize(data).to_bytes()) as Datum | Redeemer;
  }
  static from(data: Datum | Redeemer): PlutusData {
    const plutusData = C.PlutusData.from_bytes(fromHex(data));
    const deserialize = (data: Core.PlutusData): PlutusData => {
      if (data.kind() === 0) {
        const constr = data.as_constr_plutus_data()!;
        const l = constr.data();
        const desL = [];
        for (let i = 0; i < l.len(); i++) {
          desL.push(deserialize(l.get(i)));
        }
        return new Construct(parseInt(constr.alternative().to_str()), desL);
      } else if (data.kind() === 1) {
        const m = data.as_map()!;
        const desM: Map<PlutusData, PlutusData> = new Map();
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
      throw new Error('Unsupported type');
    };
    return deserialize(plutusData);
  }
  static empty(): Datum | Redeemer {
    return toHex(
      C.PlutusData.new_constr_plutus_data(
        C.ConstrPlutusData.new(C.BigNum.from_str('0'), C.PlutusList.new())
      ).to_bytes()
    );
  }
}
