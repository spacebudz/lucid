import { PlutusData, C, Datum, Redeemer } from '..';
import { fromHex, toHex } from './utils';

export class Construct {
  index: number;
  args: Data[];

  constructor(index: number, args: PlutusData[]) {
    this.index = index;
    this.args = args;
  }
}

export class Data {
  static from(data: PlutusData): Datum | Redeemer {
    const serialize = (data: PlutusData) => {
      try {
        if (typeof data === 'bigint') {
          return C.PlutusData.new_integer(C.BigInt.from_str(data.toString()));
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
        } else if (Array.isArray(data)) {
          const plutusList = C.PlutusList.new();

          data.forEach(arg => plutusList.add(serialize(arg)));

          return C.PlutusData.new_list(plutusList);
        } else if (typeof data === 'object') {
          const plutusMap = C.PlutusMap.new();

          Object.entries(data).forEach(([key, value]) => {
            plutusMap.insert(
              C.PlutusData.new_bytes(fromHex(key)),
              serialize(value)
            );
          });

          return C.PlutusData.new_map(plutusMap);
        }
        throw new Error('Unsupported type');
      } catch (error) {
        throw new Error('Could not serialize the data: ' + error);
      }
    };
    return toHex(serialize(data).to_bytes()) as Datum | Redeemer;
  }
  static empty(): Datum | Redeemer {
    return toHex(
      C.PlutusData.new_constr_plutus_data(
        C.ConstrPlutusData.new(C.BigNum.from_str('0'), C.PlutusList.new())
      ).to_bytes()
    );
  }
}
