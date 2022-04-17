import { PlutusDataJS } from 'types';
import { fromHex } from './utils';
import { C } from '../core';

export class Construct {
  index: number;
  args: PlutusDataJS[];

  constructor(index: number, args: PlutusDataJS[]) {
    this.index = index;
    this.args = args;
  }
}

export class PlutusData extends C.PlutusData {
  static fromJS(data: PlutusDataJS) {
    try {
      if (typeof data === 'bigint') {
        return C.PlutusData.new_integer(C.BigInt.from_str(data.toString()));
      } else if (typeof data === 'string') {
        return C.PlutusData.new_bytes(fromHex(data));
      } else if (data instanceof Construct) {
        const { index, args } = data;
        const plutusList = C.PlutusList.new();

        args.forEach((arg) => plutusList.add(PlutusData.fromJS(arg)));

        return C.PlutusData.new_constr_plutus_data(
          C.ConstrPlutusData.new(
            C.BigNum.from_str(index.toString()),
            plutusList,
          ),
        );
      } else if (Array.isArray(data)) {
        const plutusList = C.PlutusList.new();

        data.forEach((arg) => plutusList.add(PlutusData.fromJS(arg)));

        return C.PlutusData.new_list(plutusList);
      } else if (typeof data === 'object') {
        const plutusMap = C.PlutusMap.new();

        Object.entries(data).forEach(([key, value]) => {
          plutusMap.insert(
            C.PlutusData.new_bytes(fromHex(key)),
            PlutusData.fromJS(value),
          );
        });
      }
      throw new Error('Unsupported type');
    } catch (error) {
      throw new Error('Could not serialize the data: ' + error);
    }
  }
}
