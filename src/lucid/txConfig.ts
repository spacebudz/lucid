import { Configuration } from 'types';

export const defaultConfig: Configuration = {
  enableChangeSplitting: false,
  changeCollateral: '5000000',
  changeMinUtxo: '100000000',
  changeNativeAssetChunkSize: 20,
};
