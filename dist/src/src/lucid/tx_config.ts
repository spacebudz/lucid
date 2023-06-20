import { Configuration } from '../types/mod.js';

export const defaultConfig: Configuration = {
  enableChangeSplitting: true,
  changeCollateral: '5000000',
  changeMinUtxo: '100000000',
  changeNativeAssetChunkSize: 20,
};
