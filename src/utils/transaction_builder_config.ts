import { C, SlotConfig } from "../mod.ts";
import { ProtocolParameters } from "../types/mod.ts";
import { createCostModels } from "./cost_model.ts";
import { Freeable, Freeables } from "./freeable.ts";

export function getTransactionBuilderConfig(
  protocolParameters: ProtocolParameters,
  slotConfig: SlotConfig,
  blockfrostConfig: {
    url?: string;
    projectId?: string;
  },
) {
  const bucket: Freeable[] = [];
  let builderA = C.TransactionBuilderConfigBuilder.new();

  const coinsPerUtxoByte = C.BigNum.from_str(
    protocolParameters.coinsPerUtxoByte.toString(),
  );
  bucket.push(coinsPerUtxoByte);
  let builderB = builderA.coins_per_utxo_byte(coinsPerUtxoByte);
  builderA.free();

  const minFeeA = C.BigNum.from_str(protocolParameters.minFeeA.toString());
  bucket.push(minFeeA);
  const minFeeB = C.BigNum.from_str(protocolParameters.minFeeB.toString());
  bucket.push(minFeeB);
  const linearFee = C.LinearFee.new(minFeeA, minFeeB);
  bucket.push(linearFee);
  builderA = builderB.fee_algo(linearFee);
  builderB.free();

  const keyDeposit = C.BigNum.from_str(
    protocolParameters.keyDeposit.toString(),
  );
  bucket.push(keyDeposit);
  builderB = builderA.key_deposit(keyDeposit);
  builderA.free();

  const poolDeposit = C.BigNum.from_str(
    protocolParameters.poolDeposit.toString(),
  );
  bucket.push(poolDeposit);
  builderA = builderB.pool_deposit(poolDeposit);
  builderB.free();

  builderB = builderA.max_tx_size(protocolParameters.maxTxSize);
  builderA.free();

  builderA = builderB.max_value_size(protocolParameters.maxValSize);
  builderB.free();

  builderB = builderA.collateral_percentage(
    protocolParameters.collateralPercentage,
  );
  builderA.free();

  builderA = builderB.max_collateral_inputs(
    protocolParameters.maxCollateralInputs,
  );
  builderB.free();

  const maxTxExMem = C.BigNum.from_str(
    protocolParameters.maxTxExMem.toString(),
  );
  bucket.push(maxTxExMem);
  const maxTxExSteps = C.BigNum.from_str(
    protocolParameters.maxTxExSteps.toString(),
  );
  bucket.push(maxTxExSteps);
  const exUnits = C.ExUnits.new(maxTxExMem, maxTxExSteps);
  bucket.push(exUnits);
  builderB = builderA.max_tx_ex_units(exUnits);
  builderA.free();

  const exUnitPrices = C.ExUnitPrices.from_float(
    protocolParameters.priceMem,
    protocolParameters.priceStep,
  );
  bucket.push(exUnitPrices);
  builderA = builderB.ex_unit_prices(exUnitPrices);
  builderB.free();

  const zeroTime = C.BigNum.from_str(slotConfig.zeroTime.toString());
  bucket.push(zeroTime);
  const zeroSlot = C.BigNum.from_str(slotConfig.zeroSlot.toString());
  bucket.push(zeroSlot);
  builderB = builderA.slot_config(zeroTime, zeroSlot, slotConfig.slotLength);
  builderA.free();

  const blockfrost = C.Blockfrost.new(
    blockfrostConfig?.url ?? "" + "utils/tx/evaulate",
    blockfrostConfig?.projectId ?? "",
  );
  bucket.push(blockfrost);
  builderA = builderB.blockfrost(blockfrost);
  builderB.free();

  const costModels = createCostModels(protocolParameters.costModels);
  bucket.push(costModels);
  builderB = builderA.costmdls(costModels);

  const config = builderB.build();
  builderB.free();
  Freeables.free(...bucket);

  return config;
}
