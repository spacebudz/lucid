import { C, Core } from "../core/mod.ts";
import { CostModels } from "../mod.ts";

export function createCostModels(costModels: CostModels): Core.Costmdls {
  const costmdls = C.Costmdls.new();

  // add plutus v1
  const costmdlV1 = C.CostModel.new();
  Object.values(costModels.PlutusV1).forEach((cost, index) => {
    costmdlV1.set(index, C.Int.new(C.BigNum.from_str(cost.toString())));
  });
  costmdls.insert(C.Language.new_plutus_v1(), costmdlV1);

  // add plutus v2
  const costmdlV2 = C.CostModel.new_plutus_v2();
  Object.values(costModels.PlutusV2 || []).forEach((cost, index) => {
    costmdlV2.set(index, C.Int.new(C.BigNum.from_str(cost.toString())));
  });
  costmdls.insert(C.Language.new_plutus_v2(), costmdlV2);

  return costmdls;
}
