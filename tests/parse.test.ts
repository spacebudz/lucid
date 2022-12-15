import { assertEquals } from "https://deno.land/std@0.167.0/testing/asserts.ts";
import { Data, PlutusData } from "../src/mod.ts";
import { PType } from "../src/plutus/parse.ts";
import {
  ExtPlutusData,
  genPType,
  genPTypeData,
  gMaxDepth,
  gMaxLength,
  stripConstr,
  stripExt,
} from "./generators.ts";

Deno.test("parsing property tests", () => {
  const dataErrs = new Map<string, number>();
  const ptypeErrs = new Map<string, number>();
  const otherErrs = new Map<string, number>();

  const iterations = 1000;
  for (let i = 0; i < iterations; i++) {
    console.log(i);
    try {
      const ptype: PType = genPType(gMaxDepth, gMaxLength);
      const rawData = genPTypeData(ptype);
      const data = stripExt(rawData);
      const extData = stripConstr(rawData);

      testDataParse(data, dataErrs);
      testPTypeParse(data, extData, ptype, ptypeErrs);
    } catch (err) {
      logError(err, otherErrs);
    }
  }
  let correct = iterations;
  correct -= printErrs(dataErrs, "Data parsing errors");
  correct -= printErrs(ptypeErrs, "PType parsing errors");
  correct -= printErrs(otherErrs, "other errors");

  console.log(correct + " x correct");
  assertEquals(correct, iterations);
});

function testDataParse(data: PlutusData, record: Map<string, number>) {
  try {
    assertEquals(data, Data.from(Data.to(data)));
  } catch (err) {
    logError(err, record);
  }
}

function testPTypeParse(
  data: PlutusData,
  extData: ExtPlutusData,
  ptype: PType,
  record: Map<string, number>,
) {
  try {
    assertEquals(data, ptype.pconstant(ptype.plift(data)));
    assertEquals(extData, ptype.plift(ptype.pconstant(extData)));
  } catch (err) {
    logError(err, record);
  }
}

function logError(err: Error, record: Map<string, number>) {
  const e = err.message; //[err.name, err.message, err.cause, err.stack].join("\n");
  const num = record.get(e);
  record.set(e, num ? num + 1 : 1);
}

function printErrs(record: Map<string, number>, name: string): number {
  let total = 0;
  record.forEach((num: number, err: string) => {
    console.error(num + " x " + err);
    total += num;
  });
  if (total) {
    console.log(`${name} ==> total: ${record.size} (${total})\n`);
  } else {
    console.log(`==> no ${name}\n`);
  }
  return total;
}
