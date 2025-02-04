import { Blockfrost, Data, Lucid } from "../mod.ts";

/*
  AlwaysSucceeds Example
  Lock a UTxO with some ADA
  UTxO can be unlocked by anyone
  Showcasing PlutusV2

  Contract:

  validate :: () -> () -> ScriptContext -> Bool
  validate _ _ _ = True
 */

const lucid = new Lucid({
  provider: new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    "<projectId>",
  ),
});

const api = await (globalThis as any).cardano.nami.enable();
// Assumes you are in a browser environment
lucid.selectWalletFromApi(api);

const alwaysSucceed = lucid.newScript({
  type: "PlutusV2",
  script: "49480100002221200101",
});

const alwaysSucceedAddress = alwaysSucceed.toAddress();

const Datum = () => Data.void();
const Redeemer = () => Data.void();

export async function lockUtxo(
  lovelace: bigint,
): Promise<string> {
  const tx = await lucid
    .newTx()
    .payToContract(alwaysSucceedAddress, { Inline: Datum() }, { lovelace })
    .payToContract(alwaysSucceedAddress, {
      AsHash: Datum(),
      scriptRef: alwaysSucceed.script, // adding plutusV2 script to output
    }, {})
    .commit();

  const signedTx = await tx.sign().commit();

  const txHash = await signedTx.submit();

  return txHash;
}

export async function redeemUtxo(): Promise<string> {
  const referenceScriptUtxo = (await lucid.utxosAt(alwaysSucceedAddress)).find(
    (utxo) => Boolean(utxo.scriptRef),
  );
  if (!referenceScriptUtxo) throw new Error("Reference script not found");

  const utxo = (await lucid.utxosAt(alwaysSucceedAddress)).find((utxo) =>
    utxo.datum === Datum() && !utxo.scriptRef
  );
  if (!utxo) throw new Error("Spending script utxo not found");

  const tx = await lucid
    .newTx()
    .readFrom([referenceScriptUtxo]) // spending utxo by reading plutusV2 from reference utxo
    .collectFrom([utxo], Redeemer())
    .commit();

  const signedTx = await tx.sign().commit();

  const txHash = await signedTx.submit();

  return txHash;
}
