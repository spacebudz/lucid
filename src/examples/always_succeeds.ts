import {
  Address,
  Blockfrost,
  Data,
  Lovelace,
  Lucid,
  SpendingValidator,
  TxHash,
} from "../../mod.ts";

/*
  AlwaysSucceeds Example
  Lock a UTxO with some ADA
  UTxO can be unlocked by anyone
  Showcasing PlutusV2

  Contract:

  validate :: () -> () -> ScriptContext -> Bool
  validate _ _ _ = True
 */

const lucid = await Lucid.new(
  new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", "<projectId>"),
  "Preview",
);

const api = await window.cardano.nami.enable();
// Assumes you are in a browser environment
lucid.selectWallet(api);

const alwaysSucceedScript: SpendingValidator = {
  type: "PlutusV2",
  script: "49480100002221200101",
};

const alwaysSucceedAddress: Address = lucid.utils.validatorToAddress(
  alwaysSucceedScript,
);

const Datum = () => Data.void();
const Redeemer = () => Data.void();

export async function lockUtxo(
  lovelace: Lovelace,
): Promise<TxHash> {
  const tx = await lucid
    .newTx()
    .payToContract(alwaysSucceedAddress, { inline: Datum() }, { lovelace })
    .payToContract(alwaysSucceedAddress, {
      asHash: Datum(),
      scriptRef: alwaysSucceedScript, // adding plutusV2 script to output
    }, {})
    .complete();

  const signedTx = await tx.sign().complete();

  const txHash = await signedTx.submit();

  return txHash;
}

export async function redeemUtxo(): Promise<TxHash> {
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
    .complete();

  const signedTx = await tx.sign().complete();

  const txHash = await signedTx.submit();

  return txHash;
}
