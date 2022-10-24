import {
  Blockfrost,
  Constr,
  Data,
  Lovelace,
  Lucid,
  SpendingValidator,
  TxHash,
} from "../../mod.ts";
import * as helios from "https://raw.githubusercontent.com/Hyperion-BT/Helios/v0.4.0/helios.js";

/*
  MatchingPubKeyHash Example
  Lock a UTxO with a PubKeyHash
  UTxO can be unlocked by providing the same PubKeyHash in the redeemer
  Showcasing Helios; Link: https://github.com/Hyperion-BT/Helios
 */

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    "<project_id>",
  ),
  "Preview",
);

const privateKey = "<private_key>";

lucid.selectWalletFromPrivateKey(privateKey);

const script: SpendingValidator = {
  type: "PlutusV1",
  script: JSON.parse(
    helios.Program.new(`
    spending matching_pubKeyHash

    struct Datum {
        owner: PubKeyHash
    }

    struct Redeemer {
        owner: PubKeyHash
    }

    func main(datum : Datum, redeemer: Redeemer) -> Bool {datum.owner == redeemer.owner}
`).compile().serialize(),
  ).cborHex,
};

const scriptAddress = lucid.utils.validatorToAddress(script);

export async function lockUtxo(lovelace: Lovelace): Promise<TxHash> {
  const { paymentCredential } = lucid.utils.getAddressDetails(
    await lucid.wallet.address(),
  );

  // This represents the Datum struct from the Helios on-chain code
  const datum = Data.to(
    new Constr(0, [new Constr(0, [paymentCredential?.hash!])]),
  );

  const tx = await lucid.newTx().payToContract(scriptAddress, datum, {
    lovelace,
  })
    .complete();

  const signedTx = await tx.sign().complete();

  return signedTx.submit();
}

export async function redeemUtxo(): Promise<TxHash> {
  const { paymentCredential } = lucid.utils.getAddressDetails(
    await lucid.wallet.address(),
  );

  // This represents the Redeemer struct from the Helios on-chain code
  const redeemer = Data.to(
    new Constr(0, [new Constr(0, [paymentCredential?.hash!])]),
  );

  const datumHash = lucid.utils.datumToHash(redeemer);

  const utxos = await lucid.utxosAt(scriptAddress);

  const utxo = utxos.find((utxo) => utxo.datumHash === datumHash);

  if (!utxo) throw new Error("UTxO not found.");

  const tx = await lucid.newTx().collectFrom([utxo], redeemer)
    .attachSpendingValidator(script)
    .complete();

  const signedTx = await tx.sign().complete();

  return signedTx.submit();
}
