import { Blockfrost, Data, fromText, Lucid, TxHash } from "../mod.ts";

const lucid = await Lucid.new(
  new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", "<projectId>"),
  "Preview",
);

const api = await window.cardano.nami.enable();
// Assumes you are in a browser environment
lucid.selectWallet(api);

// Type definition could be auto generated from on-chain script
const MyDatumSchema = Data.Object({
  name: Data.Bytes(),
  age: Data.Integer(),
  colors: Data.Array(Data.Bytes()),
  description: Data.Nullable(Data.Bytes()),
});
type MyDatum = Data.Static<typeof MyDatumSchema>;
const MyDatum = MyDatumSchema as unknown as MyDatum;

export async function send(): Promise<TxHash> {
  const datum: MyDatum = {
    name: fromText("Lucid"),
    age: 0n,
    colors: [fromText("Blue"), fromText("Purple")],
    description: null,
  };

  const tx = await lucid
    .newTx()
    .payToAddressWithData("addr_test...", Data.to(datum, MyDatum), {
      lovelace: 10000000n,
    })
    .complete();

  const signedTx = await tx.sign().complete();

  const txHash = await signedTx.submit();

  return txHash;
}
