import { Blockfrost, Data, Lucid, TxHash, utf8ToHex } from "../mod.ts";

const lucid = await Lucid.new(
  new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", "<projectId>"),
  "Preview",
);

const api = await window.cardano.nami.enable();
// Assumes you are in a browser environment
lucid.selectWallet(api);

// Type definition could be auto generated from on-chain script
const MyDatum = Data.Object({
  name: Data.String,
  age: Data.BigInt,
  colors: Data.Array(Data.String),
  description: Data.Nullable(Data.String),
});
type MyDatum = Data.Static<typeof MyDatum>;

export async function send(): Promise<TxHash> {
  const datum: MyDatum = {
    name: utf8ToHex("Lucid"),
    age: 0n,
    colors: [utf8ToHex("Blue"), utf8ToHex("Purple")],
    description: null,
  };

  const tx = await lucid
    .newTx()
    .payToAddressWithData("addr_test...", Data.to<MyDatum>(datum, MyDatum), {
      lovelace: 10000000n,
    })
    .complete();

  const signedTx = await tx.sign().complete();

  const txHash = await signedTx.submit();

  return txHash;
}
