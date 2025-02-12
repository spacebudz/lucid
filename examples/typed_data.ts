import { Blockfrost, Data, fromText, Lucid } from "../mod.ts";

const lucid = new Lucid({
  provider: new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    "<projectId>",
  ),
});

const api = await (globalThis as any).cardano.nami.enable();
// Assumes you are in a browser environment
lucid.selectWalletFromApi(api);

// (Type definition could be auto generated from on-chain script e.g. Aiken)
const MyDatum = Data.Object({
  name: Data.Bytes(),
  age: Data.Integer(),
  fruits: Data.Enum("Apple", "Banana", { Berry: [Data.Bytes()] }, {
    Other: { name: Data.Bytes(), quantity: Data.Integer() },
  }),
  colors: Data.Array(Data.Bytes()),
  description: Data.Nullable(Data.Bytes()),
});

export async function send(): Promise<string> {
  const datum: typeof MyDatum = {
    name: fromText("Lucid"),
    age: 0n,
    fruits: { Other: { name: fromText("Coconut"), quantity: 123n } },
    colors: [fromText("Blue"), fromText("Purple")],
    description: null,
  };

  const tx = await lucid
    .newTx()
    .payToWithData("addr_test...", Data.to(datum, MyDatum), {
      lovelace: 10000000n,
    })
    .commit();

  const signedTx = await tx.sign().commit();

  const txHash = await signedTx.submit();

  return txHash;
}
