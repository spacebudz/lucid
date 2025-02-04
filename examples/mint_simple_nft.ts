import {
  Addresses,
  Blockfrost,
  Cardano,
  Codec,
  fromText,
  Lucid,
} from "../mod.ts";

/*
  MintSimpleNFT Example
  Mint or burn a simple NFT.
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

const { payment } = Addresses.inspect(
  await lucid.wallet.address(),
);

const mintingPolicy = lucid.newScript({
  type: "Native",
  script: Codec.encodeNativeScript(
    {
      type: "All",
      scripts: [
        { type: "Sig", keyHash: payment?.hash! },
        {
          type: "Before",
          slot: lucid.utils.unixTimeToSlots(Date.now() + 1000000),
        },
      ],
    },
  ),
});

const policyId = mintingPolicy.toHash();

export async function mintNFT(
  name: string,
): Promise<string> {
  const unit = policyId + fromText(name);

  const tx = await lucid
    .newTx()
    .mint({ [unit]: 1n })
    .validTo(Date.now() + 100000)
    .attachScript(mintingPolicy.script)
    .commit();

  const signedTx = await tx.sign().commit();

  const txHash = await signedTx.submit();

  return txHash;
}

export async function burnNFT(
  name: string,
): Promise<string> {
  const unit = policyId + fromText(name);

  const tx = await lucid
    .newTx()
    .mint({ [unit]: -1n })
    .validTo(Date.now() + 100000)
    .attachScript(mintingPolicy.script)
    .commit();

  const signedTx = await tx.sign().commit();

  const txHash = await signedTx.submit();

  return txHash;
}
