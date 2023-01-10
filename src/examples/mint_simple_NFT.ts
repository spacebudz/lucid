import {
  Blockfrost,
  fromText,
  Lucid,
  MintingPolicy,
  PolicyId,
  TxHash,
  Unit,
} from "../mod.ts";

/*
  MintSimpleNFT Example
  Mint or burn a simple NFT.
 */

const lucid = await Lucid.new(
  new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", "<projectId>"),
  "Preview",
);

const api = await window.cardano.nami.enable();
// Assumes you are in a browser environment
lucid.selectWallet(api);

const { paymentCredential } = lucid.utils.getAddressDetails(
  await lucid.wallet.address(),
);

const mintingPolicy: MintingPolicy = lucid.utils.nativeScriptFromJson(
  {
    type: "all",
    scripts: [
      { type: "sig", keyHash: paymentCredential?.hash! },
      {
        type: "before",
        slot: lucid.utils.unixTimeToSlot(Date.now() + 1000000),
      },
    ],
  },
);

const policyId: PolicyId = lucid.utils.mintingPolicyToId(
  mintingPolicy,
);

export async function mintNFT(
  name: string,
): Promise<TxHash> {
  const unit: Unit = policyId + fromText(name);

  const tx = await lucid
    .newTx()
    .mintAssets({ [unit]: 1n })
    .validTo(Date.now() + 100000)
    .attachMintingPolicy(mintingPolicy)
    .complete();

  const signedTx = await tx.sign().complete();

  const txHash = await signedTx.submit();

  return txHash;
}

export async function burnNFT(
  name: string,
): Promise<TxHash> {
  const unit: Unit = policyId + fromText(name);

  const tx = await lucid
    .newTx()
    .mintAssets({ [unit]: -1n })
    .validTo(Date.now() + 100000)
    .attachMintingPolicy(mintingPolicy)
    .complete();

  const signedTx = await tx.sign().complete();

  const txHash = await signedTx.submit();

  return txHash;
}
