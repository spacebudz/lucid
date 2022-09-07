import {
  Blockfrost,
  C,
  Lucid,
  MintingPolicy,
  PolicyId,
  toHex,
  TxHash,
  Unit,
  utf8ToHex,
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

const mintingPolicy: MintingPolicy = {
  type: "Native",
  script: toHex(
    C.NativeScript.new_script_pubkey(
      C.ScriptPubkey.new(C.Ed25519KeyHash.from_hex(paymentCredential?.hash!)),
    ).to_bytes(),
  ),
};

const policyId: PolicyId = lucid.utils.mintingPolicyToId(
  mintingPolicy,
);

export async function mintNFT(
  name: string,
): Promise<TxHash> {
  const unit: Unit = policyId + utf8ToHex(name);

  const tx = await lucid
    .newTx()
    .mintAssets({ [unit]: 1n })
    .attachMintingPolicy(mintingPolicy)
    .complete();

  const signedTx = await tx.sign().complete();

  const txHash = await signedTx.submit();

  return txHash;
}

export async function burnNFT(
  name: string,
): Promise<TxHash> {
  const unit: Unit = policyId + utf8ToHex(name);

  const tx = await lucid
    .newTx()
    .mintAssets({ [unit]: -1n })
    .attachMintingPolicy(mintingPolicy)
    .complete();

  const signedTx = await tx.sign().complete();

  const txHash = await signedTx.submit();

  return txHash;
}
