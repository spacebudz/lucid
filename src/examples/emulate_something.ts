import {
  Emulator,
  fromText,
  generatePrivateKey,
  getAddressDetails,
  Lucid,
  toUnit,
  TxHash,
} from "../../mod.ts";

const privateKey = generatePrivateKey();

const address = await (await Lucid.new(undefined, "Custom"))
  .selectWalletFromPrivateKey(privateKey).wallet.address();

const { paymentCredential } = getAddressDetails(address);

const emulator = new Emulator([{ address, assets: { lovelace: 3000000000n } }]);

const lucid = await Lucid.new(emulator);

lucid.selectWalletFromPrivateKey(privateKey);

const mintingPolicy = lucid.utils.nativeScriptFromJson({
  type: "all",
  scripts: [
    {
      type: "before",
      slot: lucid.utils.unixTimeToSlot(emulator.now() + 60000),
    },
    { type: "sig", keyHash: paymentCredential?.hash! },
  ],
});

const policyId = lucid.utils.mintingPolicyToId(mintingPolicy);

async function mint(): Promise<TxHash> {
  const tx = await lucid.newTx()
    .mintAssets({
      [toUnit(policyId, fromText("Wow"))]: 123n,
    })
    .validTo(emulator.now() + 30000)
    .attachMintingPolicy(mintingPolicy)
    .complete();
  const signedTx = await tx.sign().complete();

  return signedTx.submit();
}

await mint();

emulator.awaitBlock(4);

console.log(await lucid.wallet.getUtxos());

// This should fail
try {
  await mint();
} catch (_e) {
  console.error(
    "Error: Second transaction failed because upper bound exceeded in mint script.",
  );
}
