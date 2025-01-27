import {
  Addresses,
  Crypto,
  Emulator,
  fromText,
  Lucid,
  toUnit,
} from "../mod.ts";

const privateKey = Crypto.generatePrivateKey();

const address = Addresses.credentialToAddress(
  { Emulator: 0 },
  Crypto.privateKeyToDetails(privateKey).credential,
);

const { payment } = Addresses.inspect(address);

const emulator = new Emulator([{ address, assets: { lovelace: 3000000000n } }]);

const lucid = new Lucid({
  provider: emulator,
  wallet: { PrivateKey: privateKey },
});

const mintingPolicy = lucid.newScript({
  type: "All",
  scripts: [
    {
      type: "Before",
      slot: lucid.utils.unixTimeToSlots(emulator.now() + 60000),
    },
    { type: "Sig", keyHash: payment!.hash },
  ],
});

const policyId = mintingPolicy.toHash();

async function mint(): Promise<string> {
  const tx = await lucid.newTx()
    .mint({
      [toUnit(policyId, fromText("Wow"))]: 123n,
    })
    .validTo(emulator.now() + 30000)
    .attachScript(mintingPolicy)
    .commit();
  const signedTx = await tx.sign().commit();

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
