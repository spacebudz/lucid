---
title: Mint assets
description: Mint and burn native tokens. 
order: 6
---

<div style="padding: 14px 20px; border-radius: 6px; border: solid 1px deepskyblue">
<b>Note:</b> You need to have a wallet and a provider selected in order to build and submit transactions.
</div>

## Mint

First we need to create a minting policy for the assets we want to mint. In this example we utilize a native script time-locking policy with our wallet as required signer:

```js
const { paymentCredential } = lucid.utils.getAddressDetails(
  await lucid.wallet.address(),
);

const mintingPolicy = lucid.utils.nativeScriptFromJson(
  {
    type: "all",
    scripts: [
      { type: "sig", keyHash: paymentCredential.hash },
      {
        type: "before",
        slot: lucid.utils.unixTimeToSlot(Date.now() + 1000000),
      },
    ],
  },
);
```

Next we derive the policy id from the minting policy script:

```js
const policyId = lucid.utils.mintingPolicyToId(mintingPolicy);
```

Now we can mint our desired tokens:

```js
const unit = policyId + fromText("MyMintedToken");

const tx = await lucid.newTx()
  .mintAssets({ [unit]: 1n })
  .validTo(Date.now() + 200000)
  .attachMintingPolicy(mintingPolicy)
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```

## Burn

```js
const unit = policyId + fromText("MyMintedToken");

const tx = await lucid
  .newTx()
  .mintAssets({ [unit]: -1n })
  .validTo(Date.now() + 200000)
  .attachMintingPolicy(mintingPolicy)
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```

[Tx API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=Tx)\
[TxComplete API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=TxComplete)\
[TxSigned API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=TxSigned)\
[Utils API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=Utils)