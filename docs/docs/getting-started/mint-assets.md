---
title: Mint assets
description: Mint and burn native tokens. 
order: 6
---

<div style="padding: 14px 20px; border-radius: 6px; border: solid 1px deepskyblue">
<b>Note:</b> You need to have a wallet and a provider selected in order to build and submit transactions.
</div>

## Mint

First we need to create a minting policy for the assets we want to mint. In this
example we utilize a native script time-locking policy with our wallet as
required signer:

```js
import { Addresses } from "https://deno.land/x/lucid/mod.ts";

const { payment } = Addresses.inspect(
  await lucid.wallet.address(),
);

const mintingPolicy = lucid.newScript(
  {
    type: "All",
    scripts: [
      { type: "Sig", keyHash: paymentCredential.hash },
      {
        type: "Before",
        slot: lucid.utils.unixTimeToSlots(Date.now() + 1000000),
      },
    ],
  },
);
```

Next we derive the policy id from the minting policy script:

```js
const policyId = mintingPolicy.toHash();
```

Now we can mint our desired tokens:

```js
const unit = policyId + fromText("MyMintedToken");

const tx = await lucid.newTx()
  .mint({ [unit]: 1n })
  .validTo(Date.now() + 200000)
  .attachScript(mintingPolicy)
  .commit();

const signedTx = await tx.sign().commit();

const txHash = await signedTx.submit();
```

## Burn

```js
const unit = policyId + fromText("MyMintedToken");

const tx = await lucid
  .newTx()
  .mint({ [unit]: -1n })
  .validTo(Date.now() + 200000)
  .attachScript(mintingPolicy)
  .commit();

const signedTx = await tx.sign().commit();

const txHash = await signedTx.submit();
```