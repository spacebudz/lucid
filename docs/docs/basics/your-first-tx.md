---
title: Build your first transaction
description: How to create and submit a transaction with Lucid.
order: 2
---

Let's create a simple transaction where we send `5 ADA` to two recipients each:

```js
const tx = await lucid.newTx()
  .payToAddress("addr_testa...", { lovelace: 5000000n })
  .payToAddress("addr_testb...", { lovelace: 5000000n })
  .complete();
```

Transactions always need to end with `.complete()` in order to balance the
transaction and do coin selection.

Next we sign the transaction:

```js
const signedTx = await tx.sign().complete();
```

Here we also need to call `.complete()` when we are ready with signing.

Lastly we submit the transaction:

```js
const txHash = await signedTx.submit();

console.log(txHash);
```

The full example:

```js
import { Blockfrost, Lucid } from "https://deno.land/x/lucid/mod.ts";

const lucid = await Lucid.new(
  new Blockfrost("https://cardano-preprod.blockfrost.io/api/v0", "<projectId>"),
  "Preprod",
);

lucid.selectWalletFromPrivateKey(privateKey);

const tx = await lucid.newTx()
  .payToAddress("addr_testa...", { lovelace: 5000000n })
  .payToAddress("addr_testb...", { lovelace: 5000000n })
  .complete();

const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();

console.log(txHash);
```
