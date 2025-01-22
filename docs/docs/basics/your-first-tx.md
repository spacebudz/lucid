---
title: Build your first transaction
description: How to create and submit a transaction with Lucid.
order: 2
---

Let's create a simple transaction where we send `5 ADA` to two recipients each:

```js
const tx = await lucid.newTx()
  .payTo("addr_testa...", { lovelace: 5000000n })
  .payTo("addr_testb...", { lovelace: 5000000n })
  .commit();
```

Transactions always need to end with `.commit()` in order to balance the
transaction and do coin selection.

Next we sign the transaction:

```js
const signedTx = await tx.sign().commit();
```

Here we also need to call `.commit()` when we are ready with signing.

Lastly we submit the transaction:

```js
const txHash = await signedTx.submit();

console.log(txHash);
```

The full example:

```js
import { Blockfrost, Lucid } from "https://deno.land/x/lucid/mod.ts";

const lucid = new Lucid({
  provider: new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    "<projectId>",
  ),
});

lucid.selectWalletFromPrivateKey(privateKey);

const tx = await lucid.newTx()
  .payTo("addr_testa...", { lovelace: 5000000n })
  .payTo("addr_testb...", { lovelace: 5000000n })
  .commit();

const signedTx = await tx.sign().commit();
const txHash = await signedTx.submit();

console.log(txHash);
```
