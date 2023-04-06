---
title: Make payments
description: Send ADA and native tokens. 
order: 2
---

<div style="padding: 14px 20px; border-radius: 6px; border: solid 1px deepskyblue">
<b>Note:</b> You need to have a wallet and a provider selected in order to build and submit transactions.
</div>

## Simple ADA payment

```js
const tx = await lucid.newTx()
  .payToAddress("addr_test...", { lovelace: 5000000n })
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```

## Multiple recipients

Each `payToAddress` call creates new UTxO, also for same addresses.\
Lucid takes the order of outputs into account.

```js
const tx = await lucid.newTx()
  .payToAddress("addr_testa...", { lovelace: 5000000n })
  .payToAddress("addr_testb...", { lovelace: 5000000n })
  .payToAddress("addr_testc...", { lovelace: 5000000n })
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```

## Send native tokens

Lucid implicitly adds the minimum ADA requirement when sending native tokens.

```js
const policyId = "00...";
const assetName = "MyToken";

const tx = await lucid.newTx()
  .payToAddress("addr_test...", { [policyId + fromText(assetName)]: 10n })
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```

## Send ADA with metadata
```js
const tx = await lucid.newTx()
  .payToAddress("addr_test...", { lovelace: 5000000n })
  .attachMetadata(1, { msg: "Hello from Lucid." })
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```

## Send ADA with datum

The datum will be attached to the witness set and the datum hash is stored in the UTxO.
To inline the datum directly in the UTxO use `{ inline: Data.to("31313131") }`.\
Like with native tokens Lucid implicitly adds the minimum ADA requirement for datums.

```js
const tx = await lucid.newTx()
  .payToAddressWithData("addr_test...", Data.to("31313131"), { lovelace: 5000000n })
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```

[Tx API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=Tx)\
[TxComplete API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=TxComplete)\
[TxSigned API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=TxSigned)\
[Data API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=Data)