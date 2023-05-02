---
title: Delegate
description: Register/deregister a stake key and delegate to a stake pool. 
order: 3
---

<div style="padding: 14px 20px; border-radius: 6px; border: solid 1px deepskyblue">
<b>Note:</b> You need to have a wallet and a provider selected in order to build and submit transactions.
</div>

## Register stake key

`2 ADA` will be taken as pledge for the registration of the stake key.

```js
const rewardAddress = await lucid.wallet.rewardAddress();

const tx = await lucid.newTx()
  .registerStake(rewardAddress)
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```

## Delegate to a stake pool

```js
const rewardAddress = await lucid.wallet.rewardAddress();

const tx = await lucid.newTx()
  .delegateTo(rewardAddress, "poolabc...")
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```

## Withdraw rewards
```js
const rewardAddress = await lucid.wallet.rewardAddress();

const delegation = await lucid.wallet.getDelegation();

const tx = await lucid.newTx()
  .withdraw(rewardAddress, delegation.rewards)
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```


## Deregister stake key

Reclaim the `2 ADA` used for the registration of the stake key.

```js
const rewardAddress = await lucid.wallet.rewardAddress();

const tx = await lucid.newTx()
  .deregisterStake(rewardAddress)
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```

[Tx API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=Tx)\
[TxComplete API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=TxComplete)\
[TxSigned API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=TxSigned)