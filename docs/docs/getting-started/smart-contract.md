---
title: Interact with smart contracts
description: Build and submit transactions that include plutus validators. 
order: 7
---

It's important to note that on the Cardano blockchain, you don't directly interact with *smart contracts*. Rather, you work with validators. These validators are responsible for verifying the actions taken in a given transaction, rather than executing or calling any actions themselves. In other words, a validator checks whether the transaction meets its requirements, and if it does, the transaction is processed successfully. Conversely, if the requirements are not met, the transaction fails.
<br>
<br>

<div style="padding: 14px 20px; border-radius: 6px; border: solid 1px deepskyblue">
<b>Note:</b> You need to have a wallet and a provider selected in order to build and submit transactions.
</div>

## Matching numbers example

We demonstrate the idea of plutus validators in Lucid based on a validator that requires the number in the datum to match the number in the redeemer.

### Create and instantiate validator

Lucid consumes compiled validators. On-chain scripts can be written in PlutusTx, Aiken, Helios and many other languages available in the Cardano ecosystem.\
We then derive the address from the compiled script with Lucid.

```js
const matchingNumberScript = {
  type: "PlutusV2",
  script: "59099a590997010000...",
};

const matchingNumberAddress = lucid.utils.validatorToAddress(
  matchingNumberScript,
);
```

### Lock funds at plutus script

```js
const tx = await lucid
  .newTx()
  .payToContract(matchingNumberAddress, { inline: Data.to(100n) }, { lovelace: 20000000n })
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```

### Redeem from plutus script

```js
const [scriptUtxo] = await lucid.utxosAt(matchingNumberAddress);

const tx = await lucid
  .newTx()
  .collectFrom([scriptUtxo], Data.to(100n))
  .attachSpendingValidator(matchingNumberScript)
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();
```

## Apply parameters

Some validators are parameterized. Lucid allows you to apply parameters dynamically:

```js
const mintingPolicy = {
  type: "PlutusV2",
  script: applyParamsToScript(
    "5907945907910100...",
    [10n],
  ),
};
```

## Plutus script purposes

Like native scripts, plutus scripts can not only be used for checking the spending conditions of UTxOs, but also for verifying conditions related to minting, delegations and withdrawals.

In Lucid the following specified transaction constraints take as last parameter the redeemer. The redeemer is necessary to execute the script successfully. When leaving out the redeemer Lucid assumes you utilize puplic keys or native scripts.

```js
.collectFrom(utxos, redeemer)

.mintAssets(assets, redeemer)

.delegateTo(stakeAddress, poolId, redeemer)

.deregisterStake(stakeAddress, redeemer)

.withdraw(stakeAddress, rewardAmount, redeemer)
```

## Multi validator interactions

You can run and execute multiple validators in a single transaction with Lucid. The only limitation you have is the execution units limit:

```js
const tx = await lucid
  .newTx()
  .collectFrom([scriptUtxoA, scriptUtxoB], Data.void())
  .collectFrom([scriptUtxoC], Data.void())
  .collectFrom([scriptUtxoD], Data.void())
  .mintAssets([plutusPolicyId]: 10n, Data.void())
  .attachSpendingValidator(spendingScript1)
  .attachSpendingValidator(spendingScript2)
  .attachMintingPolicy(mintingPolicy)
  .complete();
```

## Read UTxOs and plutus scripts

Lucid allows you to conveniently read/reference UTxOs. If a plutus script is already stored in the UTxO, there is no need to attach the same script explicitly in the transaction, resulting in cost savings.

```js
const tx = await lucid
  .newTx()
  .readFrom([scriptUtxo])
  .complete();
```

[Tx API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=Tx)\
[TxComplete API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=TxComplete)\
[TxSigned API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=TxSigned)\
[Utils API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=Utils)