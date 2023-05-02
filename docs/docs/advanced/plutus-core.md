---
title: Plutus core
description: Selection of Plutus Core engine.
order: 1
---

Lucid has a built-in Plutus core engine, powered by [Aiken](https://github.com/aiken-lang/aiken). However, if you're using the [Blockfrost](https://blockfrost.io/) provider, you have the option to use the Haskell Plutus core engine instead. To do so, simply set the nativeUplc parameter to false:

```js
const tx = await lucid.newTx()
  .collectFrom([scriptUtxo], Data.void())
  .complete({ nativeUplc: false });
```
