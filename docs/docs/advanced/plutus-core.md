---
title: Plutus Core
description: Selection of Plutus Core engine
order: 1
---

Lucid has its own native Plutus core engine thanks to
[Aiken](https://github.com/aiken-lang/aiken). If you use the
[Blockfrost](https://blockfrost.io/) provider you could use the Haskell Plutus
core engine. Simply set `nativeUplc` to false:

```js
const tx = await lucid.newTx()
  .collectFrom([scriptUtxo], Data.void())
  .complete({ nativeUplc: false });
```
