---
title: Instantiate Lucid
description: Instantiate Lucid with a provider
order: 0
---

Lucid can be instantiated with a blockchain provider or without. Usually you
want to select a provider in order to query data and submit transactions.
Additionally you want to select a network. Lucid supports the `Mainnet`,
`Preprod` and `Preview` networks. If no network is selected `Mainnet` is chosen
by default. Throughout the entire docs we are making use of the `Preprod`
network.

```js
import { Blockfrost, Lucid } from "https://deno.land/x/lucid/mod.ts";

const lucid = await Lucid.new(
  new Blockfrost("https://cardano-preprod.blockfrost.io/api/v0", "<projectId>"),
  "Preprod",
);
```

Lucid allows you to choose different providers.
[Learn more](../components/provider.md) about how you can implement your own
provider.
