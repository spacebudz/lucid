---
title: Instantiate Lucid
description: Instantiate Lucid with a provider.
order: 0
---

Lucid can be used with or without a blockchain provider, which allows you to
query data and submit transactions. It's recommended to select a provider and
network when using Lucid. Lucid currently supports the `Mainnet`, `Preprod`, and
`Preview` networks, with `Mainnet` being the default selection if no network is
specified. Throughout the entire docs we are making use of the `Preprod`
network.

```js
import { Blockfrost, Lucid } from "https://deno.land/x/lucid/mod.ts";

const lucid = new Lucid({
  provider: new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    "<projectId>",
  ),
});
```

Lucid allows you to choose different providers. You also have the option to
implement your own custom provider.
