---
title: Choose a provider
description: Select a provider and query data from the blockchain. 
order: 1
---

## Provider selection

There are multiple builtin providers you can choose from in Lucid.

### Blockfrost

```js
import { Blockfrost, Lucid } from "https://deno.land/x/lucid/mod.ts";

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    "<project_id>",
  ),
  "Preprod",
);
```

### Kupmios

Kupmios is a mix of [Ogmios](https://ogmios.dev/) and
[Kupo](https://cardanosolutions.github.io/kupo/).

```js
import { Kupmios, Lucid } from "https://deno.land/x/lucid/mod.ts";

const lucid = await Lucid.new(
  new Kupmios(
    "http://localhost:1442",
    "ws://localhost:1337",
  ),
  "Preprod",
);
```

### Maestro

```js
import { Lucid, Maestro } from "https://deno.land/x/lucid/mod.ts";

const lucid = await Lucid.new(
  new Maestro({
    network: "Preprod", // For MAINNET: "Mainnet".
    apiKey: "<Your-API-Key>", // Get yours by visiting https://docs.gomaestro.org/docs/Getting-started/Sign-up-login.
    turboSubmit: false, // Read about paid turbo transaction submission feature at https://docs.gomaestro.org/docs/Dapp%20Platform/Turbo%20Transaction.
  }),
  "Preprod", // For MAINNET: "Mainnet".
);
```

### Custom

Lucid may add more providers in the future, but you also have the option to
create your own custom provider that meets your specific requirements. Simply
follow the
[provider interface](https://deno.land/x/lucid@0.10.1/mod.ts?s=Provider) to
implement your custom provider.

```js
import { Lucid } from "https://deno.land/x/lucid/mod.ts"

class MyProvider { ... }

const lucid = await Lucid.new(
  new MyProvider(), "Preprod"
);
```

## Query provider

### Query UTxOs

```js
const utxos = await lucid.provider.getUtxos("addr_test...");
```

For convenience you can also query utxos like this:

```js
const utxos = await lucid.utxosAt("addr_test...");
```

### Query datums

```js
const datum = await lucid.provider.getDatum("<datum_hash>");
```

For convenience you can also query datums directly from utxos. When you query
the datum for a UTxO, Lucid automatically adds the datum to the UTxO. This means
that subsequent queries for the same UTxO will return the result instantly,
without the need for an additional network request.

```js
const [scriptUtxo] = await lucid.utxosAt("addr_test...");
const datum = await lucid.datumOf(scriptUtxo);
```

### Query protocol parameters

```js
const protocolParameters = await lucid.provider.getProtocolParameters();
```

[Provider API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=Provider)\
[Lucid API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=Lucid)
