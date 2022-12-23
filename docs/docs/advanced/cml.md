---
title: CML
description: Opt out and use the CML
order: 0
---

Lucid is built on top of the [cardano-multiplatform-lib](https://github.com/dcSpark/cardano-multiplatform-lib), but uses a custom fork, which can be found [here](https://github.com/berry-pool/cardano-multiplatform-lib).

If Lucid doesn't provide an API for your specific needs you could opt out and make use of the CML. Additionally you can import the type definitions:

```ts
import { C, Core } from "https://deno.land/x/lucid/mod.ts";

const output: Core.TransactionOutput = C.TransactionOutput.new(
  C.Address.from_bech32("..."),
  C.Value.zero(),
);
```