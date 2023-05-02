---
title: CML
description: Opt out and use the CML.
order: 0
---


Lucid is built on the [cardano-multiplatform-lib (CML)](https://github.com/dcSpark/cardano-multiplatform-lib), but utilizes a customized fork that can be accessed [here](https://github.com/spacebudz/lucid/tree/main/src/core/libs/cardano_multiplatform_lib). If Lucid doesn't offer an API that meets your specific needs, you have the option to use the CML directly:

```ts
import { C } from "https://deno.land/x/lucid/mod.ts";

const output: C.TransactionOutput = C.TransactionOutput.new(
  C.Address.from_bech32("..."),
  C.Value.zero(),
);
```
