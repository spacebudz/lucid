---
title: Type casting
description: Cast plutus data to custom data types.
order: 1
---

Type casting is currently an experimental feature. To convert Plutus data to JavaScript data structures, you'll need information about the type's shape, which is stored in a JSON schema. To make this process more convenient, we use a tool called [Typebox](https://github.com/sinclairzx81/typebox).

```ts
import { Data } from "https://deno.land/x/lucid/mod.ts";

const Listing = Data.Object({
  owner: Data.Bytes(),
  amount: Data.Integer(),
  private: Data.Boolean(),
});
```

Furthermore, we can generate a TypeScript type definition from the data structure:

```ts
type Listing = Data.Static<typeof Listing>;
```

Cast to plutus data:

```ts
const listing = Data.to<Listing>(
  { owner: "31313131313131", amount: 5252352323n, private: false },
  Listing,
);
```

Cast from plutus data:

```ts
const listing: Listing = Data.from<Listing>(
  "d8799f47313131313131311b0000000139108943d87980ff",
  Listing,
);
```
