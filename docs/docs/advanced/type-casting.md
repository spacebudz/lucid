---
title: Type casting
description: Cast plutus data to custom data types
order: 1
---

Type casting is an experimental feature yet.\
In order to cast plutus data to JavaScript data structures you need some
information about the shape of the type. These are stored in a JSON schema. To
make this convenient to use we use
[typebox](https://github.com/sinclairzx81/typebox).

Create a data structure:

```ts
import { Data } from "https://deno.land/x/lucid/mod.ts";

const Listing = Data.Object({
  owner: Data.Bytes(),
  amount: Data.Integer(),
  private: Data.Boolean(),
});
```

Additionally we can derive a TypeScript type definition from the data structure:

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
