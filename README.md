<p align="center">
  <img width="100px" src="./logo/lucid.svg" align="center"/>
  <h1 align="center">Lucid</h1>
  <p align="center">Lucid is a library designed to simplify creating Cardano transactions and writing off-chain code for Plutus contracts.</p>

<p align="center">
    <a href="https://www.npmjs.com/package/lucid-cardano">
      <img src="https://img.shields.io/npm/v/lucid-cardano?style=for-the-badge" />
    </a>
     <a href="https://doc.deno.land/https://deno.land/x/lucid/mod.ts">
      <img src="https://img.shields.io/readthedocs/cardano-lucid?style=for-the-badge" />
    </a>
    <a href="https://www.npmjs.com/package/lucid-cardano">
      <img src="https://img.shields.io/npm/dw/lucid-cardano?style=for-the-badge" />
    </a>
    <img src="https://img.shields.io/npm/l/lucid-cardano?style=for-the-badge" />
    <a href="https://twitter.com/spacebudznft">
      <img src="https://img.shields.io/twitter/follow/spacebudznft?style=for-the-badge&logo=twitter" />
    </a>
  </p>

</p>

### Get started

#### Deno 🦕

For JavaScript and TypeScript

```js
import { Lucid } from "https://deno.land/x/lucid/mod.ts";
```

#### NPM

```
npm install lucid-cardano
```

`--experimental-wasm-modules` flag needs to be set in Node.js as well as `{ "type": "module" }` in package.json

### Build from source

Build for NPM

```
deno task build
```

Outputs a `dist` folder

### Examples

- [Basic examples](./examples/)

### Basic usage

```js
import { Blockfrost, Lucid } from "https://deno.land/x/lucid/mod.ts";

const lucid = new Lucid({
  provider: new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    "<projectId>",
  ),
});

// Assumes you are in a browser environment
const api = await window.cardano.nami.enable();
lucid.selectWalletFromApi(api);

const tx = await lucid.newTx()
  .payTo("addr...", { lovelace: 5000000n })
  .commit();

const signedTx = await tx.sign().commit();

const txHash = await signedTx.submit();

console.log(txHash);
```

### Blueprint

Lucid supports [CIP-0057](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0057) blueprints.

Run the following command in a directory with a `plutus.json` file.

```
deno run -A https://deno.land/x/lucid/blueprint.ts
```

You can import the example Aiken validator...

```
pub type MyData {
  a: Int,
  b: ByteArray,
}

validator validate(my_data: MyData) {
  mint(redeemer: MyData, _policy_id: PolicyId, _transaction: Transaction) {
    my_data == redeemer
  } 
}
```

...into Lucid using the generated `plutus.ts` file created with the blueprint command, as shown below:

```ts
import { ValidateMint } from "./plutus.ts";

const validator = new ValidateMint({ a: 123n, b: "0000" });

const policyId = lucid.newScript(validator).toHash();

const tx = await lucid
  .newTx()
  .mint(
    { [policyId]: 1n },
    Data.to({ a: 123n, b: "0000" }, ValidateMint.redeemer),
  )
  .attachScript(validator)
  .commit();
```

See [more examples](./tests/data.test.ts)

### Instructions

Lucid transactions can be converted into instructions, which are JSON, making them highly portable.

```js
const instructions = await lucid.newTx()
  .delegateTo("{{own}}", "pool...")
  .payTo("addr_test1...", { lovelace: 1000000n })
  .toInstructions();
```

The above transaction can be converted into the following object:

```js
[
  {
    "type": "DelegateTo",
    "delegation": {
      "rewardAddress": "stake...",
      "poolId": "pool..."
    },
    "redeemer": undefined
  },
  {
    "type": "PayTo",
    "address": "addr_test1...",
    "assets": { "lovelace": 1000000n }
  }
]
```

Consume the instructions in Lucid:

```js
const tx = await lucid.fromInstructions([
  {
    "type": "DelegateTo",
    "delegation": {
      "rewardAddress": "stake...",
      "poolId": "pool...",
    },
    "redeemer": undefined,
  },
  {
    "type": "PayTo",
    "address": "addr_test1...",
    "assets": { "lovelace": 1000000n },
  },
]);
```

You can avoid address resolution if you use `.toPartialInstructions()` instead of `.toInstructions()`.\
Then Lucid will resolve `{{own}}` fields with the addresses of the selected wallet.

### Test

```
deno task test
```

### Build Core

The [core library](./src/core/libs/lucid_core/) (instruction builder, crypto, hashing etc.) is written in Rust and compiled to WASM.

```
deno task build:core
```

### Test Core

```
deno task test:core
```

### Docs

[View docs](https://doc.deno.land/https://deno.land/x/lucid/mod.ts) 📖

You can generate documentation with:

```
deno doc
```

### Compatibility

Lucid is an ES Module, and to use it in the browser, a bundler that supports top-level await and WebAssembly is recommended. If you use Webpack 5 enable in
the `webpack.config.js`:

```
experiments: {
    asyncWebAssembly: true,
  }
```

### Contributing

Contributions and PRs are welcome\
The [contribution instructions](./CONTRIBUTING.md).

Join us on [Discord](https://discord.gg/82MWs63Tdm)!
