<p align="center">
  <img width="100px" src="./logo/lucid.svg" align="center"/>
  <h1 align="center">Lucid</h1>
  <p align="center">Lucid is a library, which allows you to create Cardano transactions and off-chain code for your Plutus contracts in JavaScript, Deno and Node.js.</p>

<p align="center">
    <img src="https://img.shields.io/github/commit-activity/m/berry-pool/lucid?style=for-the-badge" />
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

Lucid is a Deno first TypeScript framework. Node.js and NPM are still supported, but may be deprecated in the future.

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

```
deno run -A https://deno.land/x/lucid/blueprint.ts
```

```js

```

See [more examples](./tests/data.test.ts)

### Test

```
deno task test
```

### Build Core

The core library (instruction builder, crypto, hashing etc.) is written in Rust and compiled to WASM.

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

Lucid is an ES Module, so to run it in the browser any bundler which allows for
top level await and WebAssembly is recommended. If you use Webpack 5 enable in
the `webpack.config.js`:

```
experiments: {
    asyncWebAssembly: true,
    topLevelAwait: true,
    layers: true // optional, with some bundlers/frameworks it doesn't work without
  }
```

### Contributing

Contributions and PRs are welcome\
The [contribution instructions](./CONTRIBUTING.md).

Join us on [Discord](https://discord.gg/82MWs63Tdm)!
