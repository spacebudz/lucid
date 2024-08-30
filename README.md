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
    <a href="https://twitter.com/spacebudzNFT">
      <img src="https://img.shields.io/twitter/follow/spacebudzNFT?style=for-the-badge&logo=twitter" />
    </a>
  </p>

</p>

### Get started

(Note: Lucid version *0.10.8* works **only after Chang HFC (1. Sept)**)

#### NPM

```
npm install lucid-cardano
```

#### Deno 🦕

For JavaScript and TypeScript

```js
import { Lucid } from "https://deno.land/x/lucid@0.10.8/mod.ts";
```

#### Web

```html
<script type="module">
import { Lucid } from "https://unpkg.com/lucid-cardano@0.10.8/web/mod.js"
// ...
</script>
```

### 

### Build from source

Build NPM and Web target

```
deno task build
```

Outputs a `dist` folder

### Examples

- [Basic examples](./src/examples/)
- [Next.js Blockfrost Proxy API Example](https://github.com/GGAlanSmithee/cardano-lucid-blockfrost-proxy-example)

### Basic usage

```js
// import { Blockfrost, Lucid } from "https://deno.land/x/lucid@0.10.8/mod.ts"; Deno
import { Blockfrost, Lucid } from "lucid-cardano"; // NPM

const lucid = await Lucid.new(
  new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", "<projectId>"),
  "Preview",
);

// Assumes you are in a browser environment
const api = await window.cardano.nami.enable();
lucid.selectWallet(api);

const tx = await lucid.newTx()
  .payToAddress("addr...", { lovelace: 5000000n })
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();

console.log(txHash);
```

### Test

```
deno task test
```

### Build Core

This library is built on top of a customized version of the serialization-lib
(cardano-multiplatform-lib) and on top of the message-signing library, which are
written in Rust.

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

To run the library in Node.js you need to set `{"type" : "module"}` in your
project's `package.json`. Otherwise you will get import issues.

### Contributing

Contributions and PRs are welcome!\
The [contribution instructions](./CONTRIBUTING.md).

Join us on [Discord](https://discord.gg/82MWs63Tdm)!

### Use Lucid with React

[use-cardano](https://use-cardano.alangaming.com/) a React context, hook and set
of components built on top of Lucid.

### Use Lucid with Next.js

[Cardano Starter Kit](https://cardano-starter-kit.alangaming.com/) a Next.js
starter kit for building Cardano dApps.
