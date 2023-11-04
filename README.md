<p align="center">
  <h1 align="center">Jucid</h1>
  <p align="center">Jucid a fork of Lucid, which allows you to create Cardano transactions and off-chain code for your Plutus contracts in JavaScript, Deno and Node.js without worrying about memory leaks.</p>

<p align="center">
    <img src="https://img.shields.io/github/commit-activity/m/yHSJ/jucid?style=for-the-badge" />
    <a href="https://www.npmjs.com/package/jucid-cardano">
      <img src="https://img.shields.io/npm/v/jucid-cardano?style=for-the-badge" />
    </a>
    </a>
    <a href="https://www.npmjs.com/package/jucid-cardano">
      <img src="https://img.shields.io/npm/dw/jucid-cardano?style=for-the-badge" />
    </a>
    <img src="https://img.shields.io/npm/l/jucid-cardano?style=for-the-badge" />
    <a href="https://twitter.com/JSHyCS">
      <img src="https://img.shields.io/twitter/follow/JSHyCS?style=for-the-badge&logo=twitter" />
    </a>
  </p>

</p>

### Lucid Fork

Jucid is JSHy's fork of Lucid (Jshy + Lucid = Jucid). Lucid accidentally introduces memory leaks into applications due to incorrect memory management of WASM objects. Jucid handles internal memory correctly, exposes new interfaces to allow you to management objects you use correclty, and is a drop-in replacement for Lucid. Once this fork is merged into Lucid, it is unlikely it will be maintained.

### Get started

#### NPM

```
npm install jucid-cardano
```

#### Deno 🦕

For JavaScript and TypeScript

```js
import { jucid } from "https://deno.land/x/jucid@1.0.0-alpha.1/mod.ts";
```

#### Web

```html
<script type="module">
  import { Lucid } from "https://unpkg.com/jucid@1.0.0-alpha.1/web/mod.js";
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

- Coming Soon

### Basic usage

```js
// import { Blockfrost, Lucid } from "https://deno.land/x/jucid@1.0.0-alpha.1/mod.ts"; Deno
import { Blockfrost, Lucid } from "jucid-cardano"; // NPM

const lucid = await Lucid.new(
  new Blockfrost("https://cardano-preview.blockfrost.io/api/v0", "<projectId>"),
  "Preview"
);

// Assumes you are in a browser environment
const api = await window.cardano.nami.enable();
lucid.selectWallet(api);

const tx = lucid.newTx().payToAddress("addr...", { lovelace: 5000000n });
const completeTx = await tx.complete();
const signedTx = await completeTx.sign();

const txHash = await signedTx.submit();
Freeables.free(tx, completeTx, signedTx);

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

[View docs](https://doc.deno.land/https://deno.land/x/jucid/mod.ts) 📖

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
