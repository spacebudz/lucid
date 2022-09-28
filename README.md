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

#### NPM

```
npm install lucid-cardano
```

#### Deno 🦕

For JavaScript and TypeScript

```js
import { Lucid } from "https://deno.land/x/lucid@0.6.6/mod.ts";
```

#### Web

```html
<script type="module">
import { Lucid } from "https://unpkg.com/lucid-cardano@0.6.6/web/mod.js"
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

[View examples](./src/examples/)

### Basic usage

```js
// import { Blockfrost, Lucid } from "https://deno.land/x/lucid@0.6.6/mod.ts"; Deno
import { Blockfrost, Lucid } from "lucid-cardano"; // NPM

const lucid = await Lucid.new(
  new Blockfrost("https://cardano-testnet.blockfrost.io/api/v0", "<projectId>"),
  "Testnet",
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

<br />
This library is built on top of a customized version of the serialization-lib (cardano-multiplatform-lib) and on top of the message-signing library.

#### cardano-multiplatform-lib

Link: https://github.com/Berry-Pool/cardano-multiplatform-lib/tree/vasil

Branch: **vasil**

Commit hash: **85f4dc2ebb3db67347e1762cf995625208a93feb**

#### message-signing

Link: https://github.com/Emurgo/message-signing

Branch: **master**

Commit hash: **16dcadc69557dd7c20e62a966aaded1e051c287e**

### Contributing

Contributions and PRs are welcome!\
The [contribution instructions](./CONTRIBUTING.md).

Join us on
[Discord](https://discord.com/channels/824711383185621082/1019230128275988510)!
