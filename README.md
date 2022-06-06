<p align="center">
  <img width="100px" src="./logo/lucid.svg" align="center"/>
  <h1 align="center">Lucid</h1>
  <p align="center">Lucid is a library, which allows you to create Cardano transactions and off-chain code for your Plutus contracts in JavaScript and Node.js.</p>

  <p align="center">
    <img src="https://img.shields.io/github/commit-activity/m/berry-pool/lucid?style=for-the-badge" />
     <a href="https://berry-pool.github.io/lucid/">
      <img src="https://img.shields.io/readthedocs/cardano-lucid?style=for-the-badge" />
    </a>
    <a href="https://www.npmjs.com/package/lucid-cardano">
      <img src="https://img.shields.io/npm/v/lucid-cardano?style=for-the-badge" />
    </a>
    <a href="https://www.npmjs.com/package/lucid-cardano">
      <img src="https://img.shields.io/npm/dw/lucid-cardano?style=for-the-badge" />
    </a>
    <img src="https://img.shields.io/npm/l/lucid-cardano?style=for-the-badge" />
    <a href="https://twitter.com/berry_ales">
      <img src="https://img.shields.io/twitter/follow/berry_ales?style=for-the-badge&logo=twitter" />
    </a>
  </p>

</p>


### Installation
```
npm install lucid-cardano
```

### From Source

Install dependencies
```
npm install
```

Generate build
```
npm run build
```

### Examples

You can check out the [examples](./src/examples/) folder.

### Basic usage

See [sample-ada-transfer](./src/examples/sample-ada-transfer) example for end-to-end browser integration usage. 

```js
import {Lucid, Blockfrost} from "lucid-cardano";

const lucid = await Lucid.new(
  new Blockfrost('https://cardano-testnet.blockfrost.io/api/v0', '<projectId>'),
  'Testnet'
);

// Assumes you are in a browser environment
const api = await window.cardano.nami.enable();
lucid.selectWallet(api);

const tx = await lucid.newTx()
    .payToAddress("addr...", {lovelace: 5000000n})
    .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();

console.log(txHash);
```

### Test

```
npm test
```

### Docs

You can generate documentation with:
```
npm run docs
```
It'll be located under `/docs`.

### Compatibilty

To run it in the browser Webpack 5 is recommended or any other bundler which allows for top level await and WebAssembly. If you use Webpack 5 enable in the `webpack.config.js`:
```
experiments: {
    asyncWebAssembly: true,
    topLevelAwait: true,
    layers: true // optional, with some bundlers/frameworks it doesn't work without
  }
```

To run the library in Node.js you need to set `{"type" : "module"}` in your project's `package.json`. Otherwise you will get import issues.

<br />
This library is built on top of a customized version of the serialization-lib (cardano-multiplatform-lib).

Documentation for cardano-multiplatform-lib: https://cardano-lucid.readthedocs.io/en/latest

Link: https://github.com/Berry-Pool/cardano-multiplatform-lib/tree/plutus

Branch: Plutus

Commit hash: 97e57ec28a0a1a48a5c1f71af9ed97b058091b74