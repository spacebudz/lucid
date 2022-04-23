# Lucid


<!-- [![GitHub Release Date](https://img.shields.io/github/release-date/berry-pool/lucid?style=plastic)](https://github.com/Berry-Pool/lucid) -->

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/berry-pool/lucid?style=plastic)

[![Read the Docs](https://img.shields.io/readthedocs/cardano-lucid?style=plastic)](https://berry-pool.github.io/lucid/)

[![Twitter Follow](https://img.shields.io/twitter/follow/berry_ales?style=social)](https://twitter.com/berry_ales)

[![Discord](https://img.shields.io/discord/929963273741295696.svg?style=plastic&label=&logo=discord&logoColor=ffffff&color=7389D8&labelColor=6A7EC2)](https://discord.com/invite/sKqxrPb5fd)

[![npm version](https://img.shields.io/npm/v/lucid-cardano?style=plastic)](https://www.npmjs.com/package/lucid-cardano)

[![npm downloads](https://img.shields.io/npm/dw/lucid-cardano?style=plastic)](https://www.npmjs.com/package/lucid-cardano)
<!-- [![npm peer dependency version](https://img.shields.io/npm/dependency-version/cardano-lsd/peer/gsap?style=plastic)](https://greensock.com/gsap/) -->
[![NPM](https://img.shields.io/npm/l/lucid-cardano?style=plastic)](https://github.com/Berry-Pool/lucid)


Lucid is a library, which allows you to create Cardano transactions and off-chain code for your Plutus contracts in JavaScript and Node.js.

This library is **experimental**. Expect bugs. It's still work in progress.

### Table of contents

- [Features](#features)
- [Installation](#installation)
- [Examples](#examples)
- [Docs](#docs)
- [Preview](#preview)
- [Compatibilty](#compatibilty)

### Features

- [ ] To be added
- [ ] To be added


### Installation
```
npm install lucid-cardano
```
### Examples

You can check out the [examples](./src/examples/) folder.

#### Preview

```js
await Lucid.initialize(
  'Testnet',
  new Blockfrost('https://cardano-testnet.blockfrost.io/api/v0', '<projectId>'),
);

// Assumes you are in a browser environment
await Lucid.selectWallet('nami');

const tx = await Tx.new()
    .payToAddress("addr...", {lovelace: 5000000n})
    .complete();

const signedTx = (await tx.sign()).complete();

const txHash = await signedTx.submit();

console.log(txHash);
```
### Docs

You can generate documentation with:
```
npm run docs
```
It'll be located under `/docs`.

### Compatibilty

To run it in the browser Webpack 5 is recommended or any other bundler which allows for top level await and WebAssembly. When you use Webpack 5 enable in the `webpack.config.js`:
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

Documentation: https://cardano-lucid.readthedocs.io/en/latest

Link: https://github.com/Berry-Pool/cardano-multiplatform-lib/tree/plutus

Branch: Plutus

Commit hash: 5f41e3c1e30a44579ffeef27f497fe7a7334846f