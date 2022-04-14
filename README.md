# Lucid

Lucid is a library, which allows you to create Cardano transactions and off-chain code for your Plutus contracts in JavaScript and Node.js.

This library is **experimental**. Expect bugs. It's still work in progress.

### Getting started

You can check out the [Docs](./docs/index.html) and the [exampes](./src/examples/) folder.

### Preview

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


### NPM package

Coming soon

### Compatibilty

To run it in the browser Webpack 5 is recommended or any other bundler which allows for top level await and WebAssembly. When you use Webpack 5 enable in the `webpack.config.js`:
```
experiments: {
    asyncWebAssembly: true,
    topLevelAwait: true,
  }
```

Make sure in your `package.json` you have set `{"type" : "module"}`. Otherwise you will get import issues.

To run the library flawlessly in Node.js you need to set the flag `--es-module-specifier-resolution=node` behind `node`.

This library is built on top of a customized version of the serialization-lib (cardano-multiplatform-lib).
Link: https://github.com/Berry-Pool/cardano-multiplatform-lib/tree/plutus
Branch: Plutus
Commit hash: 9e739b6d9fc3978644b9bdda13de0c81d50e8949