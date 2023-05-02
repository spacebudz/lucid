---
title: Choose a wallet
description: Use different methods to select a wallet and query balances. 
order: 0
---

## Wallet selection

There are multiple methods to select a wallet in Lucid.

### Select wallet from private key

```js
const privateKey = lucid.utils.generatePrivateKey(); // Bech32 encoded private key
lucid.selectWalletFromPrivateKey(privateKey);
```

### Select wallet from seed phrase

```js
const seed = lucid.utils.generateSeedPhrase();
lucid.selectWalletFromSeed(seed);
```

### Select wallet from browser

The wallet type works obviously only in the browser. This method works for any [CIP-0030](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030) compliant wallet.

```js
const api = await window.cardano.nami.enable();
lucid.selectWallet(api);
```

### Select wallet from custom data

This wallet is viewable and can only handle query requests. Signing operations do not work because no private key was selected via this method.

```js
lucid.selectWalletFrom({address: "addr_test...", utxos: [...]});
```

## Query wallet

### Get address

```js
const address = await lucid.wallet.address(); // Bech32 address
```

### Query UTxOs

```js
const utxos = await lucid.wallet.getUtxos();
```

### Query delegation

```js
const delegation = await lucid.wallet.getDelegation();
```

[Wallet API reference](https://deno.land/x/lucid@0.10.1/mod.ts?s=Wallet)