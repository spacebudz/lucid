---
title: Choose a wallet
description: Use different methods to select a wallet and query balances. 
order: 0
---

## Wallet selection

There are multiple methods to select a wallet in Lucid.

### Select wallet from private key

```js
import { Crypto } from "https://deno.land/x/lucid/mod.ts";

const privateKey = Crypto.generatePrivateKey(); // Bech32 encoded private key
lucid.selectWalletFromPrivateKey(privateKey);
```

### Select wallet from seed phrase

```js
import { Crypto } from "https://deno.land/x/lucid/mod.ts";

const seed = Crypto.generateSeed();
lucid.selectWalletFromSeed(seed);
```

### Select wallet from browser

The wallet type works obviously only in the browser. This method works for any
[CIP-0030](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030)
compliant wallet.

```js
const api = await window.cardano.nami.enable();
lucid.selectWalletFromApi(api);
```

### Select wallet from custom data

This wallet is only readable and so can only handle query requests. Signing operations
do not work because no private key was selected in this method.

```js
lucid.selectReadOnlyWallet({address: "addr_test...", utxos: [...]});
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