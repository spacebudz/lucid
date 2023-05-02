---
title: Create a wallet
description: How to create a wallet and select it.
order: 1
---

There are multiple options to create and import a wallet in Lucid. In this
section we are using the private key method.

In case you do not have private key yet, you can generate one with Lucid:

```js
const privateKey = lucid.utils.generatePrivateKey(); // Bech32 encoded private key
console.log(privateKey);
```

Now we select a private key wallet with our Lucid instance:

```js
lucid.selectWalletFromPrivateKey(privateKey);
```

<div style="padding: 14px 20px; border-radius: 6px; border: solid 1px deepskyblue">
<b>Note:</b> In almost all cases you want to select a wallet. It's necessary to
build and submit transactions.
</div>
