# Sample ADA Transfer App

This is a simple ADA transfer app to demonstrate the integration and usage of the Cardano [Lucid.js](https://github.com/Berry-Pool/lucid) library with [Blockfrost API](https://blockfrost.io/dashboard).

**Note: only use for educational purposes**

## Run locally

1) Install dependecies 
```sh
npm install 
```

2) Start app server

Make sure to set your Blockfrost API Key in the **src/index.ts** before starting server.

```
npm start
```

`This will build the app and start http server.`

3) Open app at `http://localhost:8080` in your browser.

Make sure to have at least one of the support wallet providers installed and setup in your browser.

- [Nami](https://namiwallet.io/) (default)
- [Flint](https://flint-wallet.com/)
- Or [Yoroi](https://yoroi-wallet.com/#/)
