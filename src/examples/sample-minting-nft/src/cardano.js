import {
  Lucid,
  Blockfrost,
  Data,
  toHex,
} from "lucid-cardano";


const blockfrostAPIKey = 'put_your_key_here';
const lucid = await
    Lucid.new(
        new Blockfrost(
            'https://cardano-testnet.blockfrost.io/api/v0',
            blockfrostAPIKey),
        'Testnet');
const api = await window.cardano.nami.enable();
// Assumes you are in a browser environment
lucid.selectWallet(api);

const Redeemer = (number) => Data.to(number);

export const getWalletUtxos = async () => {
    const utxos = await lucid.wallet.getUtxos();
    const walletUtxos = [];
    utxos.forEach(utxo => {
        walletUtxos.push(utxo.txHash+'#'+utxo.outputIndex)
    })
    return walletUtxos;
}

export const mintNFT = async (tokenName, plutusScript, metaData) => {
    const mintingScript = {
        type: "PlutusV1",
        tokenName: tokenName,
        script: plutusScript
    };

    const token = {
        policyId: lucid.utils.validatorToScriptHash(mintingScript),
        assetName: mintingScript.tokenName,
    }

    const assets = {
        // ignore BigInt warning
        // eslint-disable-next-line
        [token.policyId + toHex(token.assetName)] : BigInt(1)
    }

    const tx = await lucid
        .newTx()
        .attachMintingPolicy(mintingScript)
        .mintAssets(assets, Redeemer(1))
        .attachMetadata("1", metaData)
        .complete();
    console.log("tx mintAssets", tx);

    const signedTx = await tx.sign().complete();
    console.log("signedTx", signedTx);

    const txHash = await signedTx.submit();
    console.log("txHash", txHash);

    return txHash;
}
