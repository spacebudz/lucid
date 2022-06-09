<template>
  <div class="sample-integration">
    <h1>{{ msg }}</h1>
  </div>
  <div id="main">
    <div id="utxo">
        Choose one utxo#index:
        <select v-model="utxo">
          <option disabled value="">Please select one</option>
          <option v-for="item in walletUtxos" :key="item" :value="item">{{item}}</option>
        </select>
        <br/>
        <div>Selected utxo: {{ utxo }}</div>
    </div>
    <div id="token-name">
        Token Name:
        <input type="text" v-model="tokenName"/>
    </div>

    <div id="gen-cardano-cli">
      <button @click="genCardanoCli">Gen cli to gen plutus script</button>
      <br>
      <textarea rows="5" cols="100" v-model="cardanoCli" placeholder="-- nothing here yet!"></textarea>
      <p>Copy above cli to run with axu-nft to gen plutus script</p>
    </div>

    <div id="plutus-script">
        Plutus script: <br/>
        (copy cborHex from axu-nft repo) <br>
        cat script/nft-mint-policy.plutus | ack -o "(?&lt;=cborHex\":\s\").*(?=\")" | clipcopy
        <br/>
        <textarea rows="5" cols="100" v-model="plutusScript" placeholder="Paste cborHex of plutus script here"></textarea>
    </div>
    <div id="meta-data">
        meta Data: (json format)<br>
        <textarea rows="5" cols="100" v-model="metaDataText"></textarea>
        <br>
    </div>
    <button @click="mint">mintNFT</button>
    <div id="txhash">
        <p>successTxHash: {{successTxHash}}</p>
        <a
          v-if="successTxHash!=''"
          target="_blank"
          v-bind:href="'https://testnet.cardanoscan.io/transaction/'+successTxHash">
            Check on cardano scan(need to wait a bit for tnx appear on cardano explorer testnet)
        </a>
    </div>
  </div>
</template>

<script>
import {getWalletUtxos, mintNFT} from '../cardano.js'

export default {
  name: 'SampleIntegration',
  props: {
    msg: String
  },
  data () {
    return {
      tokenName: '',
      utxo: '',
      plutusScript: '',
      cardanoCli: '',
      metaDataText: '',
      walletUtxos: {0:'fetching ...!'},
      successTxHash: '',
    }
  },
  created () {
      this.genSampleMetadata();
      getWalletUtxos().then(result => this.walletUtxos = result);
  },
  methods: {
    genSampleMetadata () {
        const mD = {
            1: {
                name: "AXU NFT collection",
                description: "AXU NFT Collection",
                attributes: {
                        x: 100,
                        y: 100,
                        element: "wooden"
                    }
            }
        };
        this.metaDataText = JSON.stringify(mD)
    },
    genCardanoCli () {
        //  cabal run axu-nft -- $token_name $tx_out_ref
        this.cardanoCli = '';
        this.cardanoCli += 'export token_name="'+this.tokenName+'" &&\\\n';
        this.cardanoCli += 'export tx_out_ref="'+this.utxo+'" &&\\\n';
        this.cardanoCli += 'cabal run axu-nft -- $token_name $tx_out_ref';


    },
    mint() {
        console.log("minting");
        mintNFT(this.tokenName, this.plutusScript, JSON.parse(this.metaDataText)).then(
            txHash => {
                this.successTxHash = txHash;
                console.log("done");
            });
    }
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
h3 {
  margin: 40px 0 0;
}
ul {
  list-style-type: none;
  padding: 0;
}
li {
  display: inline-block;
  margin: 5px 10px;

}
a {
  color: #42b983;
}
</style>
