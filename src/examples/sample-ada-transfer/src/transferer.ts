//@ts-ignore
import { Lucid, Blockfrost, Network, WalletProvider } from 'lucid-cardano';

export const NetworkTestnet = 'Testnet' as Network;
export const NetworkMainnet = 'Mainnet' as Network;

const blockfrostUrls: { [key: string]: string } = {
  [NetworkTestnet]: 'https://cardano-testnet.blockfrost.io/api/v0',
  [NetworkMainnet]: 'https://cardano.blockfrost.io/api/v0',
};

export class Transferer {
  walletProvider: WalletProvider;
  clientInstance: Lucid;
  network: Network;
  blockfrostKey: string;

  constructor(
    network: Network,
    walletProvider: WalletProvider,
    blockfrostKey: string
  ) {
    if (!blockfrostKey) {
      console.error(
        'Blockfrost API Key not provided. A valid key must be provided in source index'
      );
      throw new Error('Invalid API Key');
    }
    this.walletProvider = walletProvider;
    this.network = network;
    this.blockfrostKey = blockfrostKey;
  }

  async client(): Promise<Lucid> {
    if (!this.clientInstance) {
      const bfAPI = new Blockfrost(
        blockfrostUrls[this.network],
        this.blockfrostKey
      );
      const lucidClient = await Lucid.new(bfAPI, this.network);
      this.clientInstance = await lucidClient.selectWallet(this.walletProvider);
    }
    return this.clientInstance;
  }

  async sendAda(addr: string, amt: number) {
    if (!addr) {
      throw new Error('Invalid Addr. Cannot be empty');
    }

    if (!amt) {
      throw new Error('Invalid ADA Amount. Cannot be empty');
    }
    console.log(`Sending ADA to: ${addr}`);
    const client = await this.client();
    const lovelaceAmount = BigInt(Number(amt) * 1000000);
    const tx = await client
      .newTx()
      .payToAddress(addr, { lovelace: lovelaceAmount })
      .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    console.log('txHash:', txHash);
    return txHash;
  }
}

export const networkName = (key: string): Network => {
  if (!key) {
    throw new Error('Invalid network key');
  }

  switch (key.toLowerCase()) {
    case 'testnet':
      return NetworkTestnet;
    case 'mainnet':
      return NetworkMainnet;
    default:
      throw new Error(`Network not supported: ${key}`);
  }
};
