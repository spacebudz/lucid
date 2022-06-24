import { Lucid, Network, WalletProvider } from 'lucid-cardano';
export declare const NetworkTestnet: any;
export declare const NetworkMainnet: any;
export declare class Transferer {
    walletProvider: WalletProvider;
    clientInstance: Lucid;
    network: Network;
    blockfrostKey: string;
    constructor(network: Network, walletProvider: WalletProvider, blockfrostKey: string);
    client(): Promise<Lucid>;
    sendAda(addr: string, amt: number): Promise<any>;
}
export declare const networkName: (key: string) => any;
