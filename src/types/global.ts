// CIP-0030
export type WalletApi = {
  isEnabled: () => Promise<boolean>;
  getBalance: () => Promise<string>;
  getUtxos: (
    _amount?: string,
    _paginate?: { page: number; limit: number },
  ) => Promise<string[]>;
  getCollateral: () => Promise<string[] | null>;
  getUsedAddresses: () => Promise<string[]>;
  getUnusedAddresses: () => Promise<string[]>;
  getChangeAddress: () => Promise<string>;
  getRewardAddress: () => Promise<string>;
  getRewardAddresses: () => Promise<string[]>;
  getNetworkId: () => Promise<number>;
  signData(
    address: string,
    payload: string,
  ): Promise<{ signature: string; key: string }>;
  signTx: (_tx: string, _partialSign?: boolean) => Promise<string>;
  signTxs?: (txs: string[]) => Promise<string[]>;
  submitTx: (_cbor: string) => Promise<string>;
  experimental: any;
};

export type Cardano = Omit<WalletApi, "experimental"> & {
  onAccountChange?: (cb: (newAddresses?: string[]) => void) => void;
  onNetworkChange?: (cb: (newNetwork?: string) => void) => void;
} & {
  [key: string]: {
    name: string;
    icon: string;
    apiVersion: string;
    enable(): Promise<WalletApi>;
    isEnabled(): Promise<boolean>;
    onAccountChange?: (cb: (newAddresses?: string[]) => void) => void;
    onNetworkChange?: (cb: (newNetwork?: string) => void) => void;
    experimental: any;
  };
};

declare global {
  interface Window {
    cardano: Cardano;
  }
}
