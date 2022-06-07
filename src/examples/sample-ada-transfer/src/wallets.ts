//@ts-ignore
import { WalletProvider } from 'lucid-cardano';

const supportedProviders: { [key: string]: boolean } = {
  nami: true,
  flint: true,
  yoroi: true,
};

export const selectWalletProvider = async (
  provider = 'nami'
): Promise<WalletProvider> => {
  if (!supportedProviders[provider]) {
    throw new Error(`Invalid Wallet Provider: ${provider}`);
  }

  const context = window as any;
  if (!context.cardano || !context.cardano[provider]) {
    throw new Error('cardano provider instance not found in context');
  }

  const walletApi = (await context.cardano[
    provider
  ].enable()) as WalletProvider;
  return walletApi;
};
