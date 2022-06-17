import { networkName, Transferer } from './transferer';
import { selectWalletProvider } from './wallets';

const API_KEY = ''; // Blockfrost API Key;

const handleAdaTransfer = async () => {
  const addr = (document.getElementById('addr-input') as HTMLInputElement)
    ?.value;
  const amt = (document.getElementById('snd-amt') as HTMLInputElement)?.value;
  const network = (document.getElementById('network') as HTMLInputElement)
    ?.value;
  const walletProvider = (document.getElementById(
    'wallet-provider'
  ) as HTMLInputElement)?.value;

  if (!addr) {
    window.alert('A valid Cardano Testnet address must be provided.');
    return;
  }

  if (!amt || isNaN(Number(amt))) {
    window.alert('A valid ADA amount must be provided');
    return;
  }

  try {
    const wProvider = await selectWalletProvider(walletProvider);
    const transferer = new Transferer(networkName(network), wProvider, API_KEY);
    console.log(`Sending ${amt} ADA to addr: ${addr}`);
    const txHash = await transferer.sendAda(addr, Number(amt));
    window.alert(`Transfer succesfully submitted! TxHash: ${txHash}`);
  } catch (err) {
    console.error('error sending tranfer', err);
    window.alert(`Unexpected error sending transfer: ${(err as any).message}`);
  }
};

// Register Events handlers
const btn = document.getElementById('snd-btn');
btn!.addEventListener('click', handleAdaTransfer);
