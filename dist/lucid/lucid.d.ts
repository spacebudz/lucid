import { Core } from '../core';
import { Utils } from '../utils';
import { Address, Datum, ExternalWallet, Network, PrivateKey, Provider, Slot, Transaction, TxHash, Unit, UTxO, Wallet } from '../types';
import { Tx } from './tx';
import { TxComplete } from './txComplete';
export declare class Lucid {
    txBuilderConfig: Core.TransactionBuilderConfig;
    wallet: Wallet;
    provider: Provider;
    network: Network;
    utils: Utils;
    static new(provider?: Provider, network?: Network): Promise<Lucid>;
    newTx(): Tx;
    fromTx(tx: Transaction): TxComplete;
    currentSlot(): Promise<Slot>;
    utxosAt(address: Address): Promise<UTxO[]>;
    utxosAtWithUnit(address: Address, unit: Unit): Promise<UTxO[]>;
    awaitTx(txHash: TxHash): Promise<boolean>;
    datumOf(utxo: UTxO): Promise<Datum>;
    /**
     * Cardano Private key in bech32; not the BIP32 private key or any key that is not fully derived
     */
    selectWalletFromPrivateKey(privateKey: PrivateKey): this;
    selectWallet(api: WalletApi): this;
    /**
     * Emulates a CIP30 wallet by constructing it
     * with the UTxOs, collateral and addresses.
     *
     * If utxos are not set, utxos are fetched from the provided address
     */
    selectWalletFrom({ address, utxos, collateral, rewardAddress, }: ExternalWallet): this;
}
