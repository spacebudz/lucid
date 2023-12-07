import { C } from "../core/mod.js";
import { Utils } from "../utils/mod.js";
import { Address, Credential, Delegation, ExternalWallet, Json, Network, OutRef, Payload, PrivateKey, ProtocolParameters, Provider, RewardAddress, SignedMessage, Slot, Transaction, TxHash, Unit, UTxO, Wallet, WalletApi } from "../types/mod.js";
import { Tx } from "./tx.js";
import { TxComplete } from "./tx_complete.js";
import { Message } from "./message.js";
import { Data } from "../plutus/data.js";
type LucidConstructorArgs = {
    provider: Provider | undefined;
    network?: Network;
    protocolParameters?: ProtocolParameters;
};
export declare class Lucid {
    protocolParameters: ProtocolParameters;
    txBuilderConfig: C.TransactionBuilderConfig;
    wallet: Wallet;
    provider: Provider;
    network: Network;
    utils: Utils;
    static new({ provider, network, protocolParameters, }: LucidConstructorArgs): Promise<Lucid>;
    /**
     * Switch provider and/or network.
     * If provider or network unset, no overwriting happens. Provider or network from current instance are taken then.
     */
    switchProvider(provider?: Provider | undefined, network?: Network): Promise<Lucid>;
    newTx(): Tx;
    fromTx(tx: Transaction): TxComplete;
    /** Signs a message. Expects the payload to be Hex encoded. */
    newMessage(address: Address | RewardAddress, payload: Payload): Message;
    /** Verify a message. Expects the payload to be Hex encoded. */
    verifyMessage(address: Address | RewardAddress, payload: Payload, signedMessage: SignedMessage): boolean;
    currentSlot(): Slot;
    utxosAt(addressOrCredential: Address | Credential): Promise<UTxO[]>;
    utxosAtWithUnit(addressOrCredential: Address | Credential, unit: Unit): Promise<UTxO[]>;
    /** Unit needs to be an NFT (or optionally the entire supply in one UTxO). */
    utxoByUnit(unit: Unit): Promise<UTxO>;
    utxosByOutRef(outRefs: Array<OutRef>): Promise<UTxO[]>;
    delegationAt(rewardAddress: RewardAddress): Promise<Delegation>;
    awaitTx(txHash: TxHash, checkInterval?: number): Promise<boolean>;
    datumOf<T = Data>(utxo: UTxO, type?: T): Promise<T>;
    /** Query CIP-0068 metadata for a specifc asset. */
    metadataOf<T = Json>(unit: Unit): Promise<T>;
    /**
     * Cardano Private key in bech32; not the BIP32 private key or any key that is not fully derived.
     * Only an Enteprise address (without stake credential) is derived.
     */
    selectWalletFromPrivateKey(privateKey: PrivateKey): Lucid;
    selectWallet(api: WalletApi): Lucid;
    /**
     * Emulates a wallet by constructing it with the utxos and an address.
     * If utxos are not set, utxos are fetched from the provided address.
     */
    selectWalletFrom({ address, utxos, rewardAddress, collateral, }: ExternalWallet): Lucid;
    /**
     * Select wallet from a seed phrase (e.g. 15 or 24 words). You have the option to choose between a Base address (with stake credential)
     * and Enterprise address (without stake credential). You can also decide which account index to derive. By default account 0 is derived.
     */
    selectWalletFromSeed(seed: string, options?: {
        addressType?: "Base" | "Enterprise";
        accountIndex?: number;
        password?: string;
    }): Lucid;
}
export {};
