import { Address, C, KeyHash, Network, PrivateKey, RewardAddress, UTxO } from "../mod.js";
type FromSeed = {
    address: Address;
    rewardAddress: RewardAddress | null;
    paymentKey: PrivateKey;
    stakeKey: PrivateKey | null;
};
export declare function walletFromSeed(seed: string, options?: {
    password?: string;
    addressType?: "Base" | "Enterprise";
    accountIndex?: number;
    network?: Network;
}): FromSeed;
export declare function discoverOwnUsedTxKeyHashes(tx: C.Transaction, ownKeyHashes: Array<KeyHash>, ownUtxos: Array<UTxO>): Array<KeyHash>;
export {};
