import { C } from "../core/mod.js";
import { Address, Assets, CertificateValidator, Configuration, Datum, Json, Label, Lovelace, MintingPolicy, OutputData, PaymentKeyHash, PoolId, PoolParams, Redeemer, RewardAddress, SpendingValidator, StakeKeyHash, UnixTime, UTxO, WithdrawalValidator } from "../types/mod.js";
import { Lucid } from "./lucid.js";
import { TxComplete } from "./tx_complete.js";
export declare class Tx {
    txBuilder: C.TransactionBuilder;
    /** Stores the tx instructions, which get executed after calling .complete() */
    private tasks;
    private lucid;
    configuration: Configuration;
    constructor(lucid: Lucid);
    /** Read data from utxos. These utxos are only referenced and not spent. */
    readFrom(utxos: UTxO[]): Tx;
    /**
     * Customize the transaction builder
     */
    config(newConfig: Partial<Configuration>): this;
    /**
     * A public key or native script input.
     * With redeemer it's a plutus script input.
     */
    collectFrom(utxos: UTxO[], redeemer?: Redeemer): Tx;
    /**
     * All assets should be of the same policy id.
     * You can chain mintAssets functions together if you need to mint assets with different policy ids.
     * If the plutus script doesn't need a redeemer, you still need to specifiy the void redeemer.
     */
    mintAssets(assets: Assets, redeemer?: Redeemer): Tx;
    /** Pay to a public key or native script address. */
    payToAddress(address: Address, assets: Assets): Tx;
    /** Pay to a public key or native script address with datum or scriptRef. */
    payToAddressWithData(address: Address, outputData: Datum | OutputData, assets: Assets): Tx;
    /** Pay to a plutus script address with datum or scriptRef. */
    payToContract(address: Address, outputData: Datum | OutputData, assets: Assets): Tx;
    /** Delegate to a stake pool. */
    delegateTo(rewardAddress: RewardAddress, poolId: PoolId, redeemer?: Redeemer): Tx;
    /** Register a reward address in order to delegate to a pool and receive rewards. */
    registerStake(rewardAddress: RewardAddress): Tx;
    /** Deregister a reward address. */
    deregisterStake(rewardAddress: RewardAddress, redeemer?: Redeemer): Tx;
    /** Register a stake pool. A pool deposit is required. The metadataUrl needs to be hosted already before making the registration. */
    registerPool(poolParams: PoolParams): Tx;
    /** Update a stake pool. No pool deposit is required. The metadataUrl needs to be hosted already before making the update. */
    updatePool(poolParams: PoolParams): Tx;
    /**
     * Retire a stake pool. The epoch needs to be the greater than the current epoch + 1 and less than current epoch + eMax.
     * The pool deposit will be sent to reward address as reward after full retirement of the pool.
     */
    retirePool(poolId: PoolId, epoch: number): Tx;
    withdraw(rewardAddress: RewardAddress, amount: Lovelace, redeemer?: Redeemer): Tx;
    /**
     * Needs to be a public key address.
     * The PaymentKeyHash is taken when providing a Base, Enterprise or Pointer address.
     * The StakeKeyHash is taken when providing a Reward address.
     */
    addSigner(address: Address | RewardAddress): Tx;
    /** Add a payment or stake key hash as a required signer of the transaction. */
    addSignerKey(keyHash: PaymentKeyHash | StakeKeyHash): Tx;
    validFrom(unixTime: UnixTime): Tx;
    validTo(unixTime: UnixTime): Tx;
    attachMetadata(label: Label, metadata: Json): Tx;
    /** Converts strings to bytes if prefixed with **'0x'**. */
    attachMetadataWithConversion(label: Label, metadata: Json): Tx;
    /** Explicitely set the network id in the transaction body. */
    addNetworkId(id: number): Tx;
    attachSpendingValidator(spendingValidator: SpendingValidator): Tx;
    attachMintingPolicy(mintingPolicy: MintingPolicy): Tx;
    attachCertificateValidator(certValidator: CertificateValidator): Tx;
    attachWithdrawalValidator(withdrawalValidator: WithdrawalValidator): Tx;
    /** Conditionally apply to the transaction. */
    applyIf(condition: boolean, callback: (thisTx: Tx) => unknown): Tx;
    /** Apply to the transaction. */
    apply(callback: (thisTx: Tx) => unknown): Tx;
    /** Compose transactions. */
    compose(tx: Tx | null): Tx;
    complete(options?: {
        change?: {
            address?: Address;
            outputData?: OutputData;
        };
        coinSelection?: boolean;
        nativeUplc?: boolean;
    }): Promise<TxComplete>;
    /** Return the current transaction body in Hex encoded Cbor. */
    toString(): Promise<string>;
    /**
     * Splits remaining assets into multiple change outputs
     * if there's enough ADA to cover for minimum UTxO requirements.
     *
     * The objective is to create one collateral output as well as
     * as many pure outputs as possible, since they cost the least to be consumed.
     *
     * It does so by following these steps:
     * 1. Sort the native assets cannonically
     * 2. Add outputs with a maximum of N native assets until these are exhausted
     * 3. Continously create pure ADA outputs with half of the remaining amount
     *    until said remaining amount is below the minimum K
     *
     * This is the advanced UTxO management algorithm used by Eternl
     */
    private splitChange;
}
