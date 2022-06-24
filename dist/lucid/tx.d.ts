import { Core } from '../core';
import { Address, Assets, CertificateValidator, OutputData, Datum, Json, Label, Lovelace, MintingPolicy, PoolId, Redeemer, RewardAddress, SpendingValidator, UnixTime, UTxO, WithdrawalValidator, Unit, NFTMetadataDetails } from '../types';
import { Lucid } from './lucid';
import { TxComplete } from './txComplete';
export declare class Tx {
    txBuilder: Core.TransactionBuilder;
    private tasks;
    private nftMetadata;
    private lucid;
    constructor(lucid: Lucid);
    /**
     *
     * Read data from utxos. These utxos are only referenced and not spent
     */
    readFrom(utxos: UTxO[]): this;
    /**
     * A public key or native script input
     *
     * With redeemer a plutus script input
     *  */
    collectFrom(utxos: UTxO[], redeemer?: Redeemer): this;
    /** All assets should be of the same Policy Id.
     *
     * You can chain mintAssets events together if you need to mint assets with different Policy Ids.
     *
     * If the plutus script doesn't need a redeemer, you still neeed to specifiy the empty redeemer.
     *  */
    mintAssets(assets: Assets, redeemer?: Redeemer): this;
    /**
     * Pay to a public key or native script address
     *  */
    payToAddress(address: Address, assets: Assets): this;
    /**
     * Pay to a public key or native script address with datum or scriptRef
     *  */
    payToAddressWithData(address: Address, outputData: Datum | OutputData, assets: Assets): this;
    /**
     * Pay to a plutus script address with datum or scriptRef
     *  */
    payToContract(address: Address, outputData: Datum | OutputData, assets: Assets): this;
    /**
     * Delegate to a stake pool
     */
    delegateTo(rewardAddress: RewardAddress, poolId: PoolId, redeemer?: Redeemer): this;
    registerStake(rewardAddress: RewardAddress): this;
    deregisterStake(rewardAddress: RewardAddress, redeemer?: Redeemer): this;
    withdraw(rewardAddress: RewardAddress, amount: Lovelace, redeemer?: Redeemer): this;
    /**
     * Needs to be a public key address
     *
     * The PaymentKeyHash is taken when providing a Base, Enterprise or Pointer address
     *
     * The StakeKeyHash is taken when providing a Reward address
     */
    addSigner(address: Address | RewardAddress): this;
    validFrom(unixTime: UnixTime): this;
    validTo(unixTime: UnixTime): this;
    attachMetadata(label: Label, metadata: Json): this;
    /**
     * Converts strings to bytes if prefixed with **'0x'**
     */
    attachMetadataWithConversion(label: Label, metadata: Json): this;
    /**
     * Converts strings to bytes if prefixed with **'0x'**
     *
     * Uses version 2 of CIP-0025
     *
     * You don't need to add policy id and asset name to the metadata, only add the details
     */
    attachNFTMetadata(unit: Unit, metadata: NFTMetadataDetails): this;
    attachSpendingValidator(spendingValidator: SpendingValidator): this;
    attachMintingPolicy(mintingPolicy: MintingPolicy): this;
    attachCertificateValidator(certValidator: CertificateValidator): this;
    attachWithdrawalValidator(withdrawalValidator: WithdrawalValidator): this;
    /**
     * callback cannot be async
     *
     */
    applyIf(condition: boolean, callback: (tx: Tx) => void): this;
    complete(option?: {
        changeAddress?: Address;
        datum?: {
            asHash?: Datum;
            inline?: Datum;
        };
    }): Promise<TxComplete>;
}
