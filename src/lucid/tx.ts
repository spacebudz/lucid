import { C } from '../core';
import Core from 'core/types';
import {
  Address,
  Assets,
  CertificateValidator,
  Datum,
  Json,
  Label,
  Lovelace,
  MintingPolicy,
  NFTMetadata,
  PaymentKeyHash,
  PoolId,
  Redeemer,
  RewardAddress,
  SpendingValidator,
  StakeKeyHash,
  UnixTime,
  UTxO,
  WithdrawalValidator,
} from '../types';
import { utxoToCore, assetsToValue, fromHex } from '../utils';
import { Lucid } from './lucid';
import { TxComplete } from './txComplete';

export class Tx {
  txBuilder: Core.TransactionBuilder;
  private tasks: Function[];
  private nftMetadata: NFTMetadata = {};
  private lucid: Lucid;

  constructor(lucid: Lucid) {
    this.lucid = lucid;
    this.txBuilder = C.TransactionBuilder.new(this.lucid.txBuilderConfig);
    this.tasks = [];
    this.nftMetadata = {};
    this.nftMetadata.version = 2;
  }

  /**
   * A public key or native script input
   *
   * With redeemer a plutus script input
   *  */
  collectFrom(utxos: UTxO[], redeemer?: Redeemer) {
    this.tasks.push(async () => {
      for (const utxo of utxos) {
        if (utxo.datumHash && !utxo.datum) {
          utxo.datum = await this.lucid.datumOf(utxo);
        }
        const coreUtxo = utxoToCore(utxo);
        this.txBuilder.add_input(
          C.Address.from_bech32(utxo.address),
          coreUtxo.input(),
          coreUtxo.output().amount(),
          (redeemer as undefined) &&
            C.ScriptWitness.new_plutus_witness(
              C.PlutusWitness.new(
                C.PlutusData.from_bytes(fromHex(redeemer!)),
                (utxo.datum as undefined) &&
                  C.PlutusData.from_bytes(fromHex(utxo.datum!))
              )
            )
        );
      }
    });
    return this;
  }

  /** All assets should be of the same Policy Id.
   *
   * You can chain mintAssets events together if you need to mint assets with different Policy Ids.
   *
   * If the plutus script doesn't need a redeemer, you still neeed to specifiy the empty redeemer.
   *  */
  mintAssets(assets: Assets, redeemer?: Redeemer) {
    const units = Object.keys(assets);
    const policyId = units[0].slice(0, 56);
    const mintAssets = C.MintAssets.new();
    units.forEach(unit => {
      if (unit.slice(0, 56) !== policyId)
        throw new Error(
          'Only one Policy Id allowed. You can chain multiple mintAssets events together if you need to mint assets with different Policy Ids.'
        );
      mintAssets.insert(
        C.AssetName.new(fromHex(unit.slice(56))),
        C.Int.from_str(assets[unit].toString())
      );
    });
    const scriptHash = C.ScriptHash.from_bytes(fromHex(policyId));
    this.txBuilder.add_mint(
      scriptHash,
      mintAssets,
      (redeemer as undefined) &&
        C.ScriptWitness.new_plutus_witness(
          C.PlutusWitness.new(C.PlutusData.from_bytes(fromHex(redeemer!)))
        )
    );
    return this;
  }

  /**
   * Pay to a public key or native script address
   *  */
  payToAddress(address: Address, assets: Assets) {
    const output = C.TransactionOutput.new(
      C.Address.from_bech32(address),
      assetsToValue(assets)
    );
    this.txBuilder.add_output(output);
    return this;
  }

  /**
   * Pay to a public key or native script address with datum
   *  */
  payToAddressWithDatum(address: Address, datum: Datum, assets: Assets) {
    const plutusData = C.PlutusData.from_bytes(fromHex(datum));
    const output = C.TransactionOutput.new(
      C.Address.from_bech32(address),
      assetsToValue(assets)
    );
    output.set_datum(C.Datum.new_data_hash(C.hash_plutus_data(plutusData)));
    this.txBuilder.add_output(output);
    this.txBuilder.add_plutus_data(plutusData);
    return this;
  }

  /**
   * Pay to a plutus script address with datum
   *  */
  payToContract(address: Address, datum: Datum, assets: Assets) {
    const plutusData = C.PlutusData.from_bytes(fromHex(datum));
    const output = C.TransactionOutput.new(
      C.Address.from_bech32(address),
      assetsToValue(assets)
    );
    output.set_datum(C.Datum.new_data_hash(C.hash_plutus_data(plutusData)));
    this.txBuilder.add_output(output);
    this.txBuilder.add_plutus_data(plutusData);
    return this;
  }

  /**
   * Delegate to a stake pool
   */
  delegateTo(
    rewardAddress: RewardAddress,
    poolId: PoolId,
    redeemer?: Redeemer
  ) {
    const addressDetails = this.lucid.utils.getAddressDetails(rewardAddress);
    if (
      addressDetails.address.type !== 'Reward' ||
      !addressDetails.stakeCredential
    )
      throw new Error('Not a reward address provided');
    const credential =
      addressDetails.stakeCredential.type === 'Key'
        ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_bytes(
              fromHex(addressDetails.stakeCredential.hash)
            )
          )
        : C.StakeCredential.from_scripthash(
            C.ScriptHash.from_bytes(
              fromHex(addressDetails.stakeCredential.hash)
            )
          );

    this.txBuilder.add_certificate(
      C.Certificate.new_stake_delegation(
        C.StakeDelegation.new(credential, C.Ed25519KeyHash.from_bech32(poolId))
      ),
      (redeemer as undefined) &&
        C.ScriptWitness.new_plutus_witness(
          C.PlutusWitness.new(C.PlutusData.from_bytes(fromHex(redeemer!)))
        )
    );
    return this;
  }

  registerStake(rewardAddress: RewardAddress) {
    const addressDetails = this.lucid.utils.getAddressDetails(rewardAddress);
    if (
      addressDetails.address.type !== 'Reward' ||
      !addressDetails.stakeCredential
    )
      throw new Error('Not a reward address provided');
    const credential =
      addressDetails.stakeCredential.type === 'Key'
        ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_bytes(
              fromHex(addressDetails.stakeCredential.hash)
            )
          )
        : C.StakeCredential.from_scripthash(
            C.ScriptHash.from_bytes(
              fromHex(addressDetails.stakeCredential.hash)
            )
          );

    this.txBuilder.add_certificate(
      C.Certificate.new_stake_registration(C.StakeRegistration.new(credential))
    );
    return this;
  }

  deregisterStake(rewardAddress: RewardAddress, redeemer?: Redeemer) {
    const addressDetails = this.lucid.utils.getAddressDetails(rewardAddress);
    if (
      addressDetails.address.type !== 'Reward' ||
      !addressDetails.stakeCredential
    )
      throw new Error('Not a reward address provided');
    const credential =
      addressDetails.stakeCredential.type === 'Key'
        ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_bytes(
              fromHex(addressDetails.stakeCredential.hash)
            )
          )
        : C.StakeCredential.from_scripthash(
            C.ScriptHash.from_bytes(
              fromHex(addressDetails.stakeCredential.hash)
            )
          );

    this.txBuilder.add_certificate(
      C.Certificate.new_stake_deregistration(
        C.StakeDeregistration.new(credential)
      ),
      (redeemer as undefined) &&
        C.ScriptWitness.new_plutus_witness(
          C.PlutusWitness.new(C.PlutusData.from_bytes(fromHex(redeemer!)))
        )
    );
    return this;
  }

  withdraw(
    rewardAddress: RewardAddress,
    amount: Lovelace,
    redeemer?: Redeemer
  ) {
    this.txBuilder.add_withdrawal(
      C.RewardAddress.from_address(C.Address.from_bech32(rewardAddress))!,
      C.BigNum.from_str(amount.toString()),
      (redeemer as undefined) &&
        C.ScriptWitness.new_plutus_witness(
          C.PlutusWitness.new(C.PlutusData.from_bytes(fromHex(redeemer!)))
        )
    );
    return this;
  }

  /**
   * Needs to be a public key address
   *
   * The PaymentKeyHash is taken when providing a Base, Enterprise or Pointer address
   *
   * The StakeKeyHash is taken when providing a Reward address
   */
  addSigner(address: Address | RewardAddress) {
    const addressDetails = this.lucid.utils.getAddressDetails(address);

    if (!addressDetails.paymentCredential && !addressDetails.stakeCredential)
      throw new Error('Not a valid address.');

    const credential =
      addressDetails.address.type === 'Reward'
        ? addressDetails.stakeCredential!
        : addressDetails.paymentCredential!;

    if (credential.type === 'Script')
      throw new Error('Only key hashes are allow as signers.');

    return this.addSignerKey(credential.hash);
  }

  /**
   * Add a public or stake key hash as a required signer of the transaction.
   */
  addSignerKey(signer: PaymentKeyHash | StakeKeyHash) {
    this.txBuilder.add_required_signer(
      C.Ed25519KeyHash.from_bytes(fromHex(signer))
    );
    return this;
  }

  validFrom(unixTime: UnixTime) {
    const slot = this.lucid.utils.unixTimeToSlot(unixTime);
    this.txBuilder.set_validity_start_interval(
      C.BigNum.from_str(slot.toString())
    );
    return this;
  }

  validTo(unixTime: UnixTime) {
    const slot = this.lucid.utils.unixTimeToSlot(unixTime);
    this.txBuilder.set_ttl(C.BigNum.from_str(slot.toString()));
    return this;
  }

  attachMetadata(label: Label, metadata: Json) {
    this.txBuilder.add_json_metadatum(
      C.BigNum.from_str(label.toString()),
      JSON.stringify(metadata)
    );
    return this;
  }

  /**
   * Converts strings to bytes if prefixed with **'0x'**
   */
  attachMetadataWithConversion(label: Label, metadata: Json) {
    this.txBuilder.add_json_metadatum_with_schema(
      C.BigNum.from_str(label.toString()),
      JSON.stringify(metadata),
      C.MetadataJsonSchema.BasicConversions
    );
    return this;
  }

  // /**
  //  * Converts strings to bytes if prefixed with **'0x'**
  //  *
  //  * Policy id and asset name are converted to bytes
  //  */
  // attachNFTMetadata(unit: Unit, metadata: NFTMetadataDetails) {
  //   const policyId = unit.slice(0, 56);
  //   const assetName = unit.slice(56);
  //   this.nftMetadata['0x' + policyId] = {
  //     ...(this.nftMetadata['0x' + policyId] || {}),
  //     ['0x' + assetName]: metadata,
  //   };

  //   return this;
  // }

  attachSpendingValidator(spendingValidator: SpendingValidator) {
    attachScript(this, spendingValidator);
    return this;
  }

  attachMintingPolicy(mintingPolicy: MintingPolicy) {
    attachScript(this, mintingPolicy);
    return this;
  }

  attachCertificateValidator(certValidator: CertificateValidator) {
    attachScript(this, certValidator);
    return this;
  }

  attachWithdrawalValidator(withdrawalValidator: WithdrawalValidator) {
    attachScript(this, withdrawalValidator);
    return this;
  }

  /**
   * callback cannot be async
   *
   */
  applyIf(condition: boolean, callback: (tx: Tx) => void) {
    if (condition) callback(this);
    return this;
  }

  async complete(option?: { changeAddress?: Address; datum?: Datum }) {
    for (const task of this.tasks) {
      await task();
    }
    // Add NFT metadata
    if (Object.keys(this.nftMetadata).length > 1) {
      this.txBuilder.add_json_metadatum_with_schema(
        C.BigNum.from_str('721'),
        JSON.stringify(this.nftMetadata),
        C.MetadataJsonSchema.BasicConversions
      );
    }

    const utxos = await this.lucid.wallet.getUtxosCore();
    if (this.txBuilder.redeemers()!?.len() > 0) {
      const collateral = await this.lucid.wallet.getCollateralCore();
      if (collateral.length <= 0) throw new Error('No collateral UTxO found.');
      // 2 collateral utxos should be more than sufficient
      collateral.slice(0, 2).forEach(utxo => {
        this.txBuilder.add_collateral(utxo.output().address(), utxo.input());
      });
    }

    this.txBuilder.add_inputs_from(utxos);
    this.txBuilder.balance(
      C.Address.from_bech32(
        option?.changeAddress || (await this.lucid.wallet.address())
      ),
      option?.datum
        ? C.Datum.new_data_hash(
            C.hash_plutus_data(C.PlutusData.from_bytes(fromHex(option.datum)))
          )
        : undefined
    );
    if (option?.datum) {
      this.txBuilder.add_plutus_data(
        C.PlutusData.from_bytes(fromHex(option.datum))
      );
    }

    return new TxComplete(this.lucid, await this.txBuilder.construct());
  }
}

const attachScript = (
  tx: Tx,
  script:
    | SpendingValidator
    | MintingPolicy
    | CertificateValidator
    | WithdrawalValidator
) => {
  if (script.type === 'Native') {
    tx.txBuilder.add_native_script(
      C.NativeScript.from_bytes(fromHex(script.script))
    );
  }
  if (script.type === 'PlutusV1' || script.type === 'PlutusV2') {
    tx.txBuilder.add_plutus_script(
      C.PlutusScript.from_bytes(fromHex(script.script))
    );
  }
};
