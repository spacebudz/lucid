import { C } from '../core';
import Core from 'core/types';
import {
  Address,
  Assets,
  CertificateValidator,
  Configuration,
  Datum,
  Json,
  Label,
  Lovelace,
  MintingPolicy,
  PoolId,
  Redeemer,
  RewardAddress,
  SpendingValidator,
  UnixTime,
  UTxO,
  WithdrawalValidator,
} from '../types';
import {
  utxoToCore,
  assetsToValue,
  getAddressDetails,
  unixTimeToSlot,
  unixTimeToSlotTestnet,
  valueToAssets,
  chunk,
} from '../utils';
import { Lucid } from './lucid';
import { TxComplete } from './txComplete';
import { defaultConfig } from './txConfig';

export class Tx {
  txBuilder: Core.TransactionBuilder;
  configuration = defaultConfig;

  static new() {
    const t = new this();
    t.txBuilder = C.TransactionBuilder.new(Lucid.txBuilderConfig);
    return t;
  }

  /**
   * Customize the transaction builder
   */
  config(newConfig: Partial<Configuration>) {
    this.configuration = { ...this.configuration, ...newConfig };
    return this;
  }

  /**
   * A public key or native script input
   *
   * With redeemer a plutus script input
   *  */
  collectFrom(utxos: UTxO[], redeemer?: Redeemer) {
    utxos.forEach((utxo) => {
      const utxoCloned = { ...utxo };
      if (utxo.datumHash && !utxo.datum) {
        //TODO
        // utxoCloned.datum = Lucid.getDatum();
      }
      const coreUtxo = utxoToCore(utxoCloned);
      this.txBuilder.add_input(
        C.Address.from_bech32(utxo.address),
        coreUtxo.input(),
        coreUtxo.output().amount(),
        redeemer &&
          C.ScriptWitness.new_plutus_witness(
            C.PlutusWitness.new(
              C.PlutusData.from_bytes(Buffer.from(redeemer, 'hex')),
              utxoCloned.datum && C.PlutusData.from_bytes(Buffer.from(utxoCloned.datum, 'hex')),
            ),
          ),
      );
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
    units.forEach((unit) => {
      if (unit.slice(0, 56) !== policyId)
        throw new Error(
          'Only one Policy Id allowed. You can chain multiple mintAssets events together if you need to mint assets with different Policy Ids.',
        );
      mintAssets.insert(
        C.AssetName.new(Buffer.from(unit.slice(56), 'hex')),
        C.Int.from_str(assets[unit].toString()),
      );
    });
    const scriptHash = C.ScriptHash.from_bytes(Buffer.from(policyId, 'hex'));
    this.txBuilder.add_mint(
      scriptHash,
      mintAssets,
      redeemer &&
        C.ScriptWitness.new_plutus_witness(
          C.PlutusWitness.new(C.PlutusData.from_bytes(Buffer.from(redeemer, 'hex'))),
        ),
    );
    return this;
  }

  /**
   * Pay to a public key or native script address
   *  */
  payToAddress(address: Address, assets: Assets) {
    const output = C.TransactionOutput.new(C.Address.from_bech32(address), assetsToValue(assets));
    this.txBuilder.add_output(output);
    return this;
  }

  /**
   * Pay to a plutus script address with datum
   *  */
  payToContract(address: Address, datum: Datum, assets: Assets) {
    const plutusData = C.PlutusData.from_bytes(Buffer.from(datum, 'hex'));
    const output = C.TransactionOutput.new(C.Address.from_bech32(address), assetsToValue(assets));
    output.set_datum(C.Datum.new_data_hash(C.hash_plutus_data(plutusData)));
    this.txBuilder.add_output(output);
    this.txBuilder.add_plutus_data(plutusData);
    return this;
  }

  /**
   * Delegate to a stake pool
   */
  delegateTo(rewardAddress: RewardAddress, poolId: PoolId, redeemer?: Redeemer) {
    const detailedAddress = getAddressDetails(rewardAddress);
    if (detailedAddress.type !== 'Reward') throw new Error('Not a reward address provided');
    const credential =
      detailedAddress.credentialType === 'Key'
        ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_bytes(Buffer.from(detailedAddress.stakeKeyHash, 'hex')),
          )
        : C.StakeCredential.from_scripthash(
            C.Ed25519KeyHash.from_bytes(Buffer.from(detailedAddress.stakeKeyHash, 'hex')),
          );

    this.txBuilder.add_certificate(
      C.Certificate.new_stake_delegation(
        C.StakeDelegation.new(credential, C.Ed25519KeyHash.from_bech32(poolId)),
      ),
      redeemer &&
        C.ScriptWitness.new_plutus_witness(
          C.PlutusWitness.new(C.PlutusData.from_bytes(Buffer.from(redeemer, 'hex'))),
        ),
    );
    return this;
  }

  registerStake(rewardAddress: RewardAddress) {
    const detailedAddress = getAddressDetails(rewardAddress);
    if (detailedAddress.type !== 'Reward') throw new Error('Not a reward address provided');
    const credential =
      detailedAddress.credentialType === 'Key'
        ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_bytes(Buffer.from(detailedAddress.stakeKeyHash, 'hex')),
          )
        : C.StakeCredential.from_scripthash(
            C.Ed25519KeyHash.from_bytes(Buffer.from(detailedAddress.stakeKeyHash, 'hex')),
          );

    this.txBuilder.add_certificate(
      C.Certificate.new_stake_registration(C.StakeRegistration.new(credential)),
    );
    return this;
  }

  deregisterStake(rewardAddress: RewardAddress, redeemer?: Redeemer) {
    const detailedAddress = getAddressDetails(rewardAddress);
    if (detailedAddress.type !== 'Reward') throw new Error('Not a reward address provided');
    const credential =
      detailedAddress.credentialType === 'Key'
        ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_bytes(Buffer.from(detailedAddress.stakeKeyHash, 'hex')),
          )
        : C.StakeCredential.from_scripthash(
            C.Ed25519KeyHash.from_bytes(Buffer.from(detailedAddress.stakeKeyHash, 'hex')),
          );

    this.txBuilder.add_certificate(
      C.Certificate.new_stake_deregistration(C.StakeDeregistration.new(credential)),
      redeemer &&
        C.ScriptWitness.new_plutus_witness(
          C.PlutusWitness.new(C.PlutusData.from_bytes(Buffer.from(redeemer, 'hex'))),
        ),
    );
    return this;
  }

  withdraw(rewardAddress: RewardAddress, amount: Lovelace, redeemer?: Redeemer) {
    this.txBuilder.add_withdrawal(
      C.RewardAddress.from_address(C.Address.from_bech32(rewardAddress)),
      C.BigNum.from_str(amount.toString()),
      redeemer &&
        C.ScriptWitness.new_plutus_witness(
          C.PlutusWitness.new(C.PlutusData.from_bytes(Buffer.from(redeemer, 'hex'))),
        ),
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
    const addressDetailed = getAddressDetails(address);

    const keyHash =
      addressDetailed.type === 'Reward'
        ? addressDetailed.stakeKeyHash
        : addressDetailed.paymentKeyHash;

    this.txBuilder.add_required_signer(C.Ed25519KeyHash.from_bytes(Buffer.from(keyHash, 'hex')));
    return this;
  }

  validFrom(unixTime: UnixTime) {
    const slot =
      Lucid.network === 'Mainnet' ? unixTimeToSlot(unixTime) : unixTimeToSlotTestnet(unixTime);
    this.txBuilder.set_validity_start_interval(C.BigNum.from_str(slot.toString()));
    return this;
  }

  validTo(unixTime: UnixTime) {
    const slot =
      Lucid.network === 'Mainnet' ? unixTimeToSlot(unixTime) : unixTimeToSlotTestnet(unixTime);
    this.txBuilder.set_ttl(C.BigNum.from_str(slot.toString()));
    return this;
  }

  attachMetadata(label: Label, metadata: Json) {
    this.txBuilder.add_json_metadatum(
      C.BigNum.from_str(label.toString()),
      JSON.stringify(metadata),
    );
    return this;
  }

  /**
   * Converts strings to bytes if prefixed with **'0x'**
   *
   */
  attachMetadataWithConversion(label: Label, metadata: Json) {
    this.txBuilder.add_json_metadatum_with_schema(
      C.BigNum.from_str(label.toString()),
      JSON.stringify(metadata),
      C.MetadataJsonSchema.BasicConversions,
    );
    return this;
  }

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
    if (withdrawalValidator.type === 'Native') {
      this.txBuilder.add_native_script(
        C.NativeScript.from_bytes(Buffer.from(withdrawalValidator.script, 'hex')),
      );
    }
    if (withdrawalValidator.type === 'Plutus') {
      this.txBuilder.add_plutus_script(
        C.PlutusScript.from_bytes(Buffer.from(withdrawalValidator.script, 'hex')),
      );
    }
    return this;
  }

  async complete() {
    const { enableChangeSplitting } = this.configuration;

    const utxos = await Lucid.wallet.getUtxosCore();
    if (this.txBuilder.redeemers()?.len() > 0) {
      const collateral = await Lucid.wallet.getCollateralCore();
      if (collateral.length <= 0) throw new Error('No collateral UTxO found.');
      // 2 collateral utxos should be more than sufficient
      collateral.slice(0, 2).forEach((utxo) => {
        this.txBuilder.add_collateral(utxo.output().address(), utxo.input());
      });
    }

    try {
      this.txBuilder.add_inputs_from(utxos, C.CoinSelectionStrategyCIP2.RandomImproveMultiAsset);
    } catch (e) {
      try {
        this.txBuilder.add_inputs_from(utxos, C.CoinSelectionStrategyCIP2.RandomImprove);
      } catch (e) {
        try {
          this.txBuilder.add_inputs_from(utxos, C.CoinSelectionStrategyCIP2.LargestFirstMultiAsset);
        } catch (e) {
          try {
            this.txBuilder.add_inputs_from(utxos, C.CoinSelectionStrategyCIP2.LargestFirst);
          } catch (e) {
            throw new Error('Coin selection failed. Not enough funds or no fitting UTxOs found.');
          }
        }
      }
    }

    if (enableChangeSplitting) {
      this.splitChange();
    }

    this.txBuilder.add_change_if_needed(C.Address.from_bech32(Lucid.wallet.address));
    return new TxComplete(await this.txBuilder.construct());
  }

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
  private async splitChange() {
    const { coinsPerUtxoWord } = await Lucid.provider.getProtocolParameters();
    const { changeCollateral, changeNativeAssetChunkSize, changeMinUtxo } = this.configuration;
    const change = this.txBuilder
      .get_explicit_input()
      .checked_sub(this.txBuilder.get_explicit_output());

    let changeAda = change.coin();

    // Sort canonically so we group policy IDs together
    const changeAssets = Object.keys(valueToAssets(change))
      .filter((v) => v !== 'lovelace')
      .sort((a, b) => a.localeCompare(b))
      .reduce((res, key) => Object.assign(res, { [key]: change[key] }), {});

    const numOutputsWithNativeAssets = Math.ceil(
      Object.keys(changeAssets).length / changeNativeAssetChunkSize,
    );

    const minAdaPerOutput = C.min_ada_required(
      assetsToValue(changeAssets),
      false,
      C.BigNum.from_str(coinsPerUtxoWord.toString()),
    );
    // conservative estimate, multiply by 1.3 instead of 1.1
    // TODO: Division?
    // .checked_mul(C.BigNum.from_str('130'))
    // .checked_div(C.BigNum.from_str('100'));

    // Do we have enough ADA in the change to split and still
    // statisfy minADA requirements?
    const shouldSplitChange =
      minAdaPerOutput
        .checked_mul(C.BigNum.from_str(numOutputsWithNativeAssets.toString()))
        .checked_add(C.BigNum.from_str('1000000'))
        .compare(changeAda) < 0;

    if (change.multiasset() && shouldSplitChange) {
      const assetChunks = chunk(Object.keys(changeAssets), 20);

      for (const chunk of assetChunks) {
        const val = assetsToValue(
          chunk.reduce((res, key) => Object.assign(res, { [key]: change[key] }), {}),
        );
        const minAda = C.min_ada_required(
          val,
          false,
          C.BigNum.from_str(coinsPerUtxoWord.toString()),
        );

        // TODO: Division
        const coin = minAda.checked_mul(C.BigNum.from_str('110'));

        val.set_coin(coin);
        changeAda = changeAda.checked_sub(coin);

        this.txBuilder.add_output(
          C.TransactionOutput.new(C.Address.from_bech32(Lucid.wallet.address), val),
        );
      }
    }

    // Now try adding a collateral amount if possible
    // Add a collateral output if possible
    const collateralAmount = C.BigNum.from_str(changeCollateral);

    if (changeAda.compare(collateralAmount.checked_add(minAdaPerOutput)) > 0) {
      this.txBuilder.add_output(
        C.TransactionOutput.new(
          C.Address.from_bech32(Lucid.wallet.address),
          C.Value.new(collateralAmount),
        ),
      );
      changeAda = changeAda.checked_sub(collateralAmount);
    }

    // while (
    // If the half is more than the minimum, we can split it
    // changeAda
    // TODO: Division
    // .checked_div(BigNum.from_str('2'))
    // .compare(C.BigNum.from_str(changeMinUtxo)) >= 0
    // ) {
    //   // TODO: Division
    //   const half = changeAda.checked_div(BigNum.from_str('2'));
    //   changeAda = changeAda.checked_sub(half);

    //   this.txBuilder.add_output(
    //     TransactionOutput.new(this.changeAddress, Value.new(half)),
    //   );
    // }
  }
}

const attachScript = (
  tx: Tx,
  script: SpendingValidator | MintingPolicy | CertificateValidator | WithdrawalValidator,
) => {
  if (script.type === 'Native') {
    tx.txBuilder.add_native_script(C.NativeScript.from_bytes(Buffer.from(script.script, 'hex')));
  }
  if (script.type === 'Plutus') {
    tx.txBuilder.add_plutus_script(C.PlutusScript.from_bytes(Buffer.from(script.script, 'hex')));
  }
};
