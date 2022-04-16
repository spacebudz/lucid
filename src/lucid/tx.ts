import { S } from '../core';
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
} from '../utils';
import { Lucid } from './lucid';
import { TxComplete } from './txComplete';

export class Tx {
  txBuilder: Core.TransactionBuilder;

  static new() {
    const t = new this();
    t.txBuilder = S.TransactionBuilder.new(Lucid.txBuilderConfig);
    return t;
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
        S.Address.from_bech32(utxo.address),
        coreUtxo.input(),
        coreUtxo.output().amount(),
        redeemer &&
          S.ScriptWitness.new_plutus_witness(
            S.PlutusWitness.new(
              S.PlutusData.from_bytes(Buffer.from(redeemer, 'hex')),
              utxoCloned.datum &&
                S.PlutusData.from_bytes(Buffer.from(utxoCloned.datum, 'hex')),
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
    const mintAssets = S.MintAssets.new();
    units.forEach((unit) => {
      if (unit.slice(0, 56) !== policyId)
        throw new Error(
          'Only one Policy Id allowed. You can chain multiple mintAssets events together if you need to mint assets with different Policy Ids.',
        );
      mintAssets.insert(
        S.AssetName.new(Buffer.from(unit.slice(56), 'hex')),
        S.Int.from_str(assets[unit].toString()),
      );
    });
    const scriptHash = S.ScriptHash.from_bytes(Buffer.from(policyId, 'hex'));
    this.txBuilder.add_mint(
      scriptHash,
      mintAssets,
      redeemer &&
        S.ScriptWitness.new_plutus_witness(
          S.PlutusWitness.new(
            S.PlutusData.from_bytes(Buffer.from(redeemer, 'hex')),
          ),
        ),
    );
    return this;
  }

  /**
   * Pay to a public key or native script address
   *  */
  payToAddress(address: Address, assets: Assets) {
    const output = S.TransactionOutput.new(
      S.Address.from_bech32(address),
      assetsToValue(assets),
    );
    this.txBuilder.add_output(output);
    return this;
  }

  /**
   * Pay to a plutus script address with datum
   *  */
  payToContract(address: Address, datum: Datum, assets: Assets) {
    const plutusData = S.PlutusData.from_bytes(Buffer.from(datum, 'hex'));
    const output = S.TransactionOutput.new(
      S.Address.from_bech32(address),
      assetsToValue(assets),
    );
    output.set_datum(S.Datum.new_data_hash(S.hash_plutus_data(plutusData)));
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
    redeemer?: Redeemer,
  ) {
    const detailedAddress = getAddressDetails(rewardAddress);
    if (detailedAddress.type !== 'Reward')
      throw new Error('Not a reward address provided');
    const credential =
      detailedAddress.credentialType === 'Key'
        ? S.StakeCredential.from_keyhash(
            S.Ed25519KeyHash.from_bytes(
              Buffer.from(detailedAddress.stakeKeyHash, 'hex'),
            ),
          )
        : S.StakeCredential.from_scripthash(
            S.Ed25519KeyHash.from_bytes(
              Buffer.from(detailedAddress.stakeKeyHash, 'hex'),
            ),
          );

    this.txBuilder.add_certificate(
      S.Certificate.new_stake_delegation(
        S.StakeDelegation.new(credential, S.Ed25519KeyHash.from_bech32(poolId)),
      ),
      redeemer &&
        S.ScriptWitness.new_plutus_witness(
          S.PlutusWitness.new(
            S.PlutusData.from_bytes(Buffer.from(redeemer, 'hex')),
          ),
        ),
    );
    return this;
  }

  registerStake(rewardAddress: RewardAddress) {
    const detailedAddress = getAddressDetails(rewardAddress);
    if (detailedAddress.type !== 'Reward')
      throw new Error('Not a reward address provided');
    const credential =
      detailedAddress.credentialType === 'Key'
        ? S.StakeCredential.from_keyhash(
            S.Ed25519KeyHash.from_bytes(
              Buffer.from(detailedAddress.stakeKeyHash, 'hex'),
            ),
          )
        : S.StakeCredential.from_scripthash(
            S.Ed25519KeyHash.from_bytes(
              Buffer.from(detailedAddress.stakeKeyHash, 'hex'),
            ),
          );

    this.txBuilder.add_certificate(
      S.Certificate.new_stake_registration(S.StakeRegistration.new(credential)),
    );
    return this;
  }

  deregisterStake(rewardAddress: RewardAddress, redeemer?: Redeemer) {
    const detailedAddress = getAddressDetails(rewardAddress);
    if (detailedAddress.type !== 'Reward')
      throw new Error('Not a reward address provided');
    const credential =
      detailedAddress.credentialType === 'Key'
        ? S.StakeCredential.from_keyhash(
            S.Ed25519KeyHash.from_bytes(
              Buffer.from(detailedAddress.stakeKeyHash, 'hex'),
            ),
          )
        : S.StakeCredential.from_scripthash(
            S.Ed25519KeyHash.from_bytes(
              Buffer.from(detailedAddress.stakeKeyHash, 'hex'),
            ),
          );

    this.txBuilder.add_certificate(
      S.Certificate.new_stake_deregistration(
        S.StakeDeregistration.new(credential),
      ),
      redeemer &&
        S.ScriptWitness.new_plutus_witness(
          S.PlutusWitness.new(
            S.PlutusData.from_bytes(Buffer.from(redeemer, 'hex')),
          ),
        ),
    );
    return this;
  }

  withdraw(
    rewardAddress: RewardAddress,
    amount: Lovelace,
    redeemer?: Redeemer,
  ) {
    this.txBuilder.add_withdrawal(
      S.RewardAddress.from_address(S.Address.from_bech32(rewardAddress)),
      S.BigNum.from_str(amount.toString()),
      redeemer &&
        S.ScriptWitness.new_plutus_witness(
          S.PlutusWitness.new(
            S.PlutusData.from_bytes(Buffer.from(redeemer, 'hex')),
          ),
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

    this.txBuilder.add_required_signer(
      S.Ed25519KeyHash.from_bytes(Buffer.from(keyHash, 'hex')),
    );
    return this;
  }

  validFrom(unixTime: UnixTime) {
    const slot =
      Lucid.network === 'Mainnet'
        ? unixTimeToSlot(unixTime)
        : unixTimeToSlotTestnet(unixTime);
    this.txBuilder.set_validity_start_interval(
      S.BigNum.from_str(slot.toString()),
    );
    return this;
  }

  validTo(unixTime: UnixTime) {
    const slot =
      Lucid.network === 'Mainnet'
        ? unixTimeToSlot(unixTime)
        : unixTimeToSlotTestnet(unixTime);
    this.txBuilder.set_ttl(S.BigNum.from_str(slot.toString()));
    return this;
  }

  attachMetadata(label: Label, metadata: Json) {
    this.txBuilder.add_json_metadatum(
      S.BigNum.from_str(label.toString()),
      JSON.stringify(metadata),
    );
    return this;
  }

  attachSpendingValidator(spendingValidator: SpendingValidator) {
    if (spendingValidator.type === 'Native') {
      this.txBuilder.add_native_script(
        S.NativeScript.from_bytes(Buffer.from(spendingValidator.script, 'hex')),
      );
    }
    if (spendingValidator.type === 'Plutus') {
      this.txBuilder.add_plutus_script(
        S.PlutusScript.from_bytes(Buffer.from(spendingValidator.script, 'hex')),
      );
    }
    return this;
  }

  attachMintingPolicy(mintingPolicy: MintingPolicy) {
    if (mintingPolicy.type === 'Native') {
      this.txBuilder.add_native_script(
        S.NativeScript.from_bytes(Buffer.from(mintingPolicy.script, 'hex')),
      );
    }
    if (mintingPolicy.type === 'Plutus') {
      this.txBuilder.add_plutus_script(
        S.PlutusScript.from_bytes(Buffer.from(mintingPolicy.script, 'hex')),
      );
    }
    return this;
  }

  attachCertificateValidator(certValidator: CertificateValidator) {
    if (certValidator.type === 'Native') {
      this.txBuilder.add_native_script(
        S.NativeScript.from_bytes(Buffer.from(certValidator.script, 'hex')),
      );
    }
    if (certValidator.type === 'Plutus') {
      this.txBuilder.add_plutus_script(
        S.PlutusScript.from_bytes(Buffer.from(certValidator.script, 'hex')),
      );
    }
    return this;
  }

  attachWithdrawalValidator(withdrawalValidator: WithdrawalValidator) {
    if (withdrawalValidator.type === 'Native') {
      this.txBuilder.add_native_script(
        S.NativeScript.from_bytes(
          Buffer.from(withdrawalValidator.script, 'hex'),
        ),
      );
    }
    if (withdrawalValidator.type === 'Plutus') {
      this.txBuilder.add_plutus_script(
        S.PlutusScript.from_bytes(
          Buffer.from(withdrawalValidator.script, 'hex'),
        ),
      );
    }
    return this;
  }

  async complete() {
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
      this.txBuilder.add_inputs_from(
        utxos,
        S.CoinSelectionStrategyCIP2.RandomImproveMultiAsset,
      );
    } catch (e) {
      try {
        this.txBuilder.add_inputs_from(
          utxos,
          S.CoinSelectionStrategyCIP2.RandomImprove,
        );
      } catch (e) {
        try {
          this.txBuilder.add_inputs_from(
            utxos,
            S.CoinSelectionStrategyCIP2.LargestFirstMultiAsset,
          );
        } catch (e) {
          try {
            this.txBuilder.add_inputs_from(
              utxos,
              S.CoinSelectionStrategyCIP2.LargestFirst,
            );
          } catch (e) {
            throw new Error(
              'Coin selection failed. Not enough funds or no fitting UTxOs found.',
            );
          }
        }
      }
    }

    this.txBuilder.add_change_if_needed(
      S.Address.from_bech32(Lucid.wallet.address),
    );
    return new TxComplete(await this.txBuilder.construct());
  }
}
