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
  txBuilder!: Core.TransactionBuilder;

  static new() {
    const t = new this();
    t.txBuilder = C.TransactionBuilder.new(Lucid.txBuilderConfig);
    return t;
  }

  /**
   * A public key or native script input
   *
   * With redeemer a plutus script input
   *  */
  collectFrom(utxos: UTxO[], redeemer?: Redeemer) {
    utxos.forEach(utxo => {
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
        (redeemer as undefined) &&
          C.ScriptWitness.new_plutus_witness(
            C.PlutusWitness.new(
              C.PlutusData.from_bytes(Buffer.from(redeemer!, 'hex')),
              (utxoCloned.datum as undefined) &&
                C.PlutusData.from_bytes(Buffer.from(utxoCloned.datum!, 'hex'))
            )
          )
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
    units.forEach(unit => {
      if (unit.slice(0, 56) !== policyId)
        throw new Error(
          'Only one Policy Id allowed. You can chain multiple mintAssets events together if you need to mint assets with different Policy Ids.'
        );
      mintAssets.insert(
        C.AssetName.new(Buffer.from(unit.slice(56), 'hex')),
        C.Int.from_str(assets[unit].toString())
      );
    });
    const scriptHash = C.ScriptHash.from_bytes(Buffer.from(policyId, 'hex'));
    this.txBuilder.add_mint(
      scriptHash,
      mintAssets,
      (redeemer as undefined) &&
        C.ScriptWitness.new_plutus_witness(
          C.PlutusWitness.new(
            C.PlutusData.from_bytes(Buffer.from(redeemer!, 'hex'))
          )
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
    const plutusData = C.PlutusData.from_bytes(Buffer.from(datum, 'hex'));
    const output = C.TransactionOutput.new(
      C.Address.from_bech32(address),
      assetsToValue(assets)
    );
    output.set_datum(C.Datum.new_data_hash(C.hash_plutus_data(plutusData)));
    this.txBuilder.add_output(output);
    return this;
  }

  /**
   * Pay to a plutus script address with datum
   *  */
  payToContract(address: Address, datum: Datum, assets: Assets) {
    const plutusData = C.PlutusData.from_bytes(Buffer.from(datum, 'hex'));
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
    const detailedAddress = getAddressDetails(rewardAddress);
    if (detailedAddress.type !== 'Reward' || !detailedAddress.stakeKeyHash)
      throw new Error('Not a reward address provided');
    const credential =
      detailedAddress.credentialType === 'Key'
        ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_bytes(
              Buffer.from(detailedAddress.stakeKeyHash, 'hex')
            )
          )
        : C.StakeCredential.from_scripthash(
            C.Ed25519KeyHash.from_bytes(
              Buffer.from(detailedAddress.stakeKeyHash, 'hex')
            )
          );

    this.txBuilder.add_certificate(
      C.Certificate.new_stake_delegation(
        C.StakeDelegation.new(credential, C.Ed25519KeyHash.from_bech32(poolId))
      ),
      (redeemer as undefined) &&
        C.ScriptWitness.new_plutus_witness(
          C.PlutusWitness.new(
            C.PlutusData.from_bytes(Buffer.from(redeemer!, 'hex'))
          )
        )
    );
    return this;
  }

  registerStake(rewardAddress: RewardAddress) {
    const detailedAddress = getAddressDetails(rewardAddress);
    if (detailedAddress.type !== 'Reward' || !detailedAddress.stakeKeyHash)
      throw new Error('Not a reward address provided');
    const credential =
      detailedAddress.credentialType === 'Key'
        ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_bytes(
              Buffer.from(detailedAddress.stakeKeyHash, 'hex')
            )
          )
        : C.StakeCredential.from_scripthash(
            C.Ed25519KeyHash.from_bytes(
              Buffer.from(detailedAddress.stakeKeyHash, 'hex')
            )
          );

    this.txBuilder.add_certificate(
      C.Certificate.new_stake_registration(C.StakeRegistration.new(credential))
    );
    return this;
  }

  deregisterStake(rewardAddress: RewardAddress, redeemer?: Redeemer) {
    const detailedAddress = getAddressDetails(rewardAddress);
    if (detailedAddress.type !== 'Reward' || !detailedAddress.stakeKeyHash)
      throw new Error('Not a reward address provided');
    const credential =
      detailedAddress.credentialType === 'Key'
        ? C.StakeCredential.from_keyhash(
            C.Ed25519KeyHash.from_bytes(
              Buffer.from(detailedAddress.stakeKeyHash, 'hex')
            )
          )
        : C.StakeCredential.from_scripthash(
            C.Ed25519KeyHash.from_bytes(
              Buffer.from(detailedAddress.stakeKeyHash, 'hex')
            )
          );

    this.txBuilder.add_certificate(
      C.Certificate.new_stake_deregistration(
        C.StakeDeregistration.new(credential)
      ),
      (redeemer as undefined) &&
        C.ScriptWitness.new_plutus_witness(
          C.PlutusWitness.new(
            C.PlutusData.from_bytes(Buffer.from(redeemer!, 'hex'))
          )
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
          C.PlutusWitness.new(
            C.PlutusData.from_bytes(Buffer.from(redeemer!, 'hex'))
          )
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
    const addressDetailed = getAddressDetails(address);

    if (!addressDetailed.paymentKeyHash || !addressDetailed.stakeKeyHash)
      throw new Error('Not a valid address.');

    const keyHash =
      addressDetailed.type === 'Reward'
        ? addressDetailed.stakeKeyHash
        : addressDetailed.paymentKeyHash;

    this.txBuilder.add_required_signer(
      C.Ed25519KeyHash.from_bytes(Buffer.from(keyHash, 'hex'))
    );
    return this;
  }

  validFrom(unixTime: UnixTime) {
    const slot =
      Lucid.network === 'Mainnet'
        ? unixTimeToSlot(unixTime)
        : unixTimeToSlotTestnet(unixTime);
    this.txBuilder.set_validity_start_interval(
      C.BigNum.from_str(slot.toString())
    );
    return this;
  }

  validTo(unixTime: UnixTime) {
    const slot =
      Lucid.network === 'Mainnet'
        ? unixTimeToSlot(unixTime)
        : unixTimeToSlotTestnet(unixTime);
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
   *
   */
  attachMetadataWithConversion(label: Label, metadata: Json) {
    this.txBuilder.add_json_metadatum_with_schema(
      C.BigNum.from_str(label.toString()),
      JSON.stringify(metadata),
      C.MetadataJsonSchema.BasicConversions
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

  async complete() {
    const utxos = await Lucid.wallet.getUtxosCore();
    if (this.txBuilder.redeemers()!?.len() > 0) {
      const collateral = await Lucid.wallet.getCollateralCore();
      if (collateral.length <= 0) throw new Error('No collateral UTxO found.');
      // 2 collateral utxos should be more than sufficient
      collateral.slice(0, 2).forEach(utxo => {
        this.txBuilder.add_collateral(utxo.output().address(), utxo.input());
      });
    }

    this.txBuilder.add_inputs_from(utxos);
    // TODO: allow change outputs to include a datum (in case address is a plutus script)
    this.txBuilder.balance(C.Address.from_bech32(Lucid.wallet.address));

    return new TxComplete(await this.txBuilder.construct());
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
      C.NativeScript.from_bytes(Buffer.from(script.script, 'hex'))
    );
  }
  if (script.type === 'Plutus') {
    tx.txBuilder.add_plutus_script(
      C.PlutusScript.from_bytes(Buffer.from(script.script, 'hex'))
    );
  }
};
