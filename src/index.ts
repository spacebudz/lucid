import {
  TransactionBuilder,
  TransactionBuilderConfig,
  Transaction,
  TransactionWitnessSetBuilder,
} from '../custom_modules/cardano-multiplatform-lib-browser';
import { S } from './core';
import { costModel } from './costmodel';
import {
  Address,
  Assets,
  CertificateValidator,
  Datum,
  Json,
  Label,
  Lovelace,
  MintingPolicy,
  Network,
  PoolId,
  PrivateKey,
  ProtocolParameters,
  ProviderSchema,
  Redeemer,
  RewardAddress,
  Slot,
  SpendingValidator,
  TxHash,
  Unit,
  UnixTime,
  UTxO,
  Wallet,
  WalletProvider,
  WithdrawalValidator,
} from './types';
import {
  utxoToCSL,
  CSLToUtxo,
  assetsToValue,
  getAddressDetails,
  unixTimeToSlot,
  unixTimeToSlotTestnet,
} from './utils';

if (typeof window === 'undefined') {
  const fetch = await import('node-fetch');
  // @ts-ignore
  global.fetch = fetch.default;
  // @ts-ignore
  global.Headers = fetch.Headers;
  // @ts-ignore
  global.Request = fetch.Request;
  // @ts-ignore
  global.Response = fetch.Response;
}

export type Provider = Blockfrost; // more providers can be added here

export class Blockfrost implements ProviderSchema {
  url: string;
  projectId: string;
  constructor(url: string, projectId: string) {
    this.url = url;
    this.projectId = projectId;
  }

  async getProtocolParameters(): Promise<ProtocolParameters> {
    const result = await fetch(`${this.url}/epochs/latest/parameters`, {
      headers: { project_id: this.projectId },
    }).then((res) => res.json());

    return {
      minFeeA: parseInt(result.min_fee_a),
      minFeeB: parseInt(result.min_fee_b),
      maxTxSize: parseInt(result.max_tx_size),
      maxValSize: parseInt(result.max_val_size),
      keyDeposit: BigInt(result.key_deposit),
      poolDeposit: BigInt(result.pool_deposit),
      priceMem: parseFloat(result.price_mem),
      priceStep: parseFloat(result.price_step),
      coinsPerUtxoWord: BigInt(result.coins_per_utxo_word),
    };
  }
  async getCurrentSlot(): Promise<Slot> {
    return await fetch(`${this.url}/blocks/latest`, {
      headers: { project_id: this.projectId },
    })
      .then((res) => res.json())
      .then((res) => parseInt(res.slot));
  }

  async getUtxos(address: string): Promise<UTxO[]> {
    let result = [];
    let page = 1;
    /*eslint no-constant-condition: ["error", { "checkLoops": false }]*/
    while (true) {
      let pageResult = await fetch(
        `${this.url}/addresses/${address}/utxos?page=${page}`,
        { headers: { project_id: this.projectId } },
      ).then((res) => res.json());
      if (pageResult.error) {
        if ((result as any).status_code === 400) return [];
        else if ((result as any).status_code === 500) return [];
        else {
          pageResult = [];
        }
      }
      result = result.concat(pageResult);
      if (pageResult.length <= 0) break;
      page++;
    }
    return result.map((r) => ({
      txHash: r.tx_hash,
      outputIndex: r.output_index,
      assets: (() => {
        const a = {};
        r.amount.forEach((am: any) => {
          a[am.unit] = BigInt(am.quantity);
        });
        return a;
      })(),
      address,
      datumHash: r.data_hash,
    }));
  }

  async getUtxosWithUnit(address: Address, unit: Unit): Promise<UTxO[]> {
    let result = [];
    let page = 1;
    while (true) {
      let pageResult = await fetch(
        `${this.url}/addresses/${address}/utxos/${unit}?page=${page}`,
        { headers: { project_id: this.projectId } },
      ).then((res) => res.json());
      if (pageResult.error) {
        if ((result as any).status_code === 400) return [];
        else if ((result as any).status_code === 500) return [];
        else {
          pageResult = [];
        }
      }
      result = result.concat(pageResult);
      if (pageResult.length <= 0) break;
      page++;
    }
    return result.map((r) => ({
      txHash: r.tx_hash,
      outputIndex: r.output_index,
      assets: (() => {
        const a = {};
        return r.amount.forEach((am) => {
          a[am.unit] = BigInt(am.quantity);
        });
      })(),
      address,
      datumHash: r.data_hash,
    }));
  }

  async awaitTx(txHash: TxHash): Promise<boolean> {
    return new Promise((res, _) => {
      const confirmation = setInterval(async () => {
        const isConfirmed = await fetch(`${this.url}/txs/${txHash}`, {
          headers: { project_id: this.projectId },
        }).then((res) => res.json());
        if (isConfirmed && !isConfirmed.error) {
          clearInterval(confirmation);
          res(true);
          return;
        }
      }, 3000);
    });
  }

  async submitTx(tx: Transaction): Promise<TxHash> {
    const result = await fetch(`${this.url}/tx/submit`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/cbor',
        project_id: this.projectId,
      },
      body: tx.to_bytes(),
    }).then((res) => res.json());
    if (!result || result.error) {
      if (result?.status_code === 400) throw new Error(result.message);
      else throw new Error('Could not submit transaction.');
    }
    return result;
  }
}

export class Lucid {
  static txBuilderConfig: TransactionBuilderConfig;
  static wallet: Wallet;
  static provider: Provider;
  static network: Network;

  static async initialize(network: Network, provider: Provider) {
    this.provider = provider;
    this.network = network;
    const protocolParameters = await provider.getProtocolParameters();
    this.txBuilderConfig = S.TransactionBuilderConfigBuilder.new()
      .coins_per_utxo_word(
        S.BigNum.from_str(protocolParameters.coinsPerUtxoWord.toString()),
      )
      .fee_algo(
        S.LinearFee.new(
          S.BigNum.from_str(protocolParameters.minFeeA.toString()),
          S.BigNum.from_str(protocolParameters.minFeeB.toString()),
        ),
      )
      .key_deposit(S.BigNum.from_str(protocolParameters.keyDeposit.toString()))
      .pool_deposit(
        S.BigNum.from_str(protocolParameters.poolDeposit.toString()),
      )
      .max_tx_size(protocolParameters.maxTxSize)
      .max_value_size(protocolParameters.maxValSize)
      .ex_unit_prices(
        S.ExUnitPrices.from_float(
          protocolParameters.priceMem,
          protocolParameters.priceStep,
        ),
      )
      .blockfrost(
        S.Blockfrost.new(
          provider.url + '/utils/txs/evaluate',
          provider.projectId,
        ),
      )
      .costmdls(costModel.plutusV1())
      .prefer_pure_change(true)
      .build();
  }

  static async currentSlot(): Promise<Slot> {
    return this.provider.getCurrentSlot();
  }

  static async utxosAt(address: Address): Promise<UTxO[]> {
    return this.provider.getUtxos(address);
  }

  static async utxosAtWithUnit(address: Address, unit: Unit): Promise<UTxO[]> {
    return this.provider.getUtxosWithUnit(address, unit);
  }

  static async awaitTx(txHash: TxHash): Promise<boolean> {
    return this.provider.awaitTx(txHash);
  }

  /**
   * Cardano Private key in bech32; not the BIP32 private key or any key that is not fully derived
   */
  static async selectWalletFromPrivateKey(privateKey: PrivateKey) {
    const priv = S.PrivateKey.from_bech32(privateKey);
    const pubKeyHash = priv.to_public().hash();
    const address = S.EnterpriseAddress.new(
      this.network == 'Mainnet' ? 1 : 0,
      S.StakeCredential.from_keyhash(pubKeyHash),
    )
      .to_address()
      .to_bech32();
    this.wallet = {
      address,
      getCollateral: async () => {
        const utxos = await Lucid.utxosAt(address);
        return utxos.filter(
          (utxo) =>
            Object.keys(utxo.assets).length === 1 &&
            utxo.assets.lovelace >= 5000000n,
        );
      },
      getCollateralRaw: async () => {
        const utxos = await Lucid.utxosAt(address);
        return utxos
          .filter(
            (utxo) =>
              Object.keys(utxo.assets).length === 1 &&
              utxo.assets.lovelace >= 5000000n,
          )
          .map((utxo) => utxoToCSL(utxo));
      },
      getUtxos: async () => {
        return await Lucid.utxosAt(address);
      },
      getUtxosRaw: async () => {
        const utxos = await Lucid.utxosAt(address);
        const rawUtxos = S.TransactionUnspentOutputs.new();
        utxos.forEach((utxo) => {
          rawUtxos.add(utxoToCSL(utxo));
        });
        return rawUtxos;
      },
      signTx: async (tx: Transaction) => {
        const witness = S.make_vkey_witness(
          S.hash_transaction(tx.body()),
          priv,
        );
        const txWitnessSetBuilder = S.TransactionWitnessSetBuilder.new();
        txWitnessSetBuilder.add_vkey(witness);
        return txWitnessSetBuilder.build();
      },
      submitTx: async (tx: Transaction) => {
        return await Lucid.provider.submitTx(tx);
      },
    };
  }

  static async selectWallet(walletProvider: WalletProvider) {
    if (!window?.cardano?.[walletProvider]) {
      throw new Error('Wallet not installed or not in a browser environment');
    }
    const api = await window.cardano[walletProvider].enable();

    const address = S.Address.from_bytes(
      Buffer.from((await api.getUsedAddresses())[0], 'hex'),
    ).to_bech32();

    const rewardAddressHex = (await api.getRewardAddresses())[0];
    const rewardAddress =
      rewardAddressHex &&
      S.RewardAddress.from_address(
        S.Address.from_bytes(Buffer.from(rewardAddressHex, 'hex')),
      )
        .to_address()
        .to_bech32();

    this.wallet = {
      address,
      rewardAddress,
      getCollateral: async () => {
        const utxos = (await api.experimental.getCollateral()).map((utxo) => {
          const parsedUtxo = S.TransactionUnspentOutput.from_bytes(
            Buffer.from(utxo, 'hex'),
          );
          return CSLToUtxo(parsedUtxo);
        });
        return utxos;
      },
      getCollateralRaw: async () => {
        const utxos = (await api.experimental.getCollateral()).map((utxo) => {
          return S.TransactionUnspentOutput.from_bytes(
            Buffer.from(utxo, 'hex'),
          );
        });
        return utxos;
      },
      getUtxos: async () => {
        const utxos = (await api.getUtxos()).map((utxo) => {
          const parsedUtxo = S.TransactionUnspentOutput.from_bytes(
            Buffer.from(utxo, 'hex'),
          );
          return CSLToUtxo(parsedUtxo);
        });
        return utxos;
      },
      getUtxosRaw: async () => {
        const utxos = S.TransactionUnspentOutputs.new();
        (await api.getUtxos()).forEach((utxo) => {
          utxos.add(
            S.TransactionUnspentOutput.from_bytes(Buffer.from(utxo, 'hex')),
          );
        });
        return utxos;
      },
      signTx: async (tx: Transaction) => {
        const witnessSet = await api.signTx(
          Buffer.from(tx.to_bytes()).toString('hex'),
          true,
        );
        return S.TransactionWitnessSet.from_bytes(
          Buffer.from(witnessSet, 'hex'),
        );
      },
      submitTx: async (tx: Transaction) => {
        const txHash = await api.submitTx(
          Buffer.from(tx.to_bytes()).toString('hex'),
        );
        return txHash;
      },
    };
  }
}

export class Tx {
  txBuilder: TransactionBuilder;

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
      const cslUtxo = utxoToCSL(utxoCloned);
      this.txBuilder.add_input(
        S.Address.from_bech32(utxo.address),
        cslUtxo.input(),
        cslUtxo.output().amount(),
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
    const utxos = await Lucid.wallet.getUtxosRaw();
    if (this.txBuilder.redeemers()?.len() > 0) {
      const collateral = await Lucid.wallet.getCollateralRaw();
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

export class TxComplete {
  txComplete: Transaction;
  witnessSetBuilder: TransactionWitnessSetBuilder;
  constructor(tx: Transaction) {
    this.txComplete = tx;
    this.witnessSetBuilder = S.TransactionWitnessSetBuilder.new();
    this.witnessSetBuilder.add_existing(this.txComplete.witness_set());
  }
  async sign() {
    const witness = await Lucid.wallet.signTx(this.txComplete);
    this.witnessSetBuilder.add_existing(witness);
    return this;
  }

  /** Add an extra signature from a private key */
  signWithPrivateKey(privateKey: PrivateKey) {
    const priv = S.PrivateKey.from_bech32(privateKey);
    const witness = S.make_vkey_witness(
      S.hash_transaction(this.txComplete.body()),
      priv,
    );
    this.witnessSetBuilder.add_vkey(witness);
  }

  complete() {
    const signedTx = S.Transaction.new(
      this.txComplete.body(),
      this.witnessSetBuilder.build(),
      this.txComplete.auxiliary_data(),
    );
    return new TxSigned(signedTx);
  }
}

export class TxSigned {
  txSigned: Transaction;
  constructor(tx: Transaction) {
    this.txSigned = tx;
  }

  async submit(): Promise<TxHash> {
    return await Lucid.wallet.submitTx(this.txSigned);
  }
}

export * from './types';
export * from './utils';
export * from './core';
