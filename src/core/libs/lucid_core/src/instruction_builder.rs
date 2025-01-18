/// The `InstructionBuilder` struct is responsible for constructing and managing
/// instructions for a transaction. It provides methods to add inputs, outputs,
/// certificates, minting, withdrawals, and other transaction components. The
/// builder ensures that the transaction is balanced, fees are calculated, and
/// redeemers are evaluated.
use super::addresses::Network;
use super::error::CoreResult;
use super::hasher::Hasher;
use super::{
    addresses::{Addresses, Credential},
    codec::{
        Assets, AuxMetadata, Certificate, Delegation, PoolRegistration, PoolRetirement, Script,
        Utxo, Utxos, Withdrawal,
    },
    instruction_signer::InstructionSigner,
};
use crate::error::CoreError;
use crate::utils::Utils;
use num_integer::Roots;
use pallas_addresses::Address;
use pallas_primitives::{
    alonzo::PostAlonzoAuxiliaryData,
    conway::{
        AuxiliaryData, CostModels, Hash, Language, Mint, MintedTx, NativeScript, Redeemer,
        RedeemerTag, Redeemers, RedeemersKey, RedeemersValue, TransactionBody, TransactionInput,
        Tx, VKeyWitness, WitnessSet,
    },
    AddrKeyhash, ExUnits, Fragment, KeyValuePairs, Metadatum, NonEmptyKeyValuePairs, NonEmptySet,
    Nullable, PlutusData, PlutusScript, RewardAccount, ScriptHash,
};
use pallas_traverse::ComputeHash;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet, HashSet},
};
use std::{collections::HashMap, str::FromStr};
use tsify::Tsify;
use uplc::tx::eval_phase_two;
use uplc::{
    machine::cost_model::ExBudget,
    tx::{ResolvedInput, SlotConfig},
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct InstructionBuilder {
    network: Network,
    protocol_parameters: RelevantProtocolParameters,
    slot_config: SlotConfig,

    inputs: BTreeSet<BuilderInput>,
    read_inputs: Option<BTreeSet<BuilderInput>>,
    outputs: Vec<Utxo>,
    certificates: Option<Vec<BuilderCertificate>>,
    mint: Option<BTreeMap<ScriptHash, BuilderMint>>,
    withdrawals: Option<BTreeMap<String, BuilderWithdrawal>>,
    data: Option<BTreeSet<PlutusData>>,
    signers: Option<BTreeSet<AddrKeyhash>>,
    aux_metadata: Option<BTreeMap<u64, Either<AuxMetadata<false>, AuxMetadata<true>>>>,
    scripts: Option<HashMap<ScriptHash, Script>>,
    redeemers: Option<BTreeSet<BuilderRedeemer>>,
    collateral: Option<BTreeSet<BuilderInput>>,
    collateral_return: Option<Utxo>,
    total_collateral: Option<u64>,
    script_data_hash: Option<Hash<32>>,
    fee: u64,

    tx: Tx,
    total_script_ref_size: Option<u64>,
    selection: BTreeSet<BuilderInput>,
    without_coin_selection: bool,
    change: Change,
    change_outputs: Vec<Utxo>,
    change_data: Option<PlutusData>,
    found_languages: HashMap<ScriptHash, Language>,
    used_keys: HashSet<AddrKeyhash>,
    used_scripts: HashSet<ScriptHash>,
    implicit_input: Assets,
    implicit_output: Assets,
}

#[wasm_bindgen]
impl InstructionBuilder {
    #[wasm_bindgen(constructor)]
    pub fn new(
        network: Network,
        pp: RelevantProtocolParameters,
        selection: Utxos,
        change: Change,
    ) -> Self {
        let selection: BTreeSet<BuilderInput> = selection
            .0
            .iter()
            .map(|utxo| BuilderInput {
                utxo: utxo.clone(),
                redeemer: None,
            })
            .collect();

        let slot_config = match network {
            Network::Mainnet => SlotConfig {
                zero_time: 1596059091000,
                zero_slot: 4492800,
                slot_length: 1000,
            },
            Network::Preprod => SlotConfig {
                zero_time: 1655769600000,
                zero_slot: 86400,
                slot_length: 1000,
            },
            Network::Preview => SlotConfig {
                zero_time: 1666656000000,
                zero_slot: 0,
                slot_length: 1000,
            },
            Network::Emulator(zero_time) => SlotConfig {
                zero_time,
                zero_slot: 0,
                slot_length: 1000,
            },
        };

        Self {
            network,
            protocol_parameters: pp,
            slot_config,
            inputs: BTreeSet::new(),
            read_inputs: None,
            outputs: Vec::new(),
            mint: None,
            certificates: None,
            withdrawals: None,
            data: None,
            signers: None,
            aux_metadata: None,
            scripts: None,
            redeemers: None,
            collateral: None,
            collateral_return: None,
            total_collateral: None,
            script_data_hash: None,
            fee: 0,
            tx: Tx::empty(),
            total_script_ref_size: None,
            selection,
            without_coin_selection: false,
            change,
            change_outputs: Vec::new(),
            change_data: None,
            found_languages: HashMap::new(),
            used_keys: HashSet::new(),
            used_scripts: HashSet::new(),
            implicit_input: Assets::new(),
            implicit_output: Assets::new(),
        }
    }

    pub fn commit(mut self, instructions: Instructions) -> CoreResult<InstructionSigner> {
        self.process_instructions(instructions)?;

        self.pre_assemble()?;

        self.collect_redeemers()?;

        self.assemble()?;

        self.coin_selection()?;

        self.assemble()?;

        self.balance()?;

        self.collect_redeemers()?;

        self.assemble()?;

        self.evaluate_redeemers()?;

        self.assemble()?;

        self.adjust_fee()?;

        let tx = self.tx.encode_fragment().unwrap();

        if tx.len() as u32 > self.protocol_parameters.max_tx_size {
            return Err(CoreError::msg(format!(
                "Maximum transaction size: expected max {}, found {}",
                self.protocol_parameters.max_tx_size,
                tx.len()
            )));
        }

        Ok(InstructionSigner::new(self.tx, self.used_keys)).into()
    }

    fn process_instructions(&mut self, instructions: Instructions) -> CoreResult<()> {
        fn validate_network(address: &str, expected_network: &Network) -> CoreResult<()> {
            let address_network = pallas_addresses::Address::from_bech32(&address)
                .map_err(CoreError::msg)?
                .network()
                .unwrap();

            if matches!(
                (address_network, expected_network),
                (
                    pallas_addresses::Network::Mainnet,
                    Network::Preprod | Network::Preview | Network::Emulator(_)
                ) | (
                    pallas_addresses::Network::Testnet | pallas_addresses::Network::Other(_),
                    Network::Mainnet
                )
            ) {
                Err(CoreError::AddressNetworkMismatch(
                    format!("{:?}", expected_network.clone()),
                    format!("{:?}", address_network),
                )
                .to_msg())
            } else {
                Ok(())
            }
        }

        fn unix_time_to_slots(unix_time: u64, slot_config: &SlotConfig) -> u64 {
            let time_passed = unix_time - slot_config.zero_time;
            let slots_passed = time_passed / slot_config.slot_length as u64;
            slots_passed + slot_config.zero_slot
        }

        for instruction in instructions {
            match instruction {
                Instruction::CollectFrom { utxos, redeemer } => {
                    for utxo in utxos {
                        self.add_input(utxo, redeemer.clone())?;
                    }
                }
                Instruction::ReadFrom { utxos } => {
                    for utxo in utxos {
                        match &utxo.script_ref {
                            Some(script) => {
                                self.add_language_or_keys(script)?;
                                self.increment_total_script_ref_size(script)?;
                            }
                            _ => (),
                        };
                        self.read_inputs
                            .get_or_insert_with(BTreeSet::new)
                            .insert(BuilderInput {
                                utxo,
                                redeemer: None,
                            });
                    }
                }
                Instruction::Mint { assets, redeemer } => {
                    let policy_id = assets.get_policy_id()?;

                    self.used_scripts.insert(policy_id);

                    for (unit, quantity) in assets.iter() {
                        if *quantity > 0 {
                            self.implicit_input += Assets::from_unit(unit.clone(), *quantity);
                        } else {
                            self.implicit_output += Assets::from_unit(unit.clone(), -*quantity);
                        }
                    }

                    let mint = self.mint.get_or_insert_with(BTreeMap::new);

                    let entry = mint.entry(policy_id).or_insert_with(|| BuilderMint {
                        assets: Assets::new(),
                        redeemer,
                    });

                    entry.assets += assets;

                    if entry.assets.is_empty() {
                        mint.remove(&policy_id);
                    }

                    if mint.is_empty() {
                        self.mint = None;
                    }
                }
                Instruction::PayTo {
                    assets,
                    address,
                    datum_variant,
                    script_ref,
                } => {
                    validate_network(&address, &self.network)?;

                    let (datum_hash, datum) = match datum_variant {
                        Some(DatumVariant::AsHash(data)) => {
                            self.add_data(&data)?;
                            (
                                Some(
                                    PlutusData::decode_fragment(
                                        &hex::decode(data).map_err(CoreError::msg)?,
                                    )
                                    .map_err(CoreError::msg)?
                                    .compute_hash()
                                    .to_string(),
                                ),
                                None,
                            )
                        }
                        Some(DatumVariant::Hash(hash)) => (Some(hash), None),
                        Some(DatumVariant::Inline(data)) => (None, Some(data)),
                        _ => (None, None),
                    };

                    let mut utxo =
                        Utxo::from_output(address, assets, datum_hash, datum, script_ref);
                    utxo.required_lovelace_mut(self.protocol_parameters.coins_per_utxo_byte);

                    if utxo.size_assets() > self.protocol_parameters.max_val_size {
                        return Err(CoreError::MaximumAssetsSize(
                            self.protocol_parameters.max_val_size,
                            utxo.size_assets(),
                        )
                        .to_msg());
                    }

                    self.outputs.push(utxo);
                }
                Instruction::PayToContract {
                    assets,
                    address,
                    datum_variant,
                    script_ref,
                } => {
                    validate_network(&address, &self.network)?;

                    let (datum_hash, datum) = match datum_variant {
                        DatumVariant::AsHash(data) => {
                            self.add_data(&data)?;
                            (
                                Some(
                                    PlutusData::decode_fragment(
                                        &hex::decode(data).map_err(CoreError::msg)?,
                                    )
                                    .map_err(CoreError::msg)?
                                    .compute_hash()
                                    .to_string(),
                                ),
                                None,
                            )
                        }
                        DatumVariant::Hash(hash) => (Some(hash), None),
                        DatumVariant::Inline(data) => (None, Some(data)),
                    };

                    let mut utxo =
                        Utxo::from_output(address, assets, datum_hash, datum, script_ref);
                    utxo.required_lovelace_mut(self.protocol_parameters.coins_per_utxo_byte);

                    if utxo.size_assets() > self.protocol_parameters.max_val_size {
                        return Err(CoreError::MaximumAssetsSize(
                            self.protocol_parameters.max_val_size,
                            utxo.size_assets(),
                        )
                        .to_msg());
                    }

                    self.outputs.push(utxo);
                }
                Instruction::DelegateTo {
                    delegation,
                    redeemer,
                } => {
                    validate_network(&delegation.reward_address, &self.network)?;

                    match Addresses::reward_address_to_credential(&delegation.reward_address)? {
                        Credential::Key { hash } => {
                            self.used_keys.insert(hash.parse().map_err(CoreError::msg)?);
                        }
                        Credential::Script { hash } => {
                            self.used_scripts
                                .insert(hash.parse().map_err(CoreError::msg)?);
                        }
                    };

                    self.certificates
                        .get_or_insert_with(Vec::new)
                        .push(BuilderCertificate {
                            certificate: Certificate::Delegation(delegation),
                            redeemer,
                        })
                }
                Instruction::RegisterStake { reward_address } => {
                    validate_network(&reward_address, &self.network)?;

                    self.implicit_output +=
                        Assets::from_lovelace(self.protocol_parameters.key_deposit);

                    self.certificates
                        .get_or_insert_with(Vec::new)
                        .push(BuilderCertificate {
                            certificate: Certificate::StakeRegistration { reward_address },
                            redeemer: None,
                        })
                }
                Instruction::DeregisterStake {
                    reward_address,
                    redeemer,
                } => {
                    validate_network(&reward_address, &self.network)?;

                    match Addresses::reward_address_to_credential(&reward_address)? {
                        Credential::Key { hash } => {
                            self.used_keys.insert(hash.parse().map_err(CoreError::msg)?);
                        }
                        Credential::Script { hash } => {
                            self.used_scripts
                                .insert(hash.parse().map_err(CoreError::msg)?);
                        }
                    };

                    self.implicit_input +=
                        Assets::from_lovelace(self.protocol_parameters.key_deposit);

                    self.certificates
                        .get_or_insert_with(Vec::new)
                        .push(BuilderCertificate {
                            certificate: Certificate::StakeDeregistration { reward_address },
                            redeemer,
                        })
                }
                Instruction::RegisterPool(pool_registration) => {
                    validate_network(&pool_registration.reward_address, &self.network)?;

                    for owner in &pool_registration.owners {
                        match Addresses::reward_address_to_credential(owner)? {
                            Credential::Key { hash } => {
                                self.used_keys.insert(hash.parse().map_err(CoreError::msg)?);
                            }
                            _ => {
                                return Err(CoreError::msg(
                                    "Only key addresses allowed as pool owners",
                                ))
                            }
                        };
                    }

                    let (_, pool_id_raw) =
                        bech32::decode(&pool_registration.pool_id).map_err(CoreError::msg)?;
                    self.used_keys.insert(pool_id_raw.as_slice().into());

                    self.implicit_output +=
                        Assets::from_lovelace(self.protocol_parameters.pool_deposit);

                    self.certificates
                        .get_or_insert_with(Vec::new)
                        .push(BuilderCertificate {
                            certificate: Certificate::PoolRegistration(pool_registration),
                            redeemer: None,
                        })
                }
                Instruction::UpdatePool(pool_registration) => {
                    validate_network(&pool_registration.reward_address, &self.network)?;

                    for owner in &pool_registration.owners {
                        match Addresses::reward_address_to_credential(owner)? {
                            Credential::Key { hash } => {
                                self.used_keys.insert(hash.parse().map_err(CoreError::msg)?);
                            }
                            _ => {
                                return Err(CoreError::msg(
                                    "Only key addresses allowed as pool owners",
                                ))
                            }
                        };
                    }

                    let (_, pool_id_raw) =
                        bech32::decode(&pool_registration.pool_id).map_err(CoreError::msg)?;
                    self.used_keys.insert(pool_id_raw.as_slice().into());

                    self.certificates
                        .get_or_insert_with(Vec::new)
                        .push(BuilderCertificate {
                            certificate: Certificate::PoolRegistration(pool_registration),
                            redeemer: None,
                        })
                }
                Instruction::RetirePool(pool_retirement) => {
                    let (_, pool_id_raw) =
                        bech32::decode(&pool_retirement.pool_id).map_err(CoreError::msg)?;
                    self.used_keys.insert(pool_id_raw.as_slice().into());

                    self.implicit_input +=
                        Assets::from_lovelace(self.protocol_parameters.pool_deposit);

                    self.certificates
                        .get_or_insert_with(Vec::new)
                        .push(BuilderCertificate {
                            certificate: Certificate::PoolRetirement(pool_retirement),
                            redeemer: None,
                        })
                }
                Instruction::Withdraw {
                    withdrawal,
                    redeemer,
                } => {
                    validate_network(&withdrawal.reward_address, &self.network)?;

                    match Addresses::reward_address_to_credential(&withdrawal.reward_address)? {
                        Credential::Key { hash } => {
                            self.used_keys.insert(hash.parse().map_err(CoreError::msg)?);
                        }
                        Credential::Script { hash } => {
                            self.used_scripts
                                .insert(hash.parse().map_err(CoreError::msg)?);
                        }
                    };

                    self.implicit_input += Assets::from_lovelace(withdrawal.amount);

                    self.withdrawals.get_or_insert_with(BTreeMap::new).insert(
                        withdrawal.reward_address.clone(),
                        BuilderWithdrawal {
                            withdrawal,
                            redeemer,
                        },
                    );
                }
                Instruction::AddSigner { key_hash } => {
                    self.used_keys
                        .insert(key_hash.parse().map_err(CoreError::msg)?);
                    self.signers
                        .get_or_insert_with(BTreeSet::new)
                        .insert(AddrKeyhash::from_str(&key_hash).map_err(CoreError::msg)?);
                }
                Instruction::AddNetworkId { id } => {
                    self.tx.transaction_body.network_id = Some(
                        id.try_into()
                            .map_err(|_| CoreError::msg("Casting network id failed"))?,
                    )
                }
                Instruction::ValidFrom { unix_time } => {
                    let slots = unix_time_to_slots(unix_time, &self.slot_config);
                    self.tx.transaction_body.validity_interval_start = Some(slots);
                }
                Instruction::ValidTo { unix_time } => {
                    let slots = unix_time_to_slots(unix_time, &self.slot_config);
                    self.tx.transaction_body.ttl = Some(slots);
                }
                Instruction::AttachMetadata {
                    metadata: (label, value),
                } => {
                    self.aux_metadata
                        .get_or_insert_with(BTreeMap::new)
                        .insert(label, Either::Left(value));
                }
                Instruction::AttachMetadataWithConversion {
                    metadata: (label, value),
                } => {
                    self.aux_metadata
                        .get_or_insert_with(BTreeMap::new)
                        .insert(label, Either::Right(value));
                }
                Instruction::AttachScript { script } => {
                    let script = script.try_double_cbor()?;

                    self.add_language_or_keys(&script)?;

                    let hash: ScriptHash = Hasher::hash_script(script.clone())?.parse().unwrap();

                    self.scripts
                        .get_or_insert_with(HashMap::new)
                        .insert(hash, script);
                }
                Instruction::WithChangeTo(change) => {
                    self.change = change;
                }
                Instruction::WithoutCoinSelection => {
                    self.without_coin_selection = true;
                }
            }
        }
        Ok(())
    }

    fn add_input(&mut self, utxo: Utxo, redeemer: Option<String>) -> CoreResult<()> {
        if let Some(_) = redeemer {
            match (&utxo.datum_hash, &utxo.datum) {
                (Some(_), Some(data)) => self.add_data(data)?,
                _ => (),
            };
        }
        match &utxo.script_ref {
            Some(script) => {
                self.add_language_or_keys(script)?;
                self.increment_total_script_ref_size(script)?;
            }
            _ => (),
        };
        match Addresses::address_to_credential(&utxo.address)? {
            Credential::Key { hash } => {
                self.used_keys.insert(hash.parse().map_err(CoreError::msg)?)
            }
            Credential::Script { hash } => self
                .used_scripts
                .insert(hash.parse().map_err(CoreError::msg)?),
        };
        self.inputs.insert(BuilderInput { utxo, redeemer });
        Ok(())
    }

    fn add_collateral(&mut self, utxo: Utxo) -> CoreResult<()> {
        match Addresses::address_to_credential(&utxo.address)? {
            Credential::Key { hash } => {
                self.used_keys.insert(hash.parse().map_err(CoreError::msg)?)
            }
            Credential::Script { .. } => {
                return Err(CoreError::msg(
                    "Only collateral from key addresses is permitted",
                ))
            }
        };

        self.collateral
            .get_or_insert_with(BTreeSet::new)
            .insert(BuilderInput {
                utxo,
                redeemer: None,
            });
        Ok(())
    }

    fn adjust_fee(&mut self) -> CoreResult<u64> {
        let mut old_fee = self.fee;
        let mut new_fee = {
            self.assemble()?;
            self.calculate_fee()
        };

        if old_fee >= new_fee {
            return Ok(old_fee);
        }

        // we run in a loop to make 100% sure that adding to fee field and subtracting from output does not change bytes length
        // if it does we loop again and adjust accordingly
        while old_fee < new_fee {
            if self.change_outputs.len() <= 0 {
                return Err(CoreError::NotEnoughLovelaceForFee.to_msg());
            }

            let index = self.change_outputs.len() - 1;

            let change_output = &mut self.change_outputs[index];

            // assumes new fee is always higher than previously set fee
            let amount_to_subtract = new_fee - old_fee;

            change_output.assets -= Assets::from_lovelace(amount_to_subtract);

            if change_output.assets.get_lovelace()
                < change_output.required_lovelace(self.protocol_parameters.coins_per_utxo_byte)
                && change_output.assets.contains_other()
            {
                return Err(CoreError::NotEnoughLovelaceForFee.to_msg());
            }
            // burn whole output
            if change_output.assets.get_lovelace()
                < change_output.required_lovelace(self.protocol_parameters.coins_per_utxo_byte)
            {
                let fee = old_fee + change_output.assets.get_lovelace() + amount_to_subtract;
                self.fee = fee;
                self.change_outputs.pop().unwrap();
                return Ok(fee);
            }

            self.fee = new_fee;
            old_fee = new_fee;
            new_fee = {
                self.assemble()?;
                self.calculate_fee()
            };
        }

        Ok(new_fee)
    }

    fn evaluate_redeemers(&mut self) -> CoreResult<()> {
        fn to_language_view_encoding(cost_models: &CostModels) -> Vec<u8> {
            let mut language_view = Vec::new();
            let mut encoder = pallas_codec::minicbor::Encoder::new(&mut language_view);
            let cost_models_size = vec![
                &cost_models.plutus_v1,
                &cost_models.plutus_v2,
                &cost_models.plutus_v3,
            ]
            .iter()
            .filter(|c| c.is_some())
            .count();

            encoder.map(cost_models_size as u64).unwrap();

            if let Some(plutus_v1) = &cost_models.plutus_v1 {
                encoder
                    .bytes(&Language::PlutusV1.encode_fragment().unwrap())
                    .unwrap();

                let mut cost_model_buf = Vec::new();
                let mut cost_model_encoder =
                    pallas_codec::minicbor::Encoder::new(&mut cost_model_buf);
                cost_model_encoder.begin_array().unwrap();
                for cost in plutus_v1.iter() {
                    cost_model_encoder.encode(cost).unwrap();
                }
                cost_model_encoder.end().unwrap();
                encoder.bytes(&cost_model_buf).unwrap();
            }

            if let Some(plutus_v2) = &cost_models.plutus_v2 {
                encoder.encode(Language::PlutusV2).unwrap();
                encoder.encode(plutus_v2).unwrap();
            }

            if let Some(plutus_v3) = &cost_models.plutus_v3 {
                encoder.encode(Language::PlutusV3).unwrap();
                encoder.encode(plutus_v3).unwrap();
            }

            language_view
        }

        fn adjust_collateral(builder: &mut InstructionBuilder) -> CoreResult<()> {
            let mut old_total_collateral = (builder.fee
                * builder.protocol_parameters.collateral_percentage as u64)
                .div_ceil(100);
            let mut new_total_collateral = (builder.adjust_fee()?
                * builder.protocol_parameters.collateral_percentage as u64)
                .div_ceil(100);

            if old_total_collateral >= new_total_collateral {
                return Ok(());
            }

            let collateral = builder.collateral.clone().unwrap_or(BTreeSet::new());

            let collateral_assets = collateral
                .iter()
                .fold(Assets::new(), |acc, input| acc + input.utxo.assets.clone());

            while old_total_collateral < new_total_collateral {
                let collateral_return = builder.create_partial_change_output(
                    collateral_assets
                        .clone()
                        .clamped_sub(Assets::from_lovelace(new_total_collateral)),
                );

                // burn whole return output
                if collateral_return.assets.get_lovelace()
                    < collateral_return
                        .required_lovelace(builder.protocol_parameters.coins_per_utxo_byte)
                    && collateral_return.assets.get_lovelace() >= new_total_collateral
                    && !collateral_return.assets.contains_other()
                {
                    new_total_collateral = collateral_return.assets.get_lovelace();
                    builder.total_collateral = Some(new_total_collateral);
                    builder.collateral_return = None;
                } else {
                    if collateral_return.assets.get_lovelace()
                        < collateral_return
                            .required_lovelace(builder.protocol_parameters.coins_per_utxo_byte)
                    {
                        return Err(CoreError::NotEnoughLovelaceForFee.to_msg());
                    }

                    builder.total_collateral = Some(new_total_collateral);

                    builder.collateral_return = Some(collateral_return);
                }

                old_total_collateral = new_total_collateral;
                new_total_collateral = (builder.adjust_fee()?
                    * builder.protocol_parameters.collateral_percentage as u64)
                    .div_ceil(100);
            }

            Ok(())
        }

        if let Some(_) = &self.redeemers {
            let encoded_tx = self.tx.encode_fragment().unwrap();
            let minted_tx = MintedTx::decode_fragment(&encoded_tx).unwrap();
            let utxos: Vec<ResolvedInput> = self
                .inputs
                .union(&self.read_inputs.clone().unwrap_or(BTreeSet::new()))
                .map(|b| {
                    Ok(ResolvedInput {
                        input: b.utxo.clone().try_into()?,
                        output: b.utxo.clone().try_into()?,
                    })
                })
                .collect::<CoreResult<_>>()?;

            let mut used_languages = Vec::new();

            for script_hash in self.used_scripts.iter() {
                if let Some(language) = self.found_languages.get(&script_hash) {
                    used_languages.push(language.clone());
                }
            }

            let cost_models = CostModels {
                plutus_v1: match used_languages.contains(&Language::PlutusV1) {
                    true => Some(
                        self.protocol_parameters
                            .cost_models
                            .get("PlutusV1")
                            .unwrap()
                            .to_vec(),
                    ),
                    _ => None,
                },
                plutus_v2: match used_languages.contains(&Language::PlutusV2) {
                    true => Some(
                        self.protocol_parameters
                            .cost_models
                            .get("PlutusV2")
                            .unwrap()
                            .to_vec(),
                    ),
                    _ => None,
                },
                plutus_v3: match used_languages.contains(&Language::PlutusV3) {
                    true => Some(
                        self.protocol_parameters
                            .cost_models
                            .get("PlutusV3")
                            .unwrap()
                            .to_vec(),
                    ),
                    _ => None,
                },
            };

            let redeemers = eval_phase_two(
                &minted_tx,
                &utxos,
                Some(&cost_models),
                Some(&ExBudget {
                    mem: self.protocol_parameters.max_tx_ex_mem as i64,
                    cpu: self.protocol_parameters.max_tx_ex_steps as i64,
                }),
                &self.slot_config,
                false,
                |_| (),
            )
            .map_err(CoreError::msg)?;

            let builder_redeemers: BTreeSet<BuilderRedeemer> = redeemers
                .iter()
                .map(|r| BuilderRedeemer(r.clone()))
                .collect();

            self.redeemers = Some(builder_redeemers.clone());

            let redeemers = Redeemers::Map(
                NonEmptyKeyValuePairs::from_vec(
                    builder_redeemers.into_iter().map(|r| r.into()).collect(),
                )
                .unwrap(),
            );

            let mut script_data = Vec::new();
            script_data.extend(redeemers.encode_fragment().unwrap());
            if let Some(data) = &self.tx.transaction_witness_set.plutus_data {
                script_data.extend(data.encode_fragment().unwrap());
            }
            script_data.extend(to_language_view_encoding(&cost_models));

            self.script_data_hash = Some(pallas_crypto::hash::Hasher::<256>::hash(
                script_data.as_slice(),
            ));

            let mut available_collateral: Vec<Utxo> =
                self.selection.clone().into_iter().map(|b| b.utxo).collect();
            available_collateral.sort_by(|a, b| {
                self.get_pure_lovelace(&a.assets)
                    .cmp(&self.get_pure_lovelace(&b.assets))
            });

            loop {
                let collateral_count = available_collateral.len() - self.selection.len();

                if collateral_count as u16 >= self.protocol_parameters.max_collateral_inputs {
                    return Err(CoreError::msg(format!(
                        "Reached maximum collateral inputs, {} allowed",
                        self.protocol_parameters.max_collateral_inputs
                    )));
                }

                let utxo = available_collateral
                    .pop()
                    .ok_or(CoreError::msg("Collateral balance insufficient"))?;
                self.add_collateral(utxo)?;

                if adjust_collateral(self).is_ok() {
                    break;
                };
            }
        }
        Ok(())
    }

    fn collect_redeemers(&mut self) -> CoreResult<()> {
        let mut collected = BTreeSet::<BuilderRedeemer>::new();

        for (index, input) in self.inputs.iter().enumerate() {
            if let Some(redeemer) = &input.redeemer {
                collected.insert(BuilderRedeemer(Redeemer {
                    tag: RedeemerTag::Spend,
                    index: index as u32,
                    data: PlutusData::decode_fragment(
                        &hex::decode(redeemer).map_err(CoreError::msg)?,
                    )
                    .map_err(CoreError::msg)?,
                    ex_units: ExUnits { mem: 0, steps: 0 },
                }));
            }
        }

        if let Some(mint) = &self.mint {
            for (index, (_, mint)) in mint.iter().enumerate() {
                if let Some(redeemer) = &mint.redeemer {
                    collected.insert(BuilderRedeemer(Redeemer {
                        tag: RedeemerTag::Mint,
                        index: index as u32,
                        data: PlutusData::decode_fragment(
                            &hex::decode(redeemer).map_err(CoreError::msg)?,
                        )
                        .map_err(CoreError::msg)?,
                        ex_units: ExUnits { mem: 0, steps: 0 },
                    }));
                }
            }
        }

        if let Some(certs) = &self.certificates {
            for (index, cert) in certs.iter().enumerate() {
                if let Some(redeemer) = &cert.redeemer {
                    collected.insert(BuilderRedeemer(Redeemer {
                        tag: RedeemerTag::Cert,
                        index: index as u32,
                        data: PlutusData::decode_fragment(
                            &hex::decode(redeemer).map_err(CoreError::msg)?,
                        )
                        .map_err(CoreError::msg)?,
                        ex_units: ExUnits { mem: 0, steps: 0 },
                    }));
                }
            }
        }

        if let Some(withdrawals) = &self.withdrawals {
            for (index, (_, withdrawal)) in withdrawals.iter().enumerate() {
                if let Some(redeemer) = &withdrawal.redeemer {
                    collected.insert(BuilderRedeemer(Redeemer {
                        tag: RedeemerTag::Reward,
                        index: index as u32,
                        data: PlutusData::decode_fragment(
                            &hex::decode(redeemer).map_err(CoreError::msg)?,
                        )
                        .map_err(CoreError::msg)?,
                        ex_units: ExUnits { mem: 0, steps: 0 },
                    }));
                }
            }
        }

        if collected.len() > 0 {
            self.redeemers = Some(collected)
        }
        Ok(())
    }

    fn create_partial_change_output(&self, change_assets: Assets) -> Utxo {
        Utxo::from_output(
            self.change.address.clone(),
            change_assets,
            match &self.change.datum_variant {
                // AsHash data is added to witness set if change_outpus.len() > 0, checked in self.assemble()
                // unwraping is safe here as we validate DatumVariant prior
                Some(DatumVariant::AsHash(data)) => Some(
                    PlutusData::decode_fragment(&hex::decode(data.clone()).unwrap())
                        .unwrap()
                        .compute_hash()
                        .to_string(),
                ),
                Some(DatumVariant::Hash(hash)) => Some(hash.clone()),
                _ => None,
            },
            match &self.change.datum_variant {
                Some(DatumVariant::Inline(data)) => Some(data.clone()),
                _ => None,
            },
            None,
        )
    }

    fn get_pure_lovelace(&self, assets: &Assets) -> u64 {
        if assets.contains_other() {
            let placeholder_output = self.create_partial_change_output(assets.clone());
            let lovelace = assets.get_lovelace();
            let required_lovelace =
                placeholder_output.required_lovelace(self.protocol_parameters.coins_per_utxo_byte);
            if required_lovelace > lovelace {
                0
            } else {
                lovelace - required_lovelace
            }
        } else {
            assets.get_lovelace()
        }
    }

    fn coin_selection(&mut self) -> CoreResult<()> {
        if self.without_coin_selection {
            return Ok(());
        }

        static WEIGHTS: [u32; 6] = [200, 1000, 1500, 800, 800, 5000];

        fn norm(vector: &Vec<i128>) -> f64 {
            vector
                .iter()
                .fold(0 as u128, |acc, coord| acc + coord.pow(2) as u128)
                .sqrt() as f64
        }

        fn sub_vectors(vector1: &Vec<u64>, vector2: &Vec<u64>) -> Vec<i128> {
            vector1
                .iter()
                .zip(vector2.iter())
                .map(|(x, y)| *x as i128 - *y as i128)
                .collect()
        }

        fn get_normalization(vector1: &Vec<u64>, vector2: &Vec<u64>) -> f64 {
            (vector1
                .iter()
                .fold(0 as u128, |acc, x| acc + (*x as u128).pow(2))
                * vector2
                    .iter()
                    .fold(0 as u128, |acc, x| acc + (*x as u128).pow(2)))
            .sqrt() as f64
        }

        /*
            The cost function evalues each move in the improvement phase by penalizing bad actions:
            1. We try to get to an ideal ada amount (twice the target amount) => The further away the more penalized
            2. We try to get an ideal amount for all assets. We take the euclidean distance of two normalized vectors,
                where the dimensions represent the amount of assets and the coordinate the quantity of a specific asset.
                The closer the distance is to 0 the less penalized.
            3. We try to avoid assets in inputs if possible by penalizing each extra asset that was added.
            4. If the quantity for any asset (inculding ada) is less than the target quantity, apply a very big negative weight so that this move is definitely discarded.
            5. Whenever we interact with a plutus script, we increase the weights on avoiding assets, since assets are costy in plutus scripts.

        */
        fn cost(
            builder: &InstructionBuilder,
            available_selection: &Vec<Utxo>,
            current_selection: &Vec<Utxo>,
            target: &Assets,
            total_input: &Assets,
            total_output: &Assets,
        ) -> f64 {
            // If the target coin is less than 2.5 ADA we try to add more than twice the amount in order to cover the max fees in worst case
            let ideal = {
                let target_lovelace = target.get_lovelace();
                if target_lovelace > 2500000 {
                    target_lovelace * 2
                } else {
                    target_lovelace.max(1000000) * 4
                }
            };

            let available_selection_assets = available_selection
                .iter()
                .fold(Assets::new(), |acc, curr| acc + curr.assets.clone());
            let available_selection_others_len = available_selection_assets.get_all_others().len();

            let current_selection_assets = current_selection
                .iter()
                .fold(Assets::new(), |acc, curr| acc + curr.assets.clone());
            let current_selection_others = current_selection_assets.get_all_others();

            let target_others = target.get_all_others();

            let mut current_vector: Vec<u64> = Vec::new();
            let mut ideal_vector: Vec<u64> = Vec::new();

            for (target_unit, target_quantity) in target_others.iter() {
                let quantity = current_selection_others.get(target_unit).unwrap_or(&0);

                if quantity < target_quantity {
                    return 100000.0;
                }
                // For performance reasons we only try to get to an ideal amount of assets when it's below a certain threshold
                if target.len() < 100 {
                    current_vector.push(*quantity as u64);
                    ideal_vector.push((target_quantity * 2) as u64);
                }
            }

            let temp_total_input_lovelace =
                current_selection_assets.get_lovelace() + total_input.get_lovelace();

            if temp_total_input_lovelace < total_output.get_lovelace() {
                return 1000000.0;
            }

            let current_ideal = ((builder.get_pure_lovelace(&current_selection_others)) as f64
                - ideal as f64)
                / ideal as f64;

            let weight_ideal = if current_ideal > 0.0 {
                current_ideal * 0.0
            } else if current_selection.len() > 100 {
                -current_ideal * WEIGHTS[0] as f64
            } else {
                -current_ideal * WEIGHTS[1] as f64
            };

            // Normalize the asset length through the max possible asset length
            let asset_len = if available_selection_others_len > 0 {
                current_selection_others.len() as f64 / available_selection_others_len as f64
            } else {
                0.0
            };

            let weight_assets = if builder.redeemers.is_some() {
                // Assets are expensive for Plutus scripts => penalize harder if more assets are in inputs
                asset_len * WEIGHTS[2] as f64
            } else {
                // Penalize more assets a bit, but try to find the ideal quantity in order to avoid asset fractions over time.

                let distance = norm(&sub_vectors(&current_vector, &ideal_vector));
                let normalization = get_normalization(&current_vector, &ideal_vector);
                let norm_distance = distance / normalization;

                asset_len * WEIGHTS[3] as f64 + norm_distance * WEIGHTS[4] as f64
            };

            // If the UTxO set is getting quite large we start to take the UTxO count into consideration.
            let weight_utxos = if current_selection.len() > 100 && available_selection.len() > 0 {
                (current_selection.len() as f64 / available_selection.len() as f64)
                    * WEIGHTS[5] as f64
            } else {
                0.0
            };

            weight_ideal + weight_assets + weight_utxos
        }

        let available_selection: BTreeSet<_> =
            self.selection.difference(&self.inputs).cloned().collect();

        if available_selection.len() <= 0 {
            return Ok(());
        }

        let total_input = self
            .inputs
            .iter()
            .fold(Assets::new(), |acc, curr| acc + curr.utxo.assets.clone())
            + self.implicit_input.clone();

        let mut total_output = self
            .outputs
            .iter()
            .fold(Assets::new(), |acc, curr| acc + curr.assets.clone())
            + self.implicit_output.clone()
            + Assets::from_lovelace(self.calculate_fee());

        let total_change = total_input.clone().clamped_sub(total_output.clone());

        if total_change.contains_other() {
            let mut placeholder_output = self.create_partial_change_output(total_change.clone());
            placeholder_output.assets.set_lovelace(0);

            let required_lovelace =
                placeholder_output.required_lovelace(self.protocol_parameters.coins_per_utxo_byte);

            let change_lovelace = total_change.get_lovelace();

            if required_lovelace >= change_lovelace {
                total_output +=
                    Assets::from_lovelace(change_lovelace + (required_lovelace - change_lovelace))
            }
        }

        let target = total_output.clone().clamped_sub(total_input.clone());

        let mut current_assets = Assets::new();
        let mut current_selection: Vec<Utxo> = Vec::new();
        let mut available_selection: Vec<Utxo> = available_selection
            .into_iter()
            .map(|b| b.utxo.clone())
            .collect();

        // Add enough assets to inputs
        for (unit, _) in target.get_all_others().iter() {
            let mut relevant_selection = available_selection
                .clone()
                .into_iter()
                .filter(|utxo| utxo.assets.get(unit).is_some())
                .collect::<Vec<Utxo>>();

            while current_assets.get_unit(unit) < target.get_unit(unit) {
                if relevant_selection.len() <= 0 {
                    return Err(CoreError::msg(format!(
                        "Exhausted inputs for unit: {}",
                        unit
                    )));
                }
                let index = rand::thread_rng().gen_range(0..relevant_selection.len());
                let utxo = relevant_selection[index].clone();

                current_assets = current_assets + utxo.assets.clone();
                current_selection.push(utxo.clone());

                total_output += Assets::from_lovelace(
                    utxo.get_fee_for_input(self.protocol_parameters.min_fee_a),
                );

                let index_available = available_selection
                    .iter()
                    .position(|u| {
                        TransactionInput::try_from(u.clone()).unwrap()
                            == TransactionInput::try_from(utxo.clone()).unwrap()
                    })
                    .unwrap();
                available_selection.swap_remove(index_available);
                relevant_selection.swap_remove(index);
            }
        }

        // Add enough lovelace to inputs
        let mut relevant_selection = available_selection.clone();
        while self.get_pure_lovelace(&current_assets) < self.get_pure_lovelace(&target)
            || (total_input.clone() + current_assets.clone()).get_lovelace()
                < total_output.get_lovelace()
        {
            if relevant_selection.len() <= 0 {
                return Err(CoreError::msg("Exhausted inputs for lovelace"));
            }
            let index = rand::thread_rng().gen_range(0..relevant_selection.len());
            let utxo = relevant_selection[index].clone();
            current_assets += utxo.assets.clone();
            current_selection.push(utxo.clone());

            total_output +=
                Assets::from_lovelace(utxo.get_fee_for_input(self.protocol_parameters.min_fee_a));

            let index_available = available_selection
                .iter()
                .position(|u| {
                    TransactionInput::try_from(u.clone()).unwrap()
                        == TransactionInput::try_from(utxo.clone()).unwrap()
                })
                .unwrap();
            available_selection.swap_remove(index_available);
            relevant_selection.swap_remove(index);
        }

        // Improvement Phase
        let iterations = current_selection.len().max(100);

        let mut current_cost = cost(
            self,
            &available_selection,
            &current_selection,
            &target,
            &total_input,
            &total_output,
        );

        for _ in 0..iterations {
            if relevant_selection.len() <= 0 {
                break;
            }

            //  0 = Replace
            //  1 = Append
            //  2 = Delete
            for action in 0..=2 {
                if action == 0 {
                    if current_selection.len() <= 0 {
                        continue;
                    }
                    let mut current_selection_check = current_selection.clone();
                    let index = rand::thread_rng().gen_range(0..relevant_selection.len());
                    let index2 = rand::thread_rng().gen_range(0..current_selection_check.len());

                    let utxo = relevant_selection[index].clone();
                    current_selection_check[index2] = utxo.clone();

                    // Checks if replacement utxo is better than current one at this position
                    let new_cost = cost(
                        self,
                        &available_selection,
                        &current_selection,
                        &target,
                        &total_input,
                        &total_output,
                    );
                    if new_cost < current_cost {
                        let old_utxo = current_selection[index2].clone();
                        current_assets -= old_utxo.assets.clone();
                        current_assets += utxo.assets.clone();
                        current_selection = current_selection_check;
                        relevant_selection[index] = old_utxo.clone();
                        current_cost = new_cost;

                        break;
                    }
                } else if action == 1 {
                    let mut current_selection_check = current_selection.clone();
                    let index = rand::thread_rng().gen_range(0..relevant_selection.len());
                    let utxo = relevant_selection[index].clone();
                    current_selection_check.push(utxo.clone());

                    // Checks if appending a utxo improves coin selection
                    let new_cost = cost(
                        self,
                        &available_selection,
                        &current_selection,
                        &target,
                        &total_input,
                        &total_output,
                    );
                    if new_cost < current_cost {
                        current_assets += utxo.assets.clone();
                        current_selection = current_selection_check;

                        relevant_selection.swap_remove(index);

                        total_output += Assets::from_lovelace(
                            utxo.get_fee_for_input(self.protocol_parameters.min_fee_a),
                        );
                        current_cost = new_cost;

                        break;
                    }
                } else {
                    if current_selection.len() <= 0 {
                        continue;
                    }
                    let mut current_selection_check = current_selection.clone();
                    let index = rand::thread_rng().gen_range(0..current_selection_check.len());
                    let utxo = current_selection_check[index].clone();
                    current_selection_check.swap_remove(index);

                    // Checks if deleting a utxo is better than current input set
                    let new_cost = cost(
                        self,
                        &available_selection,
                        &current_selection,
                        &target,
                        &total_input,
                        &total_output,
                    );
                    if new_cost < current_cost {
                        current_assets -= utxo.assets.clone();
                        current_selection = current_selection_check;

                        total_output -= Assets::from_lovelace(
                            utxo.get_fee_for_input(self.protocol_parameters.min_fee_a),
                        );
                        current_cost = new_cost;

                        break;
                    }
                }
            }
        }

        for utxo in current_selection {
            self.add_input(utxo, None)?;
        }

        if self.inputs.len() <= 0 {
            let utxo = match available_selection.get(0) {
                Some(utxo) => utxo,
                None => return Err(CoreError::msg("At least 1 utxo required, found none")),
            };
            self.add_input(utxo.clone(), None)?;
        }
        Ok(())
    }

    fn balance(&mut self) -> CoreResult<()> {
        let mut fee = self.calculate_fee();

        let total_input = self
            .inputs
            .iter()
            .fold(Assets::new(), |acc, curr| acc + curr.utxo.assets.clone())
            + self.implicit_input.clone();

        let total_output = self
            .outputs
            .iter()
            .fold(Assets::new(), |acc, curr| acc + curr.assets.clone())
            + self.implicit_output.clone();

        match &total_input.partial_cmp(&(total_output.clone() + Assets::from_lovelace(fee))) {
            Some(Ordering::Equal) => {
                self.fee = (total_input.get_lovelace() - total_output.get_lovelace()) as u64;
                Ok(())
            }
            Some(Ordering::Less) => {
                return Err(CoreError::msg("Insufficient input in transaction"))
            }
            Some(Ordering::Greater) => {
                let mut total_change = total_input - total_output;

                let mut change_output = self.create_partial_change_output(Assets::from_lovelace(
                    total_change.get_lovelace(),
                ));

                for (unit, quantity) in total_change.get_all_others().iter() {
                    let check_assets =
                        change_output.assets.clone() + Assets::from_unit(unit.clone(), *quantity);

                    if check_assets.size() > self.protocol_parameters.max_val_size {
                        change_output.assets.set_lovelace(0);
                        change_output
                            .required_lovelace_mut(self.protocol_parameters.coins_per_utxo_byte);

                        fee += change_output.get_fee_for_output(self.protocol_parameters.min_fee_a);
                        self.change_outputs.push(change_output.clone());

                        total_change = total_change.clamped_sub(change_output.assets);

                        if total_change.len() <= 0 {
                            return Err(CoreError::msg(
                                "Not enough lovelace left to cover required lovelace",
                            ));
                        }
                        change_output.assets = Assets::from_unit(unit.clone(), *quantity);
                    } else {
                        change_output.assets = check_assets;
                    }
                }

                let try_two_outputs = |builder: &mut InstructionBuilder,
                                       total_change: &Assets,
                                       change_output: &Utxo,
                                       fee: u64|
                 -> CoreResult<()> {
                    if !total_change.contains_other() {
                        return Err(CoreError::msg(
                            "No other assets found, splitting unnecessary, continue with one",
                        ));
                    }
                    let mut fee_check = fee;
                    let mut total_change_check = total_change.clone();
                    let mut change_output_0 = change_output.clone();

                    change_output_0.assets = total_change_check.clone();
                    change_output_0.assets.set_lovelace(0);
                    let change_value_0 = Assets::from_lovelace(
                        change_output_0
                            .required_lovelace(builder.protocol_parameters.coins_per_utxo_byte),
                    );
                    change_output_0
                        .assets
                        .set_lovelace(change_value_0.get_lovelace());
                    total_change_check -= change_output_0.assets.clone();

                    fee_check +=
                        change_output_0.get_fee_for_output(builder.protocol_parameters.min_fee_a);

                    let mut change_output_1 = change_output.clone();
                    change_output_1.assets = total_change_check.clone();

                    fee_check +=
                        change_output_1.get_fee_for_output(builder.protocol_parameters.min_fee_a);

                    change_output_1
                        .assets
                        .set_lovelace(change_output_1.assets.get_lovelace() - fee_check);

                    if change_output_1.assets.get_lovelace()
                        < change_output_1
                            .required_lovelace(builder.protocol_parameters.coins_per_utxo_byte)
                    {
                        return Err(CoreError::NotEnoughLovelaceForOutput.to_msg());
                    };

                    builder.change_outputs.push(change_output_0);
                    builder.change_outputs.push(change_output_1);
                    builder.fee = fee_check;

                    Ok(())
                };

                let try_one_output = |builder: &mut InstructionBuilder,
                                      total_change: &Assets,
                                      change_output: &Utxo,
                                      fee: u64|
                 -> CoreResult<()> {
                    let mut fee_check = fee;
                    let total_change_check = total_change.clone();
                    let mut change_output_0 = change_output.clone();
                    change_output_0.assets = total_change_check.clone();

                    fee_check +=
                        change_output_0.get_fee_for_output(builder.protocol_parameters.min_fee_a);

                    change_output_0
                        .assets
                        .set_lovelace(change_output_0.assets.get_lovelace() - fee_check);

                    if change_output_0.assets.get_lovelace()
                        < change_output_0
                            .required_lovelace(builder.protocol_parameters.coins_per_utxo_byte)
                    {
                        return Err(CoreError::NotEnoughLovelaceForOutput.to_msg());
                    };

                    builder.change_outputs.push(change_output_0);
                    builder.fee = fee_check;

                    Ok(())
                };

                let try_just_fee = |builder: &mut InstructionBuilder,
                                    total_change: &Assets,
                                    fee: u64|
                 -> CoreResult<()> {
                    if total_change.contains_other() {
                        return Err(CoreError::NotEnoughLovelaceForOutput.to_msg());
                    }
                    if total_change.get_lovelace() < fee {
                        return Err(CoreError::NotEnoughLovelaceForFee.to_msg());
                    }
                    builder.fee = total_change.get_lovelace();
                    Ok(())
                };

                try_two_outputs(self, &total_change, &change_output, fee).or_else(|_| {
                    try_one_output(self, &total_change, &change_output, fee)
                        .or_else(|_| try_just_fee(self, &total_change, fee))
                })
            }
            _ => return Err(CoreError::msg("Missing input or output for some assets")),
        }
    }

    fn calculate_tx_size(&self) -> u32 {
        let mut tx = self.tx.clone();

        let placeholder_vkey: [u8; 32] = [0; 32];
        let placeholder_signature: [u8; 64] = [0; 64];

        if self.used_keys.len() > 0 {
            let vkey_witnesses: Vec<VKeyWitness> = self
                .used_keys
                .iter()
                .map(|_| VKeyWitness {
                    vkey: placeholder_vkey.to_vec().into(),
                    signature: placeholder_signature.to_vec().into(),
                })
                .collect();
            tx.transaction_witness_set.vkeywitness =
                Some(NonEmptySet::from_vec(vkey_witnesses).unwrap());
        }

        if self.data.is_some() || self.redeemers.is_some() {
            tx.transaction_body.script_data_hash = Some([0; 32].into());
        }

        if let Some(change_data) = &self.change_data {
            let mut data = self.data.clone().unwrap_or(BTreeSet::new());
            data.insert(change_data.clone());
            tx.transaction_witness_set.plutus_data =
                Some(NonEmptySet::from_vec(data.into_iter().collect()).unwrap());
        }

        tx.encode_fragment().unwrap().len() as u32
    }

    fn calculate_fee(&self) -> u64 {
        let tx_size = self.calculate_tx_size();
        let mut fee = tx_size as u64 * self.protocol_parameters.min_fee_a
            + self.protocol_parameters.min_fee_b;

        if let Some(redeemers) = &self.redeemers {
            let total_ex_units =
                redeemers
                    .iter()
                    .fold(ExUnits { mem: 0, steps: 0 }, |mut acc, curr| {
                        acc.mem += curr.0.ex_units.mem;
                        acc.steps += curr.0.ex_units.steps;
                        acc
                    });
            let redeemers_fee = (total_ex_units.mem as f64 * self.protocol_parameters.price_mem
                + total_ex_units.steps as f64 * self.protocol_parameters.price_step)
                .ceil() as u64;

            fee += redeemers_fee;
        }

        if let Some(size) = self.total_script_ref_size {
            fee += (size as f64 * self.protocol_parameters.minfee_refscript_cost_per_byte).ceil()
                as u64
        }

        // we need to take the size of the fee itself into account (usually around 3-4 bytes)
        fee -=
            self.fee.encode_fragment().unwrap().len() as u64 * self.protocol_parameters.min_fee_a;
        fee += fee.encode_fragment().unwrap().len() as u64 * self.protocol_parameters.min_fee_a;

        fee
    }

    fn increment_total_script_ref_size(&mut self, script: &Script) -> CoreResult<()> {
        let tag_size = 2;
        let script_size = match script {
            Script::Native { .. } => 0,
            Script::PlutusV1 { script }
            | Script::PlutusV2 { script }
            | Script::PlutusV3 { script } => {
                hex::decode(script).map_err(CoreError::msg)?.len() as u64
            }
        };

        *self.total_script_ref_size.get_or_insert(0) += tag_size + script_size;
        Ok(())
    }

    fn add_language_or_keys(&mut self, script: &Script) -> CoreResult<()> {
        let script_hash: ScriptHash = Hasher::hash_script(script.clone())?
            .parse()
            .map_err(CoreError::msg)?;
        match script {
            Script::PlutusV1 { .. } => self.found_languages.insert(script_hash, Language::PlutusV1),
            Script::PlutusV2 { .. } => self.found_languages.insert(script_hash, Language::PlutusV2),
            Script::PlutusV3 { .. } => self.found_languages.insert(script_hash, Language::PlutusV3),
            Script::Native { script } => {
                let native =
                    NativeScript::decode_fragment(&hex::decode(script).map_err(CoreError::msg)?)
                        .map_err(CoreError::msg)?;
                fn collect_keys(builder: &mut InstructionBuilder, native: &NativeScript) {
                    match native {
                        NativeScript::ScriptPubkey(key_hash) => {
                            builder.used_keys.insert(*key_hash);
                        }
                        NativeScript::ScriptAll(scripts)
                        | NativeScript::ScriptAny(scripts)
                        | NativeScript::ScriptNOfK(_, scripts) => {
                            scripts.iter().for_each(|s| collect_keys(builder, s));
                        }
                        _ => (),
                    }
                }
                collect_keys(self, &native);
                None
            }
        };
        Ok(())
    }

    fn add_data(&mut self, data: &String) -> CoreResult<()> {
        let decoded_data = PlutusData::decode_fragment(&hex::decode(data).map_err(CoreError::msg)?)
            .map_err(CoreError::msg)?;

        self.data
            .get_or_insert_with(BTreeSet::new)
            .insert(decoded_data);
        Ok(())
    }

    fn assemble(&mut self) -> CoreResult<()> {
        self.tx.transaction_body.fee = self.fee;

        self.tx.transaction_body.inputs = self
            .inputs
            .iter()
            .map(|i| i.utxo.clone().try_into())
            .collect::<CoreResult<Vec<TransactionInput>>>()?
            .into();

        self.tx.transaction_body.outputs = self
            .outputs
            .clone()
            .into_iter()
            .chain(self.change_outputs.clone())
            .map(|o| o.try_into())
            .collect::<CoreResult<_>>()?;

        if let Some(redeemers) = &self.redeemers {
            self.tx.transaction_witness_set.redeemer = Some(Redeemers::Map(
                NonEmptyKeyValuePairs::from_vec(
                    redeemers.iter().map(|c| c.clone().into()).collect(),
                )
                .unwrap(),
            ));
        }

        if let Some(collateral) = &self.collateral {
            self.tx.transaction_body.collateral = Some(
                NonEmptySet::from_vec(
                    collateral
                        .iter()
                        .map(|c| c.utxo.clone().try_into())
                        .collect::<CoreResult<_>>()?,
                )
                .unwrap(),
            );
        }

        if let Some(collateral_return) = &self.collateral_return {
            self.tx.transaction_body.collateral_return =
                Some(collateral_return.clone().try_into()?);
        }

        if let Some(total_collateral) = &self.total_collateral {
            self.tx.transaction_body.total_collateral = Some(*total_collateral);
        }

        if let Some(script_data_hash) = &self.script_data_hash {
            self.tx.transaction_body.script_data_hash = Some(*script_data_hash);
        }

        if self.change_outputs.len() > 0 && self.change_data.is_some() {
            self.data
                .get_or_insert_with(BTreeSet::new)
                .insert(self.change_data.clone().unwrap());
        }

        Ok(())
    }

    fn pre_assemble(&mut self) -> CoreResult<()> {
        if let Some(mint) = &self.mint {
            let value: Mint = mint
                .values()
                .fold(Assets::new(), |acc, curr| acc + curr.assets.clone())
                .try_into()?;
            self.tx.transaction_body.mint = Some(value);
        }

        if let Some(certs) = &self.certificates {
            let value: Vec<pallas_primitives::conway::Certificate> = certs
                .iter()
                .map(|c| c.certificate.clone().try_into())
                .collect::<CoreResult<_>>()?;
            self.tx.transaction_body.certificates = Some(NonEmptySet::from_vec(value).unwrap());
        }

        if let Some(inputs) = &self.read_inputs {
            let value: Vec<TransactionInput> = inputs
                .iter()
                .map(|i| i.utxo.clone().try_into())
                .collect::<CoreResult<_>>()?;
            self.tx.transaction_body.reference_inputs = Some(NonEmptySet::from_vec(value).unwrap());
        }

        if let Some(withdrawals) = &self.withdrawals {
            let value: Vec<(RewardAccount, u64)> = withdrawals
                .iter()
                .map(|(_, w)| {
                    Ok((
                        Address::from_bech32(&w.withdrawal.reward_address)
                            .map_err(CoreError::msg)?
                            .to_vec()
                            .into(),
                        w.withdrawal.amount,
                    ))
                })
                .collect::<CoreResult<_>>()?;
            self.tx.transaction_body.withdrawals =
                Some(pallas_primitives::NonEmptyKeyValuePairs::from_vec(value).unwrap())
        }

        if let Some(data) = &self.data {
            let value = NonEmptySet::from_vec(data.clone().into_iter().collect()).unwrap();
            self.tx.transaction_witness_set.plutus_data = Some(value.clone());

            let mut script_data = Vec::new();
            script_data.push(0xA0);
            script_data.extend(value.encode_fragment().unwrap());
            script_data.push(0xA0);

            self.script_data_hash = Some(pallas_crypto::hash::Hasher::<256>::hash(
                script_data.as_slice(),
            ))
        }

        if let Some(metadata) = &self.aux_metadata {
            let value: Vec<(u64, Metadatum)> = metadata
                .iter()
                .map(|(k, v)| {
                    (
                        *k,
                        match v.clone() {
                            Either::Left(m) => m.into(),
                            Either::Right(m) => m.into(),
                        },
                    )
                })
                .collect();

            let aux = AuxiliaryData::PostAlonzo(PostAlonzoAuxiliaryData {
                metadata: Some(KeyValuePairs::Def(value)),
                native_scripts: None,
                plutus_scripts: None,
            });

            self.tx.transaction_body.auxiliary_data_hash = Some(aux.compute_hash().to_vec().into());
            self.tx.auxiliary_data = Nullable::Some(aux);
        }

        if let Some(scripts) = &self.scripts {
            let mut native_script: Vec<NativeScript> = vec![];
            let mut plutus_v1_script: Vec<PlutusScript<1>> = vec![];
            let mut plutus_v2_script: Vec<PlutusScript<2>> = vec![];
            let mut plutus_v3_script: Vec<PlutusScript<3>> = vec![];

            for (_, script) in scripts {
                match script {
                    Script::Native { script } => {
                        native_script.push(
                            NativeScript::decode_fragment(
                                &hex::decode(script).map_err(CoreError::msg)?,
                            )
                            .map_err(CoreError::msg)?,
                        );
                    }
                    Script::PlutusV1 { script } => {
                        plutus_v1_script.push(
                            PlutusScript::<1>::decode_fragment(
                                &hex::decode(script).map_err(CoreError::msg)?,
                            )
                            .map_err(CoreError::msg)?,
                        );
                    }
                    Script::PlutusV2 { script } => {
                        plutus_v2_script.push(
                            PlutusScript::<2>::decode_fragment(
                                &hex::decode(script).map_err(CoreError::msg)?,
                            )
                            .map_err(CoreError::msg)?,
                        );
                    }
                    Script::PlutusV3 { script } => {
                        plutus_v3_script.push(
                            PlutusScript::<3>::decode_fragment(
                                &hex::decode(script).map_err(CoreError::msg)?,
                            )
                            .map_err(CoreError::msg)?,
                        );
                    }
                }
            }

            if native_script.len() > 0 {
                self.tx.transaction_witness_set.native_script =
                    Some(NonEmptySet::from_vec(native_script).unwrap());
            }
            if plutus_v1_script.len() > 0 {
                self.tx.transaction_witness_set.plutus_v1_script =
                    Some(NonEmptySet::from_vec(plutus_v1_script).unwrap());
            }
            if plutus_v2_script.len() > 0 {
                self.tx.transaction_witness_set.plutus_v2_script =
                    Some(NonEmptySet::from_vec(plutus_v2_script).unwrap());
            }
            if plutus_v3_script.len() > 0 {
                self.tx.transaction_witness_set.plutus_v3_script =
                    Some(NonEmptySet::from_vec(plutus_v3_script).unwrap());
            }
        }

        if let Some(signers) = &self.signers {
            self.tx.transaction_body.required_signers =
                Some(NonEmptySet::from_vec(signers.clone().into_iter().collect()).unwrap())
        }

        if let Some(DatumVariant::AsHash(data)) = &self.change.datum_variant {
            self.change_data = Some(
                PlutusData::decode_fragment(&hex::decode(data).map_err(CoreError::msg)?)
                    .map_err(CoreError::msg)?,
            );
        }

        Ok(())
    }
}

trait Empty {
    fn empty() -> Self;
}

impl Empty for Tx {
    fn empty() -> Self {
        Self {
            transaction_body: TransactionBody {
                inputs: vec![].into(),
                outputs: vec![],
                fee: 0,
                ttl: None,
                certificates: None,
                withdrawals: None,
                auxiliary_data_hash: None,
                validity_interval_start: None,
                mint: None,
                script_data_hash: None,
                collateral: None,
                required_signers: None,
                network_id: None,
                collateral_return: None,
                total_collateral: None,
                reference_inputs: None,
                voting_procedures: None,
                treasury_value: None,
                proposal_procedures: None,
                donation: None,
            },
            transaction_witness_set: WitnessSet {
                native_script: None,
                plutus_data: None,
                plutus_v1_script: None,
                plutus_v2_script: None,
                plutus_v3_script: None,
                redeemer: None,
                vkeywitness: None,
                bootstrap_witness: None,
            },
            auxiliary_data: None.into(),
            success: true,
        }
    }
}

#[derive(Tsify, Serialize, Deserialize, Debug)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub struct Instructions(pub Vec<Instruction>);

#[derive(Tsify, Serialize, Deserialize, Debug)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(tag = "type")]
pub enum Instruction {
    CollectFrom {
        utxos: Vec<Utxo>,
        #[tsify(optional)]
        redeemer: Option<String>,
    },
    ReadFrom {
        utxos: Vec<Utxo>,
    },
    Mint {
        assets: Assets,
        #[tsify(optional)]
        redeemer: Option<String>,
    },
    #[serde(rename_all = "camelCase")]
    PayTo {
        assets: Assets,
        address: String,
        #[tsify(optional)]
        datum_variant: Option<DatumVariant>,
        #[tsify(optional)]
        script_ref: Option<Script>,
    },
    #[serde(rename_all = "camelCase")]
    PayToContract {
        assets: Assets,
        address: String,
        datum_variant: DatumVariant,
        #[tsify(optional)]
        script_ref: Option<Script>,
    },
    DelegateTo {
        delegation: Delegation,
        #[tsify(optional)]
        redeemer: Option<String>,
    },
    #[serde(rename_all = "camelCase")]
    RegisterStake {
        reward_address: String,
    },
    #[serde(rename_all = "camelCase")]
    DeregisterStake {
        reward_address: String,
        #[tsify(optional)]
        redeemer: Option<String>,
    },
    RegisterPool(PoolRegistration),
    UpdatePool(PoolRegistration),
    RetirePool(PoolRetirement),
    Withdraw {
        withdrawal: Withdrawal,
        #[tsify(optional)]
        redeemer: Option<String>,
    },
    #[serde(rename_all = "camelCase")]
    AddSigner {
        key_hash: String,
    },
    AddNetworkId {
        id: u8,
    },
    #[serde(rename_all = "camelCase")]
    ValidFrom {
        unix_time: u64,
    },
    #[serde(rename_all = "camelCase")]
    ValidTo {
        unix_time: u64,
    },
    AttachMetadata {
        metadata: (u64, AuxMetadata<false>),
    },
    AttachMetadataWithConversion {
        metadata: (u64, AuxMetadata<true>),
    },
    AttachScript {
        script: Script,
    },
    WithChangeTo(Change),
    WithoutCoinSelection,
}

impl IntoIterator for Instructions {
    type Item = Instruction;
    type IntoIter = <Vec<Instruction> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a Instructions {
    type Item = &'a Instruction;
    type IntoIter = std::slice::Iter<'a, Instruction>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

#[derive(Tsify, Serialize, Deserialize, Debug)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct Change {
    pub address: String,
    #[tsify(optional)]
    pub datum_variant: Option<DatumVariant>,
}

#[derive(Tsify, Serialize, Deserialize, Debug)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub enum DatumVariant {
    Hash(String),
    AsHash(String),
    Inline(String),
}

#[derive(Tsify, Serialize, Deserialize, Debug)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct RelevantProtocolParameters {
    pub min_fee_a: u64,
    pub min_fee_b: u64,
    pub max_tx_size: u32,
    pub max_val_size: u32,
    pub key_deposit: u64,
    pub pool_deposit: u64,
    pub price_mem: f64,
    pub price_step: f64,
    pub max_tx_ex_mem: u64,
    pub max_tx_ex_steps: u64,
    pub coins_per_utxo_byte: u64,
    pub collateral_percentage: u16,
    pub max_collateral_inputs: u16,
    pub cost_models: HashMap<String, Vec<i64>>,
    pub minfee_refscript_cost_per_byte: f64,
}

#[derive(Clone, Debug)]
enum Either<L, R> {
    Left(L),
    Right(R),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuilderRedeemer(Redeemer);

impl From<BuilderRedeemer> for (RedeemersKey, RedeemersValue) {
    fn from(value: BuilderRedeemer) -> Self {
        (
            RedeemersKey {
                tag: value.0.tag,
                index: value.0.index,
            },
            RedeemersValue {
                data: value.0.data,
                ex_units: value.0.ex_units,
            },
        )
    }
}

impl PartialOrd for BuilderRedeemer {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BuilderRedeemer {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.0.tag as u8).cmp(&(other.0.tag as u8)) {
            Ordering::Equal => self.0.index.cmp(&other.0.index),
            other => other,
        }
    }
}

#[derive(Clone, Debug)]
pub struct BuilderInput {
    pub utxo: Utxo,
    pub redeemer: Option<String>,
}

impl PartialEq for BuilderInput {
    fn eq(&self, other: &Self) -> bool {
        TransactionInput::try_from(self.utxo.clone()).unwrap()
            == TransactionInput::try_from(other.utxo.clone()).unwrap()
    }
}

impl Eq for BuilderInput {}

impl PartialOrd for BuilderInput {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BuilderInput {
    fn cmp(&self, other: &Self) -> Ordering {
        TransactionInput::try_from(self.utxo.clone())
            .unwrap()
            .cmp(&TransactionInput::try_from(other.utxo.clone()).unwrap())
    }
}

#[derive(Clone, Debug)]
struct BuilderMint {
    pub assets: Assets,
    pub redeemer: Option<String>,
}

#[derive(Clone, Debug)]
struct BuilderCertificate {
    pub certificate: Certificate,
    pub redeemer: Option<String>,
}

#[derive(Clone, Debug)]
struct BuilderWithdrawal {
    pub withdrawal: Withdrawal,
    pub redeemer: Option<String>,
}

#[cfg(test)]
mod tests {
    use pallas_primitives::{conway::Redeemers, Nullable};
    use uplc::{Constr, Fragment, PlutusData};

    use super::{
        Change, DatumVariant, Instruction, InstructionBuilder, Instructions, RelevantProtocolParameters
    };
    use crate::{
        addresses::{Addresses, Network},
        codec::{
            Assets, AuxMetadata, ConstrConversion, Delegation, Script, Utxo, Utxos, Withdrawal,
        },
        hasher::Hasher,
    };
    use std::collections::HashMap;

    const ADDRESS: &'static str = "addr1v9ct2a4qdr89cgavv40rkqjs6d6ssyzflgsveesnag2055qd8trcs";
    const ADDRESS_OTHER: &'static str =
        "addr1v8pjmzspg2mwkjflpaxfqe0zfkl0ykhdpmu75565ryr9y5cuv05p3";

    fn setup_builder(extra_selection: Option<Vec<Utxo>>) -> InstructionBuilder {
        let mut selection = vec![Utxo {
            tx_hash: "00".repeat(32),
            output_index: 0,
            address: ADDRESS.to_string(),
            assets: Assets::from_lovelace(20000000),
            datum_hash: None,
            datum: None,
            script_ref: None,
        }];
        selection.extend(extra_selection.unwrap_or(Vec::new()));

        InstructionBuilder::new(
            Network::Mainnet,
            RelevantProtocolParameters {
                min_fee_a: 44,
                min_fee_b: 155381,
                max_tx_size: 16384,
                max_val_size: 5000,
                key_deposit: 2000000,
                pool_deposit: 500000000,
                price_mem: 0.0577,
                price_step: 0.0000721,
                max_tx_ex_mem: 14000000,
                max_tx_ex_steps: 10000000000,
                coins_per_utxo_byte: 4310,
                collateral_percentage: 150,
                max_collateral_inputs: 3,
                cost_models: HashMap::from([
                    (
                        "PlutusV1".to_string(),
                        vec![
                            100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32,
                            201305, 8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000,
                            100, 16000, 100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4,
                            72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 228465, 122, 0,
                            1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775,
                            558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049,
                            1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1,
                            44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32,
                            11546, 32, 85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32,
                            85848, 228465, 122, 0, 1, 1, 85848, 228465, 122, 0, 1, 1, 270652,
                            22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420,
                            1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32,
                            24623, 32, 53384111, 14333, 10,
                        ],
                    ),
                    (
                        "PlutusV2".to_string(),
                        vec![
                            100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32,
                            201305, 8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000,
                            100, 16000, 100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4,
                            72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 228465, 122, 0,
                            1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775,
                            558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049,
                            1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1,
                            44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32,
                            11546, 32, 85848, 228465, 122, 0, 1, 1, 90434, 519, 0, 1, 74433, 32,
                            85848, 228465, 122, 0, 1, 1, 85848, 228465, 122, 0, 1, 1, 955506,
                            213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0,
                            141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588,
                            32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10,
                            43574283, 26308, 10,
                        ],
                    ),
                ]),
                minfee_refscript_cost_per_byte: 15.0,
            },
            Utxos(selection),
            Change {
                address: ADDRESS.to_string(),
                datum_variant: None,
            },
        )
    }

    #[test]
    fn tx_with_two_outputs() {
        let builder = setup_builder(None);

        let signer = builder
            .commit(Instructions(vec![
                Instruction::PayTo {
                    assets: Assets::from_lovelace(2000000),
                    address: ADDRESS_OTHER.to_string(),
                    datum_variant: None,
                    script_ref: None,
                },
                Instruction::PayTo {
                    assets: Assets::from_lovelace(2000000),
                    address: ADDRESS_OTHER.to_string(),
                    datum_variant: None,
                    script_ref: None,
                },
            ]))
            .unwrap();

        let tx = signer.get_tx();

        assert_eq!(tx.transaction_body.inputs.len(), 1);
        assert_eq!(tx.transaction_body.fee, 167569);
        assert!(tx.transaction_body.outputs.len() >= 2);
    }

    #[test]
    fn mint_assets() {
        let builder = setup_builder(None);

        let signer = builder
            .commit(Instructions(vec![
                Instruction::Mint {
                    assets: Assets::from_unit("11".repeat(28), 123),
                    redeemer: None,
                },
                Instruction::Mint {
                    assets: Assets::from_unit("11".repeat(28), -23),
                    redeemer: None,
                },
                Instruction::Mint {
                    assets: Assets::from_unit("22".repeat(28), 10),
                    redeemer: None,
                },
            ]))
            .unwrap();

        let tx = signer.get_tx();

        assert_eq!(
            Assets::try_from(tx.transaction_body.mint.unwrap()).unwrap(),
            Assets::from([("11".repeat(28), 100), ("22".repeat(28), 10)])
        );
        assert!(tx.transaction_body.outputs.iter().any(|output| {
            let utxo: Utxo = output.clone().try_into().unwrap();
            utxo.assets.get_unit(&"11".repeat(28)) == 100
        }));
        assert!(tx.transaction_body.outputs.iter().any(|output| {
            let utxo: Utxo = output.clone().try_into().unwrap();
            utxo.assets.get_unit(&"22".repeat(28)) == 10
        }));
    }

    #[test]
    fn stake_and_delegate() {
        let builder = setup_builder(None);

        let input_lovelace = builder
            .selection
            .iter()
            .fold(0, |acc, curr| acc + curr.utxo.assets.get_lovelace());

        let key_deposit = builder.protocol_parameters.key_deposit;

        let signer = builder
            .commit(Instructions(vec![
                Instruction::RegisterStake {
                    reward_address: "stake1uxk96skvmq8sx5gezj4jwh0pakswf8thnrw5musz2rja48c0sfdmh"
                        .to_string(),
                },
                Instruction::DelegateTo {
                    delegation: Delegation {
                        reward_address:
                            "stake1uxk96skvmq8sx5gezj4jwh0pakswf8thnrw5musz2rja48c0sfdmh"
                                .to_string(),
                        pool_id: "pool19f6guwy97mmnxg9dz65rxyj8hq07qxud886hamyu4fgfz7dj9gl"
                            .to_string(),
                    },
                    redeemer: None,
                },
            ]))
            .unwrap();

        let tx = signer.get_tx();

        let output_lovelace = tx.transaction_body.outputs.iter().fold(0, |acc, curr| {
            acc + Utxo::try_from(curr.clone()).unwrap().assets.get_lovelace()
        });

        let fee = tx.transaction_body.fee;

        assert!(tx.transaction_body.certificates.unwrap().iter().all(|c| {
            match c {
                pallas_primitives::conway::Certificate::StakeRegistration(_) => true,
                pallas_primitives::conway::Certificate::StakeDelegation(_, _) => true,
                _ => false,
            }
        }));
        assert_eq!(input_lovelace, output_lovelace + key_deposit + fee);
    }

    #[test]
    fn coin_selection_with_assets() {
        let builder = setup_builder(Some(vec![
            Utxo {
                tx_hash: "11".repeat(32),
                output_index: 0,
                address: ADDRESS.to_string(),
                assets: Assets::from([("11".repeat(28), 23), ("22".repeat(28), 2)]),
                datum_hash: None,
                datum: None,
                script_ref: None,
            },
            Utxo {
                tx_hash: "11".repeat(32),
                output_index: 1,
                address: ADDRESS.to_string(),
                assets: Assets::from([("11".repeat(28), 2)]),
                datum_hash: None,
                datum: None,
                script_ref: None,
            },
            Utxo {
                tx_hash: "11".repeat(32),
                output_index: 2,
                address: ADDRESS.to_string(),
                assets: Assets::from([("11".repeat(28), 1), ("33".repeat(28), 5)]),
                datum_hash: None,
                datum: None,
                script_ref: None,
            },
        ]));

        let result = builder.commit(Instructions(vec![
            Instruction::PayTo {
                assets: Assets::from_unit("11".repeat(28), 20),
                address: ADDRESS_OTHER.to_string(),
                datum_variant: None,
                script_ref: None,
            },
            Instruction::PayTo {
                assets: Assets::from_unit("11".repeat(28), 4),
                address: ADDRESS_OTHER.to_string(),
                datum_variant: None,
                script_ref: None,
            },
            Instruction::PayTo {
                assets: Assets::from_unit("22".repeat(28), 2) + Assets::from_lovelace(5000000),
                address: ADDRESS_OTHER.to_string(),
                datum_variant: None,
                script_ref: None,
            },
        ]));

        assert!(result.is_ok())
    }

    #[test]
    fn consume_input_plutus_v2_script() {
        let script = Script::PlutusV2 {
            script: "49480100002221200101".to_string(),
        };

        let script_address =
            Addresses::script_to_address(Network::Mainnet, script.clone(), None).unwrap();

        let datum = hex::encode(
            PlutusData::Constr(Constr::from_index(0, vec![]))
                .encode_fragment()
                .unwrap(),
        );

        let datum_hash = Hasher::hash_data(&datum).unwrap();

        let builder = setup_builder(None);

        let signer = builder
            .commit(Instructions(vec![
                Instruction::CollectFrom {
                    utxos: vec![
                        Utxo {
                            tx_hash: "22".repeat(32),
                            output_index: 0,
                            address: script_address.to_string(),
                            assets: Assets::from_lovelace(2000000),
                            datum_hash: Some(datum_hash.clone()),
                            datum: Some(datum.clone()),
                            script_ref: None,
                        },
                        Utxo {
                            tx_hash: "22".repeat(32),
                            output_index: 1,
                            address: script_address.to_string(),
                            assets: Assets::from_lovelace(1000000),
                            datum_hash: None,
                            datum: Some(datum.clone()),
                            script_ref: None,
                        },
                    ],
                    redeemer: Some(datum),
                },
                Instruction::AttachScript { script },
            ]))
            .unwrap();

        let tx = signer.get_tx();

        assert_eq!(
            match tx.transaction_witness_set.redeemer.unwrap() {
                Redeemers::Map(m) => m.len(),
                _ => 0,
            },
            2
        );
        assert!(tx.transaction_body.collateral.is_some());
        assert!(tx.transaction_body.collateral_return.is_some());
        assert!(tx.transaction_body.total_collateral.is_some());
        assert!(tx.transaction_body.script_data_hash.is_some());
    }

    #[test]
    fn mint_and_delegate_and_withdraw_plutus_v2() {
        let script = Script::PlutusV2 {
            script: "49480100002221200101".to_string(),
        };

        let policy_id = Hasher::hash_script(script.clone()).unwrap();
        let reward_address =
            Addresses::script_to_reward_address(Network::Mainnet, script.clone()).unwrap();

        let redeemer = hex::encode(
            PlutusData::Constr(Constr::from_index(0, vec![]))
                .encode_fragment()
                .unwrap(),
        );

        let builder = setup_builder(Some(vec![Utxo {
            tx_hash: "22".repeat(32),
            output_index: 0,
            address: ADDRESS.to_string(),
            assets: Assets::from_unit(format!("{}33", policy_id), 10),
            datum_hash: None,
            datum: None,
            script_ref: None,
        }]));

        let signer = builder
            .commit(Instructions(vec![
                Instruction::Mint {
                    assets: Assets::from_unit(format!("{}11", policy_id), 10),
                    redeemer: Some(redeemer.clone()),
                },
                Instruction::Mint {
                    assets: Assets::from_unit(format!("{}22", policy_id), 5),
                    redeemer: Some(redeemer.clone()),
                },
                Instruction::Mint {
                    assets: Assets::from_unit(format!("{}33", policy_id), -9),
                    redeemer: Some(redeemer.clone()),
                },
                Instruction::Mint {
                    assets: Assets::from_unit("00".repeat(28), 2), // requires native script normally
                    redeemer: None,
                },
                Instruction::RegisterStake {
                    reward_address: reward_address.clone(),
                },
                Instruction::DelegateTo {
                    delegation: Delegation {
                        reward_address: reward_address.clone(),
                        pool_id: "pool19f6guwy97mmnxg9dz65rxyj8hq07qxud886hamyu4fgfz7dj9gl"
                            .to_string(),
                    },
                    redeemer: Some(redeemer.clone()),
                },
                Instruction::Withdraw {
                    withdrawal: Withdrawal {
                        reward_address: reward_address.clone(),
                        amount: 30000000,
                    },
                    redeemer: Some(redeemer.clone()),
                },
                Instruction::AttachScript { script },
            ]))
            .unwrap();

        let tx = signer.get_tx();

        assert_eq!(
            match tx.transaction_witness_set.redeemer.unwrap() {
                Redeemers::Map(m) => m.len(),
                _ => 0,
            },
            3
        );
        assert!(tx.transaction_body.certificates.is_some());
        assert!(tx.transaction_body.withdrawals.is_some());
        assert!(tx.transaction_body.collateral.is_some());
        assert!(tx.transaction_body.collateral_return.is_some());
        assert!(tx.transaction_body.total_collateral.is_some());
        assert!(tx.transaction_body.script_data_hash.is_some());
    }

    #[test]
    fn deregister_stake_plutus_v2() {
        let script = Script::PlutusV2 {
            script: "49480100002221200101".to_string(),
        };

        let reward_address =
            Addresses::script_to_reward_address(Network::Mainnet, script.clone()).unwrap();

        let redeemer = hex::encode(
            PlutusData::Constr(Constr::from_index(0, vec![]))
                .encode_fragment()
                .unwrap(),
        );

        let builder = setup_builder(None);

        let input_lovelace = builder
            .selection
            .iter()
            .fold(0, |acc, curr| acc + curr.utxo.assets.get_lovelace());

        let key_deposit = builder.protocol_parameters.key_deposit;

        let signer = builder
            .commit(Instructions(vec![
                Instruction::DeregisterStake {
                    reward_address: reward_address.clone(),
                    redeemer: Some(redeemer.clone()),
                },
                Instruction::AttachScript { script },
            ]))
            .unwrap();

        let tx = signer.get_tx();

        let output_lovelace = tx.transaction_body.outputs.iter().fold(0, |acc, curr| {
            acc + Utxo::try_from(curr.clone()).unwrap().assets.get_lovelace()
        });

        let fee = tx.transaction_body.fee;

        assert_eq!(
            match tx.transaction_witness_set.redeemer.unwrap() {
                Redeemers::Map(m) => m.len(),
                _ => 0,
            },
            1
        );
        assert!(tx.transaction_body.certificates.is_some());
        assert!(tx.transaction_body.collateral.is_some());
        assert!(tx.transaction_body.collateral_return.is_some());
        assert!(tx.transaction_body.total_collateral.is_some());
        assert_eq!(input_lovelace, output_lovelace - key_deposit + fee);
    }

    #[test]
    fn tx_with_aux_metadata() {
        let builder = setup_builder(None);

        let signer = builder
            .commit(Instructions(vec![Instruction::AttachMetadata {
                metadata: (123, AuxMetadata::String("Hello".to_string())),
            }]))
            .unwrap();

        let tx = signer.get_tx();

        assert!(match tx.auxiliary_data {
            Nullable::Some(_) => true,
            _ => false,
        });
        assert!(tx.transaction_body.auxiliary_data_hash.is_some());
        assert!(tx.transaction_body.script_data_hash.is_none());
    }

    #[test]
    fn test_nebula_create_royalty() {
        let builder = setup_builder(None);


        let redeemer = hex::encode(
            PlutusData::Constr(Constr::from_index(0, vec![]))
                .encode_fragment()
                .unwrap(),
        );


        let signer = builder
            .commit(Instructions(vec![
              Instruction::CollectFrom { utxos: vec![
                Utxo {tx_hash: "3b06c12f0df26630dc5b454a3738bfbf318d3caba5754dc9096e01094d2e2e46".to_string(),
                output_index: 4, 
                address: ADDRESS_OTHER.to_string(),
                 assets: Assets::from([("afcaf4f8dc27c51ada41ace7008ccef79b7d3a71d4a7c995adfd7cd0000de1404275643332".to_string(), 1), 
                 ("afcaf4f8dc27c51ada41ace7008ccef79b7d3a71d4a7c995adfd7cd0000de14042756430".to_string(), 1),
                  ("afcaf4f8dc27c51ada41ace7008ccef79b7d3a71d4a7c995adfd7cd0000de1404275643235".to_string(), 1), 
                  ("lovelace".to_string(), 1314550),
                  ("afcaf4f8dc27c51ada41ace7008ccef79b7d3a71d4a7c995adfd7cd0000de14042756431313131".to_string(),1)]),
                  datum: None, 
                  datum_hash: None, 
                  script_ref: None}
              ], redeemer: None },
              Instruction::Mint { assets: Assets::from_unit("62d864f576a29db34bc681a44d06639f593d71751453c5f2fcb9d663001f4d70526f79616c7479".to_string(), 1), redeemer: Some(redeemer.clone()) },
              Instruction::PayTo { assets: Assets::from_unit("62d864f576a29db34bc681a44d06639f593d71751453c5f2fcb9d663001f4d70526f79616c7479".to_string(), 1), address: ADDRESS.to_string(), datum_variant: Some(DatumVariant::Inline("d8798383d87984d87982d87981581c1a3d2cd7e9f8a05714ba8c4a0d96f7e1f60d5455956bfe74de7b157fd87a80190271d879811a00061a80d87a80d87984d87982d87981581ca9516d49d80c2deb7a9029ef421ca8b5e5b9e0b28dbd6fc2c6726fa7d87a801909c4d87a80d87a80d87984d87982d87981581cde467543f7cee91138085797279a458e74020c30be0b325ceada11d2d87a801909c4d87a80d879811a0016e36001d87980".to_string())), script_ref: None },
              Instruction::AttachScript { script: Script::PlutusV2 { script: "5836583458e3010030034c0129d87982d8798158203b06c12f0df26630dc5b454a3738bfbf318d3caba5754dc9096e01094d2e2e46040001".to_string() } }
            ]))
            .unwrap();

        // let tx = signer.get_tx();
    }
}
