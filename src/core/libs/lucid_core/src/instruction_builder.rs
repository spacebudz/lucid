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

        self.build()?;

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
                        if let (Some(_), Some(data)) = (&utxo.datum_hash, &utxo.datum) {
                            self.add_data(data)?;
                        }
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
                        if *quantity >= 0 {
                            self.implicit_input += Assets::from_unit(unit.clone(), *quantity);
                        } else {
                            self.implicit_output += Assets::from_unit(unit.clone(), -quantity);
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

                    let mut utxo = Utxo::from_output(
                        address,
                        assets,
                        datum_hash,
                        datum,
                        script_ref.map(|s| s.try_double_cbor()).transpose()?,
                    );
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

                    let mut utxo = Utxo::from_output(
                        address,
                        assets,
                        datum_hash,
                        datum,
                        script_ref.map(|s| s.try_double_cbor()).transpose()?,
                    );
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
        if let (Some(_), Some(data)) = (&utxo.datum_hash, &utxo.datum) {
            self.add_data(data)?;
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

    fn adjust_fee(&mut self) -> CoreResult<(u64, bool)> {
        let mut old_fee = self.fee;
        let mut new_fee = {
            self.assemble()?;
            self.calculate_fee()
        };

        if old_fee >= new_fee {
            return Ok((old_fee, true));
        }

        // we run in a loop to make 100% sure that adding to fee field and subtracting from output does not change bytes length
        // if it does we loop again and adjust accordingly
        while old_fee < new_fee {
            if self.change_outputs.len() <= 0 {
                return Ok((0, false));
            }

            let index = self.change_outputs.len() - 1;

            let change_output = &mut self.change_outputs[index];

            // assumes new fee is always higher than previously set fee
            let amount_to_subtract = new_fee - old_fee;

            change_output.assets = change_output
                .assets
                .clone()
                .clamped_sub(Assets::from_lovelace(amount_to_subtract));

            if change_output.assets.get_lovelace()
                < change_output.required_lovelace(self.protocol_parameters.coins_per_utxo_byte)
                && change_output.assets.contains_other()
            {
                return Ok((0, false));
            }
            // burn whole output
            if change_output.assets.get_lovelace()
                < change_output.required_lovelace(self.protocol_parameters.coins_per_utxo_byte)
            {
                let fee = old_fee + change_output.assets.get_lovelace() + amount_to_subtract;
                self.fee = fee;
                self.change_outputs.pop().unwrap();
                return Ok((fee, true));
            }

            self.fee = new_fee;
            old_fee = new_fee;
            new_fee = {
                self.assemble()?;
                self.calculate_fee()
            };
        }

        Ok((new_fee, true))
    }

    fn evaluate_redeemers(&mut self) -> CoreResult<bool> {
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

        fn adjust_collateral(builder: &mut InstructionBuilder) -> CoreResult<bool> {
            let mut old_total_collateral = (builder.fee
                * builder.protocol_parameters.collateral_percentage as u64)
                .div_ceil(100);

            let adjusted_fee = match builder.adjust_fee()? {
                (fee, true) => fee,
                (_, false) => return Ok(false),
            };

            let mut new_total_collateral = (adjusted_fee
                * builder.protocol_parameters.collateral_percentage as u64)
                .div_ceil(100);

            if old_total_collateral >= new_total_collateral {
                return Ok(true);
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
                        return Err(CoreError::NotEnoughLovelaceForOutput.to_msg());
                    }

                    builder.total_collateral = Some(new_total_collateral);

                    builder.collateral_return = Some(collateral_return);
                }

                old_total_collateral = new_total_collateral;

                let adjusted_fee = match builder.adjust_fee()? {
                    (fee, true) => fee,
                    (_, false) => return Ok(false),
                };

                new_total_collateral = (adjusted_fee
                    * builder.protocol_parameters.collateral_percentage as u64)
                    .div_ceil(100);
            }

            Ok(true)
        }

        match &self.redeemers {
            Some(_) => {
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

                let collateral = self.collateral.clone().unwrap_or_default();

                let mut available_collateral: Vec<Utxo> = self
                    .selection
                    .difference(&collateral)
                    .cloned()
                    .map(|b| b.utxo)
                    .collect();

                available_collateral.sort_by(|a, b| {
                    self.get_pure_lovelace(&a.assets)
                        .cmp(&self.get_pure_lovelace(&b.assets))
                });

                for _ in (collateral.len() as u16)..self.protocol_parameters.max_collateral_inputs {
                    if let Ok(b) = adjust_collateral(self) {
                        return Ok(b);
                    }

                    let utxo = available_collateral
                        .pop()
                        .ok_or(CoreError::msg("Collateral balance insufficient"))?;

                    self.add_collateral(utxo)?;
                }

                Err(CoreError::msg(format!(
                    "Reached maximum collateral inputs, {} allowed",
                    self.protocol_parameters.max_collateral_inputs
                )))
            }
            None => Ok(true),
        }
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

    fn build(&mut self) -> CoreResult<()> {
        let mut available_selection: Vec<_> = self
            .selection
            .difference(&self.inputs)
            .cloned()
            .map(|b| b.utxo)
            .collect();

        self.coin_selection(&mut available_selection)?;

        available_selection.sort_by(|a, b| a.assets.get_lovelace().cmp(&b.assets.get_lovelace()));

        for i in 0..10 {
            if i > 0 {
                if self.without_coin_selection {
                    return Err(CoreError::TxBuildFail.to_msg());
                }

                self.add_input(
                    available_selection
                        .pop()
                        .ok_or(CoreError::ExhaustedInputs("lovelace".to_string()).to_msg())?,
                    None,
                )?
            }

            self.assemble()?;

            if !self.balance() {
                continue;
            }

            self.collect_redeemers()?;

            self.assemble()?;

            if !self.evaluate_redeemers()? {
                continue;
            }

            self.assemble()?;

            if !self.adjust_fee()?.1 {
                continue;
            }

            return Ok(());
        }

        Err(CoreError::TxBuildFail.to_msg())
    }

    fn coin_selection(&mut self, available_selection: &mut Vec<Utxo>) -> CoreResult<()> {
        if self.without_coin_selection {
            return Ok(());
        }

        let mut total_input = self
            .inputs
            .iter()
            .fold(Assets::new(), |acc, curr| acc + curr.utxo.assets.clone())
            + self.implicit_input.clone();

        let total_output = self
            .outputs
            .iter()
            .fold(Assets::new(), |acc, curr| acc + curr.assets.clone())
            + self.implicit_output.clone();

        let target = total_output.clone().clamped_sub(total_input.clone());

        // largest first
        for (unit, _) in target
            .get_all_others()
            .iter()
            .chain(Assets::from_lovelace(total_input.get_lovelace()).iter())
        {
            if unit == "lovelace" {
                available_selection.sort_by(|a, b| {
                    self.get_pure_lovelace(&a.assets)
                        .cmp(&self.get_pure_lovelace(&b.assets))
                });
            } else {
                available_selection
                    .sort_by(|a, b| a.assets.get_unit(unit).cmp(&b.assets.get_unit(unit)));
            }

            while total_input.get_unit(unit) < target.get_unit(unit) {
                let utxo = available_selection
                    .pop()
                    .ok_or(CoreError::ExhaustedInputs(unit.to_string()).to_msg())?;
                if utxo.assets.get(unit).is_none() {
                    return Err(CoreError::ExhaustedInputs(unit.to_string()).to_msg());
                }
                self.add_input(utxo.clone(), None)?;
                total_input += utxo.assets;
            }
        }

        if self.inputs.len() <= 0 {
            match available_selection.pop() {
                Some(utxo) => self.add_input(utxo, None),
                None => Err(CoreError::msg("At least 1 utxo required, found none")),
            }
        } else {
            Ok(())
        }
    }

    fn balance(&mut self) -> bool {
        let mut fee = self.calculate_fee();
        let mut change_outputs = Vec::new();

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
                true
            }
            Some(Ordering::Less) => return false,
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
                        change_outputs.push(change_output.clone());

                        total_change = total_change.clamped_sub(change_output.assets);

                        if total_change.len() <= 0 {
                            return false;
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
                 -> Result<(Vec<Utxo>, u64), ()> {
                    if !total_change.contains_other() {
                        return Err(());
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

                    total_change_check =
                        total_change_check.clamped_sub(change_output_0.assets.clone());

                    fee_check +=
                        change_output_0.get_fee_for_output(builder.protocol_parameters.min_fee_a);

                    let mut change_output_1 = change_output.clone();
                    change_output_1.assets = total_change_check.clone();

                    fee_check +=
                        change_output_1.get_fee_for_output(builder.protocol_parameters.min_fee_a);

                    change_output_1.assets.set_lovelace(
                        change_output_1
                            .assets
                            .get_lovelace()
                            .saturating_sub(fee_check),
                    );

                    if change_output_1.assets.get_lovelace()
                        < change_output_1
                            .required_lovelace(builder.protocol_parameters.coins_per_utxo_byte)
                    {
                        return Err(());
                    };

                    Ok((vec![change_output_0, change_output_1], fee_check))
                };

                let try_one_output = |builder: &mut InstructionBuilder,
                                      total_change: &Assets,
                                      change_output: &Utxo,
                                      fee: u64|
                 -> Result<(Vec<Utxo>, u64), ()> {
                    let mut fee_check = fee;
                    let total_change_check = total_change.clone();
                    let mut change_output_0 = change_output.clone();
                    change_output_0.assets = total_change_check.clone();

                    fee_check +=
                        change_output_0.get_fee_for_output(builder.protocol_parameters.min_fee_a);

                    change_output_0.assets.set_lovelace(
                        change_output_0
                            .assets
                            .get_lovelace()
                            .saturating_sub(fee_check),
                    );

                    if change_output_0.assets.get_lovelace()
                        < change_output_0
                            .required_lovelace(builder.protocol_parameters.coins_per_utxo_byte)
                    {
                        return Err(());
                    };

                    Ok((vec![change_output_0], fee_check))
                };

                let try_just_fee =
                    |total_change: &Assets, fee: u64| -> Result<(Vec<Utxo>, u64), ()> {
                        if total_change.contains_other() || total_change.get_lovelace() < fee {
                            return Err(());
                        }

                        Ok((vec![], total_change.get_lovelace()))
                    };

                match try_two_outputs(self, &total_change, &change_output, fee)
                    .or_else(|_| try_one_output(self, &total_change, &change_output, fee))
                    .or_else(|_| try_just_fee(&total_change, fee))
                {
                    Ok((outputs, fee)) => {
                        change_outputs.extend(outputs);
                        self.change_outputs = change_outputs;
                        self.fee = fee;
                        true
                    }
                    Err(_) => false,
                }
            }
            None => false,
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
        Change, Instruction, InstructionBuilder, Instructions, RelevantProtocolParameters,
    };
    use crate::{
        addresses::{Addresses, Network},
        codec::{
            Assets, AuxMetadata, ConstrConversion, DelegVariant, Delegation, Script, Utxo, Utxos,
            Withdrawal,
        },
        hasher::Hasher,
        instruction_builder::DatumVariant,
        utils::Utils,
    };
    use std::{collections::HashMap, vec};

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
                    (
                        "PlutusV3".to_string(),
                        vec![
                            100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32,
                            201305, 8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000,
                            100, 16000, 100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4,
                            72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 123203, 7305,
                            -900, 1716, 549, 57, 85848, 0, 1, 1, 1000, 42921, 4, 2, 24548, 29498,
                            38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895,
                            32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1,
                            28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32,
                            72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 123203, 7305, -900,
                            1716, 549, 57, 85848, 0, 1, 90434, 519, 0, 1, 74433, 32, 85848, 123203,
                            7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 85848, 123203, 7305, -900,
                            1716, 549, 57, 85848, 0, 1, 955506, 213312, 0, 2, 270652, 22588, 4,
                            1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1,
                            81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32,
                            24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10,
                            16000, 100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1, 52538055,
                            3756, 18, 267929, 18, 76433006, 8868, 18, 52948122, 18, 1995836, 36,
                            3227919, 12, 901022, 1, 166917843, 4307, 36, 284546, 36, 158221314,
                            26549, 36, 74698472, 36, 333849714, 1, 254006273, 72, 2174038, 72,
                            2261318, 64571, 4, 207616, 8310, 4, 1293828, 28716, 63, 0, 1, 1006041,
                            43623, 251, 0, 1, 100181, 726, 719, 0, 1, 100181, 726, 719, 0, 1,
                            100181, 726, 719, 0, 1, 107878, 680, 0, 1, 95336, 1, 281145, 18848, 0,
                            1, 180194, 159, 1, 1, 158519, 8942, 0, 1, 159378, 8813, 0, 1, 107490,
                            3298, 1, 106057, 655, 1, 1964219, 24520, 3,
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
                        variant: DelegVariant::Pool(
                            "pool19f6guwy97mmnxg9dz65rxyj8hq07qxud886hamyu4fgfz7dj9gl".to_string(),
                        ),
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
                        variant: DelegVariant::Pool(
                            "pool19f6guwy97mmnxg9dz65rxyj8hq07qxud886hamyu4fgfz7dj9gl".to_string(),
                        ),
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
    fn test_output_reference_spending() {
        let builder = setup_builder(None);

        let redeemer = hex::encode(
            PlutusData::Constr(Constr::from_index(0, vec![]))
                .encode_fragment()
                .unwrap(),
        );

        let raw_script =  Utils::apply_params_to_script("9fd8799f5820c9021f3a374a011798535b290c047f601891bba6a127d08ce66b6e801efba09e01ffd8799f4200009f010203ffffff", "59010d01010032323232323232323222253330053232323232533300a3370e900018061baa00113253333330120031533300b3370e900018069baa0031533300f300e37540062a6660166464660020026eb0c008c040dd50031129998090008a501332253330103375e600a60266ea80080385288998020020009809800980a00091808800899baf0084c10cd8799f4200009f010203ffff0014a00140140140140140146eb8c03cc034dd50008b1807180780198068011806001180600098041baa001149854cc01924011856616c696461746f722072657475726e65642066616c73650013656153300249010f5f72656465656d65723a20566f696400165734ae7155ceaab9e5573eae855d12ba41").unwrap();

        let script = Script::PlutusV3 { script: raw_script };

        let policy_id = Hasher::hash_script(script.clone()).unwrap();

        let result = builder.commit(Instructions(vec![
            Instruction::CollectFrom {
                utxos: vec![Utxo {
                    tx_hash: "c9021f3a374a011798535b290c047f601891bba6a127d08ce66b6e801efba09e"
                        .to_string(),
                    output_index: 1,
                    address: ADDRESS_OTHER.to_string(),
                    assets: Assets::from([("lovelace".to_string(), 10000000)]),
                    datum: None,
                    datum_hash: None,
                    script_ref: None,
                }],
                redeemer: None,
            },
            Instruction::Mint {
                assets: Assets::from_unit(policy_id.clone(), 1),
                redeemer: Some(redeemer.clone()),
            },
            Instruction::PayTo {
                assets: Assets::from_unit(policy_id, 1),
                address: ADDRESS.to_string(),
                datum_variant: None,
                script_ref: None,
            },
            Instruction::AttachScript { script },
        ]));

        assert!(result.is_ok())
    }

    #[test]
    fn test_another_coin_selection() {
        let user_utxo = Utxo {
            tx_hash: "b3620862b488ed0ac0d6f52638c328431d12931ff73755dfeab9d4e41633861a".to_string(),
            output_index: 0,
            address: ADDRESS.to_string(),
            assets: Assets::from([
                ("834e3084c6fbb4ba9236be316f033e46db8dd043b468b13bbb4c9d60000de140427564343434".to_string(),1),
                ("834e3084c6fbb4ba9236be316f033e46db8dd043b468b13bbb4c9d60000de14042756431393033".to_string(),2),
                ("834e3084c6fbb4ba9236be316f033e46db8dd043b468b13bbb4c9d60000de140427564373032".to_string(),1),
                ("834e3084c6fbb4ba9236be316f033e46db8dd043b468b13bbb4c9d60000de14042756439393939".to_string(),1),
                ("834e3084c6fbb4ba9236be316f033e46db8dd043b468b13bbb4c9d60000de14042756431313131".to_string(),1),
                ("834e3084c6fbb4ba9236be316f033e46db8dd043b468b13bbb4c9d60000de14042756431".to_string(),1),
                ("834e3084c6fbb4ba9236be316f033e46db8dd043b468b13bbb4c9d60000de1404275643535".to_string(),1),
                ("834e3084c6fbb4ba9236be316f033e46db8dd043b468b13bbb4c9d60000de1404275643133".to_string(),1),
                ("834e3084c6fbb4ba9236be316f033e46db8dd043b468b13bbb4c9d60000de140427564363030".to_string(),1),
                ("lovelace".to_string(),2087182),
            ]),
            datum_hash: None,
            datum: None,
            script_ref:None
        };

        let builder = setup_builder(Some(vec![user_utxo.clone()]));

        let signer = builder.commit(Instructions(vec![
            Instruction::CollectFrom {
                utxos: vec![Utxo {
                    tx_hash: "2bbe956acdbea3552c7d46e05080ede65f93961e8b75d0232b6312488deb9a10"
                        .to_string(),
                    output_index: 2,
                    address: "addr1w93w70usdv9ryunwtwm7mjultqcg6k86gc4uncpz7axww5sfjjsl8".to_string(),
                    assets: Assets::from([("lovelace".to_string(), 1215420), ("834e3084c6fbb4ba9236be316f033e46db8dd043b468b13bbb4c9d60000643b042756431393033".to_string(),1)]),
                    datum_hash: Some("2cbdc700d2becc75619bd0cdcdf2de9f2f79533f258bfdc358257b8fa8476eeb".to_string()),
                    datum: Some("d8799fa5446e616d654e5370616365427564202331393033467472616974739f4943616d6f20537569744442656c744e436f76657265642048656c6d65744b536b6920476f67676c6573ff44747970654341706545696d6167655f5840697066733a2f2f6261666b7265696374676274676b6c6c3373347537667463733670767835366773356c7267756b6772737574716f67336b666b6875666d6672423365ff467368613235365820533066652d7b9729f2cc52f3eb7ef8d2eae26a28d19527071b6a2a8f42b0b1d901d87980ff".to_string()),
                    script_ref: None,
                }],
                redeemer: Some("d87a80".to_string()),
            },
            Instruction::CollectFrom { utxos: vec![user_utxo],redeemer: None },
            Instruction::PayToContract {
                assets: Assets::from_unit("834e3084c6fbb4ba9236be316f033e46db8dd043b468b13bbb4c9d60000643b042756431393033".to_string(), 1),
                address: "addr1w93w70usdv9ryunwtwm7mjultqcg6k86gc4uncpz7axww5sfjjsl8".to_string(),
                datum_variant: DatumVariant::AsHash("d8799fa5446e616d654e5370616365427564202331393033467472616974739f4943616d6f20537569744442656c744e436f76657265642048656c6d65744b536b6920476f67676c6573ff44747970654341706545696d6167655f5840697066733a2f2f6261666b7265696374676274676b6c6c3373347537667463733670767835366773356c7267756b6772737574716f67336b666b6875666d6672423365ff467368613235365820533066652d7b9729f2cc52f3eb7ef8d2eae26a28d19527071b6a2a8f42b0b1d901d87980ff".to_string()),
                script_ref: None,
            },
            Instruction::PayTo {
                assets: Assets::from_lovelace(2000000),
                address: ADDRESS.to_string(),
                datum_variant: Some(DatumVariant::AsHash("d87a80".to_string())),
                script_ref: None,
            },
            Instruction::AttachScript { script: Script::PlutusV2 { script: "59104b01000033332323232323233223232323232332232323232323232323232323232323232323322323232323232323232323232323322323232323232322223223232322322323253353332223232323232323232323232533500d13304b3304148004d5401c888888010cc12ccc10520013550072222220013304b3302a50023550072222220063304b3302a50023550072222220033304b3302a3305003f50063305003f50043304b3302a3301d03f500650183304b3302a3301d03f50045335330293302a500548810431393033003302a50054890436343133001375c03626eb8068cc0a8d5400c8800540104cc12cc8c8ccd54c0a448004d414141308d400488ccd54c0b048004d414d413c8d400488ccd40048cc1292000001223304b00200123304a00148000004cc088008004c07cd54008880054028cc12cc8d403c888d40108894cd4cc14001800c54cd4cc1200140084cc140010004414c414cd5400488008ccd54c09c48004d409540a08d400488c8ccd5cd19b8800148008140144ccc088c07c004d5403088008cdc5280d998298211aa80611000991a80091111111111100628040a99aa99a9a805911a8011111111111111999a8069282012820128201199aa981a8900099a82e91299a801108018800a82011a80091299a9982880100209a8220018a821806909a800911a80091111a809111a801111111111111199aa981b09000911a8011111299a9a80c111a803111919a802919a8021299a999ab9a3371e0040020dc0da2a00620da40da466a00840da4a66a666ae68cdc78010008370368a80188368a99a80190a99a8011099a801119a801119a801119a8011198270010009038119a801103811982700100091103811119a8021038111299a999ab9a3370e00c0060e60e42a66a666ae68cdc38028010398390998338020008839083908358a99a800908358835899a82f8030028802a82d00509931901e19ab9c4901024c660003c1622153350011533532323235001222222222222300e002500b320013550552253350011503622135002225335333573466e3c00801c1541504d40ec0044c01800cd54ccd4d400888880085885884d412800480048c8c8c8c854cd4ccccccd5d200291999ab9a3370e6aae7540152000233335573ea00a4a07646666aae7d4014940f08cccd55cfa8029281e91999aab9f35744a00c4a66aa66a646666666ae900049410094100941008d4104dd6801128200251aba1500821350403305e35742a0140022a07c426a08060026ae854020540f8940f812011c118114940e810c940e4940e4940e4940e410c84cd54138004c08c020584d5d1280089aba25001135573ca00226ea80045888584d540048800854cd4c0fcc06cc065402058884d40088894cd40104cd5412400c00888584d5400c8888880084cc12d200e5001135500122222200515335303b323500122222222222200850011622135002222533500416221350022225335004112333333001009008007004003002221613500422002153353039500116221350022225335004133550430030022216130143012500113235001220013500122350022222222222223333500d26262625335333553026120012253353500222353500122220042233500220572058133504b0020011001504a00d16221533500110022215335001153355335333573466e3cd4d4d4d401088004888801088cd4008988d414c0048004d4d4d4d400888004888801088cd4008988d414c00480041541504150415454cd4cc120d40108800801440104008588858cccd5cd19b8735573aa010900011998221aba15008375a6ae85401cd5d09aba2500723263203033573806206005c6666ae68cdc3a80224004424400246666ae68cdc3a802a40004244004464c6406266ae700c80c40bc0b8cccd5cd19b8735573aa0049000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233502d02e35742a01866a05a05c6ae85402ccd40b40bcd5d0a805199aa818bae503035742a012666aa062eb940c0d5d0a80419a81681c1aba150073335503103975a6ae854018c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233504375a6ae854008c110d5d09aba2500223263204633573808e08c08826aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a086eb4d5d0a80118221aba135744a004464c6408c66ae7011c1181104d55cf280089baa001357426ae8940088c98c8108cd5ce02182102009aab9e5001137540026ae854014cd40b5d71aba1500433355031035200135742a006666aa062eb88004d5d0a801181b9aba135744a004464c6407c66ae700fc0f80f04d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a80118139aba135744a004464c6406066ae700c40c00b840bc4c98c80bccd5ce2481035054350002f135573ca00226ea80044d55ce9baa001135744a00226aae7940044dd500089bae001235001222200322333718900000100091919aa98050900091a8009119aa81700119aa98068900091a8009119aa818801199a80091981ea4000002446607c00400246607a0029000000998020010009919aa98050900091a8009119aa81700119aa98068900091a8009119aa81880119b8248004004004004cd40a0cd540a8038cd40a0cd540a8038ccc00800403803940a540a4888c8c8c004014c8004d540e088cd400520002235002225335333573466e3c0080240e00dc4c01c0044c01800cc8004d540dc88cd400520002235002225335333573466e3c00801c0dc0d840044c01800c88cd54c020480048d400488cd540b0008ccd40048cd54c030480048d400488cd540c0008d5403400400488ccd5540200440080048cd54c030480048d400488cd540c0008d54030004004ccd55400c030008004444888ccd54c01048005409ccd54c020480048d400488cd540b0008d54024004ccd54c0104800488d4008894cd4ccd54c03448004d402d40388d400488cc028008014018400c4cd40ac01000d40a0004cd54c020480048d400488c8cd540b400cc004014c8004d540e0894cd40044d5402800c884d4008894cd4cc03000802044888cc0080280104c01800c008c8004d540c488448894cd40044008884cc014008ccd54c01c480040140100044484888c00c0104484888c00401048cd40ac88ccd400c88008008004d400488004c8004d540b48844894cd400454090884cd4094c010008cd54c01848004010004c8004d540b088448894cd40044d400c88004884ccd401488008c010008ccd54c01c48004014010004448cc004008094894cd40084098400488ccd5cd19b8f0020010250244881001232230023758002640026aa050446666aae7c004940788cd4074c010d5d080118019aba2002014232323333573466e1cd55cea8012400046644246600200600460186ae854008c014d5d09aba2500223263201433573802a02802426aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea80124000466442466002006004602a6ae854008cd4034050d5d09aba2500223263201933573803403202e26aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931900d99ab9c01c01b019018017135573aa00226ea8004d5d0a80119a804bae357426ae8940088c98c8054cd5ce00b00a80989aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5409488c8cccd55cf8011280e119a80d99aa80e98031aab9d5002300535573ca00460086ae8800c0484d5d080089119191999ab9a3370ea002900011a80398029aba135573ca00646666ae68cdc3a801240044a00e464c6402466ae7004c04804003c4d55cea80089baa0011212230020031122001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402066ae7004404003803403002c4d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6401866ae700340300284d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200a33573801601401026ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201333573802802602202001e01c01a01801626aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900619ab9c00d00c00a009135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8024cd5ce00500480380309aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900519ab9c00b00a008007006135573aa00226ea80048c8cccd5cd19b8750014800880548cccd5cd19b8750024800080548c98c8018cd5ce00380300200189aab9d37540029309000a49035054310048020894cd4ccd5cd19b8f3500222002350012200200f00e1333573466e1cd400888004d40048800403c038403888ccd5cd19b8700200100e00d233002500500132001355010222533500110022213500222330073330080020060010033200135500f22225335001100222135002225335333573466e1c00520000110101333008007006003133300800733500912333001008003002006003112200212212233001004003112212330010030021212300100222333573466ebc008004018014448cc004008010894cd40084004400c48800848800448cd400888ccd400c88008008004d40048800448848cc00400c00888ccdc600119b81371a002004002444246660020080060044466e00008004448c8c00400488cc00cc0080080053001054400001070004c010544000643b0004c010544000de1400001".to_string() } },
        ])).unwrap();

        let tx = signer.get_tx();

        assert_eq!(
            tx.transaction_witness_set
                .plutus_data
                .map_or(0, |d| d.len()),
            2
        )
    }
}
