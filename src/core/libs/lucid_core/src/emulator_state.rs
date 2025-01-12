/// The `EmulatorState` struct represents the state of the emulator, including the ledger, mempool,
/// staking information, datum table, block height, slot, and time.
use super::addresses::{Addresses, Credential};
use super::codec::{Certificate, Delegation, Script, Utxos};
use super::error::{CoreError, CoreResult};
use super::instruction_builder::BuilderInput;
use crate::addresses::Network;
use crate::codec::Utxo;
use crate::hasher::Hasher;
use bech32::Hrp;
use pallas_crypto::key::ed25519::{PublicKey, Signature};
use pallas_primitives::conway::{
    Hash, MintedTx, NativeScript, PlutusData, RedeemerTag, Redeemers, RedeemersKey, Tx,
};
use pallas_primitives::DatumHash;
use pallas_traverse::ComputeHash;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap, HashSet};
use tsify::Tsify;
use uplc::Fragment;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub struct EmulatorState {
    ledger: BTreeSet<BuilderInput>,
    mempool: BTreeSet<BuilderInput>,
    staking: HashMap<String, Staking>,
    datum_table: HashMap<DatumHash, PlutusData>,
    block_height: u64,
    slot: u64,
    time: u64,
}

#[wasm_bindgen]
impl EmulatorState {
    #[wasm_bindgen(constructor)]
    pub fn new(time: f64, utxos: Option<Utxos>) -> Self {
        let mut ledger = BTreeSet::new();
        match utxos {
            Some(utxos) => {
                for utxo in utxos.0 {
                    ledger.insert(BuilderInput {
                        utxo,
                        redeemer: None,
                    });
                }
            }
            None => (),
        };
        Self {
            ledger,
            mempool: BTreeSet::new(),
            staking: HashMap::new(),
            datum_table: HashMap::new(),
            block_height: 0,
            slot: 0,
            time: time as u64,
        }
    }

    pub fn validate(&mut self, tx: String) -> CoreResult<String> {
        let decoded_tx = hex::decode(tx).map_err(CoreError::msg)?;
        let tx: Tx = MintedTx::decode_fragment(&decoded_tx)
            .map_err(CoreError::msg)?
            .into();

        let tx_hash = tx.transaction_body.compute_hash();

        let lower_bound = tx.transaction_body.validity_interval_start;
        let upper_bound = tx.transaction_body.ttl;

        if let Some(lower_bound) = lower_bound {
            if self.slot < lower_bound {
                return Err(CoreError::msg(format!(
                    "Transaction is not valid yet: current slot {}, found {}",
                    self.slot, lower_bound
                )));
            }
        }

        if let Some(upper_bound) = upper_bound {
            if self.slot > upper_bound {
                return Err(CoreError::msg(format!(
                    "Transaction is expired: current slot {}, found {}",
                    self.slot, upper_bound
                )));
            }
        }

        let datum_hashes = if let Some(plutus_data) = &tx.transaction_witness_set.plutus_data {
            plutus_data
                .iter()
                .map(|data| (data.compute_hash(), data.clone()))
                .collect::<HashMap<DatumHash, PlutusData>>()
        } else {
            HashMap::new()
        };

        let consumed_hashes = RefCell::new(HashSet::<Hash<28>>::new());
        let mut consumed_datum_hashes: HashSet<Hash<32>> = HashSet::new();

        let key_hashes = if let Some(vkey_witnesses) = tx.transaction_witness_set.vkeywitness {
            vkey_witnesses
                .iter()
                .map(|w| {
                    let vkey_bytes: [u8; 32] = w
                        .vkey
                        .to_vec()
                        .try_into()
                        .map_err(|_| CoreError::msg("Invalid vkey length"))?;
                    let signature_bytes: [u8; 64] = w
                        .signature
                        .to_vec()
                        .try_into()
                        .map_err(|_| CoreError::msg("Invalid signature length"))?;
                    let public_key = PublicKey::from(vkey_bytes);
                    let key_hash = public_key.compute_hash();
                    let signature = Signature::from(signature_bytes);
                    if !public_key.verify(&tx_hash, &signature) {
                        return Err(CoreError::msg(format!(
                            "Invalid signature: key hash {}",
                            key_hash.to_string()
                        )));
                    }
                    Ok(key_hash)
                })
                .collect::<CoreResult<_>>()?
        } else {
            HashSet::new()
        };

        let native_script_hashes =
            if let Some(native_scripts) = tx.transaction_witness_set.native_script {
                native_scripts
                    .iter()
                    .map(|script| {
                        let script_hash = script.compute_hash();
                        if verify_script(
                            script,
                            lower_bound,
                            upper_bound,
                            &key_hashes,
                            &mut consumed_hashes.borrow_mut(),
                        ) {
                            Ok(script_hash)
                        } else {
                            Err(CoreError::msg(format!(
                                "Invalid native script witness: script hash {}",
                                script_hash.to_string()
                            )))
                        }
                    })
                    .collect::<CoreResult<_>>()?
            } else {
                HashSet::new()
            };

        let native_script_hashes_optional = RefCell::new(HashMap::new());
        let plutus_script_hashes_optional = RefCell::new(HashSet::new());

        let plutus_v1_script_hashes =
            if let Some(scripts) = &tx.transaction_witness_set.plutus_v1_script {
                scripts
                    .iter()
                    .map(|script| {
                        let script_hash = script.compute_hash();
                        plutus_script_hashes_optional
                            .borrow_mut()
                            .insert(script_hash);
                        script_hash
                    })
                    .collect::<HashSet<_>>()
            } else {
                HashSet::new()
            };

        let plutus_v2_script_hashes =
            if let Some(scripts) = &tx.transaction_witness_set.plutus_v2_script {
                scripts
                    .iter()
                    .map(|script| {
                        let script_hash = script.compute_hash();
                        plutus_script_hashes_optional
                            .borrow_mut()
                            .insert(script_hash);
                        script_hash
                    })
                    .collect::<HashSet<_>>()
            } else {
                HashSet::new()
            };

        let plutus_v3_script_hashes =
            if let Some(scripts) = &tx.transaction_witness_set.plutus_v3_script {
                scripts
                    .iter()
                    .map(|script| {
                        let script_hash = script.compute_hash();
                        plutus_script_hashes_optional
                            .borrow_mut()
                            .insert(script_hash);
                        script_hash
                    })
                    .collect::<HashSet<_>>()
            } else {
                HashSet::new()
            };

        let plutus_script_hashes = plutus_v1_script_hashes
            .union(&plutus_v2_script_hashes)
            .cloned()
            .collect::<HashSet<_>>()
            .union(&plutus_v3_script_hashes)
            .cloned()
            .collect::<HashSet<_>>();

        let redeemers_keys = if let Some(redeemers) = &tx.transaction_witness_set.redeemer {
            match redeemers {
                Redeemers::List(redeemers) => redeemers
                    .iter()
                    .map(|redeemer| RedeemersKey {
                        tag: redeemer.tag.clone(),
                        index: redeemer.index,
                    })
                    .collect::<Vec<_>>(),
                Redeemers::Map(redeemers) => redeemers
                    .iter()
                    .map(|(key, _)| key.clone())
                    .collect::<Vec<_>>(),
            }
        } else {
            Vec::new()
        };

        let check_and_consum = |credential: Credential, redeemers_key: Option<RedeemersKey>| {
            check_and_consum(
                &key_hashes,
                &mut consumed_hashes.borrow_mut(),
                &redeemers_keys,
                &native_script_hashes,
                &native_script_hashes_optional.borrow(),
                &plutus_script_hashes,
                &plutus_script_hashes_optional.borrow(),
                lower_bound,
                upper_bound,
                credential,
                redeemers_key,
            )
        };

        let mut node_inputs = Vec::new();

        for input in &tx.transaction_body.inputs {
            let ledger_entry = self.ledger.get(&BuilderInput {
                utxo: Utxo::from_input(input.transaction_id.to_string(), input.index),
                redeemer: None,
            });

            let mempool_entry = self.mempool.get(&BuilderInput {
                utxo: Utxo::from_input(input.transaction_id.to_string(), input.index),
                redeemer: None,
            });

            let entry = match (ledger_entry, mempool_entry) {
                (Some(entry), _) => NodeInput::Ledger(entry.clone()),
                (_, Some(entry)) => NodeInput::Mempool(entry.clone()),
                _ => {
                    return Err(CoreError::msg(format!(
                        "Utxo not found or already spent: txid {}, index {}",
                        input.transaction_id, input.index
                    )))
                }
            };

            match &entry {
                NodeInput::Ledger(entry) | NodeInput::Mempool(entry) => {
                    match &entry.utxo.script_ref {
                        Some(script) => match script {
                            Script::Native { script } => {
                                let script = NativeScript::decode_fragment(
                                    &hex::decode(script).map_err(CoreError::msg)?,
                                )
                                .map_err(CoreError::msg)?;
                                let script_hash = script.compute_hash();
                                native_script_hashes_optional
                                    .borrow_mut()
                                    .insert(script_hash, script);
                            }
                            plutus => {
                                let script_hash = Hasher::hash_script(plutus.clone())?;
                                plutus_script_hashes_optional
                                    .borrow_mut()
                                    .insert(script_hash.parse().map_err(CoreError::msg)?);
                            }
                        },
                        _ => {}
                    };
                    match &entry.utxo.datum_hash {
                        Some(datum_hash) => {
                            consumed_hashes
                                .borrow_mut()
                                .insert(datum_hash.parse().map_err(CoreError::msg)?);
                        }
                        _ => {}
                    }
                }
            };

            node_inputs.push(entry);
        }

        if let Some(read_inputs) = &tx.transaction_body.reference_inputs {
            for input in read_inputs {
                let ledger_entry = self.ledger.get(&BuilderInput {
                    utxo: Utxo::from_input(input.transaction_id.to_string(), input.index),
                    redeemer: None,
                });

                let mempool_entry = self.mempool.get(&BuilderInput {
                    utxo: Utxo::from_input(input.transaction_id.to_string(), input.index),
                    redeemer: None,
                });

                let entry = match (ledger_entry, mempool_entry) {
                    (Some(entry), _) => entry,
                    (_, Some(entry)) => entry,
                    _ => {
                        return Err(CoreError::msg(format!(
                            "Read utxo not found or already spent: txid {}, index {}",
                            input.transaction_id, input.index
                        )))
                    }
                };

                match &entry.utxo.script_ref {
                    Some(script) => match script {
                        Script::Native { script } => {
                            let script = NativeScript::decode_fragment(
                                &hex::decode(script).map_err(CoreError::msg)?,
                            )
                            .map_err(CoreError::msg)?;
                            let script_hash = script.compute_hash();
                            native_script_hashes_optional
                                .borrow_mut()
                                .insert(script_hash, script);
                        }
                        plutus => {
                            let script_hash = Hasher::hash_script(plutus.clone())?;
                            plutus_script_hashes_optional
                                .borrow_mut()
                                .insert(script_hash.parse().map_err(CoreError::msg)?);
                        }
                    },
                    _ => {}
                };
                match &entry.utxo.datum_hash {
                    Some(datum_hash) => {
                        consumed_hashes
                            .borrow_mut()
                            .insert(datum_hash.parse().map_err(CoreError::msg)?);
                    }
                    _ => {}
                }
            }
        }

        // Check collateral
        if let Some(collateral) = &tx.transaction_body.collateral {
            for input in collateral {
                let ledger_entry = self.ledger.get(&BuilderInput {
                    utxo: Utxo::from_input(input.transaction_id.to_string(), input.index),
                    redeemer: None,
                });

                let mempool_entry = self.mempool.get(&BuilderInput {
                    utxo: Utxo::from_input(input.transaction_id.to_string(), input.index),
                    redeemer: None,
                });

                let entry = match (ledger_entry, mempool_entry) {
                    (Some(entry), _) => entry,
                    (_, Some(entry)) => entry,
                    _ => {
                        return Err(CoreError::msg(format!(
                            "Collateral utxo not found or already spent: txid {}, index {}",
                            input.transaction_id, input.index
                        )))
                    }
                };

                let credential = Addresses::address_to_credential(&entry.utxo.address)?;
                check_and_consum(credential, None)?;
            }
        }

        // Check required signers
        if let Some(required_signers) = &tx.transaction_body.required_signers {
            for signer in required_signers {
                let credential = Credential::Key {
                    hash: signer.to_string(),
                };
                check_and_consum(credential, None)?;
            }
        }

        // Check mint witnesses
        if let Some(mint) = &tx.transaction_body.mint {
            for (index, (policy_id, _)) in mint.iter().enumerate() {
                let credential = Credential::Script {
                    hash: policy_id.to_string(),
                };
                check_and_consum(
                    credential,
                    Some(RedeemersKey {
                        tag: RedeemerTag::Mint,
                        index: index as u32,
                    }),
                )?;
            }
        }

        // Check withdrawal witnesses
        let withdrawals_requests = if let Some(withdrawals) = &tx.transaction_body.withdrawals {
            let mut withdrawals_requests = HashMap::new();

            for (index, (reward_address, rewards)) in withdrawals.iter().enumerate() {
                let reward_address = pallas_addresses::Address::from_bytes(&reward_address)
                    .map_err(CoreError::msg)?
                    .to_bech32()
                    .map_err(CoreError::msg)?;
                let credential = Addresses::reward_address_to_credential(&reward_address)?;

                check_and_consum(
                    credential,
                    Some(RedeemersKey {
                        tag: RedeemerTag::Reward,
                        index: index as u32,
                    }),
                )?;

                let staking = self
                    .staking
                    .get(&reward_address)
                    .cloned()
                    .unwrap_or_default();

                if staking.rewards != *rewards {
                    return Err(CoreError::msg(format!(
                        "Invalid withdrawal amount: expected {}, found {}",
                        staking.rewards, rewards
                    )));
                }

                withdrawals_requests.insert(
                    reward_address,
                    Staking {
                        registered: staking.registered,
                        rewards: *rewards,
                        pool_id: staking.pool_id.clone(),
                    },
                );
            }
            withdrawals_requests
        } else {
            HashMap::new()
        };

        // Check certificate requests
        // Only:
        // 1. Stake registration
        // 2. Stake deregistration
        // 3. Stake delegation
        // all other certificate types are not checked and are considered valid
        let certificate_requests = if let Some(certificates) = tx.transaction_body.certificates {
            let mut certificate_requests = Vec::new();

            for (index, cert) in certificates.iter().enumerate() {
                match cert {
                    pallas_primitives::conway::Certificate::StakeRegistration(credential) => {
                        let reward_address = match credential {
                            pallas_primitives::conway::StakeCredential::AddrKeyhash(hash) => {
                                Addresses::credential_to_reward_address(
                                    Network::Preprod,
                                    Credential::Key {
                                        hash: hash.to_string(),
                                    },
                                )?
                            }
                            pallas_primitives::conway::StakeCredential::ScriptHash(hash) => {
                                Addresses::credential_to_reward_address(
                                    Network::Preprod,
                                    Credential::Script {
                                        hash: hash.to_string(),
                                    },
                                )?
                            }
                        };

                        if let Some(staking) = self.staking.get(&reward_address) {
                            if staking.registered {
                                return Err(CoreError::msg(format!(
                                    "Stake registration already exists: {}",
                                    &reward_address
                                )));
                            }
                        };

                        certificate_requests
                            .push(Certificate::StakeRegistration { reward_address });
                    }
                    pallas_primitives::conway::Certificate::StakeDeregistration(credential) => {
                        let (reward_address, credential) = match credential {
                            pallas_primitives::conway::StakeCredential::AddrKeyhash(hash) => (
                                Addresses::credential_to_reward_address(
                                    Network::Preprod,
                                    Credential::Key {
                                        hash: hash.to_string(),
                                    },
                                )?,
                                Credential::Key {
                                    hash: hash.to_string(),
                                },
                            ),
                            pallas_primitives::conway::StakeCredential::ScriptHash(hash) => (
                                Addresses::credential_to_reward_address(
                                    Network::Preprod,
                                    Credential::Script {
                                        hash: hash.to_string(),
                                    },
                                )?,
                                Credential::Script {
                                    hash: hash.to_string(),
                                },
                            ),
                        };

                        check_and_consum(
                            credential,
                            Some(RedeemersKey {
                                tag: RedeemerTag::Cert,
                                index: index as u32,
                            }),
                        )?;

                        let staking = self
                            .staking
                            .get(&reward_address)
                            .cloned()
                            .unwrap_or_default();

                        if !staking.registered {
                            return Err(CoreError::msg(format!(
                                "Stake registration not found: {}",
                                &reward_address
                            )));
                        }

                        certificate_requests
                            .push(Certificate::StakeDeregistration { reward_address });
                    }
                    pallas_primitives::conway::Certificate::StakeDelegation(
                        credential,
                        pool_key_hash,
                    ) => {
                        let (reward_address, credential) = match credential {
                            pallas_primitives::conway::StakeCredential::AddrKeyhash(hash) => (
                                Addresses::credential_to_reward_address(
                                    Network::Preprod,
                                    Credential::Key {
                                        hash: hash.to_string(),
                                    },
                                )?,
                                Credential::Key {
                                    hash: hash.to_string(),
                                },
                            ),
                            pallas_primitives::conway::StakeCredential::ScriptHash(hash) => (
                                Addresses::credential_to_reward_address(
                                    Network::Preprod,
                                    Credential::Script {
                                        hash: hash.to_string(),
                                    },
                                )?,
                                Credential::Script {
                                    hash: hash.to_string(),
                                },
                            ),
                        };
                        let pool_id = bech32::encode::<bech32::Bech32>(
                            Hrp::parse("pool").unwrap(),
                            &pool_key_hash.to_vec(),
                        )
                        .map_err(CoreError::msg)?;

                        check_and_consum(
                            credential,
                            Some(RedeemersKey {
                                tag: RedeemerTag::Cert,
                                index: index as u32,
                            }),
                        )?;

                        let staking = self
                            .staking
                            .get(&reward_address)
                            .cloned()
                            .unwrap_or_default();

                        if !staking.registered
                            && !certificate_requests
                                .iter()
                                .find(|c| match c {
                                    Certificate::StakeRegistration { reward_address: r } => {
                                        r == &reward_address
                                    }
                                    _ => false,
                                })
                                .is_some()
                        {
                            return Err(CoreError::msg(format!(
                                "Stake registration not found: {}",
                                &reward_address
                            )));
                        }

                        certificate_requests.push(Certificate::Delegation(Delegation {
                            reward_address,
                            pool_id,
                        }));
                    }
                    _ => {}
                }
            }
            certificate_requests
        } else {
            Vec::new()
        };

        // Check input witnesses
        for (index, input) in node_inputs.iter().enumerate() {
            match input {
                NodeInput::Ledger(entry) | NodeInput::Mempool(entry) => {
                    let credential = Addresses::address_to_credential(&entry.utxo.address)?;
                    check_and_consum(
                        credential,
                        Some(RedeemersKey {
                            tag: RedeemerTag::Spend,
                            index: index as u32,
                        }),
                    )?;
                }
            }
        }

        // Consume datum hashes
        let outputs =
            tx.transaction_body
                .outputs
                .iter()
                .enumerate()
                .map(|(index, output)| {
                    let output: Utxo = output.clone().try_into()?;
                    let utxo = Utxo {
                        tx_hash: tx_hash.to_string(),
                        output_index: index as u64,
                        ..output
                    };
                    match &utxo.datum_hash {
                        Some(datum_hash) => consumed_datum_hashes
                            .insert(datum_hash.parse().map_err(CoreError::msg)?),
                        _ => false,
                    };
                    Ok(BuilderInput {
                        utxo,
                        redeemer: None,
                    })
                })
                .collect::<CoreResult<Vec<_>>>()?;

        // Check consumed witnesses
        if let Some(extra_key_hash) = key_hashes.difference(&consumed_hashes.borrow_mut()).next() {
            return Err(CoreError::msg(format!(
                "Extraneous vkey witness: key hash {}",
                extra_key_hash.to_string()
            )));
        }

        if let Some(extra_native_hash) = native_script_hashes
            .difference(&consumed_hashes.borrow_mut())
            .next()
        {
            return Err(CoreError::msg(format!(
                "Extraneous native script: script hash {}",
                extra_native_hash.to_string()
            )));
        }

        if let Some(extra_plutus_hash) = plutus_script_hashes
            .difference(&consumed_hashes.borrow_mut())
            .next()
        {
            return Err(CoreError::msg(format!(
                "Extraneous plutus script: script hash {}",
                extra_plutus_hash.to_string()
            )));
        }

        if let Some(extra_datum_hash) = datum_hashes
            .keys()
            .find(|datum_hash| !consumed_datum_hashes.contains(*datum_hash))
        {
            return Err(CoreError::msg(format!(
                "Extraneous plutus data: datum hash {}",
                extra_datum_hash.to_string()
            )));
        }

        // Apply transitions
        for input in node_inputs {
            match input {
                NodeInput::Ledger(mut entry) => {
                    entry.redeemer = Some("spent".to_string());
                    self.ledger.replace(entry);
                }
                NodeInput::Mempool(mut entry) => {
                    entry.redeemer = Some("spent".to_string());
                    self.mempool.replace(entry);
                }
            }
        }

        for (reward_address, staking) in withdrawals_requests {
            if let Some(entry) = self.staking.get_mut(&reward_address) {
                entry.rewards -= staking.rewards;
            }
        }

        for cert in certificate_requests {
            match cert {
                Certificate::StakeRegistration { reward_address } => {
                    if let Some(staking) = self.staking.get_mut(&reward_address) {
                        staking.registered = true;
                    } else {
                        self.staking.insert(
                            reward_address.clone(),
                            Staking {
                                registered: true,
                                rewards: 0,
                                pool_id: None,
                            },
                        );
                    }
                }
                Certificate::StakeDeregistration { reward_address } => {
                    if let Some(staking) = self.staking.get_mut(&reward_address) {
                        staking.registered = false;
                        staking.pool_id = None;
                    }
                }
                Certificate::Delegation(Delegation {
                    reward_address,
                    pool_id,
                }) => {
                    if let Some(staking) = self.staking.get_mut(&reward_address) {
                        staking.pool_id = Some(pool_id);
                    }
                }
                _ => {}
            }
        }

        for output in outputs {
            self.mempool.insert(output);
        }

        for (datum_hash, datum) in datum_hashes {
            self.datum_table.insert(datum_hash, datum);
        }

        Ok(tx_hash.to_string())
    }

    #[wasm_bindgen(js_name = getTime)]
    pub fn get_time(&self) -> f64 {
        self.time as f64
    }

    #[wasm_bindgen(js_name = getDatum)]
    pub fn get_datum(&self, hash: &str) -> CoreResult<Option<String>> {
        let hash = hex::decode(hash).map_err(CoreError::msg)?.as_slice().into();
        let datum = self.datum_table.get(&hash);

        Ok(match datum {
            Some(datum) => Some(hex::encode(datum.encode_fragment().unwrap())),
            _ => None,
        })
    }

    #[wasm_bindgen(js_name = getStaking)]
    pub fn get_staking(&self, address: &str) -> Option<Staking> {
        let staking = self.staking.get(address);

        staking.cloned()
    }

    #[wasm_bindgen(js_name = getLedger)]
    pub fn get_ledger(&self) -> Utxos {
        Utxos(self.ledger.iter().map(|b| b.utxo.clone()).collect())
    }

    #[wasm_bindgen(js_name = getMempool)]
    pub fn get_mempool(&self) -> Utxos {
        Utxos(self.mempool.iter().map(|b| b.utxo.clone()).collect())
    }

    /// Emulates the behaviour of the reward distribution at epoch boundaries.
    #[wasm_bindgen(js_name = distributeRewards)]
    pub fn distribute_rewards(&mut self, rewards: u64) {
        for (_, staking) in self.staking.iter_mut() {
            if staking.registered && staking.pool_id.is_some() {
                staking.rewards += rewards;
            }
        }
        self.await_block(None);
    }

    #[wasm_bindgen(js_name = awaitSlot)]
    pub fn await_slot(&mut self, slot: Option<u32>) {
        let slot_length = slot.unwrap_or(1) as u64;
        self.slot += slot_length;
        self.time += slot_length * 1000;
        let current_height = self.block_height;
        self.block_height = self.slot / 20;
        if self.block_height > current_height {
            self.update_ledger();
        }
    }

    #[wasm_bindgen(js_name = awaitBlock)]
    pub fn await_block(&mut self, height: Option<u32>) {
        let height = height.unwrap_or(1) as u64;
        self.block_height += height;
        self.slot += height * 20;
        self.time += height * 20 * 1000;
        self.update_ledger();
    }

    fn update_ledger(&mut self) {
        self.ledger.extend(self.mempool.iter().cloned());
        self.mempool.clear();
        self.ledger.retain(|input| input.redeemer.is_none());
    }
}

fn verify_script(
    script: &NativeScript,
    lower_bound: Option<u64>,
    upper_bound: Option<u64>,
    key_hashes: &HashSet<Hash<28>>,
    consumed_hashes: &mut HashSet<Hash<28>>,
) -> bool {
    match script {
        NativeScript::InvalidBefore(slot) => match lower_bound {
            Some(lower_bound_slot) => lower_bound_slot >= *slot,
            _ => false,
        },
        NativeScript::InvalidHereafter(slot) => match upper_bound {
            Some(upper_bound_slot) => upper_bound_slot < *slot,
            _ => false,
        },
        NativeScript::ScriptPubkey(key) => {
            consumed_hashes.insert(key.clone());
            key_hashes.contains(&key)
        }
        NativeScript::ScriptAll(scripts) => scripts.iter().all(|script| {
            verify_script(
                script,
                lower_bound,
                upper_bound,
                key_hashes,
                consumed_hashes,
            )
        }),
        NativeScript::ScriptAny(scripts) => scripts.iter().any(|script| {
            verify_script(
                script,
                lower_bound,
                upper_bound,
                key_hashes,
                consumed_hashes,
            )
        }),
        NativeScript::ScriptNOfK(n, scripts) => {
            scripts
                .iter()
                .filter(|script| {
                    verify_script(
                        script,
                        lower_bound,
                        upper_bound,
                        key_hashes,
                        consumed_hashes,
                    )
                })
                .count()
                >= *n as usize
        }
    }
}

fn check_and_consum(
    key_hashes: &HashSet<Hash<28>>,
    consumed_hashes: &mut HashSet<Hash<28>>,
    redeemers_keys: &Vec<RedeemersKey>,
    native_script_hashes: &HashSet<Hash<28>>,
    native_script_hashes_optional: &HashMap<Hash<28>, NativeScript>,
    plutus_script_hashes: &HashSet<Hash<28>>,
    plutus_script_hashes_optional: &HashSet<Hash<28>>,
    lower_bound: Option<u64>,
    upper_bound: Option<u64>,
    credential: Credential,
    redeemers_key: Option<RedeemersKey>,
) -> CoreResult<()> {
    match credential {
        Credential::Key { hash } => {
            let hash_bytes: Hash<28> = hash.parse().map_err(CoreError::msg)?;
            if !key_hashes.contains(&hash_bytes) {
                return Err(CoreError::msg(format!(
                    "Missing vkey witness: key hash {}",
                    hash
                )));
            }
            consumed_hashes.insert(hash_bytes);
        }
        Credential::Script { hash } => {
            let hash_bytes: Hash<28> = hash.parse().map_err(CoreError::msg)?;
            if native_script_hashes.contains(&hash_bytes) {
                consumed_hashes.insert(hash_bytes);
            } else if let Some(script) = native_script_hashes_optional.get(&hash_bytes) {
                if !verify_script(
                    script,
                    lower_bound,
                    upper_bound,
                    &key_hashes,
                    consumed_hashes,
                ) {
                    return Err(CoreError::msg(format!(
                        "Invalid native script witness: script hash {}",
                        hash
                    )));
                }
            } else if plutus_script_hashes.contains(&hash_bytes)
                || plutus_script_hashes_optional.contains(&hash_bytes)
            {
                if let Some(redeemers_key) = redeemers_key {
                    if redeemers_keys.contains(&redeemers_key) {
                        consumed_hashes.insert(hash_bytes);
                    } else {
                        return Err(CoreError::msg(format!(
                            "Missing redeemer for script witness: script hash {}",
                            hash
                        )));
                    }
                } else {
                    return Err(CoreError::msg(format!(
                        "Missing redeemer for script witness: script hash {}",
                        hash
                    )));
                }
            } else {
                return Err(CoreError::msg(format!(
                    "Missing script witness: script hash {}",
                    hash
                )));
            }
        }
    }
    Ok(())
}

#[derive(Debug, Clone)]
enum NodeInput {
    Ledger(BuilderInput),
    Mempool(BuilderInput),
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct Staking {
    pub registered: bool,
    pub rewards: u64,
    pub pool_id: Option<String>,
}

impl Default for Staking {
    fn default() -> Self {
        Staking {
            registered: false,
            rewards: 0,
            pool_id: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        codec::Assets,
        crypto::Crypto,
        instruction_builder::{
            Change, Instruction, InstructionBuilder, Instructions, RelevantProtocolParameters,
        },
    };

    fn setup_builder(selection: Vec<Utxo>, change_address: &str) -> InstructionBuilder {
        InstructionBuilder::new(
            Network::Preprod,
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
                address: change_address.to_string(),
                datum_variant: None,
            },
        )
    }

    #[test]
    fn test_validate() -> CoreResult<()> {
        let private_key = Crypto::generate_private_key();
        let address = Addresses::credential_to_address(
            Network::Preprod,
            Crypto::private_key_to_details(&private_key)?.credential,
            None,
        )?;

        let private_key_other = Crypto::generate_private_key();
        let address_other = Addresses::credential_to_address(
            Network::Preprod,
            Crypto::private_key_to_details(&private_key_other)?.credential,
            None,
        )?;

        let start_utxos = vec![Utxo {
            tx_hash: "00".repeat(32),
            output_index: 0,
            assets: Assets::from_lovelace(20000000),
            address: address.to_string(),
            datum_hash: None,
            datum: None,
            script_ref: None,
        }];

        let mut state = EmulatorState::new(0., Some(Utxos(start_utxos.clone())));

        let builder = setup_builder(start_utxos, &address);

        let tx = builder
            .commit(Instructions(vec![Instruction::PayTo {
                assets: Assets::from_lovelace(10000000),
                address: address_other.to_string(),
                datum_variant: None,
                script_ref: None,
            }]))?
            .sign_with_key(&private_key)?
            .commit();

        assert!(state.validate(tx.clone()).is_ok());

        Ok(())
    }
}
