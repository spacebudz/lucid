/// The `EmulatorState` struct represents the state of the emulator, including the ledger, mempool,
/// staking information, datum table, block height, slot, and time.
use super::addresses::{Addresses, Credential};
use super::codec::{Certificate, Delegation, Script, Utxos};
use super::error::{CoreError, CoreResult};
use super::instruction_builder::BuilderInput;
use crate::addresses::Network;
use crate::codec::{DelegVariant, Utxo};
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

    pub fn validate(&mut self, tx: &str) -> CoreResult<String> {
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
                            consumed_datum_hashes
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
                        consumed_datum_hashes.insert(datum_hash.parse().map_err(CoreError::msg)?);
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

                withdrawals_requests.insert(reward_address, *rewards);
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
                            variant: DelegVariant::Pool(pool_id),
                        }));
                    }
                    pallas_primitives::conway::Certificate::VoteDeleg(credential, drep) => {
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

                        let variant = match drep {
                            pallas_primitives::conway::DRep::Abstain => DelegVariant::Abstain,
                            pallas_primitives::conway::DRep::NoConfidence => {
                                DelegVariant::NoConfidence
                            }
                            pallas_primitives::conway::DRep::Key(key) => {
                                let mut key_with_header = Vec::new();
                                key_with_header.push(0b0010_0010);
                                key_with_header.extend(key.to_vec());
                                let drep = bech32::encode::<bech32::Bech32>(
                                    Hrp::parse("drep").unwrap(),
                                    &key_with_header,
                                )
                                .map_err(CoreError::msg)?;
                                DelegVariant::DRep(drep)
                            }
                            pallas_primitives::conway::DRep::Script(script) => {
                                let mut script_with_header = Vec::new();
                                script_with_header.push(0b0010_0011);
                                script_with_header.extend(script.to_vec());
                                let drep = bech32::encode::<bech32::Bech32>(
                                    Hrp::parse("drep").unwrap(),
                                    &script_with_header,
                                )
                                .map_err(CoreError::msg)?;
                                DelegVariant::DRep(drep)
                            }
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

                        if !staking.registered
                            && !certificate_requests
                                .iter()
                                .find(|c| match c {
                                    Certificate::Delegation(delegation) => {
                                        &delegation.reward_address == &reward_address
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
                            variant,
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

        for (reward_address, rewards) in withdrawals_requests {
            if let Some(entry) = self.staking.get_mut(&reward_address) {
                entry.rewards -= rewards;
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
                                drep: None,
                            },
                        );
                    }
                }
                Certificate::StakeDeregistration { reward_address } => {
                    self.staking.remove(&reward_address);
                }
                Certificate::Delegation(Delegation {
                    reward_address,
                    variant,
                }) => {
                    if let Some(staking) = self.staking.get_mut(&reward_address) {
                        match variant {
                            DelegVariant::Pool(pool_id) => staking.pool_id = Some(pool_id),
                            DelegVariant::Abstain => staking.drep = Some(DRep::Abstain),
                            DelegVariant::NoConfidence => staking.drep = Some(DRep::NoConfidence),
                            DelegVariant::DRep(id) => staking.drep = Some(DRep::Id(id)),
                        };
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
    pub drep: Option<DRep>,
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub enum DRep {
    Abstain,
    NoConfidence,
    Id(String),
}

impl Default for Staking {
    fn default() -> Self {
        Staking {
            registered: false,
            rewards: 0,
            pool_id: None,
            drep: None,
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
        instruction_signer::InstructionSigner,
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

        let signed = builder
            .commit(Instructions(vec![Instruction::PayTo {
                assets: Assets::from_lovelace(10000000),
                address: address_other.to_string(),
                datum_variant: None,
                script_ref: None,
            }]))?
            .sign_with_key(&private_key)?
            .commit();

        assert!(state.validate(&signed.tx).is_ok());

        Ok(())
    }

    #[test]
    fn test_spending_plutus_v1_script() -> CoreResult<()> {
        let private_key = Crypto::generate_private_key();
        let address = Addresses::credential_to_address(
            Network::Preprod,
            Crypto::private_key_to_details(&private_key)?.credential,
            None,
        )?;

        let start_utxos = vec![
            Utxo {
                tx_hash: "966e4223a4bf5f0b31e4676668b52b276a0f9af39eed03ff88c654979c0f2622"
                    .to_string(),
                output_index: 1,
                assets: Assets::from_lovelace(20000000),
                address: address.to_string(),
                datum_hash: None,
                datum: None,
                script_ref: None,
            },
            Utxo {
                tx_hash: "966e4223a4bf5f0b31e4676668b52b276a0f9af39eed03ff88c654979c0f2622"
                    .to_string(),
                output_index: 0,
                assets: Assets::from_lovelace(20000000),
                address: "addr_test1wz8jmzsx9uh2pgcxj7za36jeln7sprheumhkd3srnytfacg6cgclw"
                    .to_string(),
                datum_hash: Some(
                    "613baf6bfa3607ffc2d721491eb3e406d46d2168c2268388b31ef6acaace1c41".to_string(),
                ),
                datum: None,
                script_ref: None,
            },
        ];

        let mut state = EmulatorState::new(0., Some(Utxos(start_utxos.clone())));

        let tx = "84A700D9010281825820966E4223A4BF5F0B31E4676668B52B276A0F9AF39EED03FF88C654979C0F2622000181A2005839007A10CEB1B9517F511F392F8C9021CF940F30BCCD7DF2745C272AC13B7BFB7BCDA91F6D2C84BFDB558F63E8F22FF492295847394D295DC319011A0019B42E021A0004D0520B5820E9EB787EB922C2BAAAE3588EAA4A001612FBC14E4DB662AA7573A70D50B1AB760DD9010281825820966E4223A4BF5F0B31E4676668B52B276A0F9AF39EED03FF88C654979C0F26220110A2005839007A10CEB1B9517F511F392F8C9021CF940F30BCCD7DF2745C272AC13B7BFB7BCDA91F6D2C84BFDB558F63E8F22FF492295847394D295DC319011A05CD89C4111A0007387BA303D901028159099A59099701000033233223322323233322232333222323333333322222222323332223233332222323233223233322232333222323233223322323233333222223322332233223322332233222222323253353031333006375A00A6EB4010CCCD5CD19B8735573AA004900011980499191919191919191919191999AB9A3370E6AAE754029200023333333333017335025232323333573466E1CD55CEA8012400046603A60706AE854008C0A8D5D09ABA250022350573530583357389201035054310005949926135573CA00226EA8004D5D0A80519A8128131ABA150093335502C75CA0566AE854020CCD540B1D728159ABA1500733502504135742A00C66A04A66AA0A4094EB4D5D0A8029919191999AB9A3370E6AAE7540092000233501F3232323333573466E1CD55CEA80124000466A04E66A080EB4D5D0A80118229ABA135744A00446A0B66A60B866AE712401035054310005D49926135573CA00226EA8004D5D0A8011919191999AB9A3370E6AAE7540092000233502533504075A6AE854008C114D5D09ABA2500223505B35305C3357389201035054310005D49926135573CA00226EA8004D5D09ABA250022350573530583357389201035054310005949926135573CA00226EA8004D5D0A80219A812BAE35742A00666A04A66AA0A4EB88004D5D0A801181B9ABA135744A00446A0A66A60A866AE71241035054310005549926135744A00226AE8940044D5D1280089ABA25001135744A00226AE8940044D5D1280089ABA25001135573CA00226EA8004D5D0A8011919191999AB9A3370EA00290031180E181C9ABA135573CA00646666AE68CDC3A801240084603660866AE84D55CF280211999AB9A3370EA00690011180D98171ABA135573CA00A46666AE68CDC3A802240004603C6EB8D5D09AAB9E500623504E35304F3357389201035054310005049926499264984D55CEA80089BAA001357426AE8940088D411CD4C120CD5CE2490350543100049499261048135046353047335738920103505435000484984D55CF280089BAA0012212330010030022001222222222212333333333300100B00A00900800700600500400300220012212330010030022001122123300100300212001122123300100300212001122123300100300212001212222300400521222230030052122223002005212222300100520011232230023758002640026AA068446666AAE7C004940388CD4034C010D5D080118019ABA200203323232323333573466E1CD55CEA801A4000466600E6464646666AE68CDC39AAB9D5002480008CC034C0C4D5D0A80119A8098169ABA135744A00446A06C6A606E66AE712401035054310003849926135573CA00226EA8004D5D0A801999AA805BAE500A35742A00466A01EEB8D5D09ABA25002235032353033335738921035054310003449926135744A00226AAE7940044DD50009110919980080200180110009109198008018011000899AA800BAE75A224464460046EAC004C8004D540B888C8CCCD55CF80112804919A80419AA81898031AAB9D5002300535573CA00460086AE8800C0B84D5D08008891001091091198008020018900089119191999AB9A3370EA002900011A80418029ABA135573CA00646666AE68CDC3A801240044A01046A0526A605466AE712401035054310002B499264984D55CEA80089BAA001121223002003112200112001232323333573466E1CD55CEA8012400046600C600E6AE854008DD69ABA135744A00446A0466A604866AE71241035054310002549926135573CA00226EA80048848CC00400C00880048C8CCCD5CD19B8735573AA002900011BAE357426AAE7940088D407CD4C080CD5CE24810350543100021499261375400224464646666AE68CDC3A800A40084A00E46666AE68CDC3A8012400446A014600C6AE84D55CF280211999AB9A3370EA00690001280511A8111A981199AB9C490103505431000244992649926135573AA00226EA8004484888C00C0104488800844888004480048C8CCCD5CD19B8750014800880188CCCD5CD19B8750024800080188D4068D4C06CCD5CE249035054310001C499264984D55CE9BAA0011220021220012001232323232323333573466E1D4005200C200B23333573466E1D4009200A200D23333573466E1D400D200823300B375C6AE854014DD69ABA135744A00A46666AE68CDC3A8022400C46601A6EB8D5D0A8039BAE357426AE89401C8CCCD5CD19B875005480108CC048C050D5D0A8049BAE357426AE8940248CCCD5CD19B875006480088C050C054D5D09AAB9E500B23333573466E1D401D2000230133016357426AAE7940308D407CD4C080CD5CE2481035054310002149926499264992649926135573AA00826AAE79400C4D55CF280109AAB9E500113754002424444444600E01044244444446600C012010424444444600A010244444440082444444400644244444446600401201044244444446600201201040024646464646666AE68CDC3A800A400446660106EB4D5D0A8021BAD35742A0066EB4D5D09ABA2500323333573466E1D400920002300A300B357426AAE7940188D4040D4C044CD5CE2490350543100012499264984D55CEA80189ABA25001135573CA00226EA80048488C00800C888488CCC00401401000C80048C8C8CCCD5CD19B875001480088C018DD71ABA135573CA00646666AE68CDC3A80124000460106EB8D5D09AAB9E500423500A35300B3357389201035054310000C499264984D55CEA80089BAA001212230020032122300100320011122232323333573466E1CD55CEA80124000466AA016600C6AE854008C014D5D09ABA25002235007353008335738921035054310000949926135573CA00226EA8004498480048004448848CC00400C008448004448C8C00400488CC00CC008008004CCC888CCC888CCCCCCCC88888888CC88CCCCC88888CCCC8888CCC888CC88CC88CC88CCC888CC88CC88CCC888CC88CC88CC88CC88888CCD5CD19B8700300201E01D2212330010030022001222222222212333333333300100B00A0090080070060050040030022001221233001003002200122212333001004003002200111220021221223300100400312001112212330010030021120012212330010030022001121223002003112200112001122123300100300212001122123300100300212001122123300100300212001122002122001200112122230030041122200211222001120012122223004005212222300300521222230020052122223001005200122123300100300220012122222223007008221222222233006009008212222222300500812222222004122222220032212222222330020090082212222222330010090082001212230020032221223330010050040032001212230020032122300100320010104D9010281182105A1820000821821821A00067EC21A07AF27B2F5F6";

        let mut tx = InstructionSigner::from_tx(tx, Some(Utxos(start_utxos)))?;

        let signed = tx.sign_with_key(&private_key)?.commit();

        state.validate(&signed.tx).unwrap();
        Ok(())
    }
}
