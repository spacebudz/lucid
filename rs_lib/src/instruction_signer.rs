use crate::addresses::Addresses;

/// The `InstructionSigner` struct is responsible for managing the signing of transactions.
/// It holds the transaction (`tx`), a set of verification key witnesses (`vkey_witnesses`),
/// and a set of used keys (`used_keys`).
///
/// # Fields
/// - `tx`: The transaction to be signed.
/// - `vkey_witnesses`: A set of verification key witnesses.
/// - `used_keys`: A set of used keys.
///
/// # Methods
/// - `new(tx: Tx, used_keys: HashSet<AddrKeyhash>) -> Self`: Creates a new `InstructionSigner` instance.
/// - `get_tx(&self) -> Tx`: Returns a clone of the transaction.
///
/// # WASM Bindings
/// - `from_tx(tx: &str) -> CoreResult<Self>`: Creates an `InstructionSigner` from a transaction string.
/// - `sign_with_key(self, key: &str) -> CoreResult<Self>`: Signs the transaction with a given private key.
/// - `sign_with_seed(self, seed: &str, index: u32) -> CoreResult<Self>`: Signs the transaction with a seed and index.
/// - `sign_with_witness(self, witness: &str) -> CoreResult<Self>`: Signs the transaction with a given witness.
/// - `sign_with_witness_set(self, set: &str) -> CoreResult<Self>`: Signs the transaction with a given witness set.
/// - `commit(self) -> String`: Commits the signatures to the transaction and returns the encoded transaction string.
///
/// # Helper Structs
/// - `SignerVKeyWitness`: A wrapper around `VKeyWitness` with ordering implemented.
use super::{
    addresses::Credential,
    codec::Utxos,
    crypto::{Crypto, Part},
    error::{CoreError, CoreResult},
};
use pallas_primitives::{
    conway::{MintedTx, Tx, VKeyWitness, WitnessSet},
    AddrKeyhash, Fragment, NonEmptySet, TransactionInput,
};
use pallas_traverse::ComputeHash;
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashSet},
};
use tsify::Tsify;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct InstructionSigner {
    tx: Tx,
    vkey_witnesses: BTreeSet<SignerVKeyWitness>,
    partial_witnesses: BTreeSet<SignerVKeyWitness>,
    used_keys: HashSet<AddrKeyhash>,
}

impl InstructionSigner {
    pub fn new(tx: Tx, used_keys: HashSet<AddrKeyhash>) -> Self {
        Self {
            tx,
            vkey_witnesses: BTreeSet::new(),
            partial_witnesses: BTreeSet::new(),
            used_keys,
        }
    }

    pub fn get_tx(&self) -> Tx {
        self.tx.clone()
    }
}

#[wasm_bindgen]
impl InstructionSigner {
    #[wasm_bindgen(js_name = fromTx)]
    pub fn from_tx(tx: &str, utxos: Option<Utxos>) -> CoreResult<Self> {
        let utxos = match utxos {
            Some(utxos) => utxos.0,
            _ => vec![],
        };

        let tx: Tx = MintedTx::decode_fragment(&hex::decode(tx).map_err(CoreError::msg)?)
            .map_err(CoreError::msg)?
            .into();

        let vkey_witnesses = match &tx.transaction_witness_set.vkeywitness {
            Some(witnesses) => witnesses
                .iter()
                .map(|w| SignerVKeyWitness(w.clone()))
                .collect(),
            _ => BTreeSet::new(),
        };

        let mut used_keys = HashSet::new();

        for input in tx.transaction_body.inputs.iter() {
            let utxo = utxos
                .iter()
                .find(|u| TransactionInput::try_from((*u).clone()).unwrap() == *input);

            if let Some(utxo) = utxo {
                let credential = Addresses::address_to_credential(&utxo.address)?;
                if let Credential::Key { hash } = credential {
                    used_keys.insert(hash.parse().map_err(CoreError::msg)?);
                }
            }
        }

        if let Some(certificates) = &tx.transaction_body.certificates {
            for cert in certificates.iter() {
                match cert {
                    pallas_primitives::conway::Certificate::Reg(credential, _) // does this require a signature?
                    | pallas_primitives::conway::Certificate::UnReg(credential, _)
                    | pallas_primitives::conway::Certificate::VoteDeleg(credential, _)
                    | pallas_primitives::conway::Certificate::VoteRegDeleg(credential, _, _)
                    | pallas_primitives::conway::Certificate::StakeVoteDeleg(credential, _, _)
                    | pallas_primitives::conway::Certificate::StakeVoteRegDeleg(
                        credential,
                        _,
                        _,
                        _,
                    )
                    | pallas_primitives::conway::Certificate::StakeRegDeleg(credential, _, _)
                    | pallas_primitives::conway::Certificate::UnRegDRepCert(credential, _)
                    | pallas_primitives::conway::Certificate::RegDRepCert(credential, _, _)
                    | pallas_primitives::conway::Certificate::UpdateDRepCert(credential, _)
                    | pallas_primitives::conway::Certificate::StakeDeregistration(credential)
                    | pallas_primitives::conway::Certificate::ResignCommitteeCold(credential, _)
                    | pallas_primitives::conway::Certificate::StakeDelegation(credential, _) => {
                        if let pallas_primitives::conway::StakeCredential::AddrKeyhash(hash) =
                            credential
                        {
                            used_keys.insert(hash.clone());
                        }
                    }
                    pallas_primitives::conway::Certificate::PoolRegistration {
                        operator,
                        pool_owners,
                        ..
                    } => {
                        used_keys.insert(operator.clone());
                        for owner in pool_owners.iter() {
                            used_keys.insert(owner.clone());
                        }
                    }
                    pallas_primitives::conway::Certificate::PoolRetirement(key_hash, _) => {
                        used_keys.insert(key_hash.clone());
                    }
                    pallas_primitives::conway::Certificate::AuthCommitteeHot(
                        cold_credential,
                        hot_credential,
                    ) => {
                        if let pallas_primitives::conway::StakeCredential::AddrKeyhash(hash) =
                            cold_credential
                        {
                            used_keys.insert(hash.clone());
                        }
                        if let pallas_primitives::conway::StakeCredential::AddrKeyhash(hash) =
                            hot_credential
                        // does this require a signature in the tx?
                        {
                            used_keys.insert(hash.clone());
                        }
                    }
                    pallas_primitives::conway::Certificate::StakeRegistration(_) => (),
                }
            }
        }

        if let Some(collateral) = &tx.transaction_body.collateral {
            for input in collateral.iter() {
                let utxo = utxos
                    .iter()
                    .find(|u| TransactionInput::try_from((*u).clone()).unwrap() == *input);

                if let Some(utxo) = utxo {
                    let credential = Addresses::address_to_credential(&utxo.address)?;
                    if let Credential::Key { hash } = credential {
                        used_keys.insert(hash.parse().map_err(CoreError::msg)?);
                    }
                }
            }
        }

        if let Some(withdrawals) = &tx.transaction_body.withdrawals {
            for (key, _) in withdrawals.iter() {
                let credential =
                    Addresses::reward_address_to_credential(&hex::encode(key.to_vec()))?;
                if let Credential::Key { hash } = credential {
                    used_keys.insert(hash.parse().map_err(CoreError::msg)?);
                }
            }
        }

        if let Some(required_signers) = &tx.transaction_body.required_signers {
            for signer in required_signers.iter() {
                used_keys.insert(signer.clone());
            }
        }

        if let Some(native_scripts) = &tx.transaction_witness_set.native_script {
            fn collect_native_script_keys(
                used_keys: &mut HashSet<AddrKeyhash>,
                native_script: &pallas_primitives::conway::NativeScript,
            ) {
                match native_script {
                    pallas_primitives::conway::NativeScript::ScriptPubkey(key_hash) => {
                        used_keys.insert(*key_hash);
                    }
                    pallas_primitives::conway::NativeScript::ScriptAll(scripts)
                    | pallas_primitives::conway::NativeScript::ScriptAny(scripts)
                    | pallas_primitives::conway::NativeScript::ScriptNOfK(_, scripts) => {
                        scripts
                            .iter()
                            .for_each(|s| collect_native_script_keys(used_keys, s));
                    }
                    _ => (),
                }
            }

            for native_script in native_scripts.iter() {
                collect_native_script_keys(&mut used_keys, native_script);
            }
        }

        Ok(Self {
            tx,
            vkey_witnesses,
            partial_witnesses: BTreeSet::new(),
            used_keys,
        })
    }

    /// ed25519 private key hex or bech32 encoded
    #[wasm_bindgen(js_name = signWithKey)]
    pub fn sign_with_key(&mut self, key: &str) -> CoreResult<Self> {
        let private_key = Crypto::decode_private_key(key)?;

        let public_key = private_key.public_key();
        let key_hash = public_key.compute_hash();

        if !self.used_keys.contains(&key_hash) {
            return Err(CoreError::msg(format!(
                "Cannot sign transaction, not included key hash: {}",
                key_hash.to_string()
            )));
        }

        let signature = private_key.sign(self.tx.transaction_body.compute_hash());

        let mut partial_witnesses = BTreeSet::new();

        partial_witnesses.insert(SignerVKeyWitness(VKeyWitness {
            vkey: public_key.as_ref().to_vec().into(),
            signature: signature.as_ref().to_vec().into(),
        }));
        self.partial_witnesses = partial_witnesses.clone();
        self.vkey_witnesses.extend(partial_witnesses);

        Ok(self.clone())
    }

    #[wasm_bindgen(js_name = signWithSeed)]
    pub fn sign_with_seed(&mut self, seed: &str, index: u32) -> CoreResult<Self> {
        let payment_private_key = Crypto::seed_to_private_key(seed, index, Part::Payment)?;
        let payment_public_key = payment_private_key.public_key();
        let payment_key_hash = payment_public_key.compute_hash();

        let delegation_private_key = Crypto::seed_to_private_key(seed, index, Part::Delegation)?;
        let delegation_public_key = delegation_private_key.public_key();
        let delegation_key_hash = delegation_public_key.compute_hash();

        let tx_hash = self.tx.transaction_body.compute_hash();

        let mut partial_witnesses = BTreeSet::new();

        if self.used_keys.contains(&payment_key_hash) {
            let signature = payment_private_key.sign(&tx_hash);

            partial_witnesses.insert(SignerVKeyWitness(VKeyWitness {
                vkey: payment_public_key.as_ref().to_vec().into(),
                signature: signature.as_ref().to_vec().into(),
            }));
        }

        if self.used_keys.contains(&delegation_key_hash) {
            let signature = delegation_private_key.sign(&tx_hash);

            partial_witnesses.insert(SignerVKeyWitness(VKeyWitness {
                vkey: delegation_public_key.as_ref().to_vec().into(),
                signature: signature.as_ref().to_vec().into(),
            }));
        }

        if partial_witnesses.len() == 0 {
            return Err(CoreError::msg(format!(
                "Cannot sign transaction, not included key hashes {} {}",
                payment_key_hash.to_string(),
                delegation_key_hash.to_string()
            )));
        }

        self.partial_witnesses = partial_witnesses.clone();
        self.vkey_witnesses.extend(partial_witnesses);

        Ok(self.clone())
    }

    #[wasm_bindgen(js_name = signWithWitness)]
    pub fn sign_with_witness(&mut self, witness: &str) -> CoreResult<Self> {
        let mut partial_witnesses = BTreeSet::new();
        partial_witnesses.insert(SignerVKeyWitness(
            VKeyWitness::decode_fragment(&hex::decode(witness).map_err(CoreError::msg)?)
                .map_err(CoreError::msg)?,
        ));
        self.partial_witnesses = partial_witnesses.clone();
        self.vkey_witnesses.extend(partial_witnesses);

        Ok(self.clone())
    }

    #[wasm_bindgen(js_name = signWithWitnessSet)]
    pub fn sign_with_witness_set(&mut self, set: &str) -> CoreResult<Self> {
        let witness_set = WitnessSet::decode_fragment(&hex::decode(set).map_err(CoreError::msg)?)
            .map_err(CoreError::msg)?;

        if let Some(set) = &witness_set.vkeywitness {
            let mut partial_witnesses = BTreeSet::new();
            for witness in set.iter() {
                partial_witnesses.insert(SignerVKeyWitness(witness.clone()));
            }
            self.partial_witnesses = partial_witnesses.clone();
            self.vkey_witnesses.extend(partial_witnesses);
        }
        Ok(self.clone())
    }

    #[wasm_bindgen(js_name = getPartialWitnessSet)]
    pub fn get_partial_witness_set(&self) -> CoreResult<String> {
        if self.partial_witnesses.len() > 0 {
            let witness_set = WitnessSet {
                vkeywitness: Some(
                    NonEmptySet::from_vec(
                        self.partial_witnesses.iter().map(|v| v.0.clone()).collect(),
                    )
                    .unwrap(),
                ),
                native_script: None,
                bootstrap_witness: None,
                plutus_data: None,
                plutus_v1_script: None,
                plutus_v2_script: None,
                plutus_v3_script: None,
                redeemer: None,
            };
            Ok(hex::encode(witness_set.encode_fragment().unwrap()))
        } else {
            Err(CoreError::msg("No partial witness found"))
        }
    }

    pub fn commit(&mut self) -> SignerResult {
        if self.vkey_witnesses.len() > 0 {
            self.tx.transaction_witness_set.vkeywitness = Some(
                NonEmptySet::from_vec(self.vkey_witnesses.iter().map(|s| s.0.clone()).collect())
                    .unwrap(),
            )
        }

        SignerResult {
            tx: hex::encode(self.tx.encode_fragment().unwrap()),
            witness_set: hex::encode(self.tx.transaction_witness_set.encode_fragment().unwrap()),
        }
    }
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct SignerResult {
    pub tx: String,
    pub witness_set: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct SignerVKeyWitness(VKeyWitness);

impl PartialOrd for SignerVKeyWitness {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SignerVKeyWitness {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.vkey.cmp(&other.0.vkey)
    }
}
