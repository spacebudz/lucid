use super::{
    codec::Script,
    error::{CoreError, CoreResult},
};
use pallas_crypto::key::ed25519::PublicKey;
use pallas_primitives::{
    conway::{MintedTx, NativeScript},
    Bytes, Fragment, PlutusData, PlutusScript,
};
use pallas_traverse::{ComputeHash, OriginalHash};
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub struct Hasher;

#[wasm_bindgen]
impl Hasher {
    #[wasm_bindgen(js_name = hashData)]
    pub fn hash_data(data: &str) -> CoreResult<String> {
        Ok(
            PlutusData::decode_fragment(&hex::decode(data).map_err(CoreError::msg)?)
                .unwrap()
                .compute_hash()
                .to_string(),
        )
    }

    #[wasm_bindgen(js_name = hashVrfKey)]
    pub fn hash_vrf_key(pubkey: &str) -> CoreResult<String> {
        let raw = hex::decode(pubkey).map_err(CoreError::msg)?;
        let raw_slice: [u8; 32] = if raw.len() == 32 {
            raw.try_into().unwrap()
        } else {
            pallas_codec::minicbor::decode::<Bytes>(&raw)
                .map_err(CoreError::msg)?
                .to_vec()
                .try_into()
                .map_err(|_| CoreError::msg("Failed to decode public key: invalid size"))?
        };

        Ok(pallas_crypto::hash::Hasher::<256>::hash(&raw_slice).to_string())
    }

    #[wasm_bindgen(js_name = hashPublicKey)]
    pub fn hash_public_key(pubkey: &str) -> CoreResult<String> {
        let raw = hex::decode(pubkey).map_err(CoreError::msg)?;
        let raw_slice: [u8; 32] = if raw.len() == 32 {
            raw.try_into().unwrap()
        } else {
            pallas_codec::minicbor::decode::<Bytes>(&raw)
                .map_err(CoreError::msg)?
                .to_vec()
                .try_into()
                .map_err(|_| CoreError::msg("Failed to decode public key: invalid size"))?
        };

        let public_key = PublicKey::from(raw_slice);
        Ok(public_key.compute_hash().to_string())
    }

    #[wasm_bindgen(js_name = hashWithBlake2b224)]
    pub fn hash_with_blake2b224(s: &str) -> CoreResult<String> {
        Ok(
            pallas_crypto::hash::Hasher::<224>::hash(&hex::decode(s).map_err(CoreError::msg)?)
                .to_string(),
        )
    }

    #[wasm_bindgen(js_name = hashWithBlake2b256)]
    pub fn hash_with_blake2b256(s: &str) -> CoreResult<String> {
        Ok(
            pallas_crypto::hash::Hasher::<256>::hash(&hex::decode(s).map_err(CoreError::msg)?)
                .to_string(),
        )
    }

    #[wasm_bindgen(js_name = hashScript)]
    pub fn hash_script(script: Script) -> CoreResult<String> {
        let script = script.try_double_cbor()?;
        Ok(match script {
            Script::Native { script } => {
                NativeScript::decode_fragment(&hex::decode(script).map_err(CoreError::msg)?)
                    .map_err(CoreError::msg)?
                    .compute_hash()
                    .to_string()
            }
            Script::PlutusV1 { script } => {
                PlutusScript::<1>::decode_fragment(&hex::decode(script).map_err(CoreError::msg)?)
                    .map_err(CoreError::msg)?
                    .compute_hash()
                    .to_string()
            }
            Script::PlutusV2 { script } => {
                PlutusScript::<2>::decode_fragment(&hex::decode(script).map_err(CoreError::msg)?)
                    .map_err(CoreError::msg)?
                    .compute_hash()
                    .to_string()
            }
            Script::PlutusV3 { script } => {
                PlutusScript::<3>::decode_fragment(&hex::decode(script).map_err(CoreError::msg)?)
                    .map_err(CoreError::msg)?
                    .compute_hash()
                    .to_string()
            }
        })
    }

    #[wasm_bindgen(js_name = hashTransaction)]
    pub fn hash_transaction(tx: &str) -> CoreResult<String> {
        Ok(
            MintedTx::decode_fragment(&hex::decode(tx).map_err(CoreError::msg)?)
                .map_err(CoreError::msg)?
                .transaction_body
                .original_hash()
                .to_string(),
        )
    }
}
