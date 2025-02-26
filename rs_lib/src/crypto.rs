use super::addresses::Credential;
use crate::error::{CoreError, CoreResult};
use bech32::Hrp;
use pallas_crypto::key::ed25519::{PublicKey, SecretKey, SecretKeyExtended, Signature};
use pallas_primitives::Bytes;
use pallas_traverse::ComputeHash;
use pallas_wallet::{
    hd::{Bip32PrivateKey, Bip32PublicKey},
    PrivateKey,
};
use rand::rngs::OsRng;
use serde::{Deserialize, Serialize};
use std::str::FromStr;
use tsify::Tsify;
use wasm_bindgen::prelude::wasm_bindgen;

const HRP_ED25519: &'static str = "addr_sk";
const HRP_ED25519_EXTENDED: &'static str = "addr_xsk";

#[wasm_bindgen]
pub struct Crypto;

#[wasm_bindgen]
impl Crypto {
    #[wasm_bindgen(js_name = privateKeyToDetails)]
    pub fn private_key_to_details(key: &str) -> CoreResult<KeyDetails> {
        let private_key = Self::decode_private_key(key)?;

        let bech32_encoded = match &private_key {
            PrivateKey::Extended(p) => {
                let bytes = unsafe { SecretKeyExtended::leak_into_bytes(p.clone()) };
                bech32::encode::<bech32::Bech32>(Hrp::parse(&HRP_ED25519_EXTENDED).unwrap(), &bytes)
                    .map_err(CoreError::msg)?
            }
            PrivateKey::Normal(p) => {
                let bytes = unsafe { SecretKey::leak_into_bytes(p.clone()) };
                bech32::encode::<bech32::Bech32>(Hrp::parse(&HRP_ED25519).unwrap(), &bytes)
                    .map_err(CoreError::msg)?
            }
        };

        let public_key = private_key.public_key();
        let key_hash = public_key.compute_hash();

        Ok(KeyDetails {
            private_key: bech32_encoded,
            public_key: public_key.to_string(),
            credential: Credential::Key {
                hash: key_hash.to_string(),
            },
        })
    }

    #[wasm_bindgen(js_name = seedToDetails)]
    pub fn seed_to_details(seed: &str, index: u32, part: Part) -> CoreResult<KeyDetails> {
        let private_key = Self::seed_to_private_key(seed, index, part)?;
        let public_key = private_key.public_key();
        let key_hash = public_key.compute_hash();

        let bech32_encoded = match private_key {
            PrivateKey::Extended(p) => {
                let bytes = unsafe { SecretKeyExtended::leak_into_bytes(p) };
                bech32::encode::<bech32::Bech32>(Hrp::parse(&HRP_ED25519_EXTENDED).unwrap(), &bytes)
                    .map_err(CoreError::msg)?
            }
            _ => panic!(),
        };

        Ok(KeyDetails {
            private_key: bech32_encoded,
            public_key: public_key.to_string(),
            credential: Credential::Key {
                hash: key_hash.to_string(),
            },
        })
    }

    #[wasm_bindgen(js_name = seedToXpub)]
    pub fn seed_to_xpub(seed: &str, index: u32) -> CoreResult<String> {
        let bip32_priv = Bip32PrivateKey::from_bip39_mnenomic(seed.to_string(), "".to_string())
            .map_err(CoreError::msg)?;

        let account_key = bip32_priv
            .derive(harden(1852))
            .derive(harden(1815))
            .derive(harden(index));

        Ok(account_key.to_public().to_bech32())
    }

    #[wasm_bindgen(js_name = xpubToPublicKey)]
    pub fn xpub_to_public_key(xpub: &str, part: Part) -> CoreResult<String> {
        let raw = bech32::decode(xpub)
            .map(|(_, raw)| raw)
            .or_else(|bech32_err| {
                hex::decode(xpub).map_err(|hex_err| {
                    CoreError::msg(format!(
                        "Both decoding attempts failed:\n  Bech32 error: {}\n  Hex error: {}",
                        bech32_err, hex_err
                    ))
                })
            })?;

        let bip32_public = Bip32PublicKey::from_bytes(
            <[u8; 64]>::try_from(raw)
                .map_err(|_| CoreError::msg("Invalid xpub size: expected 64 bytes"))?,
        );

        let public_key = bip32_public
            .derive(match part {
                Part::Payment => 0,
                Part::Delegation => 2,
            })
            .map_err(CoreError::msg)?
            .derive(0)
            .map_err(CoreError::msg)?
            .to_ed25519_pubkey();

        Ok(public_key.to_string())
    }

    #[wasm_bindgen(js_name = generateSeed)]
    pub fn generate_seed() -> String {
        Bip32PrivateKey::generate_with_mnemonic(OsRng, "".to_string())
            .1
            .to_string()
    }

    /// generates extended ed25519 private key
    #[wasm_bindgen(js_name = generatePrivateKey)]
    pub fn generate_private_key() -> String {
        let private_key = match Bip32PrivateKey::generate(OsRng).to_ed25519_private_key() {
            PrivateKey::Extended(p) => unsafe { SecretKeyExtended::leak_into_bytes(p) },
            _ => panic!(),
        };

        let bech_32_encoded = bech32::encode::<bech32::Bech32>(
            Hrp::parse(&HRP_ED25519_EXTENDED).unwrap(),
            &private_key,
        )
        .unwrap();

        bech_32_encoded
    }

    pub fn sign(key: &str, message: &str) -> CoreResult<String> {
        let message = hex::decode(message).map_err(CoreError::msg)?;
        let private_key = Crypto::decode_private_key(key)?;
        let signature = private_key.sign(&message);
        Ok(signature.to_string())
    }

    pub fn verify(pubkey: &str, message: &str, signature: &str) -> CoreResult<bool> {
        let message = hex::decode(message).map_err(CoreError::msg)?;
        let public_key = PublicKey::from_str(pubkey).map_err(CoreError::msg)?;
        let signature: Signature = Signature::from_str(signature).map_err(CoreError::msg)?;
        Ok(public_key.verify(message, &signature))
    }
}

impl Crypto {
    pub fn decode_private_key(key: &str) -> CoreResult<PrivateKey> {
        let raw = bech32::decode(key)
            .map(|(_, raw)| raw)
            .or_else(|bech32_err| {
                hex::decode(key).map_err(|hex_err| {
                    CoreError::msg(format!(
                        "Both decoding attempts failed:\n  Bech32 error: {}\n  Hex error: {}",
                        bech32_err, hex_err
                    ))
                })
            })?;

        let raw = if raw.len() == 32 || raw.len() == 64 {
            raw
        } else {
            pallas_codec::minicbor::decode::<Bytes>(&raw)
                .map_err(CoreError::msg)?
                .to_vec()
        };

        let private_key = match <[u8; 32]>::try_from(raw.clone()) {
            Ok(raw) => PrivateKey::Normal(SecretKey::from(raw)),
            _ => PrivateKey::Extended(
                SecretKeyExtended::from_bytes(<[u8; 64]>::try_from(raw).unwrap()).unwrap(),
            ),
        };

        Ok(private_key)
    }

    pub fn seed_to_private_key(seed: &str, index: u32, part: Part) -> CoreResult<PrivateKey> {
        let bip32_priv = Bip32PrivateKey::from_bip39_mnenomic(seed.to_string(), "".to_string())
            .map_err(CoreError::msg)?;
        let account_key = bip32_priv
            .derive(harden(1852))
            .derive(harden(1815))
            .derive(harden(index));

        let private_key = account_key
            .derive(match part {
                Part::Payment => 0,
                Part::Delegation => 2,
            })
            .derive(0)
            .to_ed25519_private_key();

        Ok(private_key)
    }
}

fn harden(n: u32) -> u32 {
    0x80000000 + n
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub enum Part {
    Payment,
    Delegation,
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct KeyDetails {
    pub private_key: String,
    pub public_key: String,
    pub credential: Credential,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_seed() {
        let seed = Crypto::generate_seed();
        assert!(!seed.is_empty());
    }

    #[test]
    fn test_generate_private_key() {
        let private_key = Crypto::generate_private_key();
        assert!(private_key.starts_with(HRP_ED25519_EXTENDED));
    }

    #[test]
    fn test_private_key_to_details() {
        let private_key = Crypto::generate_private_key();
        let details = Crypto::private_key_to_details(&private_key).unwrap();
        assert!(details.private_key.starts_with(HRP_ED25519_EXTENDED));
        assert!(!details.public_key.is_empty());
        assert!(matches!(details.credential, Credential::Key { .. }));
    }

    #[test]
    fn test_seed_to_details() {
        let seed = Crypto::generate_seed();
        let details = Crypto::seed_to_details(&seed, 0, Part::Payment).unwrap();
        assert!(details.private_key.starts_with(HRP_ED25519_EXTENDED));
        assert!(!details.public_key.is_empty());
        assert!(matches!(details.credential, Credential::Key { .. }));
    }

    #[test]
    fn test_decode_private_key() {
        let private_key = Crypto::generate_private_key();
        let decoded_key = Crypto::decode_private_key(&private_key).unwrap();
        match decoded_key {
            PrivateKey::Extended(_) => assert!(private_key.starts_with(HRP_ED25519_EXTENDED)),
            PrivateKey::Normal(_) => assert!(private_key.starts_with(HRP_ED25519)),
        }
    }

    #[test]
    fn test_seed_to_private_key() {
        let seed = Crypto::generate_seed();
        let private_key = Crypto::seed_to_private_key(&seed, 0, Part::Payment).unwrap();
        match private_key {
            PrivateKey::Extended(_) => assert!(true),
            _ => assert!(false, "Expected an extended private key"),
        }
    }

    #[test]
    fn test_invalid_private_key() {
        let result = Crypto::private_key_to_details("invalid_key");
        assert!(result.is_err());
    }

    #[test]
    fn test_invalid_seed() {
        let result = Crypto::seed_to_details("invalid_seed", 0, Part::Payment);
        assert!(result.is_err());
    }

    #[test]
    fn test_invalid_decode_private_key() {
        let result = Crypto::decode_private_key("invalid_key");
        assert!(result.is_err());
    }

    #[test]
    fn test_invalid_seed_to_private_key() {
        let result = Crypto::seed_to_private_key("invalid_seed", 0, Part::Payment);
        assert!(result.is_err());
    }
}
