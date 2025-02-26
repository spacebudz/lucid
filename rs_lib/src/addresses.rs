use super::codec::Script;
use super::error::{CoreErr, CoreError, CoreResult};
use super::hasher::Hasher;
use crate::utils::Utils;
use pallas_addresses::{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart};
use serde::{Deserialize, Serialize};
use std::str::FromStr;
use tsify::Tsify;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub struct Addresses;

#[wasm_bindgen]
impl Addresses {
    #[wasm_bindgen(js_name = keyHashToCredential)]
    pub fn key_hash_to_credential(hash: String) -> Credential {
        Credential::Key { hash }
    }

    #[wasm_bindgen(js_name = scriptHashToCredential)]
    pub fn script_hash_to_credential(hash: String) -> Credential {
        Credential::Script { hash }
    }

    #[wasm_bindgen(js_name = scriptToCredential)]
    pub fn script_to_credential(script: Script) -> CoreResult<Credential> {
        Ok(Credential::Script {
            hash: Hasher::hash_script(script)?,
        })
    }

    #[wasm_bindgen(js_name = scriptToAddress)]
    pub fn script_to_address(
        network: Network,
        script: Script,
        delegation: Option<Credential>,
    ) -> CoreResult<String> {
        Self::credential_to_address(network, Self::script_to_credential(script)?, delegation)
    }

    #[wasm_bindgen(js_name = scriptToRewardAddress)]
    pub fn script_to_reward_address(network: Network, script: Script) -> CoreResult<String> {
        Self::credential_to_reward_address(network, Self::script_to_credential(script)?)
    }

    #[wasm_bindgen(js_name = scriptToDrep)]
    pub fn script_to_drep(script: Script) -> CoreResult<String> {
        Self::credential_to_drep(Self::script_to_credential(script)?)
    }

    #[wasm_bindgen(js_name = credentialToDrep)]
    pub fn credential_to_drep(credential: Credential) -> CoreResult<String> {
        match credential {
            Credential::Key { hash } => {
                Utils::encode_bech32("drep", &(hex::encode([0b0010_0010]) + &hash))
            }
            Credential::Script { hash } => {
                Utils::encode_bech32("drep", &(hex::encode([0b0010_0011]) + &hash))
            }
        }
    }

    #[wasm_bindgen(js_name = drepToCredential)]
    pub fn drep_to_credential(id: &str) -> CoreResult<Credential> {
        let (_, id_raw) = bech32::decode(&id).map_err(CoreError::msg)?;
        Ok(match id_raw[0] {
            0b0010_0010 => Credential::Key {
                hash: hex::encode(&id_raw[1..]),
            },
            0b0010_0011 => Credential::Script {
                hash: hex::encode(&id_raw[1..]),
            },
            _ => return Err(CoreError::msg("Invalid drep id")),
        })
    }

    #[wasm_bindgen(js_name = credentialToAddress)]
    pub fn credential_to_address(
        network: Network,
        payment: Credential,
        delegation: Option<Credential>,
    ) -> CoreResult<String> {
        Address::Shelley(ShelleyAddress::new(
            network.try_into().map_err(CoreError::msg)?,
            payment.try_into()?,
            match delegation {
                Some(part) => part.try_into()?,
                _ => ShelleyDelegationPart::Null,
            },
        ))
        .to_bech32()
        .map_err(CoreError::msg)
    }

    #[wasm_bindgen(js_name = credentialToRewardAddress)]
    pub fn credential_to_reward_address(
        network: Network,
        delegation: Credential,
    ) -> CoreResult<String> {
        let payment_pseudo: ShelleyPaymentPart = delegation.clone().try_into()?;
        let delegation: ShelleyDelegationPart = delegation.try_into()?;
        Address::Stake(
            ShelleyAddress::new(network.into(), payment_pseudo, delegation)
                .try_into()
                .map_err(CoreError::msg)?,
        )
        .to_bech32()
        .map_err(CoreError::msg)
    }

    #[wasm_bindgen(js_name = addressToCredential)]
    pub fn address_to_credential(address: &str) -> CoreResult<Credential> {
        let inspected_address = Self::inspect(address)?;
        match inspected_address {
            AddressDetails::Base(details) | AddressDetails::Enterprise(details) => {
                Ok(details.payment.unwrap())
            }
            _ => Err(CoreError::msg("Expected base or enterprise address")),
        }
    }

    #[wasm_bindgen(js_name = rewardAddressToCredential)]
    pub fn reward_address_to_credential(address: &str) -> CoreResult<Credential> {
        let inspected_address = Self::inspect(address)?;
        match inspected_address {
            AddressDetails::Reward(details) => Ok(details.delegation.unwrap()),
            _ => Err(CoreError::msg("Expected reward address")),
        }
    }

    /// Address can be bech32 or hex encoded
    pub fn inspect(address: &str) -> CoreResult<AddressDetails> {
        let address = Address::from_str(address).map_err(CoreError::msg)?;
        Ok(match address {
            Address::Byron(b) => AddressDetails::Byron(AddressDetailsInner {
                network_id: 1,
                address: b.to_base58(),
                address_raw: b.to_hex(),
                payment: None,
                delegation: None,
            }),
            Address::Shelley(s) if s.delegation().as_hash().is_some() => {
                AddressDetails::Base(AddressDetailsInner {
                    network_id: s.network().value(),
                    address: s.to_bech32().unwrap(),
                    address_raw: s.to_hex(),
                    payment: Some(s.payment().clone().into()),
                    delegation: Some(s.delegation().clone().try_into().unwrap()),
                })
            }
            Address::Shelley(s) if s.delegation().as_hash().is_none() => {
                AddressDetails::Enterprise(AddressDetailsInner {
                    network_id: s.network().value(),
                    address: s.to_bech32().unwrap(),
                    address_raw: s.to_hex(),
                    payment: Some(s.payment().clone().into()),
                    delegation: None,
                })
            }
            Address::Stake(s) => AddressDetails::Reward(AddressDetailsInner {
                network_id: s.network().value(),
                address: s.to_bech32().unwrap(),
                address_raw: s.to_hex(),
                payment: None,
                delegation: Some(if s.payload().is_script() {
                    Credential::Script {
                        hash: s.payload().as_hash().to_string(),
                    }
                } else {
                    Credential::Key {
                        hash: s.payload().as_hash().to_string(),
                    }
                }),
            }),
            _ => return Err(CoreError::msg("Pointer addresses not supported")),
        })
    }
}

#[derive(Tsify, Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub enum Network {
    Mainnet,
    Preprod,
    Preview,
    Emulator(u64),
}

impl From<Network> for pallas_addresses::Network {
    fn from(value: Network) -> Self {
        match value {
            Network::Mainnet => pallas_addresses::Network::Mainnet,
            Network::Preprod => pallas_addresses::Network::Testnet,
            Network::Preview => pallas_addresses::Network::Testnet,
            Network::Emulator(_) => pallas_addresses::Network::Testnet,
        }
    }
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone, Eq, PartialEq)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(tag = "type")]
pub enum Credential {
    Key { hash: String },
    Script { hash: String },
}

impl TryFrom<Credential> for ShelleyPaymentPart {
    type Error = CoreErr;
    fn try_from(value: Credential) -> Result<Self, Self::Error> {
        Ok(match value {
            Credential::Key { hash } => {
                ShelleyPaymentPart::key_hash(hash.parse().map_err(CoreError::msg)?)
            }
            Credential::Script { hash } => {
                ShelleyPaymentPart::script_hash(hash.parse().map_err(CoreError::msg)?)
            }
        })
    }
}

impl From<ShelleyPaymentPart> for Credential {
    fn from(value: ShelleyPaymentPart) -> Self {
        match value {
            ShelleyPaymentPart::Key(hash) => Credential::Key {
                hash: hash.to_string(),
            },
            ShelleyPaymentPart::Script(hash) => Credential::Script {
                hash: hash.to_string(),
            },
        }
    }
}

impl TryFrom<Credential> for ShelleyDelegationPart {
    type Error = CoreErr;
    fn try_from(value: Credential) -> Result<Self, Self::Error> {
        Ok(match value {
            Credential::Key { hash } => {
                ShelleyDelegationPart::key_hash(hash.parse().map_err(CoreError::msg)?)
            }
            Credential::Script { hash } => {
                ShelleyDelegationPart::script_hash(hash.parse().map_err(CoreError::msg)?)
            }
        })
    }
}

impl TryFrom<ShelleyDelegationPart> for Credential {
    type Error = CoreErr;
    fn try_from(value: ShelleyDelegationPart) -> Result<Self, Self::Error> {
        match value {
            ShelleyDelegationPart::Key(hash) => Ok(Credential::Key {
                hash: hash.to_string(),
            }),
            ShelleyDelegationPart::Script(hash) => Ok(Credential::Script {
                hash: hash.to_string(),
            }),
            _ => Err(CoreError::msg(
                "Cannot cast delegation part to key or script credential",
            )),
        }
    }
}

impl TryFrom<Credential> for pallas_primitives::StakeCredential {
    type Error = CoreErr;
    fn try_from(credential: Credential) -> Result<Self, Self::Error> {
        Ok(match credential {
            Credential::Key { hash } => Self::AddrKeyhash(hash.parse().map_err(CoreError::msg)?),
            Credential::Script { hash } => Self::ScriptHash(hash.parse().map_err(CoreError::msg)?),
        })
    }
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct AddressDetailsInner {
    network_id: u8,
    address: String,
    address_raw: String,
    payment: Option<Credential>,
    delegation: Option<Credential>,
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(tag = "type")]
pub enum AddressDetails {
    Base(AddressDetailsInner),
    Enterprise(AddressDetailsInner),
    Reward(AddressDetailsInner),
    Byron(AddressDetailsInner),
}

#[cfg(test)]
mod tests {
    use super::Credential;
    use crate::addresses::Addresses;

    #[test]
    fn test_roundtrip_drep() {
        let credential = Credential::Key {
            hash: "9257e68f13d3a9fcebc6be8997ccc092781e2fbd45c8891dc73f7e1d".to_string(),
        };

        assert_eq!(
            Addresses::drep_to_credential(
                &Addresses::credential_to_drep(credential.clone()).unwrap()
            )
            .unwrap(),
            credential
        );
    }
}
