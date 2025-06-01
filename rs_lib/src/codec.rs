use super::{
    addresses::{Addresses, Credential},
    error::{CoreErr, CoreError, CoreResult},
};
use crate::utils::Utils;
use pallas_addresses::Address;
use pallas_codec::utils::CborWrap;
use pallas_primitives::{
    conway::{
        DatumOption, Mint, PostAlonzoTransactionOutput, PseudoScript, TransactionOutput, Value,
    },
    BigInt, BoundedBytes, Bytes, Constr, Fragment, Hash, Int, KeyValuePairs, MaybeIndefArray,
    NonEmptyKeyValuePairs, NonZeroInt, Nullable, PlutusData, PlutusScript, PoolMetadata,
    PositiveCoin, RationalNumber, TransactionInput,
};
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    i128,
    net::{Ipv4Addr, Ipv6Addr},
    ops::{Add, AddAssign, Deref, DerefMut, Sub, SubAssign},
    str::FromStr,
};
use tsify::Tsify;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub struct Codec;

#[wasm_bindgen]
impl Codec {
    #[wasm_bindgen(js_name = encodeData)]
    pub fn encode_data(d: DataJson) -> CoreResult<String> {
        fn encode(d: DataJson) -> CoreResult<PlutusData> {
            Ok(match d {
                DataJson::Int { int } => {
                    PlutusData::BigInt(BigInt::Int(Int::try_from(int).map_err(CoreError::msg)?))
                }
                DataJson::Bytes { bytes } => PlutusData::BoundedBytes(BoundedBytes::from(
                    hex::decode(bytes).map_err(CoreError::msg)?,
                )),
                DataJson::List { list } => {
                    if list.len() == 0 {
                        PlutusData::Array(MaybeIndefArray::Def(vec![]))
                    } else {
                        PlutusData::Array(MaybeIndefArray::Indef(
                            list.into_iter()
                                .map(|entry| encode(entry))
                                .collect::<CoreResult<_>>()?,
                        ))
                    }
                }
                DataJson::Map { map } => PlutusData::Map(KeyValuePairs::Def(
                    map.into_iter()
                        .map(|MapEntry { k, v }| Ok((encode(*k)?, encode(*v)?)))
                        .collect::<CoreResult<_>>()?,
                )),
                DataJson::Constructor {
                    constructor,
                    fields,
                } => PlutusData::Constr(Constr::from_index(
                    constructor,
                    fields
                        .into_iter()
                        .map(|entry| encode(entry))
                        .collect::<CoreResult<_>>()?,
                )),
            })
        }
        let encoded = encode(d)?;
        Ok(hex::encode(encoded.encode_fragment().unwrap()))
    }

    #[wasm_bindgen(js_name = decodeData)]
    pub fn decode_data(s: &str) -> CoreResult<DataJson> {
        let data = PlutusData::decode_fragment(&hex::decode(s).map_err(CoreError::msg)?)
            .map_err(CoreError::msg)?;
        fn decode(d: PlutusData) -> CoreResult<DataJson> {
            Ok(match d {
                PlutusData::BigInt(BigInt::Int(int)) => DataJson::Int { int: int.into() },
                PlutusData::BigInt(BigInt::BigUInt(uint)) => {
                    let int: i128 = num_bigint::BigUint::from_bytes_be(&uint)
                        .try_into()
                        .map_err(|_| {
                            CoreError::msg(
                                "Infinitely large numbers are not supported! Max is Rust i128",
                            )
                        })?;
                    DataJson::Int { int }
                }
                PlutusData::BigInt(BigInt::BigNInt(nint)) => {
                    let int: u128 = num_bigint::BigUint::from_bytes_be(&nint)
                        .try_into()
                        .map_err(|_| {
                            CoreError::msg(
                                "Infinitely large numbers are not supported! Max is Rust i128",
                            )
                        })?;
                    if int <= i128::MAX as u128 + 1 {
                        DataJson::Int {
                            int: -(int as i128),
                        }
                    } else {
                        return Err(CoreError::msg(
                            "Infinitely large numbers are not supported! Max is Rust i128",
                        ));
                    }
                }
                PlutusData::BoundedBytes(bytes) => DataJson::Bytes {
                    bytes: bytes.to_string(),
                },
                PlutusData::Array(array) => DataJson::List {
                    list: array
                        .iter()
                        .map(|entry| decode(entry.clone()))
                        .collect::<CoreResult<_>>()?,
                },
                PlutusData::Map(map) => DataJson::Map {
                    map: map
                        .iter()
                        .map(|(k, v)| {
                            Ok(MapEntry {
                                k: Box::new(decode(k.clone())?),
                                v: Box::new(decode(v.clone())?),
                            })
                        })
                        .collect::<CoreResult<_>>()?,
                },
                PlutusData::Constr(ref constr @ Constr { ref fields, .. }) => {
                    DataJson::Constructor {
                        constructor: constr.get_index(),
                        fields: fields
                            .iter()
                            .map(|entry| decode(entry.clone()))
                            .collect::<CoreResult<_>>()?,
                    }
                }
            })
        }
        decode(data)
    }

    #[wasm_bindgen(js_name = encodeUtxo)]
    pub fn encode_utxo(utxo: Utxo) -> CoreResult<String> {
        Ok(hex::encode(
            <(TransactionInput, TransactionOutput)>::try_from(utxo)?
                .encode_fragment()
                .unwrap(),
        ))
    }

    #[wasm_bindgen(js_name = decodeUtxo)]
    pub fn decode_utxo(s: &str) -> CoreResult<Utxo> {
        let raw_utxo = <(TransactionInput, TransactionOutput)>::decode_fragment(
            &hex::decode(s).map_err(CoreError::msg)?,
        )
        .map_err(CoreError::msg)?;

        let mut utxo: Utxo = raw_utxo.1.try_into()?;

        utxo.tx_hash = raw_utxo.0.transaction_id.to_string();
        utxo.output_index = raw_utxo.0.index;

        Ok(utxo)
    }

    #[wasm_bindgen(js_name = encodeNativeScript)]
    pub fn encode_native_script(s: NativeScript) -> CoreResult<String> {
        let native_script: pallas_primitives::conway::NativeScript = s.try_into()?;
        Ok(hex::encode(native_script.encode_fragment().unwrap()))
    }
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(tag = "type")]
pub enum NativeScript {
    #[serde(rename_all = "camelCase")]
    Sig {
        key_hash: String,
    },
    Any {
        scripts: Vec<NativeScript>,
    },
    All {
        scripts: Vec<NativeScript>,
    },
    AtLeast {
        required: u32,
        scripts: Vec<NativeScript>,
    },
    Before {
        slot: u64,
    },
    After {
        slot: u64,
    },
}

impl TryFrom<NativeScript> for pallas_primitives::conway::NativeScript {
    type Error = CoreErr;
    fn try_from(value: NativeScript) -> Result<Self, Self::Error> {
        Ok(match value {
            NativeScript::Sig { key_hash } => {
                Self::ScriptPubkey(key_hash.parse().map_err(CoreError::msg)?)
            }
            NativeScript::Any { scripts } => Self::ScriptAny(
                scripts
                    .into_iter()
                    .map(|script| script.try_into())
                    .collect::<CoreResult<_>>()?,
            ),
            NativeScript::All { scripts } => Self::ScriptAll(
                scripts
                    .into_iter()
                    .map(|script| script.try_into())
                    .collect::<CoreResult<_>>()?,
            ),
            NativeScript::AtLeast { required, scripts } => Self::ScriptNOfK(
                required,
                scripts
                    .into_iter()
                    .map(|script| script.try_into())
                    .collect::<CoreResult<_>>()?,
            ),
            NativeScript::Before { slot } => Self::InvalidHereafter(slot),
            NativeScript::After { slot } => Self::InvalidBefore(slot),
        })
    }
}

#[derive(Tsify, Serialize, Deserialize, Debug)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(untagged)]
pub enum DataJson {
    Int {
        int: i128,
    },
    Bytes {
        bytes: String,
    },
    List {
        list: Vec<DataJson>,
    },
    Map {
        map: Vec<MapEntry>,
    },
    Constructor {
        constructor: u64,
        fields: Vec<DataJson>,
    },
}

#[derive(Tsify, Serialize, Deserialize, Debug)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub struct MapEntry {
    pub k: Box<DataJson>,
    pub v: Box<DataJson>,
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub struct Utxos(pub Vec<Utxo>);

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct Utxo {
    pub tx_hash: String,
    pub output_index: u64,
    pub address: String,
    pub assets: Assets,
    #[tsify(optional)]
    pub datum_hash: Option<String>,
    #[tsify(optional)]
    pub datum: Option<String>,
    #[tsify(optional)]
    pub script_ref: Option<Script>,
}

impl Utxo {
    pub fn required_lovelace(&self, coins_per_utxo_byte: u64) -> u64 {
        let constant_overhead = 160 as u64;

        let output_size = TransactionOutput::try_from(self.clone())
            .unwrap()
            .encode_fragment()
            .unwrap()
            .len() as u64;

        let old_lovelace_size = self.assets.get_lovelace().encode_fragment().unwrap().len() as u64;
        let new_lovelace_size = ((output_size as u64 + constant_overhead) * coins_per_utxo_byte)
            .encode_fragment()
            .unwrap()
            .len();

        let lovelace_size: u64 =
            (new_lovelace_size as i128 - old_lovelace_size as i128).max(0) as u64;

        (output_size + constant_overhead + lovelace_size) * coins_per_utxo_byte
    }

    pub fn required_lovelace_mut(&mut self, coins_per_utxo_byte: u64) -> u64 {
        let lovelace = self.required_lovelace(coins_per_utxo_byte);
        if self.assets.get_lovelace() < lovelace {
            self.assets.set_lovelace(lovelace);
        }
        lovelace
    }

    pub fn get_fee_for_input(&self, min_fee_a: u64) -> u64 {
        TransactionInput::try_from(self.clone())
            .unwrap()
            .encode_fragment()
            .unwrap()
            .len() as u64
            * min_fee_a
    }

    pub fn get_fee_for_output(&self, min_fee_a: u64) -> u64 {
        self.size_output() as u64 * min_fee_a
    }

    pub fn from_input(tx_hash: String, output_index: u64) -> Self {
        Self {
            tx_hash,
            output_index,
            address: "".to_string(),
            assets: Assets::new(),
            datum_hash: None,
            datum: None,
            script_ref: None,
        }
    }

    /// mock input,
    /// use this function if you care only about the output
    pub fn from_output(
        address: String,
        assets: Assets,
        datum_hash: Option<String>,
        datum: Option<String>,
        script_ref: Option<Script>,
    ) -> Self {
        Self {
            tx_hash: "00".repeat(32),
            output_index: 0,
            address,
            assets,
            datum_hash,
            datum,
            script_ref,
        }
    }

    pub fn size_output(&self) -> u32 {
        TransactionOutput::try_from(self.clone())
            .unwrap()
            .encode_fragment()
            .unwrap()
            .len() as u32
    }

    pub fn size_assets(&self) -> u32 {
        self.assets.size()
    }
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(tag = "type")]
pub enum Script {
    Native { script: String },
    PlutusV1 { script: String },
    PlutusV2 { script: String },
    PlutusV3 { script: String },
}

impl Script {
    pub fn try_double_cbor(&self) -> CoreResult<Script> {
        Ok(match self {
            Self::PlutusV1 { script } => Script::PlutusV1 {
                script: Utils::apply_double_cbor_encoding(&script)?,
            },
            Self::PlutusV2 { script } => Script::PlutusV2 {
                script: Utils::apply_double_cbor_encoding(&script)?,
            },
            Self::PlutusV3 { script } => Script::PlutusV3 {
                script: Utils::apply_double_cbor_encoding(&script)?,
            },
            native => native.clone(),
        })
    }
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone, PartialEq)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub struct Assets(HashMap<String, i128>);

impl Assets {
    pub fn from<const N: usize>(arr: [(String, i128); N]) -> Self {
        Self(HashMap::from(arr))
    }

    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn from_unit(unit: String, quantity: i128) -> Self {
        Self(HashMap::from([(unit, quantity)]))
    }

    pub fn from_lovelace(lovelace: u64) -> Self {
        Self(HashMap::from([("lovelace".to_string(), lovelace as i128)]))
    }

    pub fn get_lovelace(&self) -> u64 {
        *self.0.get("lovelace").unwrap_or(&0).max(&0) as u64
    }

    pub fn set_lovelace(&mut self, lovelace: u64) {
        self.0.insert("lovelace".to_string(), lovelace as i128);
    }

    pub fn get_policy_id(&self) -> CoreResult<Hash<28>> {
        let mut keys = self.0.keys();
        let key = keys
            .next()
            .ok_or(CoreError::msg("No policy id in assets"))?;
        let (policy_id, _) = key.split_at(56);
        match keys.all(|k| {
            let (next_policy_id, _) = k.split_at(56);
            next_policy_id == policy_id
        }) {
            true => Hash::<28>::from_str(policy_id).map_err(CoreError::msg),
            false => Err(CoreError::msg("Found multiple policy ids in assets")),
        }
    }

    pub fn clamped_sub(self, rhs: Self) -> Self {
        let mut lhs = self - rhs;
        lhs.0.retain(|_, quantity| *quantity > 0);
        lhs
    }

    pub fn contains_other(&self) -> bool {
        self.0
            .iter()
            .filter(|(unit, _)| *unit != "lovelace")
            .count()
            > 0
    }

    pub fn get_all_others(&self) -> Self {
        Self(
            self.0
                .clone()
                .into_iter()
                .filter(|(unit, _)| *unit != "lovelace")
                .collect::<HashMap<String, i128>>(),
        )
    }

    pub fn get_unit(&self, unit: &str) -> i128 {
        *self.0.get(unit).unwrap_or(&0)
    }

    pub fn size(&self) -> u32 {
        Value::try_from(self.clone())
            .unwrap()
            .encode_fragment()
            .unwrap()
            .len() as u32
    }
}

impl Deref for Assets {
    type Target = HashMap<String, i128>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Assets {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Add for Assets {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut lhs = self;

        for (unit, quantity_rhs) in rhs.0 {
            lhs.0
                .entry(unit)
                .and_modify(|quantity_lhs| *quantity_lhs = *quantity_lhs + quantity_rhs)
                .or_insert(quantity_rhs);
        }

        lhs.0.retain(|_, quantity| *quantity != 0);

        lhs
    }
}

impl AddAssign for Assets {
    fn add_assign(&mut self, other: Self) {
        for (key, value) in other.0 {
            *self.0.entry(key).or_insert(0) += value;
        }
        self.0.retain(|_, quantity| *quantity != 0);
    }
}

impl Sub for Assets {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        let mut lhs = self;

        for (unit, quantity_rhs) in rhs.0 {
            lhs.0
                .entry(unit)
                .and_modify(|quantity_lhs| *quantity_lhs = *quantity_lhs - quantity_rhs)
                .or_insert(0 - quantity_rhs);
        }

        lhs.0.retain(|_, quantity| *quantity != 0);

        lhs
    }
}

impl SubAssign for Assets {
    fn sub_assign(&mut self, other: Self) {
        for (key, value) in other.0 {
            *self.0.entry(key).or_insert(0) -= value;
        }
        self.0.retain(|_, quantity| *quantity != 0);
    }
}

impl PartialOrd for Assets {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        fn compare_others(lhs: &Assets, rhs: &Assets) -> Option<Ordering> {
            fn is_all_zeros(lhs: &Assets, rhs: &Assets) -> bool {
                for (unit, quantity) in lhs.iter() {
                    match (quantity - rhs.get_unit(unit)).max(0).cmp(&0) {
                        Ordering::Equal => (),
                        _ => return false,
                    }
                }
                true
            }
            match (is_all_zeros(lhs, rhs), is_all_zeros(rhs, lhs)) {
                (true, true) => Some(Ordering::Equal),
                (true, false) => Some(Ordering::Less),
                (false, true) => Some(Ordering::Greater),
                (false, false) => None,
            }
        }

        compare_others(&self.get_all_others(), &other.get_all_others()).and_then(
            |others_ordering| {
                let lovelace_ordering = self.get_lovelace().cmp(&other.get_lovelace());

                match (lovelace_ordering, others_ordering) {
                    (order, Ordering::Equal) => Some(order),
                    (Ordering::Equal, Ordering::Less) => Some(Ordering::Less),
                    (Ordering::Less, Ordering::Less) => Some(Ordering::Less),
                    (Ordering::Equal, Ordering::Greater) => Some(Ordering::Greater),
                    (Ordering::Greater, Ordering::Greater) => Some(Ordering::Greater),
                    _ => None,
                }
            },
        )
    }
}

impl TryFrom<Assets> for Value {
    type Error = CoreErr;
    fn try_from(assets: Assets) -> Result<Self, Self::Error> {
        let mut assets_inner = assets.0.clone();
        let lovelace = u64::try_from(assets_inner.remove("lovelace").unwrap_or(0))
            .map_err(|_| CoreError::msg("lovelace cannot be negative"))?;
        if assets_inner.len() > 0 {
            let mut value: BTreeMap<Vec<u8>, BTreeMap<Vec<u8>, i128>> = BTreeMap::new();
            for (unit, quantity) in assets_inner.into_iter() {
                if quantity == 0 {
                    continue;
                }
                let unit_vec = hex::decode(unit).map_err(CoreError::msg)?;
                let (policy_id, asset_name) = unit_vec.split_at_checked(28).ok_or(
                    CoreError::msg("Unit needs to be at least 28 bytes (length of policy id)"),
                )?;
                value
                    .entry(policy_id.to_vec())
                    .or_default()
                    .insert(asset_name.to_vec(), quantity);
            }
            Ok(Value::Multiasset(
                lovelace,
                NonEmptyKeyValuePairs::Def(
                    value
                        .into_iter()
                        .map(|(policy_id, multi_asset)| {
                            Ok((
                                policy_id.as_slice().into(),
                                NonEmptyKeyValuePairs::Def(
                                    multi_asset
                                        .into_iter()
                                        .map(|(asset_name, quantity)| {
                                            Ok((
                                                asset_name.into(),
                                                PositiveCoin::try_from(
                                                    u64::try_from(quantity)
                                                        .map_err(CoreError::msg)?,
                                                )
                                                .map_err(CoreError::msg)?,
                                            ))
                                        })
                                        .collect::<CoreResult<_>>()?,
                                ),
                            ))
                        })
                        .collect::<CoreResult<_>>()?,
                ),
            ))
        } else {
            Ok(Value::Coin(lovelace))
        }
    }
}

impl From<Value> for Assets {
    fn from(value: Value) -> Self {
        match value {
            Value::Coin(c) => Assets(HashMap::from([("lovelace".to_string(), c as i128)])),
            Value::Multiasset(c, ma) => Assets(ma.into_iter().fold(
                HashMap::from([("lovelace".to_string(), c as i128)]),
                |mut acc, (policy_id, multi_asset)| {
                    for (asset_name, quantity) in multi_asset {
                        let unit =
                            hex::encode([policy_id.as_slice(), asset_name.as_slice()].concat());
                        acc.insert(unit, Into::<u64>::into(quantity).into());
                    }
                    acc
                },
            )),
        }
    }
}

impl TryFrom<Assets> for Mint {
    type Error = CoreErr;
    fn try_from(assets: Assets) -> Result<Self, Self::Error> {
        let mut value: BTreeMap<Vec<u8>, BTreeMap<Vec<u8>, i128>> = BTreeMap::new();
        for (unit, quantity) in assets.0.into_iter() {
            if quantity == 0 {
                continue;
            }
            let unit_vec = hex::decode(unit).map_err(CoreError::msg)?;
            let (policy_id, asset_name) = unit_vec.split_at_checked(28).ok_or(CoreError::msg(
                "Unit needs to be at least 28 bytes (length of policy id)",
            ))?;
            value
                .entry(policy_id.to_vec())
                .or_default()
                .insert(asset_name.to_vec(), quantity);
        }
        Ok(NonEmptyKeyValuePairs::Def(
            value
                .into_iter()
                .map(|(policy_id, multi_asset)| {
                    Ok((
                        policy_id.as_slice().into(),
                        NonEmptyKeyValuePairs::Def(
                            multi_asset
                                .into_iter()
                                .map(|(asset_name, quantity)| {
                                    Ok((
                                        asset_name.into(),
                                        NonZeroInt::try_from(quantity as i64)
                                            .map_err(CoreError::msg)?,
                                    ))
                                })
                                .collect::<CoreResult<_>>()?,
                        ),
                    ))
                })
                .collect::<CoreResult<_>>()?,
        ))
    }
}

impl From<Mint> for Assets {
    fn from(mint: Mint) -> Self {
        Assets(
            mint.into_iter()
                .fold(HashMap::new(), |mut acc, (policy_id, multi_asset)| {
                    for (asset_name, quantity) in multi_asset {
                        let unit =
                            hex::encode([policy_id.as_slice(), asset_name.as_slice()].concat());
                        acc.insert(unit, Into::<i64>::into(quantity).into());
                    }
                    acc
                }),
        )
    }
}

impl TryFrom<Utxo> for TransactionInput {
    type Error = CoreErr;
    fn try_from(utxo: Utxo) -> Result<Self, Self::Error> {
        Ok(Self {
            transaction_id: utxo.tx_hash.parse().map_err(CoreError::msg)?,
            index: utxo.output_index,
        })
    }
}

impl TryFrom<Utxo> for TransactionOutput {
    type Error = CoreErr;
    fn try_from(utxo: Utxo) -> Result<Self, Self::Error> {
        Ok(Self::PostAlonzo(PostAlonzoTransactionOutput {
            address: Address::from_str(&utxo.address)
                .map_err(CoreError::msg)?
                .to_vec()
                .into(),
            value: utxo.assets.try_into()?,
            datum_option: match (utxo.datum, utxo.datum_hash) {
                (Some(d), None) => Some(DatumOption::Data(CborWrap(
                    PlutusData::decode_fragment(&hex::decode(d).map_err(CoreError::msg)?)
                        .map_err(CoreError::msg)?,
                ))),
                (Some(_), Some(h)) => Some(DatumOption::Hash(h.parse().map_err(CoreError::msg)?)),
                (None, Some(h)) => Some(DatumOption::Hash(h.parse().map_err(CoreError::msg)?)),
                _ => None,
            },
            script_ref: match &utxo.script_ref {
                Some(script) => match script {
                    Script::Native { script } => Some(CborWrap(PseudoScript::NativeScript(
                        pallas_primitives::conway::NativeScript::decode_fragment(
                            &hex::decode(script).map_err(CoreError::msg)?,
                        )
                        .map_err(CoreError::msg)?,
                    ))),
                    Script::PlutusV1 { script } => Some(CborWrap(PseudoScript::PlutusV1Script(
                        PlutusScript::<1>::decode_fragment(
                            &hex::decode(script).map_err(CoreError::msg)?,
                        )
                        .map_err(CoreError::msg)?,
                    ))),
                    Script::PlutusV2 { script } => Some(CborWrap(PseudoScript::PlutusV2Script(
                        PlutusScript::<2>::decode_fragment(
                            &hex::decode(script).map_err(CoreError::msg)?,
                        )
                        .map_err(CoreError::msg)?,
                    ))),
                    Script::PlutusV3 { script } => Some(CborWrap(PseudoScript::PlutusV3Script(
                        PlutusScript::<3>::decode_fragment(
                            &hex::decode(script).map_err(CoreError::msg)?,
                        )
                        .map_err(CoreError::msg)?,
                    ))),
                },
                _ => None,
            },
        }))
    }
}

impl TryFrom<Utxo> for (TransactionInput, TransactionOutput) {
    type Error = CoreErr;
    fn try_from(value: Utxo) -> Result<Self, Self::Error> {
        Ok((value.clone().try_into()?, value.try_into()?))
    }
}

impl TryFrom<TransactionOutput> for Utxo {
    type Error = CoreErr;
    fn try_from(value: TransactionOutput) -> Result<Self, Self::Error> {
        let output = match value {
            TransactionOutput::Legacy(output) => (
                output.address,
                match output.amount {
                    pallas_primitives::alonzo::Value::Coin(c) => Value::Coin(c),
                    pallas_primitives::alonzo::Value::Multiasset(c, ma) => Value::Multiasset(
                        c,
                        NonEmptyKeyValuePairs::Def(
                            ma.iter()
                                .map(|(policy_id, multi_asset)| {
                                    Ok((
                                        policy_id.clone(),
                                        NonEmptyKeyValuePairs::Def(
                                            multi_asset
                                                .iter()
                                                .map(|(asset_name, quantity)| {
                                                    Ok((
                                                        asset_name.clone(),
                                                        PositiveCoin::try_from(*quantity)
                                                            .map_err(CoreError::msg)?,
                                                    ))
                                                })
                                                .collect::<CoreResult<_>>()?,
                                        ),
                                    ))
                                })
                                .collect::<CoreResult<_>>()?,
                        ),
                    ),
                },
                match output.datum_hash {
                    Some(hash) => Some(DatumOption::Hash(hash)),
                    _ => None,
                },
                None,
            ),
            TransactionOutput::PostAlonzo(output) => (
                output.address,
                output.value,
                output.datum_option,
                output.script_ref,
            ),
        };
        Ok(Utxo::from_output(
            // placeholder value for input
            Address::from_bytes(&output.0).unwrap().to_bech32().unwrap(),
            output.1.into(),
            match &output.2 {
                Some(option) => match option {
                    DatumOption::Hash(h) => Some(h.to_string()),
                    _ => None,
                },
                _ => None,
            },
            match &output.2 {
                Some(option) => match option {
                    DatumOption::Data(d) => Some(hex::encode(d.0.encode_fragment().unwrap())),
                    _ => None,
                },
                _ => None,
            },
            match &output.3 {
                Some(script_ref) => match &script_ref.0 {
                    PseudoScript::NativeScript(native_script) => Some(Script::Native {
                        script: hex::encode(native_script.encode_fragment().unwrap()),
                    }),
                    PseudoScript::PlutusV1Script(plutus_script) => Some(Script::PlutusV1 {
                        script: hex::encode(plutus_script.encode_fragment().unwrap()),
                    }),
                    PseudoScript::PlutusV2Script(plutus_script) => Some(Script::PlutusV2 {
                        script: hex::encode(plutus_script.encode_fragment().unwrap()),
                    }),
                    PseudoScript::PlutusV3Script(plutus_script) => Some(Script::PlutusV3 {
                        script: hex::encode(plutus_script.encode_fragment().unwrap()),
                    }),
                },
                _ => None,
            },
        ))
    }
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(tag = "type")]
pub enum Certificate {
    Delegation(Delegation),
    #[serde(rename_all = "camelCase")]
    StakeRegistration {
        reward_address: String,
    },
    #[serde(rename_all = "camelCase")]
    StakeDeregistration {
        reward_address: String,
    },
    PoolRegistration(PoolRegistration),
    PoolRetirement(PoolRetirement),
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct Delegation {
    pub reward_address: String,
    pub variant: DelegVariant,
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub enum DelegVariant {
    Abstain,
    NoConfidence,
    DRep(String),
    Pool(String),
}

impl TryFrom<Certificate> for pallas_primitives::conway::Certificate {
    type Error = CoreErr;
    fn try_from(certificate: Certificate) -> Result<Self, Self::Error> {
        Ok(match certificate {
            Certificate::Delegation(delegation) => {
                let credential =
                    Addresses::reward_address_to_credential(&delegation.reward_address)?;
                match delegation.variant {
                    DelegVariant::Pool(id) => {
                        let (_, id_raw) = bech32::decode(&id).map_err(CoreError::msg)?;
                        pallas_primitives::conway::Certificate::StakeDelegation(
                            credential.try_into()?,
                            id_raw.as_slice().into(),
                        )
                    }
                    variant => pallas_primitives::conway::Certificate::VoteDeleg(
                        credential.try_into()?,
                        match variant {
                            DelegVariant::Abstain => pallas_primitives::conway::DRep::Abstain,
                            DelegVariant::NoConfidence => {
                                pallas_primitives::conway::DRep::NoConfidence
                            }
                            DelegVariant::DRep(id) => {
                                let credential = Addresses::drep_to_credential(&id)?;
                                match credential {
                                    Credential::Key { hash } => {
                                        pallas_primitives::conway::DRep::Key(
                                            hash.parse().map_err(CoreError::msg)?,
                                        )
                                    }
                                    Credential::Script { hash } => {
                                        pallas_primitives::conway::DRep::Script(
                                            hash.parse().map_err(CoreError::msg)?,
                                        )
                                    }
                                }
                            }
                            _ => unreachable!(),
                        },
                    ),
                }
            }
            Certificate::StakeRegistration { reward_address } => {
                let credential = Addresses::reward_address_to_credential(&reward_address)?;
                pallas_primitives::conway::Certificate::StakeRegistration(credential.try_into()?)
            }
            Certificate::StakeDeregistration { reward_address } => {
                let credential = Addresses::reward_address_to_credential(&reward_address)?;
                pallas_primitives::conway::Certificate::StakeDeregistration(credential.try_into()?)
            }
            Certificate::PoolRegistration(pool_registration) => {
                let (_, pool_id_raw) =
                    bech32::decode(&pool_registration.pool_id).map_err(CoreError::msg)?;
                pallas_primitives::conway::Certificate::PoolRegistration {
                    operator: pool_id_raw.as_slice().into(),
                    vrf_keyhash: pool_registration
                        .vrf_key_hash
                        .parse()
                        .map_err(CoreError::msg)?,
                    pledge: pool_registration.pledge,
                    cost: pool_registration.cost,
                    margin: RationalNumber::from_f32(pool_registration.margin),
                    reward_account: Address::from_bech32(&pool_registration.reward_address)
                        .map_err(CoreError::msg)?
                        .to_vec()
                        .into(),
                    pool_owners: pool_registration
                        .owners
                        .iter()
                        .map(
                            |owner| match Addresses::reward_address_to_credential(&owner)? {
                                Credential::Key { hash } => hash.parse().map_err(CoreError::msg),
                                _ => Err(CoreError::msg("Only key hashes allowed for pool owners")),
                            },
                        )
                        .collect::<CoreResult<Vec<Hash<28>>>>()?
                        .into(),
                    relays: pool_registration
                        .relays
                        .into_iter()
                        .map(|relay| {
                            Ok(match relay {
                                Relay::SingleHostIp { ip_v4, ip_v6, port } => {
                                    pallas_primitives::Relay::SingleHostAddr(
                                        port.map_or(Nullable::Null, |p| Nullable::Some(p)),
                                        ip_v4
                                            .map(|ip| {
                                                ip.parse::<Ipv4Addr>()
                                                    .map(|ip| ip.octets().to_vec().into())
                                                    .map_err(CoreError::msg)
                                            })
                                            .transpose()?
                                            .map_or(Nullable::Null, Nullable::Some),
                                        ip_v6
                                            .map(|ip| {
                                                ip.parse::<Ipv6Addr>()
                                                    .map(|ip| ip.octets().to_vec().into())
                                                    .map_err(CoreError::msg)
                                            })
                                            .transpose()?
                                            .map_or(Nullable::Null, Nullable::Some),
                                    )
                                }
                                Relay::SingleHostDomainName { domain_name, port } => {
                                    pallas_primitives::Relay::SingleHostName(
                                        port.map_or(Nullable::Null, |p| Nullable::Some(p)),
                                        domain_name,
                                    )
                                }
                                Relay::MultiHost { domain_name } => {
                                    pallas_primitives::Relay::MultiHostName(domain_name)
                                }
                            })
                        })
                        .collect::<CoreResult<_>>()?,
                    pool_metadata: match pool_registration.metadata_url {
                        Some(url) => Nullable::Some(PoolMetadata {
                            hash: pool_registration.metadata_hash.map_or_else(
                                || Err(CoreError::msg("Url metadata hash missing")),
                                |hash| hash.parse().map_err(CoreError::msg),
                            )?,
                            url,
                        }),
                        _ => Nullable::Null,
                    },
                }
            }
            Certificate::PoolRetirement(pool_retirement) => {
                let (_, pool_id_raw) =
                    bech32::decode(&pool_retirement.pool_id).map_err(CoreError::msg)?;
                pallas_primitives::conway::Certificate::PoolRetirement(
                    pool_id_raw.as_slice().into(),
                    pool_retirement.epoch,
                )
            }
        })
    }
}

trait ToRationalNumber {
    fn from_f32(n: f32) -> Self;
}

impl ToRationalNumber for RationalNumber {
    fn from_f32(n: f32) -> Self {
        let fraction = fraction::Fraction::from(n);
        Self {
            numerator: *fraction.numer().unwrap() as u64,
            denominator: *fraction.denom().unwrap() as u64,
        }
    }
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct PoolRegistration {
    pub pool_id: String,
    pub vrf_key_hash: String,
    pub pledge: u64,
    pub cost: u64,
    pub margin: f32,
    pub reward_address: String,
    pub owners: Vec<String>,
    pub relays: Vec<Relay>,
    #[tsify(optional)]
    pub metadata_url: Option<String>,
    #[tsify(optional)]
    pub metadata_hash: Option<String>,
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(tag = "type")]
pub enum Relay {
    #[serde(rename_all = "camelCase")]
    SingleHostIp {
        ip_v4: Option<String>,
        ip_v6: Option<String>,
        port: Option<u32>,
    },
    #[serde(rename_all = "camelCase")]
    SingleHostDomainName {
        domain_name: String,
        #[tsify(optional)]
        port: Option<u32>,
    },
    #[serde(rename_all = "camelCase")]
    MultiHost { domain_name: String },
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct PoolRetirement {
    pub pool_id: String,
    pub epoch: u64,
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct Withdrawal {
    pub reward_address: String,
    pub amount: u64,
}

#[derive(Tsify, Serialize, Deserialize, Debug, Clone)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(untagged)]
pub enum AuxMetadata<const CONVERSION: bool> {
    Number(i64),
    String(String),
    Array(#[tsify(type = "any")] Vec<AuxMetadata<CONVERSION>>),
    Object(#[tsify(type = "any")] HashMap<String, AuxMetadata<CONVERSION>>),
}

impl<const CONVERSION: bool> From<AuxMetadata<CONVERSION>> for pallas_primitives::Metadatum {
    fn from(value: AuxMetadata<CONVERSION>) -> Self {
        match value {
            AuxMetadata::Number(n) => Self::Int(Int::from(n)),
            AuxMetadata::String(s) => {
                if CONVERSION && s.starts_with("0x") {
                    match Bytes::from_str(&s[2..]) {
                        Ok(b) => Self::Bytes(b),
                        _ => Self::Text(s),
                    }
                } else {
                    Self::Text(s)
                }
            }
            AuxMetadata::Array(array) => Self::Array(array.into_iter().map(|e| e.into()).collect()),
            AuxMetadata::Object(object) => Self::Map(KeyValuePairs::Def(
                object
                    .into_iter()
                    .map(|(k, v)| (Self::Text(k), v.into()))
                    .collect(),
            )),
        }
    }
}

pub trait ConstrConversion<A> {
    fn from_index(index: u64, fields: Vec<A>) -> Constr<A>;
    fn get_index(&self) -> u64;
}

impl<A> ConstrConversion<A> for Constr<A> {
    fn from_index(index: u64, fields: Vec<A>) -> Constr<A> {
        fn convert_index_to_tag(constr: u64) -> Option<u64> {
            match constr {
                0..=6 => Some(121 + constr),
                7..=127 => Some(1280 - 7 + constr),
                _ => None, // 102 otherwise
            }
        }
        let tag = convert_index_to_tag(index);
        Constr {
            tag: tag.unwrap_or(102),
            any_constructor: tag.map_or(Some(index), |_| None),
            fields: if fields.len() == 0 {
                MaybeIndefArray::Def(vec![])
            } else {
                MaybeIndefArray::Indef(fields)
            },
        }
    }
    fn get_index(&self) -> u64 {
        fn convert_tag_to_index(tag: u64) -> Option<u64> {
            match tag {
                121..=127 => Some(tag - 121),
                1280..=1400 => Some(tag - 1280 + 7),
                _ => None,
            }
        }
        convert_tag_to_index(self.tag).unwrap_or_else(|| self.any_constructor.unwrap())
    }
}

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;

    use crate::codec::{ConstrConversion, ToRationalNumber};
    use fraction::FromPrimitive;
    use pallas_primitives::{Constr, PlutusData};

    use super::Assets;

    #[test]
    fn compact_tag_range() {
        assert_eq!(Constr::from_index(0, Vec::<PlutusData>::new()).tag, 121);
        assert_eq!(Constr::from_index(1, Vec::<PlutusData>::new()).tag, 122);
        assert_eq!(Constr::from_index(6, Vec::<PlutusData>::new()).tag, 127);
        assert_ne!(Constr::from_index(7, Vec::<PlutusData>::new()).tag, 128);
    }
    #[test]
    fn compact_tag_mid_range() {
        assert_eq!(Constr::from_index(7, Vec::<PlutusData>::new()).tag, 1280);
        assert_eq!(Constr::from_index(8, Vec::<PlutusData>::new()).tag, 1281);
        assert_eq!(Constr::from_index(100, Vec::<PlutusData>::new()).tag, 1373);
        assert_eq!(Constr::from_index(127, Vec::<PlutusData>::new()).tag, 1400);
        assert_ne!(Constr::from_index(128, Vec::<PlutusData>::new()).tag, 1401);
    }
    #[test]
    fn roundtrip_tag_index() {
        assert_eq!(
            Constr::from_index(128, Vec::<PlutusData>::new()).get_index(),
            128
        );
        assert_eq!(
            Constr::from_index(123124125125, Vec::<PlutusData>::new()).get_index(),
            123124125125
        );
        assert_eq!(
            Constr::from_index(0, Vec::<PlutusData>::new()).get_index(),
            0
        );
        assert_eq!(
            Constr::from_index(1, Vec::<PlutusData>::new()).get_index(),
            1
        );
        assert_eq!(
            Constr::from_index(6, Vec::<PlutusData>::new()).get_index(),
            6
        );
        assert_eq!(
            Constr::from_index(7, Vec::<PlutusData>::new()).get_index(),
            7
        );
        assert_eq!(
            Constr::from_index(127, Vec::<PlutusData>::new()).get_index(),
            127
        );
    }

    #[test]
    fn test_partial_compare_assets() {
        let assets1 = Assets::from([("lovelace".to_string(), 100), ("00".to_string(), 10)]);
        let assets2 = Assets::from([("lovelace".to_string(), 100), ("00".to_string(), 12)]);
        let assets3 = Assets::from([("lovelace".to_string(), 50), ("00".to_string(), 10)]);
        let assets4 = Assets::from([
            ("lovelace".to_string(), 50),
            ("00".to_string(), 5),
            ("11".to_string(), 5),
        ]);

        assert_eq!(assets1.partial_cmp(&assets1), Some(Ordering::Equal));
        assert_eq!(assets1.partial_cmp(&assets2), Some(Ordering::Less));
        assert_eq!(assets2.partial_cmp(&assets1), Some(Ordering::Greater));
        assert_eq!(assets1.partial_cmp(&assets3), Some(Ordering::Greater));
        assert_eq!(assets3.partial_cmp(&assets1), Some(Ordering::Less));
        assert_eq!(assets1.partial_cmp(&assets4), None);
        assert_eq!(assets4.partial_cmp(&assets1), None);
    }

    #[test]
    fn test_big_num() {
        let original_num = 1234567890;
        let bytes = num_bigint::BigInt::from_i128(original_num)
            .unwrap()
            .to_bytes_be()
            .1;
        let num: i128 = num_bigint::BigUint::from_bytes_be(&bytes)
            .try_into()
            .unwrap();
        assert_eq!(original_num, num);
    }

    #[test]
    fn test_big_num_negative() {
        let original_num = -1234567890;
        let bytes = num_bigint::BigInt::from_i128(original_num)
            .unwrap()
            .to_bytes_be()
            .1;
        let num: u128 = num_bigint::BigUint::from_bytes_be(&bytes)
            .try_into()
            .unwrap();
        assert_eq!(original_num, -(num as i128));
    }

    #[test]
    fn test_fraction() {
        let rat = pallas_primitives::RationalNumber::from_f32(0.015);
        let frac = fraction::Fraction::from((rat.numerator, rat.denominator));
        let f: f64 = frac.try_into().unwrap();

        assert_eq!(f, 0.015);
    }
}
