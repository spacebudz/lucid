use super::error::{CoreError, CoreResult};
use bech32::hrp;
use pallas_primitives::Bytes;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub struct Utils;

#[wasm_bindgen]
impl Utils {
    #[wasm_bindgen(js_name = applyParamsToScript)]
    pub fn apply_params_to_script(params: &str, script: &str) -> CoreResult<String> {
        let params_bytes = hex::decode(params).map_err(CoreError::msg)?;
        let script_bytes =
            hex::decode(Utils::apply_single_cbor_encoding(script)?).map_err(CoreError::msg)?;
        let result = uplc::tx::apply_params_to_script(&params_bytes, &script_bytes)
            .map_err(CoreError::msg)?;
        Ok(hex::encode(result))
    }

    #[wasm_bindgen(js_name = encodeBech32)]
    pub fn encode_bech32(hrp: &str, data: &str) -> CoreResult<String> {
        let data = hex::decode(data).map_err(CoreError::msg)?;
        let hrp = hrp::Hrp::parse(hrp).map_err(CoreError::msg)?;
        let result = bech32::encode::<bech32::Bech32>(hrp, &data).map_err(CoreError::msg)?;
        Ok(result)
    }

    #[wasm_bindgen(js_name = applySingleCborEncoding)]
    pub fn apply_single_cbor_encoding(script: &str) -> CoreResult<String> {
        let script_bytes: Bytes = hex::decode(script).map_err(CoreError::msg)?.into();
        match pallas_codec::minicbor::decode::<Bytes>(&script_bytes) {
            Ok(script_bytes_decoded) => {
                match pallas_codec::minicbor::decode::<Bytes>(&script_bytes_decoded) {
                    Ok(_) => return Ok(hex::encode(script_bytes_decoded.to_vec())),
                    Err(_) => return Ok(script.to_string()),
                }
            }
            Err(_) => {
                return Err(CoreError::msg(
                    "Plutus script does not have any cbor wrapper",
                ))
            }
        }
    }

    #[wasm_bindgen(js_name = applyDoubleCborEncoding)]
    pub fn apply_double_cbor_encoding(script: &str) -> CoreResult<String> {
        let script_bytes: Bytes = hex::decode(script).map_err(CoreError::msg)?.into();
        match pallas_codec::minicbor::decode::<Bytes>(&script_bytes) {
            Ok(script_bytes_decoded) => {
                match pallas_codec::minicbor::decode::<Bytes>(&script_bytes_decoded) {
                    Ok(_) => Ok(script.to_string()),
                    Err(_) => {
                        let mut buffer = Vec::new();
                        pallas_codec::minicbor::encode(&script_bytes, &mut buffer)
                            .map_err(CoreError::msg)?;
                        Ok(hex::encode(buffer))
                    }
                }
            }
            Err(_) => {
                return Err(CoreError::msg(
                    "Plutus script does not have any cbor wrapper",
                ))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_apply_single_cbor_encoding() {
        assert_eq!(
            Utils::apply_single_cbor_encoding("49480100002221200101").unwrap(),
            "480100002221200101"
        );
        assert_eq!(
            Utils::apply_single_cbor_encoding("480100002221200101").unwrap(),
            "480100002221200101"
        );
        assert!(Utils::apply_single_cbor_encoding("0100002221200101").is_err());
    }

    #[test]
    fn test_apply_double_cbor_encoding() {
        assert_eq!(
            Utils::apply_double_cbor_encoding("49480100002221200101").unwrap(),
            "49480100002221200101"
        );
        assert_eq!(
            Utils::apply_double_cbor_encoding("480100002221200101").unwrap(),
            "49480100002221200101"
        );
        assert!(Utils::apply_double_cbor_encoding("0100002221200101").is_err());
    }
}
