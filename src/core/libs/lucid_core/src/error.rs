use thiserror::Error;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::JsError;

#[derive(Error, Debug)]
pub enum CoreError {
    #[error("Address network mismatch: expected {0}, found {1}")]
    AddressNetworkMismatch(String, String),
    #[error("Exceeded maximum assets size: expected max {0}, found {1}")]
    MaximumAssetsSize(u32, u32),
    #[error("Not enough lovelace leftover to cover required lovelace for output")]
    NotEnoughLovelaceForOutput,
    #[error("Not enough lovelace leftover to cover fee")]
    NotEnoughLovelaceForFee,
    #[error("Could not build transaction")]
    TxBuildFail,
    #[error("Exhausted inputs: {0}")]
    ExhaustedInputs(String),
}

impl CoreError {
    #[cfg(not(target_arch = "wasm32"))]
    pub fn to_msg(&self) -> String {
        self.to_string()
    }

    #[cfg(target_arch = "wasm32")]
    pub fn to_msg(&self) -> JsError {
        JsError::new(&self.to_string())
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn msg<T>(message: T) -> String
    where
        T: std::fmt::Display,
    {
        message.to_string()
    }

    #[cfg(target_arch = "wasm32")]
    pub fn msg<T>(message: T) -> JsError
    where
        T: std::fmt::Display,
    {
        JsError::new(&message.to_string())
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub type CoreErr = String;
#[cfg(target_arch = "wasm32")]
pub type CoreErr = JsError;

pub type CoreResult<T> = Result<T, CoreErr>;
