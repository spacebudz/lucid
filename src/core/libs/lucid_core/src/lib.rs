#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::wasm_bindgen;
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();
}

pub mod addresses;
pub mod codec;
pub mod crypto;
pub mod emulator_state;
pub mod error;
pub mod hasher;
pub mod instruction_builder;
pub mod instruction_signer;
pub mod utils;
