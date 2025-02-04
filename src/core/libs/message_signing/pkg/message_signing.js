import * as wasm from "./message_signing_bg.wasm";
export * from "./message_signing_bg.js";
import { __wbg_set_wasm } from "./message_signing_bg.js";
__wbg_set_wasm(wasm);