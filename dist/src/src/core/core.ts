import * as C from "./libs/cardano_multiplatform_lib/cardano_multiplatform_lib.generated.js";
import * as M from "./libs/cardano_message_signing/cardano_message_signing.generated.js";
import packageJson from "../../package.js";

async function unsafeInstantiate(module: any, url: string) {
  try {
    await module.instantiate({
      // Exception for Deno fresh framework
      url: new URL(
        url,
        `https://deno.land/x/lucid@${packageJson.version}/src/core/libs/`,
      ),
    });
  } catch (_e) {
    // This only ever happens during SSR rendering
  }
}

await Promise.all([
  unsafeInstantiate(
    C,
    `cardano_multiplatform_lib/cardano_multiplatform_lib_bg.wasm`,
  ),
  unsafeInstantiate(
    M,
    `cardano_message_signing/cardano_message_signing_bg.wasm`,
  ),
]);

export { C, M };
