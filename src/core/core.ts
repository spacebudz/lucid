import * as Core from "./libs/cardano_multiplatform_lib/cardano_multiplatform_lib.generated.js";
import * as Msg from "./libs/cardano_message_signing/cardano_message_signing.generated.js";

export { Core, Msg };

async function importForEnvironmentCore(): Promise<typeof Core | null> {
  try {
    await Core.instantiate();
    return Core;
  } catch (_e) {
    // This only ever happens during SSR rendering
    return null;
  }
}

async function importForEnvironmentMsg(): Promise<typeof Msg | null> {
  try {
    await Msg.instantiate();
    return Msg;
  } catch (_e) {
    // This only ever happens during SSR rendering
    return null;
  }
}

const [resolvedCore, resolvedMessage] = await Promise.all([
  importForEnvironmentCore(),
  importForEnvironmentMsg(),
]);

export const C: typeof Core = resolvedCore!;

export const M: typeof Msg = resolvedMessage!;
