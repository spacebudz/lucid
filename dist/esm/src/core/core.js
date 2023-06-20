const isNode = globalThis?.process?.versions?.node;
if (isNode) {
    if (typeof btoa === 'undefined') {globalThis.btoa = function (str) {return Buffer.from(str, 'binary').toString('base64');}; globalThis.atob = function (b64Encoded) {return Buffer.from(b64Encoded, 'base64').toString('binary');};}
    const fetch = /* #__PURE__ */ await import(/* webpackIgnore: true */ "node-fetch");
    const { Crypto } = /* #__PURE__ */ await import(/* webpackIgnore: true */ "@peculiar/webcrypto");
    const { WebSocket } = /* #__PURE__ */ await import(/* webpackIgnore: true */ "ws");
    const fs = /* #__PURE__ */ await import(/* webpackIgnore: true */ "fs");
    if (!globalThis.WebSocket) globalThis.WebSocket = WebSocket;
    if (!globalThis.crypto) globalThis.crypto = new Crypto();
    if (!globalThis.fetch) globalThis.fetch = fetch.default;
    if (!globalThis.Headers) globalThis.Headers = fetch.Headers;
    if (!globalThis.Request) globalThis.Request = fetch.Request;
    if (!globalThis.Response) globalThis.Response = fetch.Response;
    if (!globalThis.fs) globalThis.fs = fs; 
}

const C = await (async () => {
  try {
    if (isNode) {
      return await import(
        /* webpackIgnore: true */ "./libs/cardano_multiplatform_lib/nodejs/cardano_multiplatform_lib.generated.js"
      );
    }
    return await import(
      "./libs/cardano_multiplatform_lib/cardano_multiplatform_lib.generated.js"
    );
  } catch (_e) {
    // This only ever happens during SSR rendering
    return null;
  }
})();
const M = await (async () => {
  try {
    if (isNode) {
      return await import(
        /* webpackIgnore: true */ "./libs/cardano_message_signing/nodejs/cardano_message_signing.generated.js"
      );
    }
    return await import(
      "./libs/cardano_message_signing/cardano_message_signing.generated.js"
    );
  } catch (_e) {
    // This only ever happens during SSR rendering
    return null;
  }
})();
if (!isNode) {
  async function unsafeInstantiate(module) {
    try {
      await module.instantiate();
    } catch (_e) {
      // This only ever happens during SSR rendering
    }
  }
  await Promise.all([
    unsafeInstantiate(C),
    unsafeInstantiate(M),
  ]);
}
export { C, M };
