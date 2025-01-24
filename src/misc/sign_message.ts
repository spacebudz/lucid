import {
  Addresses,
  Crypto,
  fromHex,
  Hasher,
  MessageSigning,
  type SignedMessage,
  toHex,
} from "../mod.ts";

export function signMessage(
  address: string,
  payload: string,
  privateKey: string,
): SignedMessage {
  const { addressRaw } = Addresses.inspect(
    address,
  );

  const protectedHeaders = MessageSigning.HeaderMap.new();
  protectedHeaders.set_algorithm_id(
    MessageSigning.Label.from_algorithm_id(
      MessageSigning.AlgorithmId.EdDSA,
    ),
  );
  protectedHeaders.set_header(
    MessageSigning.Label.new_text("address"),
    MessageSigning.CBORValue.new_bytes(fromHex(addressRaw)),
  );
  const protectedSerialized = MessageSigning.ProtectedHeaderMap.new(
    protectedHeaders,
  );
  const unprotectedHeaders = MessageSigning.HeaderMap.new();
  const headers = MessageSigning.Headers.new(
    protectedSerialized,
    unprotectedHeaders,
  );
  const builder = MessageSigning.COSESign1Builder.new(
    headers,
    fromHex(payload),
    false,
  );
  const toSign = builder.make_data_to_sign().to_bytes();

  const signedSigStruc = fromHex(Crypto.sign(privateKey, toHex(toSign)));
  const coseSign1 = builder.build(signedSigStruc);

  const key = MessageSigning.COSEKey.new(
    MessageSigning.Label.from_key_type(MessageSigning.KeyType.OKP), //OKP
  );
  key.set_algorithm_id(
    MessageSigning.Label.from_algorithm_id(
      MessageSigning.AlgorithmId.EdDSA,
    ),
  );
  key.set_header(
    MessageSigning.Label.new_int(
      MessageSigning.Int.new_negative(
        MessageSigning.BigNum.from_str("1"),
      ),
    ),
    MessageSigning.CBORValue.new_int(
      MessageSigning.Int.new_i32(6), //MessageSigning.CurveType.Ed25519
    ),
  ); // crv (-1) set to Ed25519 (6)
  key.set_header(
    MessageSigning.Label.new_int(
      MessageSigning.Int.new_negative(
        MessageSigning.BigNum.from_str("2"),
      ),
    ),
    MessageSigning.CBORValue.new_bytes(
      fromHex(Crypto.privateKeyToDetails(privateKey).privateKey),
    ),
  ); // x (-2) set to public key

  return {
    signature: toHex(coseSign1.to_bytes()),
    key: toHex(key.to_bytes()),
  };
}

export function verifyMessage(
  address: string,
  payload: string,
  signedMessage: SignedMessage,
): boolean {
  const { payment, delegation, addressRaw } = Addresses.inspect(
    address,
  );
  const keyHash = payment?.hash || delegation?.hash;
  if (!keyHash) throw new Error("Not a valid address provided.");

  const cose1 = MessageSigning.COSESign1.from_bytes(
    fromHex(signedMessage.signature),
  );
  const key = MessageSigning.COSEKey.from_bytes(fromHex(signedMessage.key));

  const protectedHeaders = cose1.headers().protected()
    .deserialized_headers();

  const cose1Address = (() => {
    try {
      return toHex(
        protectedHeaders.header(MessageSigning.Label.new_text("address"))
          ?.as_bytes()!,
      );
    } catch (_e) {
      throw new Error("No address found in signature.");
    }
  })();

  const cose1AlgorithmId = (() => {
    try {
      const int = protectedHeaders.algorithm_id()?.as_int();
      if (int?.is_positive()) return parseInt(int.as_positive()?.to_str()!);
      return parseInt(int?.as_negative()?.to_str()!);
    } catch (_e) {
      throw new Error("Failed to retrieve Algorithm Id.");
    }
  })();

  const keyAlgorithmId = (() => {
    try {
      const int = key.algorithm_id()?.as_int();
      if (int?.is_positive()) return parseInt(int.as_positive()?.to_str()!);
      return parseInt(int?.as_negative()?.to_str()!);
    } catch (_e) {
      throw new Error("Failed to retrieve Algorithm Id.");
    }
  })();

  const keyCurve = (() => {
    try {
      const int = key.header(MessageSigning.Label.new_int(
        MessageSigning.Int.new_negative(
          MessageSigning.BigNum.from_str("1"),
        ),
      ))?.as_int();
      if (int?.is_positive()) return parseInt(int.as_positive()?.to_str()!);
      return parseInt(int?.as_negative()?.to_str()!);
    } catch (_e) {
      throw new Error("Failed to retrieve Curve.");
    }
  })();

  const keyType = (() => {
    try {
      const int = key.key_type().as_int();
      if (int?.is_positive()) return parseInt(int.as_positive()?.to_str()!);
      return parseInt(int?.as_negative()?.to_str()!);
    } catch (_e) {
      throw new Error("Failed to retrieve Key Type.");
    }
  })();

  const publicKey = (() => {
    try {
      return toHex(
        key.header(MessageSigning.Label.new_int(
          MessageSigning.Int.new_negative(
            MessageSigning.BigNum.from_str("2"),
          ),
        ))?.as_bytes()!,
      );
    } catch (_e) {
      throw new Error("No public key found.");
    }
  })();

  const cose1Payload = (() => {
    try {
      return toHex(cose1.payload()!);
    } catch (_e) {
      throw new Error("No payload found.");
    }
  })();

  const signature = toHex(cose1.signature());

  const data = cose1.signed_data(undefined, undefined).to_bytes();

  if (cose1Address !== addressRaw) return false;

  if (keyHash !== Hasher.hashPublicKey(publicKey)) return false;

  if (
    cose1AlgorithmId !== keyAlgorithmId &&
    cose1AlgorithmId !== MessageSigning.AlgorithmId.EdDSA
  ) {
    return false;
  }

  if (keyCurve !== 6) return false;

  if (keyType !== 1) return false;

  if (cose1Payload !== payload) return false;

  return Crypto.verify(publicKey, toHex(data), signature);
}
