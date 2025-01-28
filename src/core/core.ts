import * as Core from "./libs/lucid_core/pkg/lucid_core.js";
import type * as CoreTypes from "./libs/lucid_core/pkg/lucid_core.d.ts";
import * as MessageSigningInstance from "./libs/message_signing/pkg/message_signing.js";
import type * as MessageSigningTypes from "./libs/message_signing/pkg/message_signing.d.ts";

export type Addresses = CoreTypes.Addresses;
export const Addresses: typeof CoreTypes.Addresses = Core.Addresses;

export type Codec = CoreTypes.Codec;
export const Codec: typeof CoreTypes.Codec = Core.Codec;

export type Crypto = CoreTypes.Crypto;
export const Crypto: typeof CoreTypes.Crypto = Core.Crypto;

export type EmulatorState = CoreTypes.EmulatorState;
export const EmulatorState: typeof CoreTypes.EmulatorState = Core.EmulatorState;

export type Hasher = CoreTypes.Hasher;
export const Hasher: typeof CoreTypes.Hasher = Core.Hasher;

export type InstructionBuilder = CoreTypes.InstructionBuilder;
export const InstructionBuilder: typeof CoreTypes.InstructionBuilder =
  Core.InstructionBuilder;

export type InstructionSigner = CoreTypes.InstructionSigner;
export const InstructionSigner: typeof CoreTypes.InstructionSigner =
  Core.InstructionSigner;

export type Utils = CoreTypes.Utils;
export const Utils: typeof CoreTypes.Utils = Core.Utils;

export type MessageSigning = typeof MessageSigningTypes;
export const MessageSigning: typeof MessageSigningTypes =
  MessageSigningInstance;

export type * from "./libs/lucid_core/pkg/lucid_core.d.ts";
