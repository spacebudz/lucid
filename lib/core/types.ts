import type {
  Credential,
  DatumVariant,
  DRep,
  Instruction,
  InstructionSigner,
  Network,
  RelevantProtocolParameters,
  Script,
  Utxo,
} from "../../rs_lib/pkg/lucid_core.d.ts";

type OmitInstruction<T extends Instruction, K extends T["type"]> = T extends
  { type: K } ? never : T;

export type PartialInstruction =
  | {
    type: "CollectFrom";
    utxos: OutRef[];
    redeemer?: string;
  }
  | {
    type: "ReadFrom";
    utxos: OutRef[];
  }
  | {
    type: "Withdraw";
    withdrawal: { rewardAddress: string; amount?: number };
    redeemer?: string;
  }
  | OmitInstruction<Instruction, "CollectFrom">
    & OmitInstruction<Instruction, "ReadFrom">
    & OmitInstruction<Instruction, "Withdraw">;

export type ActiveDelegation = {
  poolId: string | null;
  drep: DRep | null;
  rewards: bigint;
};

export interface Provider {
  network?: Network;
  getProtocolParameters(): Promise<RelevantProtocolParameters>;
  getUtxos(addressOrCredential: string | Credential): Promise<Utxo[]>;
  getUtxosWithUnit(
    addressOrCredential: string | Credential,
    unit: string,
  ): Promise<Utxo[]>;
  getUtxoByUnit(unit: string): Promise<Utxo>;
  getUtxosByOutRef(outRefs: Array<OutRef>): Promise<Utxo[]>;
  getDelegation(rewardAddress: string): Promise<ActiveDelegation>;
  getDatum(datumHash: string): Promise<string>;
  awaitTx(txHash: string, checkInterval?: number): Promise<boolean>;
  submit(tx: string): Promise<string>;
}

/**
 * **Hash** adds only the datum hash to the output.
 *
 * **AsHash** hashes the datum and adds the datum hash to the output and the datum to the witness set.
 *
 * **Inline** adds the datum to the output.
 *
 * **scriptRef** will add any script to the output.
 */
export type OutputData =
  | DatumVariant & {
    scriptRef?: Script;
  }
  | { scriptRef: Script };

export type OutRef = { txHash: string; outputIndex: number };

export interface ReadOnlyWallet {
  address: string;
  rewardAddress?: string;
  utxos?: Utxo[];
}

export type SignedMessage = { signature: string; key: string };

export interface Wallet {
  address(): Promise<string>;
  rewardAddress(): Promise<string | null>;
  getUtxos(): Promise<Utxo[]>;
  getDelegation(): Promise<ActiveDelegation>;
  sign(instructionSigner: InstructionSigner): Promise<string>;
  signMessage(
    address: string,
    payload: string,
  ): Promise<SignedMessage>;
  submit(tx: string): Promise<string>;
}

export type WalletSelection =
  | { PrivateKey: string }
  | { Api: WalletApi }
  | {
    Seed: {
      seed: string;
      options?: { addressType?: "Base" | "Enterprise"; index?: number };
    };
  }
  | {
    ReadOnly: ReadOnlyWallet;
  };

// deno-lint-ignore no-explicit-any
export type Json = any;

export type Exact<T> = T extends infer U ? U : never;

export type Metadata = {
  222: {
    name: string;
    image: string;
    mediaType?: string;
    description?: string;
    files?: {
      name?: string;
      mediaType: string;
      src: string;
    }[];
    [key: string]: Json;
  };
  333: {
    name: string;
    description: string;
    ticker?: string;
    url?: string;
    logo?: string;
    decimals?: number;
    [key: string]: Json;
  };
  444: Metadata["222"] & { decimals?: number };
};

// CIP-0030
export type WalletApi = {
  getNetworkId(): Promise<number>;
  getUtxos(): Promise<string[] | undefined>;
  getBalance(): Promise<string>;
  getUsedAddresses(): Promise<string[]>;
  getUnusedAddresses(): Promise<string[]>;
  getChangeAddress(): Promise<string>;
  getRewardAddresses(): Promise<string[]>;
  signTx(tx: string, partialSign: boolean): Promise<string>;
  signData(
    address: string,
    payload: string,
  ): Promise<{ signature: string; key: string }>;
  submitTx(tx: string): Promise<string>;
  getCollateral(): Promise<string[]>;
  experimental: {
    getCollateral(): Promise<string[]>;
    on(eventName: string, callback: (...args: unknown[]) => void): void;
    off(eventName: string, callback: (...args: unknown[]) => void): void;
  };
};

export type Cardano = {
  [key: string]: {
    name: string;
    icon: string;
    apiVersion: string;
    enable(): Promise<WalletApi>;
    isEnabled(): Promise<boolean>;
  };
};
