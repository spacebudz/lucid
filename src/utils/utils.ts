import { C } from '../core';
import Core from 'core/types';
import {
  Address,
  AddressDetails,
  Assets,
  Credential,
  Datum,
  DatumHash,
  KeyHash,
  MintingPolicy,
  Network,
  PolicyId,
  PrivateKey,
  ScriptHash,
  Slot,
  SpendingValidator,
  UnixTime,
  UTxO,
  Validator,
} from '../types';
import { Lucid } from '..';

export class Utils {
  private lucid: Lucid;
  constructor(lucid: Lucid) {
    this.lucid = lucid;
  }

  validatorToAddress(
    validator: SpendingValidator,
    stakeCredential?: Credential
  ): Address {
    const validatorHash = this.validatorToScriptHash(validator);
    if (stakeCredential) {
      return C.BaseAddress.new(
        networkToId(this.lucid.network),
        C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(validatorHash)),
        stakeCredential.type === 'Key'
          ? C.StakeCredential.from_keyhash(
              C.Ed25519KeyHash.from_hex(stakeCredential.hash)
            )
          : C.StakeCredential.from_scripthash(
              C.ScriptHash.from_hex(stakeCredential.hash)
            )
      )
        .to_address()
        .to_bech32();
    } else {
      return C.EnterpriseAddress.new(
        networkToId(this.lucid.network),
        C.StakeCredential.from_scripthash(C.ScriptHash.from_hex(validatorHash))
      )
        .to_address()
        .to_bech32();
    }
  }

  validatorToScriptHash(validator: Validator): ScriptHash {
    if (validator.type === 'Native') {
      return C.NativeScript.from_bytes(fromHex(validator.script))
        .hash(C.ScriptHashNamespace.NativeScript)
        .to_hex();
    } else if (validator.type === 'PlutusV1') {
      return C.PlutusScript.from_bytes(fromHex(validator.script))
        .hash(C.ScriptHashNamespace.PlutusV1)
        .to_hex();
    } else if (validator.type === 'PlutusV2') {
      C.PlutusScript.from_bytes(fromHex(validator.script))
        .hash(C.ScriptHashNamespace.PlutusV2)
        .to_hex();
    }
    throw new Error('No variant matched');
  }

  mintingPolicyToId: (mp: MintingPolicy) => PolicyId = this.validatorToScriptHash;

  datumToHash(data: Datum): DatumHash {
    return C.hash_plutus_data(C.PlutusData.from_bytes(fromHex(data))).to_hex();
  }

  scriptHashToCredential(scriptHash: ScriptHash): Credential {
    return {
      type: 'Script',
      hash: scriptHash,
    };
  }

  keyHashToCredential(keyHash: KeyHash): Credential {
    return {
      type: 'Key',
      hash: keyHash,
    };
  }

  generatePrivateKey(): PrivateKey {
    return C.PrivateKey.generate_ed25519().to_bech32();
  }

  unixTimeToSlot(unixTime: UnixTime): Slot {
    return this.lucid.network === 'Mainnet'
      ? unixTimeToSlot(unixTime)
      : unixTimeToSlotTestnet(unixTime);
  }

  slotToUnixTime(slot: Slot): UnixTime {
    return this.lucid.network === 'Mainnet'
      ? slotToUnixTime(slot)
      : slotToUnixTimeTestnet(slot);
  }

  /** Address can be in bech32 or hex */
  getAddressDetails(address: string): AddressDetails {
    /* eslint no-empty: ["error", { "allowEmptyCatch": true }] */
    // Base Address
    try {
      const parsedAddress = C.BaseAddress.from_address(
        C.Address.from_bytes(fromHex(address))
      )!;
      const paymentCredential: Credential =
        parsedAddress.payment_cred().kind() === 0
          ? {
              type: 'Key',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_keyhash()!
                  .to_bytes()
              ),
            }
          : {
              type: 'Script',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_scripthash()!
                  .to_bytes()
              ),
            };
      const stakeCredential: Credential =
        parsedAddress.stake_cred().kind() === 0
          ? {
              type: 'Key',
              hash: toHex(
                parsedAddress
                  .stake_cred()
                  .to_keyhash()!
                  .to_bytes()
              ),
            }
          : {
              type: 'Script',
              hash: toHex(
                parsedAddress
                  .stake_cred()
                  .to_scripthash()!
                  .to_bytes()
              ),
            };
      return {
        address: {
          type: 'Base',
          address: parsedAddress.to_address().to_bech32(),
        },
        paymentCredential,
        stakeCredential,
      };
    } catch (e) {}
    try {
      const parsedAddress = C.BaseAddress.from_address(
        C.Address.from_bech32(address)
      )!;
      const paymentCredential: Credential =
        parsedAddress.payment_cred().kind() === 0
          ? {
              type: 'Key',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_keyhash()!
                  .to_bytes()
              ),
            }
          : {
              type: 'Script',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_scripthash()!
                  .to_bytes()
              ),
            };
      const stakeCredential: Credential =
        parsedAddress.stake_cred().kind() === 0
          ? {
              type: 'Key',
              hash: toHex(
                parsedAddress
                  .stake_cred()
                  .to_keyhash()!
                  .to_bytes()
              ),
            }
          : {
              type: 'Script',
              hash: toHex(
                parsedAddress
                  .stake_cred()
                  .to_scripthash()!
                  .to_bytes()
              ),
            };
      return {
        address: {
          type: 'Base',
          address: parsedAddress.to_address().to_bech32(),
        },
        paymentCredential,
        stakeCredential,
      };
    } catch (e) {}

    // Enterprise Address
    try {
      const parsedAddress = C.EnterpriseAddress.from_address(
        C.Address.from_bytes(fromHex(address))
      )!;
      const paymentCredential: Credential =
        parsedAddress.payment_cred().kind() === 0
          ? {
              type: 'Key',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_keyhash()!
                  .to_bytes()
              ),
            }
          : {
              type: 'Script',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_scripthash()!
                  .to_bytes()
              ),
            };
      return {
        address: {
          type: 'Enterprise',
          address: parsedAddress.to_address().to_bech32(),
        },
        paymentCredential,
      };
    } catch (e) {}

    try {
      const parsedAddress = C.EnterpriseAddress.from_address(
        C.Address.from_bech32(address)
      )!;
      const paymentCredential: Credential =
        parsedAddress.payment_cred().kind() === 0
          ? {
              type: 'Key',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_keyhash()!
                  .to_bytes()
              ),
            }
          : {
              type: 'Script',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_scripthash()!
                  .to_bytes()
              ),
            };
      return {
        address: {
          type: 'Enterprise',
          address: parsedAddress.to_address().to_bech32(),
        },
        paymentCredential,
      };
    } catch (e) {}

    // Pointer Address
    try {
      const parsedAddress = C.PointerAddress.from_address(
        C.Address.from_bytes(fromHex(address))
      )!;
      const paymentCredential: Credential =
        parsedAddress.payment_cred().kind() === 0
          ? {
              type: 'Key',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_keyhash()!
                  .to_bytes()
              ),
            }
          : {
              type: 'Script',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_scripthash()!
                  .to_bytes()
              ),
            };
      return {
        address: {
          type: 'Pointer',
          address: parsedAddress.to_address().to_bech32(),
        },
        paymentCredential,
      };
    } catch (e) {}

    try {
      const parsedAddress = C.PointerAddress.from_address(
        C.Address.from_bech32(address)
      )!;
      const paymentCredential: Credential =
        parsedAddress.payment_cred().kind() === 0
          ? {
              type: 'Key',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_keyhash()!
                  .to_bytes()
              ),
            }
          : {
              type: 'Script',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_scripthash()!
                  .to_bytes()
              ),
            };
      return {
        address: {
          type: 'Pointer',
          address: parsedAddress.to_address().to_bech32(),
        },
        paymentCredential,
      };
    } catch (e) {}

    // Reward Address
    try {
      const parsedAddress = C.RewardAddress.from_address(
        C.Address.from_bytes(fromHex(address))
      )!;
      const stakeCredential: Credential =
        parsedAddress.payment_cred().kind() === 0
          ? {
              type: 'Key',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_keyhash()!
                  .to_bytes()
              ),
            }
          : {
              type: 'Script',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_scripthash()!
                  .to_bytes()
              ),
            };
      return {
        address: {
          type: 'Reward',
          address: parsedAddress.to_address().to_bech32(),
        },
        stakeCredential,
      };
    } catch (e) {}

    try {
      const parsedAddress = C.RewardAddress.from_address(
        C.Address.from_bech32(address)
      )!;
      const stakeCredential: Credential =
        parsedAddress.payment_cred().kind() === 0
          ? {
              type: 'Key',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_keyhash()!
                  .to_bytes()
              ),
            }
          : {
              type: 'Script',
              hash: toHex(
                parsedAddress
                  .payment_cred()
                  .to_scripthash()!
                  .to_bytes()
              ),
            };
      return {
        address: {
          type: 'Reward',
          address: parsedAddress.to_address().to_bech32(),
        },
        stakeCredential,
      };
    } catch (e) {}
    throw new Error('No address type matched for: ' + address);
  }
}

export const valueToAssets = (value: Core.Value): Assets => {
  const assets: Assets = {};
  assets['lovelace'] = BigInt(value.coin().to_str());
  const ma = value.multiasset();
  if (ma) {
    const multiAssets = ma.keys();
    for (let j = 0; j < multiAssets.len(); j++) {
      const policy = multiAssets.get(j);
      const policyAssets = ma.get(policy)!;
      const assetNames = policyAssets.keys();
      for (let k = 0; k < assetNames.len(); k++) {
        const policyAsset = assetNames.get(k);
        const quantity = policyAssets.get(policyAsset)!;
        const unit = toHex(policy.to_bytes()) + toHex(policyAsset.name());
        assets[unit] = BigInt(quantity.to_str());
      }
    }
  }
  return assets;
};

export const assetsToValue = (assets: Assets) => {
  const multiAsset = C.MultiAsset.new();
  const lovelace = assets['lovelace'];
  const units = Object.keys(assets);
  const policies = Array.from(
    new Set(
      units.filter(unit => unit !== 'lovelace').map(unit => unit.slice(0, 56))
    )
  );
  policies.forEach(policy => {
    const policyUnits = units.filter(unit => unit.slice(0, 56) === policy);
    const assetsValue = C.Assets.new();
    policyUnits.forEach(unit => {
      assetsValue.insert(
        C.AssetName.new(fromHex(unit.slice(56))),
        C.BigNum.from_str(assets[unit].toString())
      );
    });
    multiAsset.insert(C.ScriptHash.from_bytes(fromHex(policy)), assetsValue);
  });
  const value = C.Value.new(
    C.BigNum.from_str(lovelace ? lovelace.toString() : '0')
  );
  if (units.length > 1 || !lovelace) value.set_multiasset(multiAsset);
  return value;
};

export const utxoToCore = (utxo: UTxO): Core.TransactionUnspentOutput => {
  const output = C.TransactionOutput.new(
    C.Address.from_bech32(utxo.address),
    assetsToValue(utxo.assets)
  );
  if (utxo.datumHash) {
    output.set_datum(
      C.Datum.new_data_hash(C.DataHash.from_bytes(fromHex(utxo.datumHash)))
    );
  }
  return C.TransactionUnspentOutput.new(
    C.TransactionInput.new(
      C.TransactionHash.from_bytes(fromHex(utxo.txHash)),
      C.BigNum.from_str(utxo.outputIndex.toString())
    ),
    output
  );
};

export const coreToUtxo = (coreUtxo: Core.TransactionUnspentOutput): UTxO => {
  return {
    txHash: toHex(
      coreUtxo
        .input()
        .transaction_id()
        .to_bytes()
    ),
    outputIndex: parseInt(
      coreUtxo
        .input()
        .index()
        .to_str()
    ),
    assets: valueToAssets(coreUtxo.output().amount()),
    address: coreUtxo
      .output()
      .address()
      .to_bech32(), // TODO add byron address
    datumHash: coreUtxo
      .output()
      ?.datum()
      ?.as_data_hash()
      ?.to_hex(),
  };
};

export const networkToId = (network: Network): number => {
  if (network === 'Testnet') return 0;
  else if (network === 'Mainnet') return 1;
  throw new Error('Network not found');
};

export const fromHex = (hex: string) => Buffer.from(hex, 'hex');

export const toHex = (bytes: Buffer | Uint8Array): string =>
  Buffer.from(bytes).toString('hex');

const unixTimeToSlot = (unixTime: UnixTime): Slot =>
  Math.floor((unixTime - 1596491091000 + 4924800000) / 1000);

const unixTimeToSlotTestnet = (unixTime: UnixTime): Slot =>
  Math.floor((unixTime - 1564431616000 - 29937600000) / 1000);

const slotToUnixTime = (slot: Slot): UnixTime =>
  1596491091000 + (slot * 1000 - 4924800000);

const slotToUnixTimeTestnet = (slot: Slot): UnixTime =>
  1564431616000 + slot * 1000 + 29937600000;
