import Core from 'core/types';
import { Lucid } from '../index';
import {
  AddressDetailed,
  Assets,
  CredentialType,
  Slot,
  UnixTime,
  UTxO,
} from '../types';

export const getAddressDetails = (address: string): AddressDetailed => {
  /* eslint no-empty: ["error", { "allowEmptyCatch": true }] */
  // Base Address
  try {
    const parsedAddress = Lucid.C.BaseAddress.from_address(
      Lucid.C.Address.from_bytes(Buffer.from(address, 'hex')),
    );
    const credentialType = getCredentialType(parsedAddress.payment_cred());
    const paymentKeyHash = Buffer.from(
      parsedAddress.payment_cred().kind() === 0
        ? parsedAddress.payment_cred().to_keyhash().to_bytes()
        : parsedAddress.payment_cred().to_scripthash().to_bytes(),
    ).toString('hex');
    const stakeKeyHash = Buffer.from(
      parsedAddress.stake_cred().kind() === 0
        ? parsedAddress.stake_cred().to_keyhash().to_bytes()
        : parsedAddress.stake_cred().to_scripthash().to_bytes(),
    ).toString('hex');
    return {
      type: 'Base',
      credentialType,
      address: parsedAddress.to_address().to_bech32(),
      paymentKeyHash,
      stakeKeyHash,
    };
  } catch (e) {}
  try {
    const parsedAddress = Lucid.C.BaseAddress.from_address(
      Lucid.C.Address.from_bech32(address),
    );
    const credentialType = getCredentialType(parsedAddress.payment_cred());
    const paymentKeyHash = Buffer.from(
      parsedAddress.payment_cred().kind() === 0
        ? parsedAddress.payment_cred().to_keyhash().to_bytes()
        : parsedAddress.payment_cred().to_scripthash().to_bytes(),
    ).toString('hex');
    const stakeKeyHash = Buffer.from(
      parsedAddress.stake_cred().kind() === 0
        ? parsedAddress.stake_cred().to_keyhash().to_bytes()
        : parsedAddress.stake_cred().to_scripthash().to_bytes(),
    ).toString('hex');
    return {
      type: 'Base',
      credentialType,
      address: parsedAddress.to_address().to_bech32(),
      paymentKeyHash,
      stakeKeyHash,
    };
  } catch (e) {}

  // Enterprise Address
  try {
    const parsedAddress = Lucid.C.EnterpriseAddress.from_address(
      Lucid.C.Address.from_bytes(Buffer.from(address, 'hex')),
    );
    const credentialType = getCredentialType(parsedAddress.payment_cred());
    const paymentKeyHash = Buffer.from(
      parsedAddress.payment_cred().kind() === 0
        ? parsedAddress.payment_cred().to_keyhash().to_bytes()
        : parsedAddress.payment_cred().to_scripthash().to_bytes(),
    ).toString('hex');
    return {
      type: 'Enterprise',
      credentialType,
      address: parsedAddress.to_address().to_bech32(),
      paymentKeyHash,
    };
  } catch (e) {}

  try {
    const parsedAddress = Lucid.C.EnterpriseAddress.from_address(
      Lucid.C.Address.from_bech32(address),
    );
    const credentialType = getCredentialType(parsedAddress.payment_cred());
    const paymentKeyHash = Buffer.from(
      parsedAddress.payment_cred().kind() === 0
        ? parsedAddress.payment_cred().to_keyhash().to_bytes()
        : parsedAddress.payment_cred().to_scripthash().to_bytes(),
    ).toString('hex');
    return {
      type: 'Enterprise',
      credentialType,
      address: parsedAddress.to_address().to_bech32(),
      paymentKeyHash,
    };
  } catch (e) {}

  // Pointer Address
  try {
    const parsedAddress = Lucid.C.PointerAddress.from_address(
      Lucid.C.Address.from_bytes(Buffer.from(address, 'hex')),
    );
    const credentialType = getCredentialType(parsedAddress.payment_cred());
    const paymentKeyHash = Buffer.from(
      parsedAddress.payment_cred().kind() === 0
        ? parsedAddress.payment_cred().to_keyhash().to_bytes()
        : parsedAddress.payment_cred().to_scripthash().to_bytes(),
    ).toString('hex');
    return {
      type: 'Pointer',
      credentialType,
      address: parsedAddress.to_address().to_bech32(),
      paymentKeyHash,
    };
  } catch (e) {}

  try {
    const parsedAddress = Lucid.C.PointerAddress.from_address(
      Lucid.C.Address.from_bech32(address),
    );
    const credentialType = getCredentialType(parsedAddress.payment_cred());
    const paymentKeyHash = Buffer.from(
      parsedAddress.payment_cred().kind() === 0
        ? parsedAddress.payment_cred().to_keyhash().to_bytes()
        : parsedAddress.payment_cred().to_scripthash().to_bytes(),
    ).toString('hex');
    return {
      type: 'Pointer',
      credentialType,
      address: parsedAddress.to_address().to_bech32(),
      paymentKeyHash,
    };
  } catch (e) {}

  // Reward Address
  try {
    const parsedAddress = Lucid.C.RewardAddress.from_address(
      Lucid.C.Address.from_bytes(Buffer.from(address, 'hex')),
    );
    const credentialType = getCredentialType(parsedAddress.payment_cred());
    const stakeKeyHash = Buffer.from(
      parsedAddress.payment_cred().kind() === 0
        ? parsedAddress.payment_cred().to_keyhash().to_bytes()
        : parsedAddress.payment_cred().to_scripthash().to_bytes(),
    ).toString('hex');
    return {
      type: 'Reward',
      credentialType,
      address: parsedAddress.to_address().to_bech32(),
      stakeKeyHash,
    };
  } catch (e) {}

  try {
    const parsedAddress = Lucid.C.RewardAddress.from_address(
      Lucid.C.Address.from_bech32(address),
    );
    const credentialType = getCredentialType(parsedAddress.payment_cred());
    const stakeKeyHash = Buffer.from(
      parsedAddress.payment_cred().kind() === 0
        ? parsedAddress.payment_cred().to_keyhash().to_bytes()
        : parsedAddress.payment_cred().to_scripthash().to_bytes(),
    ).toString('hex');
    return {
      type: 'Reward',
      credentialType,
      address: parsedAddress.to_address().to_bech32(),
      stakeKeyHash,
    };
  } catch (e) {}
  throw new Error('No address type matched for: ' + address);
};

const getCredentialType = (
  credential: Core.StakeCredential,
): CredentialType => {
  if (credential.kind() === 0) return 'Key';
  if (credential.kind() === 1) return 'Script';
  return null;
};

export const valueToAssets = (value: Core.Value): Assets => {
  const assets = {};
  assets['lovelace'] = BigInt(value.coin().to_str());
  if (value.multiasset()) {
    const multiAssets = value.multiasset().keys();
    for (let j = 0; j < multiAssets.len(); j++) {
      const policy = multiAssets.get(j);
      const policyAssets = value.multiasset().get(policy);
      const assetNames = policyAssets.keys();
      for (let k = 0; k < assetNames.len(); k++) {
        const policyAsset = assetNames.get(k);
        const quantity = policyAssets.get(policyAsset);
        const unit =
          Buffer.from(policy.to_bytes()).toString('hex') +
          Buffer.from(policyAsset.name()).toString('hex');
        assets[unit] = BigInt(quantity.to_str());
      }
    }
  }
  return assets;
};

export const assetsToValue = (assets: Assets) => {
  const multiAsset = Lucid.C.MultiAsset.new();
  const lovelace = assets['lovelace'];
  const units = Object.keys(assets);
  const policies = [
    ...new Set(
      units
        .filter((unit) => unit !== 'lovelace')
        .map((unit) => unit.slice(0, 56)),
    ),
  ];
  policies.forEach((policy) => {
    const policyUnits = units.filter((unit) => unit.slice(0, 56) === policy);
    const assetsValue = Lucid.C.Assets.new();
    policyUnits.forEach((unit) => {
      assetsValue.insert(
        Lucid.C.AssetName.new(Buffer.from(unit.slice(56), 'hex')),
        Lucid.C.BigNum.from_str(assets[unit].toString()),
      );
    });
    multiAsset.insert(
      Lucid.C.ScriptHash.from_bytes(Buffer.from(policy, 'hex')),
      assetsValue,
    );
  });
  const value = Lucid.C.Value.new(
    Lucid.C.BigNum.from_str(lovelace ? lovelace.toString() : '0'),
  );
  if (units.length > 1 || !lovelace) value.set_multiasset(multiAsset);
  return value;
};

export const utxoToCore = (utxo: UTxO): Core.TransactionUnspentOutput => {
  const output = Lucid.C.TransactionOutput.new(
    Lucid.C.Address.from_bech32(utxo.address),
    assetsToValue(utxo.assets),
  );
  if (utxo.datumHash) {
    output.set_datum(
      Lucid.C.Datum.new_data_hash(
        Lucid.C.DataHash.from_bytes(Buffer.from(utxo.datumHash, 'hex')),
      ),
    );
  }
  return Lucid.C.TransactionUnspentOutput.new(
    Lucid.C.TransactionInput.new(
      Lucid.C.TransactionHash.from_bytes(Buffer.from(utxo.txHash, 'hex')),
      Lucid.C.BigNum.from_str(utxo.outputIndex.toString()),
    ),
    output,
  );
};

export const coreToUtxo = (coreUtxo: Core.TransactionUnspentOutput): UTxO => {
  return {
    txHash: Buffer.from(coreUtxo.input().transaction_id().to_bytes()).toString(
      'hex',
    ),
    outputIndex: parseInt(coreUtxo.input().index().to_str()),
    assets: valueToAssets(coreUtxo.output().amount()),
    address: coreUtxo.output().address().to_bech32(), // TODO add byron address
    datumHash: coreUtxo.output()?.datum()?.as_data_hash()?.to_hex(),
  };
};

export const fromHex = (hex: string) => Buffer.from(hex, 'hex');

export const toHex = (bytes: Buffer | Uint8Array): string =>
  Buffer.from(bytes).toString('hex');

export const unixTimeToSlot = (unixTime: UnixTime): Slot =>
  Math.floor((unixTime - 1596491091000 + 4924800000) / 1000);

export const unixTimeToSlotTestnet = (unixTime: UnixTime): Slot =>
  Math.floor((unixTime - 1564431616000 - 29937600000) / 1000);

export const slotToUnixTime = (slot: Slot): UnixTime =>
  1596491091000 + (slot * 1000 - 4924800000);

export const slotToUnixTimeTestnet = (slot: Slot): UnixTime =>
  1596491091000 + slot * 1000 + 29937600000;
