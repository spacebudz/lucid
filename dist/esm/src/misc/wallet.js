import { C, fromHex, getAddressDetails, toHex, } from "../mod.js";
import { mnemonicToEntropy } from "./bip39.js";
export function walletFromSeed(seed, options = { addressType: "Base", accountIndex: 0, network: "Mainnet" }) {
    function harden(num) {
        if (typeof num !== "number")
            throw new Error("Type number required here!");
        return 0x80000000 + num;
    }
    const entropy = mnemonicToEntropy(seed);
    const rootKey = C.Bip32PrivateKey.from_bip39_entropy(fromHex(entropy), options.password
        ? new TextEncoder().encode(options.password)
        : new Uint8Array());
    const accountKey = rootKey.derive(harden(1852))
        .derive(harden(1815))
        .derive(harden(options.accountIndex));
    const paymentKey = accountKey.derive(0).derive(0).to_raw_key();
    const stakeKey = accountKey.derive(2).derive(0).to_raw_key();
    const paymentKeyHash = paymentKey.to_public().hash();
    const stakeKeyHash = stakeKey.to_public().hash();
    const networkId = options.network === "Mainnet" ? 1 : 0;
    const address = options.addressType === "Base"
        ? C.BaseAddress.new(networkId, C.StakeCredential.from_keyhash(paymentKeyHash), C.StakeCredential.from_keyhash(stakeKeyHash)).to_address().to_bech32(undefined)
        : C.EnterpriseAddress.new(networkId, C.StakeCredential.from_keyhash(paymentKeyHash)).to_address().to_bech32(undefined);
    const rewardAddress = options.addressType === "Base"
        ? C.RewardAddress.new(networkId, C.StakeCredential.from_keyhash(stakeKeyHash)).to_address().to_bech32(undefined)
        : null;
    return {
        address,
        rewardAddress,
        paymentKey: paymentKey.to_bech32(),
        stakeKey: options.addressType === "Base" ? stakeKey.to_bech32() : null,
    };
}
export function discoverOwnUsedTxKeyHashes(tx, ownKeyHashes, ownUtxos) {
    const usedKeyHashes = [];
    // key hashes from inputs
    const inputs = tx.body().inputs();
    for (let i = 0; i < inputs.len(); i++) {
        const input = inputs.get(i);
        const txHash = toHex(input.transaction_id().to_bytes());
        const outputIndex = parseInt(input.index().to_str());
        const utxo = ownUtxos.find((utxo) => utxo.txHash === txHash && utxo.outputIndex === outputIndex);
        if (utxo) {
            const { paymentCredential } = getAddressDetails(utxo.address);
            usedKeyHashes.push(paymentCredential?.hash);
        }
    }
    const txBody = tx.body();
    // key hashes from certificates
    function keyHashFromCert(txBody) {
        const certs = txBody.certs();
        if (!certs)
            return;
        for (let i = 0; i < certs.len(); i++) {
            const cert = certs.get(i);
            if (cert.kind() === 0) {
                const credential = cert.as_stake_registration()?.stake_credential();
                if (credential?.kind() === 0) {
                    // Key hash not needed for registration
                }
            }
            else if (cert.kind() === 1) {
                const credential = cert.as_stake_deregistration()?.stake_credential();
                if (credential?.kind() === 0) {
                    const keyHash = toHex(credential.to_keyhash().to_bytes());
                    usedKeyHashes.push(keyHash);
                }
            }
            else if (cert.kind() === 2) {
                const credential = cert.as_stake_delegation()?.stake_credential();
                if (credential?.kind() === 0) {
                    const keyHash = toHex(credential.to_keyhash().to_bytes());
                    usedKeyHashes.push(keyHash);
                }
            }
            else if (cert.kind() === 3) {
                const poolParams = cert
                    .as_pool_registration()?.pool_params();
                const owners = poolParams
                    ?.pool_owners();
                if (!owners)
                    break;
                for (let i = 0; i < owners.len(); i++) {
                    const keyHash = toHex(owners.get(i).to_bytes());
                    usedKeyHashes.push(keyHash);
                }
                const operator = poolParams.operator().to_hex();
                usedKeyHashes.push(operator);
            }
            else if (cert.kind() === 4) {
                const operator = cert.as_pool_retirement().pool_keyhash().to_hex();
                usedKeyHashes.push(operator);
            }
            else if (cert.kind() === 6) {
                const instantRewards = cert
                    .as_move_instantaneous_rewards_cert()
                    ?.move_instantaneous_reward().as_to_stake_creds()
                    ?.keys();
                if (!instantRewards)
                    break;
                for (let i = 0; i < instantRewards.len(); i++) {
                    const credential = instantRewards.get(i);
                    if (credential.kind() === 0) {
                        const keyHash = toHex(credential.to_keyhash().to_bytes());
                        usedKeyHashes.push(keyHash);
                    }
                }
            }
        }
    }
    if (txBody.certs())
        keyHashFromCert(txBody);
    // key hashes from withdrawals
    const withdrawals = txBody.withdrawals();
    function keyHashFromWithdrawal(withdrawals) {
        const rewardAddresses = withdrawals.keys();
        for (let i = 0; i < rewardAddresses.len(); i++) {
            const credential = rewardAddresses.get(i).payment_cred();
            if (credential.kind() === 0) {
                usedKeyHashes.push(credential.to_keyhash().to_hex());
            }
        }
    }
    if (withdrawals)
        keyHashFromWithdrawal(withdrawals);
    // key hashes from scripts
    const scripts = tx.witness_set().native_scripts();
    function keyHashFromScript(scripts) {
        for (let i = 0; i < scripts.len(); i++) {
            const script = scripts.get(i);
            if (script.kind() === 0) {
                const keyHash = toHex(script.as_script_pubkey().addr_keyhash().to_bytes());
                usedKeyHashes.push(keyHash);
            }
            if (script.kind() === 1) {
                keyHashFromScript(script.as_script_all().native_scripts());
                return;
            }
            if (script.kind() === 2) {
                keyHashFromScript(script.as_script_any().native_scripts());
                return;
            }
            if (script.kind() === 3) {
                keyHashFromScript(script.as_script_n_of_k().native_scripts());
                return;
            }
        }
    }
    if (scripts)
        keyHashFromScript(scripts);
    // keyHashes from required signers
    const requiredSigners = txBody.required_signers();
    if (requiredSigners) {
        for (let i = 0; i < requiredSigners.len(); i++) {
            usedKeyHashes.push(toHex(requiredSigners.get(i).to_bytes()));
        }
    }
    // keyHashes from collateral
    const collateral = txBody.collateral();
    if (collateral) {
        for (let i = 0; i < collateral.len(); i++) {
            const input = collateral.get(i);
            const txHash = toHex(input.transaction_id().to_bytes());
            const outputIndex = parseInt(input.index().to_str());
            const utxo = ownUtxos.find((utxo) => utxo.txHash === txHash && utxo.outputIndex === outputIndex);
            if (utxo) {
                const { paymentCredential } = getAddressDetails(utxo.address);
                usedKeyHashes.push(paymentCredential?.hash);
            }
        }
    }
    return usedKeyHashes.filter((k) => ownKeyHashes.includes(k));
}
