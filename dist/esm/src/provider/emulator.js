import { C } from "../core/core.js";
import { PROTOCOL_PARAMETERS_DEFAULT } from "../utils/mod.js";
import { coreToUtxo, fromHex, getAddressDetails, toHex, } from "../utils/utils.js";
export class Emulator {
    constructor(accounts, protocolParameters = PROTOCOL_PARAMETERS_DEFAULT) {
        Object.defineProperty(this, "ledger", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "mempool", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: {}
        });
        /**
         * Only stake key registrations/delegations and rewards are tracked.
         * Other certificates are not tracked.
         */
        Object.defineProperty(this, "chain", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: {}
        });
        Object.defineProperty(this, "blockHeight", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "slot", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "time", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "protocolParameters", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "datumTable", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: {}
        });
        const GENESIS_HASH = "00".repeat(32);
        this.blockHeight = 0;
        this.slot = 0;
        this.time = Date.now();
        this.ledger = {};
        accounts.forEach(({ address, assets }, index) => {
            this.ledger[GENESIS_HASH + index] = {
                utxo: {
                    txHash: GENESIS_HASH,
                    outputIndex: index,
                    address,
                    assets,
                },
                spent: false,
            };
        });
        this.protocolParameters = protocolParameters;
    }
    now() {
        return this.time;
    }
    awaitSlot(length = 1) {
        this.slot += length;
        this.time += length * 1000;
        const currentHeight = this.blockHeight;
        this.blockHeight = Math.floor(this.slot / 20);
        if (this.blockHeight > currentHeight) {
            for (const [outRef, { utxo, spent }] of Object.entries(this.mempool)) {
                this.ledger[outRef] = { utxo, spent };
            }
            for (const [outRef, { spent }] of Object.entries(this.ledger)) {
                if (spent)
                    delete this.ledger[outRef];
            }
            this.mempool = {};
        }
    }
    awaitBlock(height = 1) {
        this.blockHeight += height;
        this.slot += height * 20;
        this.time += height * 20 * 1000;
        for (const [outRef, { utxo, spent }] of Object.entries(this.mempool)) {
            this.ledger[outRef] = { utxo, spent };
        }
        for (const [outRef, { spent }] of Object.entries(this.ledger)) {
            if (spent)
                delete this.ledger[outRef];
        }
        this.mempool = {};
    }
    getUtxos(addressOrCredential) {
        const utxos = Object.values(this.ledger).flatMap(({ utxo }) => {
            if (typeof addressOrCredential === "string") {
                return addressOrCredential === utxo.address ? utxo : [];
            }
            else {
                const { paymentCredential } = getAddressDetails(utxo.address);
                return paymentCredential?.hash === addressOrCredential.hash ? utxo : [];
            }
        });
        return Promise.resolve(utxos);
    }
    getProtocolParameters() {
        return Promise.resolve(this.protocolParameters);
    }
    getDatum(datumHash) {
        return Promise.resolve(this.datumTable[datumHash]);
    }
    getUtxosWithUnit(addressOrCredential, unit) {
        const utxos = Object.values(this.ledger).flatMap(({ utxo }) => {
            if (typeof addressOrCredential === "string") {
                return addressOrCredential === utxo.address && utxo.assets[unit] > 0n
                    ? utxo
                    : [];
            }
            else {
                const { paymentCredential } = getAddressDetails(utxo.address);
                return paymentCredential?.hash === addressOrCredential.hash &&
                    utxo.assets[unit] > 0n
                    ? utxo
                    : [];
            }
        });
        return Promise.resolve(utxos);
    }
    getUtxosByOutRef(outRefs) {
        return Promise.resolve(outRefs.flatMap((outRef) => this.ledger[outRef.txHash + outRef.outputIndex]?.utxo || []));
    }
    getUtxoByUnit(unit) {
        const utxos = Object.values(this.ledger).flatMap(({ utxo }) => utxo.assets[unit] > 0n ? utxo : []);
        if (utxos.length > 1) {
            throw new Error("Unit needs to be an NFT or only held by one address.");
        }
        return Promise.resolve(utxos[0]);
    }
    getDelegation(rewardAddress) {
        return Promise.resolve({
            poolId: this.chain[rewardAddress]?.delegation?.poolId || null,
            rewards: this.chain[rewardAddress]?.delegation?.rewards || 0n,
        });
    }
    awaitTx(txHash) {
        if (this.mempool[txHash + 0]) {
            this.awaitBlock();
            return Promise.resolve(true);
        }
        return Promise.resolve(true);
    }
    /**
     * Emulates the behaviour of the reward distribution at epoch boundaries.
     * Stake keys need to be registered and delegated like on a real chain in order to receive rewards.
     */
    distributeRewards(rewards) {
        for (const [rewardAddress, { registeredStake, delegation }] of Object.entries(this.chain)) {
            if (registeredStake && delegation.poolId) {
                this.chain[rewardAddress] = {
                    registeredStake,
                    delegation: {
                        poolId: delegation.poolId,
                        rewards: delegation.rewards += rewards,
                    },
                };
            }
        }
        this.awaitBlock();
    }
    submitTx(tx) {
        /*
            Checks that are already handled by the transaction builder:
              - Fee calculation
              - Phase 2 evaluation
              - Input value == Output value (including mint value)
              - Min ada requirement
              - Stake key registration deposit amount
              - Collateral
    
            Checks that need to be done:
              - Verify witnesses
              - Correct count of scripts and vkeys
              - Stake key registration
              - Withdrawals
              - Validity interval
         */
        const desTx = C.Transaction.from_bytes(fromHex(tx));
        const body = desTx.body();
        const witnesses = desTx.witness_set();
        const datums = witnesses.plutus_data();
        const txHash = C.hash_transaction(body).to_hex();
        // Validity interval
        // Lower bound is inclusive?
        // Upper bound is inclusive?
        const lowerBound = body.validity_start_interval()
            ? parseInt(body.validity_start_interval().to_str())
            : null;
        const upperBound = body.ttl() ? parseInt(body.ttl().to_str()) : null;
        if (Number.isInteger(lowerBound) && this.slot < lowerBound) {
            throw new Error(`Lower bound (${lowerBound}) not in slot range (${this.slot}).`);
        }
        if (Number.isInteger(upperBound) && this.slot > upperBound) {
            throw new Error(`Upper bound (${upperBound}) not in slot range (${this.slot}).`);
        }
        // Datums in witness set
        const datumTable = (() => {
            const table = {};
            for (let i = 0; i < (datums?.len() || 0); i++) {
                const datum = datums.get(i);
                const datumHash = C.hash_plutus_data(datum).to_hex();
                table[datumHash] = toHex(datum.to_bytes());
            }
            return table;
        })();
        const consumedHashes = new Set();
        // Witness keys
        const keyHashes = (() => {
            const keyHashes = [];
            for (let i = 0; i < (witnesses.vkeys()?.len() || 0); i++) {
                const witness = witnesses.vkeys().get(i);
                const publicKey = witness.vkey().public_key();
                const keyHash = publicKey.hash().to_hex();
                if (!publicKey.verify(fromHex(txHash), witness.signature())) {
                    throw new Error(`Invalid vkey witness. Key hash: ${keyHash}`);
                }
                keyHashes.push(keyHash);
            }
            return keyHashes;
        })();
        // We only need this to verify native scripts. The check happens in the CML.
        const edKeyHashes = C.Ed25519KeyHashes.new();
        keyHashes.forEach((keyHash) => edKeyHashes.add(C.Ed25519KeyHash.from_hex(keyHash)));
        const nativeHashes = (() => {
            const scriptHashes = [];
            for (let i = 0; i < (witnesses.native_scripts()?.len() || 0); i++) {
                const witness = witnesses.native_scripts().get(i);
                const scriptHash = witness.hash(C.ScriptHashNamespace.NativeScript)
                    .to_hex();
                if (!witness.verify(Number.isInteger(lowerBound)
                    ? C.BigNum.from_str(lowerBound.toString())
                    : undefined, Number.isInteger(upperBound)
                    ? C.BigNum.from_str(upperBound.toString())
                    : undefined, edKeyHashes)) {
                    throw new Error(`Invalid native script witness. Script hash: ${scriptHash}`);
                }
                for (let i = 0; i < witness.get_required_signers().len(); i++) {
                    const keyHash = witness.get_required_signers().get(i).to_hex();
                    consumedHashes.add(keyHash);
                }
                scriptHashes.push(scriptHash);
            }
            return scriptHashes;
        })();
        const nativeHashesOptional = {};
        const plutusHashesOptional = [];
        const plutusHashes = (() => {
            const scriptHashes = [];
            for (let i = 0; i < (witnesses.plutus_scripts()?.len() || 0); i++) {
                const script = witnesses.plutus_scripts().get(i);
                const scriptHash = script.hash(C.ScriptHashNamespace.PlutusV1)
                    .to_hex();
                scriptHashes.push(scriptHash);
            }
            for (let i = 0; i < (witnesses.plutus_v2_scripts()?.len() || 0); i++) {
                const script = witnesses.plutus_v2_scripts().get(i);
                const scriptHash = script.hash(C.ScriptHashNamespace.PlutusV2)
                    .to_hex();
                scriptHashes.push(scriptHash);
            }
            return scriptHashes;
        })();
        const inputs = body.inputs();
        inputs.sort();
        const resolvedInputs = [];
        // Check existence of inputs and look for script refs.
        for (let i = 0; i < inputs.len(); i++) {
            const input = inputs.get(i);
            const outRef = input.transaction_id().to_hex() + input.index().to_str();
            const entryLedger = this.ledger[outRef];
            const { entry, type } = !entryLedger
                ? { entry: this.mempool[outRef], type: "Mempool" }
                : { entry: entryLedger, type: "Ledger" };
            if (!entry || entry.spent) {
                throw new Error(`Could not spend UTxO: ${JSON.stringify({
                    txHash: entry?.utxo.txHash,
                    outputIndex: entry?.utxo.outputIndex,
                })}\nIt does not exist or was already spent.`);
            }
            const scriptRef = entry.utxo.scriptRef;
            if (scriptRef) {
                switch (scriptRef.type) {
                    case "Native": {
                        const script = C.NativeScript.from_bytes(fromHex(scriptRef.script));
                        nativeHashesOptional[script.hash(C.ScriptHashNamespace.NativeScript).to_hex()] = script;
                        break;
                    }
                    case "PlutusV1": {
                        const script = C.PlutusScript.from_bytes(fromHex(scriptRef.script));
                        plutusHashesOptional.push(script.hash(C.ScriptHashNamespace.PlutusV1).to_hex());
                        break;
                    }
                    case "PlutusV2": {
                        const script = C.PlutusScript.from_bytes(fromHex(scriptRef.script));
                        plutusHashesOptional.push(script.hash(C.ScriptHashNamespace.PlutusV2).to_hex());
                        break;
                    }
                }
            }
            if (entry.utxo.datumHash)
                consumedHashes.add(entry.utxo.datumHash);
            resolvedInputs.push({ entry, type });
        }
        // Check existence of reference inputs and look for script refs.
        for (let i = 0; i < (body.reference_inputs()?.len() || 0); i++) {
            const input = body.reference_inputs().get(i);
            const outRef = input.transaction_id().to_hex() + input.index().to_str();
            const entry = this.ledger[outRef] || this.mempool[outRef];
            if (!entry || entry.spent) {
                throw new Error(`Could not read UTxO: ${JSON.stringify({
                    txHash: entry?.utxo.txHash,
                    outputIndex: entry?.utxo.outputIndex,
                })}\nIt does not exist or was already spent.`);
            }
            const scriptRef = entry.utxo.scriptRef;
            if (scriptRef) {
                switch (scriptRef.type) {
                    case "Native": {
                        const script = C.NativeScript.from_bytes(fromHex(scriptRef.script));
                        nativeHashesOptional[script.hash(C.ScriptHashNamespace.NativeScript).to_hex()] = script;
                        break;
                    }
                    case "PlutusV1": {
                        const script = C.PlutusScript.from_bytes(fromHex(scriptRef.script));
                        plutusHashesOptional.push(script.hash(C.ScriptHashNamespace.PlutusV1).to_hex());
                        break;
                    }
                    case "PlutusV2": {
                        const script = C.PlutusScript.from_bytes(fromHex(scriptRef.script));
                        plutusHashesOptional.push(script.hash(C.ScriptHashNamespace.PlutusV2).to_hex());
                        break;
                    }
                }
            }
            if (entry.utxo.datumHash)
                consumedHashes.add(entry.utxo.datumHash);
        }
        const redeemers = (() => {
            const tagMap = {
                0: "Spend",
                1: "Mint",
                2: "Cert",
                3: "Reward",
            };
            const collected = [];
            for (let i = 0; i < (witnesses.redeemers()?.len() || 0); i++) {
                const redeemer = witnesses.redeemers().get(i);
                collected.push({
                    tag: tagMap[redeemer.tag().kind()],
                    index: parseInt(redeemer.index().to_str()),
                });
            }
            return collected;
        })();
        function checkAndConsumeHash(credential, tag, index) {
            switch (credential.type) {
                case "Key": {
                    if (!keyHashes.includes(credential.hash)) {
                        throw new Error(`Missing vkey witness. Key hash: ${credential.hash}`);
                    }
                    consumedHashes.add(credential.hash);
                    break;
                }
                case "Script": {
                    if (nativeHashes.includes(credential.hash)) {
                        consumedHashes.add(credential.hash);
                        break;
                    }
                    else if (nativeHashesOptional[credential.hash]) {
                        if (!nativeHashesOptional[credential.hash].verify(Number.isInteger(lowerBound)
                            ? C.BigNum.from_str(lowerBound.toString())
                            : undefined, Number.isInteger(upperBound)
                            ? C.BigNum.from_str(upperBound.toString())
                            : undefined, edKeyHashes)) {
                            throw new Error(`Invalid native script witness. Script hash: ${credential.hash}`);
                        }
                        break;
                    }
                    else if (plutusHashes.includes(credential.hash) ||
                        plutusHashesOptional.includes(credential.hash)) {
                        if (redeemers.find((redeemer) => redeemer.tag === tag && redeemer.index === index)) {
                            consumedHashes.add(credential.hash);
                            break;
                        }
                    }
                    throw new Error(`Missing script witness. Script hash: ${credential.hash}`);
                }
            }
        }
        // Check collateral inputs
        for (let i = 0; i < (body.collateral()?.len() || 0); i++) {
            const input = body.collateral().get(i);
            const outRef = input.transaction_id().to_hex() + input.index().to_str();
            const entry = this.ledger[outRef] || this.mempool[outRef];
            if (!entry || entry.spent) {
                throw new Error(`Could not read UTxO: ${JSON.stringify({
                    txHash: entry?.utxo.txHash,
                    outputIndex: entry?.utxo.outputIndex,
                })}\nIt does not exist or was already spent.`);
            }
            const { paymentCredential } = getAddressDetails(entry.utxo.address);
            if (paymentCredential?.type === "Script") {
                throw new Error("Collateral inputs can only contain vkeys.");
            }
            checkAndConsumeHash(paymentCredential, null, null);
        }
        // Check required signers
        for (let i = 0; i < (body.required_signers()?.len() || 0); i++) {
            const signer = body.required_signers().get(i);
            checkAndConsumeHash({ type: "Key", hash: signer.to_hex() }, null, null);
        }
        // Check mint witnesses
        for (let index = 0; index < (body.mint()?.keys().len() || 0); index++) {
            const policyId = body.mint().keys().get(index).to_hex();
            checkAndConsumeHash({ type: "Script", hash: policyId }, "Mint", index);
        }
        // Check withdrawal witnesses
        const withdrawalRequests = [];
        for (let index = 0; index < (body.withdrawals()?.keys().len() || 0); index++) {
            const rawAddress = body.withdrawals().keys().get(index);
            const withdrawal = BigInt(body.withdrawals().get(rawAddress).to_str());
            const rewardAddress = rawAddress.to_address().to_bech32(undefined);
            const { stakeCredential } = getAddressDetails(rewardAddress);
            checkAndConsumeHash(stakeCredential, "Reward", index);
            if (this.chain[rewardAddress]?.delegation.rewards !== withdrawal) {
                throw new Error("Withdrawal amount doesn't match actual reward balance.");
            }
            withdrawalRequests.push({ rewardAddress, withdrawal });
        }
        // Check cert witnesses
        const certRequests = [];
        for (let index = 0; index < (body.certs()?.len() || 0); index++) {
            /*
              Checking only:
              1. Stake registration
              2. Stake deregistration
              3. Stake delegation
      
              All other certificate types are not checked and considered valid.
            */
            const cert = body.certs().get(index);
            switch (cert.kind()) {
                case 0: {
                    const registration = cert.as_stake_registration();
                    const rewardAddress = C.RewardAddress.new(C.NetworkInfo.testnet().network_id(), registration.stake_credential()).to_address().to_bech32(undefined);
                    if (this.chain[rewardAddress]?.registeredStake) {
                        throw new Error(`Stake key is already registered. Reward address: ${rewardAddress}`);
                    }
                    certRequests.push({ type: "Registration", rewardAddress });
                    break;
                }
                case 1: {
                    const deregistration = cert.as_stake_deregistration();
                    const rewardAddress = C.RewardAddress.new(C.NetworkInfo.testnet().network_id(), deregistration.stake_credential()).to_address().to_bech32(undefined);
                    const { stakeCredential } = getAddressDetails(rewardAddress);
                    checkAndConsumeHash(stakeCredential, "Cert", index);
                    if (!this.chain[rewardAddress]?.registeredStake) {
                        throw new Error(`Stake key is already deregistered. Reward address: ${rewardAddress}`);
                    }
                    certRequests.push({ type: "Deregistration", rewardAddress });
                    break;
                }
                case 2: {
                    const delegation = cert.as_stake_delegation();
                    const rewardAddress = C.RewardAddress.new(C.NetworkInfo.testnet().network_id(), delegation.stake_credential()).to_address().to_bech32(undefined);
                    const poolId = delegation.pool_keyhash().to_bech32("pool");
                    const { stakeCredential } = getAddressDetails(rewardAddress);
                    checkAndConsumeHash(stakeCredential, "Cert", index);
                    if (!this.chain[rewardAddress]?.registeredStake &&
                        !certRequests.find((request) => request.type === "Registration" &&
                            request.rewardAddress === rewardAddress)) {
                        throw new Error(`Stake key is not registered. Reward address: ${rewardAddress}`);
                    }
                    certRequests.push({ type: "Delegation", rewardAddress, poolId });
                    break;
                }
            }
        }
        // Check input witnesses
        resolvedInputs.forEach(({ entry: { utxo } }, index) => {
            const { paymentCredential } = getAddressDetails(utxo.address);
            checkAndConsumeHash(paymentCredential, "Spend", index);
        });
        // Create outputs and consume datum hashes
        const outputs = (() => {
            const collected = [];
            for (let i = 0; i < body.outputs().len(); i++) {
                const output = body.outputs().get(i);
                const unspentOutput = C.TransactionUnspentOutput.new(C.TransactionInput.new(C.TransactionHash.from_hex(txHash), C.BigNum.from_str(i.toString())), output);
                const utxo = coreToUtxo(unspentOutput);
                if (utxo.datumHash)
                    consumedHashes.add(utxo.datumHash);
                collected.push({
                    utxo,
                    spent: false,
                });
            }
            return collected;
        })();
        // Check consumed witnesses
        const [extraKeyHash] = keyHashes.filter((keyHash) => !consumedHashes.has(keyHash));
        if (extraKeyHash) {
            throw new Error(`Extraneous vkey witness. Key hash: ${extraKeyHash}`);
        }
        const [extraNativeHash] = nativeHashes.filter((scriptHash) => !consumedHashes.has(scriptHash));
        if (extraNativeHash) {
            throw new Error(`Extraneous native script. Script hash: ${extraNativeHash}`);
        }
        const [extraPlutusHash] = plutusHashes.filter((scriptHash) => !consumedHashes.has(scriptHash));
        if (extraPlutusHash) {
            throw new Error(`Extraneous plutus script. Script hash: ${extraPlutusHash}`);
        }
        const [extraDatumHash] = Object.keys(datumTable).filter((datumHash) => !consumedHashes.has(datumHash));
        if (extraDatumHash) {
            throw new Error(`Extraneous plutus data. Datum hash: ${extraDatumHash}`);
        }
        // Apply transitions
        resolvedInputs.forEach(({ entry, type }) => {
            const outRef = entry.utxo.txHash + entry.utxo.outputIndex;
            entry.spent = true;
            if (type === "Ledger")
                this.ledger[outRef] = entry;
            else if (type === "Mempool")
                this.mempool[outRef] = entry;
        });
        withdrawalRequests.forEach(({ rewardAddress, withdrawal }) => {
            this.chain[rewardAddress].delegation.rewards -= withdrawal;
        });
        certRequests.forEach(({ type, rewardAddress, poolId }) => {
            switch (type) {
                case "Registration": {
                    if (this.chain[rewardAddress]) {
                        this.chain[rewardAddress].registeredStake = true;
                    }
                    else {
                        this.chain[rewardAddress] = {
                            registeredStake: true,
                            delegation: { poolId: null, rewards: 0n },
                        };
                    }
                    break;
                }
                case "Deregistration": {
                    this.chain[rewardAddress].registeredStake = false;
                    this.chain[rewardAddress].delegation.poolId = null;
                    break;
                }
                case "Delegation": {
                    this.chain[rewardAddress].delegation.poolId = poolId;
                }
            }
        });
        outputs.forEach(({ utxo, spent }) => {
            this.mempool[utxo.txHash + utxo.outputIndex] = {
                utxo,
                spent,
            };
        });
        for (const [datumHash, datum] of Object.entries(datumTable)) {
            this.datumTable[datumHash] = datum;
        }
        return Promise.resolve(txHash);
    }
    log() {
        function getRandomColor(unit) {
            const seed = unit === "lovelace" ? "1" : unit;
            // Convert the seed string to a number
            let num = 0;
            for (let i = 0; i < seed.length; i++) {
                num += seed.charCodeAt(i);
            }
            // Generate a color based on the seed number
            const r = (num * 123) % 256;
            const g = (num * 321) % 256;
            const b = (num * 213) % 256;
            // Return the color as a hex string
            return "#" + ((1 << 24) + (r << 16) + (g << 8) + b).toString(16).slice(1);
        }
        const totalBalances = {};
        const balances = {};
        for (const { utxo } of Object.values(this.ledger)) {
            for (const [unit, quantity] of Object.entries(utxo.assets)) {
                if (!balances[utxo.address]) {
                    balances[utxo.address] = { [unit]: quantity };
                }
                else if (!balances[utxo.address]?.[unit]) {
                    balances[utxo.address][unit] = quantity;
                }
                else {
                    balances[utxo.address][unit] += quantity;
                }
                if (!totalBalances[unit]) {
                    totalBalances[unit] = quantity;
                }
                else {
                    totalBalances[unit] += quantity;
                }
            }
        }
        console.log("\n%cBlockchain state", "color:purple");
        console.log(`
    Block height:   %c${this.blockHeight}%c
    Slot:           %c${this.slot}%c
    Unix time:      %c${this.time}
  `, "color:yellow", "color:white", "color:yellow", "color:white", "color:yellow");
        console.log("\n");
        for (const [address, assets] of Object.entries(balances)) {
            console.log(`Address: %c${address}`, "color:blue", "\n");
            for (const [unit, quantity] of Object.entries(assets)) {
                const barLength = Math.max(Math.floor(60 * (Number(quantity) / Number(totalBalances[unit]))), 1);
                console.log(`%c${"\u2586".repeat(barLength) + " ".repeat(60 - barLength)}`, `color: ${getRandomColor(unit)}`, "", `${unit}:`, quantity, "");
            }
            console.log(`\n${"\u2581".repeat(60)}\n`);
        }
    }
}
