/**
 * @param {string} password
 * @param {string} salt
 * @param {string} nonce
 * @param {string} data
 * @returns {string}
 */
export function encrypt_with_password(password: string, salt: string, nonce: string, data: string): string;
/**
 * @param {string} password
 * @param {string} data
 * @returns {string}
 */
export function decrypt_with_password(password: string, data: string): string;
/**
 * @param {Transaction} tx
 * @param {LinearFee} linear_fee
 * @param {ExUnitPrices} ex_unit_prices
 * @returns {BigNum}
 */
export function min_fee(tx: Transaction, linear_fee: LinearFee, ex_unit_prices: ExUnitPrices): BigNum;
/**
 * @param {Uint8Array} bytes
 * @returns {TransactionMetadatum}
 */
export function encode_arbitrary_bytes_as_metadatum(bytes: Uint8Array): TransactionMetadatum;
/**
 * @param {TransactionMetadatum} metadata
 * @returns {Uint8Array}
 */
export function decode_arbitrary_bytes_from_metadatum(metadata: TransactionMetadatum): Uint8Array;
/**
 * @param {string} json
 * @param {number} schema
 * @returns {TransactionMetadatum}
 */
export function encode_json_str_to_metadatum(json: string, schema: number): TransactionMetadatum;
/**
 * @param {TransactionMetadatum} metadatum
 * @param {number} schema
 * @returns {string}
 */
export function decode_metadatum_to_json_str(metadatum: TransactionMetadatum, schema: number): string;
/**
 * @param {string} json
 * @param {number} schema
 * @returns {PlutusData}
 */
export function encode_json_str_to_plutus_datum(json: string, schema: number): PlutusData;
/**
 * @param {PlutusData} datum
 * @param {number} schema
 * @returns {string}
 */
export function decode_plutus_datum_to_json_str(datum: PlutusData, schema: number): string;
/**
 * @param {TransactionHash} tx_body_hash
 * @param {ByronAddress} addr
 * @param {LegacyDaedalusPrivateKey} key
 * @returns {BootstrapWitness}
 */
export function make_daedalus_bootstrap_witness(tx_body_hash: TransactionHash, addr: ByronAddress, key: LegacyDaedalusPrivateKey): BootstrapWitness;
/**
 * @param {TransactionHash} tx_body_hash
 * @param {ByronAddress} addr
 * @param {Bip32PrivateKey} key
 * @returns {BootstrapWitness}
 */
export function make_icarus_bootstrap_witness(tx_body_hash: TransactionHash, addr: ByronAddress, key: Bip32PrivateKey): BootstrapWitness;
/**
 * @param {TransactionHash} tx_body_hash
 * @param {PrivateKey} sk
 * @returns {Vkeywitness}
 */
export function make_vkey_witness(tx_body_hash: TransactionHash, sk: PrivateKey): Vkeywitness;
/**
 * @param {AuxiliaryData} auxiliary_data
 * @returns {AuxiliaryDataHash}
 */
export function hash_auxiliary_data(auxiliary_data: AuxiliaryData): AuxiliaryDataHash;
/**
 * @param {TransactionBody} tx_body
 * @returns {TransactionHash}
 */
export function hash_transaction(tx_body: TransactionBody): TransactionHash;
/**
 * @param {PlutusData} plutus_data
 * @returns {DataHash}
 */
export function hash_plutus_data(plutus_data: PlutusData): DataHash;
/**
 * @param {Uint8Array} data
 * @returns {Uint8Array}
 */
export function hash_blake2b256(data: Uint8Array): Uint8Array;
/**
 * @param {Uint8Array} data
 * @returns {Uint8Array}
 */
export function hash_blake2b224(data: Uint8Array): Uint8Array;
/**
 * @param {Redeemers} redeemers
 * @param {Costmdls} cost_models
 * @param {PlutusList | undefined} datums
 * @returns {ScriptDataHash}
 */
export function hash_script_data(redeemers: Redeemers, cost_models: Costmdls, datums: PlutusList | undefined): ScriptDataHash;
/**
 * @param {TransactionBody} txbody
 * @param {BigNum} pool_deposit
 * @param {BigNum} key_deposit
 * @returns {Value}
 */
export function get_implicit_input(txbody: TransactionBody, pool_deposit: BigNum, key_deposit: BigNum): Value;
/**
 * @param {TransactionBody} txbody
 * @param {BigNum} pool_deposit
 * @param {BigNum} key_deposit
 * @returns {BigNum}
 */
export function get_deposit(txbody: TransactionBody, pool_deposit: BigNum, key_deposit: BigNum): BigNum;
/**
 * @param {TransactionOutput} output
 * @param {BigNum} coins_per_utxo_byte
 * @returns {BigNum}
 */
export function min_ada_required(output: TransactionOutput, coins_per_utxo_byte: BigNum): BigNum;
/**
 * Receives a script JSON string
 * and returns a NativeScript.
 * Cardano Wallet and Node styles are supported.
 *
 * * wallet: https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/api/swagger.yaml
 * * node: https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md
 *
 * self_xpub is expected to be a Bip32PublicKey as hex-encoded bytes
 * @param {string} json
 * @param {string} self_xpub
 * @param {number} schema
 * @returns {NativeScript}
 */
export function encode_json_str_to_native_script(json: string, self_xpub: string, schema: number): NativeScript;
/**
 * @param {PlutusList} params
 * @param {PlutusScript} plutus_script
 * @returns {PlutusScript}
 */
export function apply_params_to_plutus_script(params: PlutusList, plutus_script: PlutusScript): PlutusScript;
/**
 * Decompression callback
 *
 * @callback DecompressCallback
 * @param {Uint8Array} compressed
 * @return {Uint8Array} decompressed
 */
/**
 * Options for instantiating a Wasm instance.
 * @typedef {Object} InstantiateOptions
 * @property {URL=} url - Optional url to the Wasm file to instantiate.
 * @property {DecompressCallback=} decompress - Callback to decompress the
 * raw Wasm file bytes before instantiating.
 */
/** Instantiates an instance of the Wasm module returning its functions.
 * @remarks It is safe to call this multiple times and once successfully
 * loaded it will always return a reference to the same object.
 * @param {InstantiateOptions=} opts
 */
export function instantiate(opts?: InstantiateOptions | undefined): Promise<{
    encrypt_with_password: typeof encrypt_with_password;
    decrypt_with_password: typeof decrypt_with_password;
    min_fee: typeof min_fee;
    encode_arbitrary_bytes_as_metadatum: typeof encode_arbitrary_bytes_as_metadatum;
    decode_arbitrary_bytes_from_metadatum: typeof decode_arbitrary_bytes_from_metadatum;
    encode_json_str_to_metadatum: typeof encode_json_str_to_metadatum;
    decode_metadatum_to_json_str: typeof decode_metadatum_to_json_str;
    encode_json_str_to_plutus_datum: typeof encode_json_str_to_plutus_datum;
    decode_plutus_datum_to_json_str: typeof decode_plutus_datum_to_json_str;
    make_daedalus_bootstrap_witness: typeof make_daedalus_bootstrap_witness;
    make_icarus_bootstrap_witness: typeof make_icarus_bootstrap_witness;
    make_vkey_witness: typeof make_vkey_witness;
    hash_auxiliary_data: typeof hash_auxiliary_data;
    hash_transaction: typeof hash_transaction;
    hash_plutus_data: typeof hash_plutus_data;
    hash_blake2b256: typeof hash_blake2b256;
    hash_blake2b224: typeof hash_blake2b224;
    hash_script_data: typeof hash_script_data;
    get_implicit_input: typeof get_implicit_input;
    get_deposit: typeof get_deposit;
    min_ada_required: typeof min_ada_required;
    encode_json_str_to_native_script: typeof encode_json_str_to_native_script;
    apply_params_to_plutus_script: typeof apply_params_to_plutus_script;
    Address: typeof Address;
    Anchor: typeof Anchor;
    AssetName: typeof AssetName;
    AssetNames: typeof AssetNames;
    Assets: typeof Assets;
    AuxiliaryData: typeof AuxiliaryData;
    AuxiliaryDataHash: typeof AuxiliaryDataHash;
    AuxiliaryDataSet: typeof AuxiliaryDataSet;
    BaseAddress: typeof BaseAddress;
    BigInt: typeof BigInt;
    BigNum: typeof BigNum;
    Bip32PrivateKey: typeof Bip32PrivateKey;
    Bip32PublicKey: typeof Bip32PublicKey;
    Block: typeof Block;
    BlockHash: typeof BlockHash;
    Blockfrost: typeof Blockfrost;
    BootstrapWitness: typeof BootstrapWitness;
    BootstrapWitnesses: typeof BootstrapWitnesses;
    ByronAddress: typeof ByronAddress;
    Certificate: typeof Certificate;
    Certificates: typeof Certificates;
    ConstrPlutusData: typeof ConstrPlutusData;
    CostModel: typeof CostModel;
    Costmdls: typeof Costmdls;
    DNSRecordAorAAAA: typeof DNSRecordAorAAAA;
    DNSRecordSRV: typeof DNSRecordSRV;
    Data: typeof Data;
    DataHash: typeof DataHash;
    Datum: typeof Datum;
    Drep: typeof Drep;
    DrepVotingThresholds: typeof DrepVotingThresholds;
    Ed25519KeyHash: typeof Ed25519KeyHash;
    Ed25519KeyHashes: typeof Ed25519KeyHashes;
    Ed25519Signature: typeof Ed25519Signature;
    EnterpriseAddress: typeof EnterpriseAddress;
    ExUnitPrices: typeof ExUnitPrices;
    ExUnits: typeof ExUnits;
    GeneralTransactionMetadata: typeof GeneralTransactionMetadata;
    GenesisDelegateHash: typeof GenesisDelegateHash;
    GenesisHash: typeof GenesisHash;
    GenesisHashes: typeof GenesisHashes;
    GenesisKeyDelegation: typeof GenesisKeyDelegation;
    GovernanceAction: typeof GovernanceAction;
    GovernanceActionId: typeof GovernanceActionId;
    HardForkInitiationAction: typeof HardForkInitiationAction;
    Header: typeof Header;
    HeaderBody: typeof HeaderBody;
    Int: typeof Int;
    Ipv4: typeof Ipv4;
    Ipv6: typeof Ipv6;
    KESSignature: typeof KESSignature;
    KESVKey: typeof KESVKey;
    Language: typeof Language;
    Languages: typeof Languages;
    LegacyDaedalusPrivateKey: typeof LegacyDaedalusPrivateKey;
    LinearFee: typeof LinearFee;
    MIRToStakeCredentials: typeof MIRToStakeCredentials;
    MetadataList: typeof MetadataList;
    MetadataMap: typeof MetadataMap;
    Mint: typeof Mint;
    MintAssets: typeof MintAssets;
    MoveInstantaneousReward: typeof MoveInstantaneousReward;
    MoveInstantaneousRewardsCert: typeof MoveInstantaneousRewardsCert;
    MultiAsset: typeof MultiAsset;
    MultiHostName: typeof MultiHostName;
    NativeScript: typeof NativeScript;
    NativeScripts: typeof NativeScripts;
    NetworkId: typeof NetworkId;
    NetworkInfo: typeof NetworkInfo;
    NewCommittee: typeof NewCommittee;
    NewConstitution: typeof NewConstitution;
    Nonce: typeof Nonce;
    OperationalCert: typeof OperationalCert;
    ParameterChangeAction: typeof ParameterChangeAction;
    PlutusData: typeof PlutusData;
    PlutusList: typeof PlutusList;
    PlutusMap: typeof PlutusMap;
    PlutusScript: typeof PlutusScript;
    PlutusScripts: typeof PlutusScripts;
    PlutusWitness: typeof PlutusWitness;
    Pointer: typeof Pointer;
    PointerAddress: typeof PointerAddress;
    PoolMetadata: typeof PoolMetadata;
    PoolMetadataHash: typeof PoolMetadataHash;
    PoolParams: typeof PoolParams;
    PoolRegistration: typeof PoolRegistration;
    PoolRetirement: typeof PoolRetirement;
    PoolVotingThresholds: typeof PoolVotingThresholds;
    PrivateKey: typeof PrivateKey;
    ProposalProcedure: typeof ProposalProcedure;
    ProposalProcedures: typeof ProposalProcedures;
    ProposedProtocolParameterUpdates: typeof ProposedProtocolParameterUpdates;
    ProtocolParamUpdate: typeof ProtocolParamUpdate;
    ProtocolVersion: typeof ProtocolVersion;
    PublicKey: typeof PublicKey;
    PublicKeys: typeof PublicKeys;
    Redeemer: typeof Redeemer;
    RedeemerTag: typeof RedeemerTag;
    RedeemerWitnessKey: typeof RedeemerWitnessKey;
    Redeemers: typeof Redeemers;
    RegCert: typeof RegCert;
    RegCommitteeHotKeyCert: typeof RegCommitteeHotKeyCert;
    RegDrepCert: typeof RegDrepCert;
    Relay: typeof Relay;
    Relays: typeof Relays;
    RequiredWitnessSet: typeof RequiredWitnessSet;
    RewardAddress: typeof RewardAddress;
    RewardAddresses: typeof RewardAddresses;
    Script: typeof Script;
    ScriptAll: typeof ScriptAll;
    ScriptAny: typeof ScriptAny;
    ScriptDataHash: typeof ScriptDataHash;
    ScriptHash: typeof ScriptHash;
    ScriptHashes: typeof ScriptHashes;
    ScriptNOfK: typeof ScriptNOfK;
    ScriptPubkey: typeof ScriptPubkey;
    ScriptRef: typeof ScriptRef;
    ScriptWitness: typeof ScriptWitness;
    SingleHostAddr: typeof SingleHostAddr;
    SingleHostName: typeof SingleHostName;
    StakeCredential: typeof StakeCredential;
    StakeCredentials: typeof StakeCredentials;
    StakeDelegation: typeof StakeDelegation;
    StakeDeregistration: typeof StakeDeregistration;
    StakeRegDelegCert: typeof StakeRegDelegCert;
    StakeRegistration: typeof StakeRegistration;
    StakeVoteDelegCert: typeof StakeVoteDelegCert;
    StakeVoteRegDelegCert: typeof StakeVoteRegDelegCert;
    Strings: typeof Strings;
    TimelockExpiry: typeof TimelockExpiry;
    TimelockStart: typeof TimelockStart;
    Transaction: typeof Transaction;
    TransactionBodies: typeof TransactionBodies;
    TransactionBody: typeof TransactionBody;
    TransactionBuilder: typeof TransactionBuilder;
    TransactionBuilderConfig: typeof TransactionBuilderConfig;
    TransactionBuilderConfigBuilder: typeof TransactionBuilderConfigBuilder;
    TransactionHash: typeof TransactionHash;
    TransactionIndexes: typeof TransactionIndexes;
    TransactionInput: typeof TransactionInput;
    TransactionInputs: typeof TransactionInputs;
    TransactionMetadatum: typeof TransactionMetadatum;
    TransactionMetadatumLabels: typeof TransactionMetadatumLabels;
    TransactionOutput: typeof TransactionOutput;
    TransactionOutputAmountBuilder: typeof TransactionOutputAmountBuilder;
    TransactionOutputBuilder: typeof TransactionOutputBuilder;
    TransactionOutputs: typeof TransactionOutputs;
    TransactionUnspentOutput: typeof TransactionUnspentOutput;
    TransactionUnspentOutputs: typeof TransactionUnspentOutputs;
    TransactionWitnessSet: typeof TransactionWitnessSet;
    TransactionWitnessSetBuilder: typeof TransactionWitnessSetBuilder;
    TransactionWitnessSets: typeof TransactionWitnessSets;
    TreasuryWithdrawals: typeof TreasuryWithdrawals;
    TreasuryWithdrawalsAction: typeof TreasuryWithdrawalsAction;
    UnitInterval: typeof UnitInterval;
    UnregCert: typeof UnregCert;
    UnregCommitteeHotKeyCert: typeof UnregCommitteeHotKeyCert;
    UnregDrepCert: typeof UnregDrepCert;
    Update: typeof Update;
    Url: typeof Url;
    VRFCert: typeof VRFCert;
    VRFKeyHash: typeof VRFKeyHash;
    VRFVKey: typeof VRFVKey;
    Value: typeof Value;
    Vkey: typeof Vkey;
    Vkeys: typeof Vkeys;
    Vkeywitness: typeof Vkeywitness;
    Vkeywitnesses: typeof Vkeywitnesses;
    Vote: typeof Vote;
    VoteDelegCert: typeof VoteDelegCert;
    VoteRegDelegCert: typeof VoteRegDelegCert;
    Voter: typeof Voter;
    VotingProcedure: typeof VotingProcedure;
    VotingProcedures: typeof VotingProcedures;
    Withdrawals: typeof Withdrawals;
}>;
/** Instantiates an instance of the Wasm module along with its exports.
 * @remarks It is safe to call this multiple times and once successfully
 * loaded it will always return a reference to the same object.
 * @param {InstantiateOptions=} opts
 * @returns {Promise<{
 *   instance: WebAssembly.Instance;
 *   exports: { encrypt_with_password: typeof encrypt_with_password; decrypt_with_password: typeof decrypt_with_password; min_fee: typeof min_fee; encode_arbitrary_bytes_as_metadatum: typeof encode_arbitrary_bytes_as_metadatum; decode_arbitrary_bytes_from_metadatum: typeof decode_arbitrary_bytes_from_metadatum; encode_json_str_to_metadatum: typeof encode_json_str_to_metadatum; decode_metadatum_to_json_str: typeof decode_metadatum_to_json_str; encode_json_str_to_plutus_datum: typeof encode_json_str_to_plutus_datum; decode_plutus_datum_to_json_str: typeof decode_plutus_datum_to_json_str; make_daedalus_bootstrap_witness: typeof make_daedalus_bootstrap_witness; make_icarus_bootstrap_witness: typeof make_icarus_bootstrap_witness; make_vkey_witness: typeof make_vkey_witness; hash_auxiliary_data: typeof hash_auxiliary_data; hash_transaction: typeof hash_transaction; hash_plutus_data: typeof hash_plutus_data; hash_blake2b256: typeof hash_blake2b256; hash_blake2b224: typeof hash_blake2b224; hash_script_data: typeof hash_script_data; get_implicit_input: typeof get_implicit_input; get_deposit: typeof get_deposit; min_ada_required: typeof min_ada_required; encode_json_str_to_native_script: typeof encode_json_str_to_native_script; apply_params_to_plutus_script: typeof apply_params_to_plutus_script; Address : typeof Address ; Anchor : typeof Anchor ; AssetName : typeof AssetName ; AssetNames : typeof AssetNames ; Assets : typeof Assets ; AuxiliaryData : typeof AuxiliaryData ; AuxiliaryDataHash : typeof AuxiliaryDataHash ; AuxiliaryDataSet : typeof AuxiliaryDataSet ; BaseAddress : typeof BaseAddress ; BigInt : typeof BigInt ; BigNum : typeof BigNum ; Bip32PrivateKey : typeof Bip32PrivateKey ; Bip32PublicKey : typeof Bip32PublicKey ; Block : typeof Block ; BlockHash : typeof BlockHash ; Blockfrost : typeof Blockfrost ; BootstrapWitness : typeof BootstrapWitness ; BootstrapWitnesses : typeof BootstrapWitnesses ; ByronAddress : typeof ByronAddress ; Certificate : typeof Certificate ; Certificates : typeof Certificates ; ConstrPlutusData : typeof ConstrPlutusData ; CostModel : typeof CostModel ; Costmdls : typeof Costmdls ; DNSRecordAorAAAA : typeof DNSRecordAorAAAA ; DNSRecordSRV : typeof DNSRecordSRV ; Data : typeof Data ; DataHash : typeof DataHash ; Datum : typeof Datum ; Drep : typeof Drep ; DrepVotingThresholds : typeof DrepVotingThresholds ; Ed25519KeyHash : typeof Ed25519KeyHash ; Ed25519KeyHashes : typeof Ed25519KeyHashes ; Ed25519Signature : typeof Ed25519Signature ; EnterpriseAddress : typeof EnterpriseAddress ; ExUnitPrices : typeof ExUnitPrices ; ExUnits : typeof ExUnits ; GeneralTransactionMetadata : typeof GeneralTransactionMetadata ; GenesisDelegateHash : typeof GenesisDelegateHash ; GenesisHash : typeof GenesisHash ; GenesisHashes : typeof GenesisHashes ; GenesisKeyDelegation : typeof GenesisKeyDelegation ; GovernanceAction : typeof GovernanceAction ; GovernanceActionId : typeof GovernanceActionId ; HardForkInitiationAction : typeof HardForkInitiationAction ; Header : typeof Header ; HeaderBody : typeof HeaderBody ; Int : typeof Int ; Ipv4 : typeof Ipv4 ; Ipv6 : typeof Ipv6 ; KESSignature : typeof KESSignature ; KESVKey : typeof KESVKey ; Language : typeof Language ; Languages : typeof Languages ; LegacyDaedalusPrivateKey : typeof LegacyDaedalusPrivateKey ; LinearFee : typeof LinearFee ; MIRToStakeCredentials : typeof MIRToStakeCredentials ; MetadataList : typeof MetadataList ; MetadataMap : typeof MetadataMap ; Mint : typeof Mint ; MintAssets : typeof MintAssets ; MoveInstantaneousReward : typeof MoveInstantaneousReward ; MoveInstantaneousRewardsCert : typeof MoveInstantaneousRewardsCert ; MultiAsset : typeof MultiAsset ; MultiHostName : typeof MultiHostName ; NativeScript : typeof NativeScript ; NativeScripts : typeof NativeScripts ; NetworkId : typeof NetworkId ; NetworkInfo : typeof NetworkInfo ; NewCommittee : typeof NewCommittee ; NewConstitution : typeof NewConstitution ; Nonce : typeof Nonce ; OperationalCert : typeof OperationalCert ; ParameterChangeAction : typeof ParameterChangeAction ; PlutusData : typeof PlutusData ; PlutusList : typeof PlutusList ; PlutusMap : typeof PlutusMap ; PlutusScript : typeof PlutusScript ; PlutusScripts : typeof PlutusScripts ; PlutusWitness : typeof PlutusWitness ; Pointer : typeof Pointer ; PointerAddress : typeof PointerAddress ; PoolMetadata : typeof PoolMetadata ; PoolMetadataHash : typeof PoolMetadataHash ; PoolParams : typeof PoolParams ; PoolRegistration : typeof PoolRegistration ; PoolRetirement : typeof PoolRetirement ; PoolVotingThresholds : typeof PoolVotingThresholds ; PrivateKey : typeof PrivateKey ; ProposalProcedure : typeof ProposalProcedure ; ProposalProcedures : typeof ProposalProcedures ; ProposedProtocolParameterUpdates : typeof ProposedProtocolParameterUpdates ; ProtocolParamUpdate : typeof ProtocolParamUpdate ; ProtocolVersion : typeof ProtocolVersion ; PublicKey : typeof PublicKey ; PublicKeys : typeof PublicKeys ; Redeemer : typeof Redeemer ; RedeemerTag : typeof RedeemerTag ; RedeemerWitnessKey : typeof RedeemerWitnessKey ; Redeemers : typeof Redeemers ; RegCert : typeof RegCert ; RegCommitteeHotKeyCert : typeof RegCommitteeHotKeyCert ; RegDrepCert : typeof RegDrepCert ; Relay : typeof Relay ; Relays : typeof Relays ; RequiredWitnessSet : typeof RequiredWitnessSet ; RewardAddress : typeof RewardAddress ; RewardAddresses : typeof RewardAddresses ; Script : typeof Script ; ScriptAll : typeof ScriptAll ; ScriptAny : typeof ScriptAny ; ScriptDataHash : typeof ScriptDataHash ; ScriptHash : typeof ScriptHash ; ScriptHashes : typeof ScriptHashes ; ScriptNOfK : typeof ScriptNOfK ; ScriptPubkey : typeof ScriptPubkey ; ScriptRef : typeof ScriptRef ; ScriptWitness : typeof ScriptWitness ; SingleHostAddr : typeof SingleHostAddr ; SingleHostName : typeof SingleHostName ; StakeCredential : typeof StakeCredential ; StakeCredentials : typeof StakeCredentials ; StakeDelegation : typeof StakeDelegation ; StakeDeregistration : typeof StakeDeregistration ; StakeRegDelegCert : typeof StakeRegDelegCert ; StakeRegistration : typeof StakeRegistration ; StakeVoteDelegCert : typeof StakeVoteDelegCert ; StakeVoteRegDelegCert : typeof StakeVoteRegDelegCert ; Strings : typeof Strings ; TimelockExpiry : typeof TimelockExpiry ; TimelockStart : typeof TimelockStart ; Transaction : typeof Transaction ; TransactionBodies : typeof TransactionBodies ; TransactionBody : typeof TransactionBody ; TransactionBuilder : typeof TransactionBuilder ; TransactionBuilderConfig : typeof TransactionBuilderConfig ; TransactionBuilderConfigBuilder : typeof TransactionBuilderConfigBuilder ; TransactionHash : typeof TransactionHash ; TransactionIndexes : typeof TransactionIndexes ; TransactionInput : typeof TransactionInput ; TransactionInputs : typeof TransactionInputs ; TransactionMetadatum : typeof TransactionMetadatum ; TransactionMetadatumLabels : typeof TransactionMetadatumLabels ; TransactionOutput : typeof TransactionOutput ; TransactionOutputAmountBuilder : typeof TransactionOutputAmountBuilder ; TransactionOutputBuilder : typeof TransactionOutputBuilder ; TransactionOutputs : typeof TransactionOutputs ; TransactionUnspentOutput : typeof TransactionUnspentOutput ; TransactionUnspentOutputs : typeof TransactionUnspentOutputs ; TransactionWitnessSet : typeof TransactionWitnessSet ; TransactionWitnessSetBuilder : typeof TransactionWitnessSetBuilder ; TransactionWitnessSets : typeof TransactionWitnessSets ; TreasuryWithdrawals : typeof TreasuryWithdrawals ; TreasuryWithdrawalsAction : typeof TreasuryWithdrawalsAction ; UnitInterval : typeof UnitInterval ; UnregCert : typeof UnregCert ; UnregCommitteeHotKeyCert : typeof UnregCommitteeHotKeyCert ; UnregDrepCert : typeof UnregDrepCert ; Update : typeof Update ; Url : typeof Url ; VRFCert : typeof VRFCert ; VRFKeyHash : typeof VRFKeyHash ; VRFVKey : typeof VRFVKey ; Value : typeof Value ; Vkey : typeof Vkey ; Vkeys : typeof Vkeys ; Vkeywitness : typeof Vkeywitness ; Vkeywitnesses : typeof Vkeywitnesses ; Vote : typeof Vote ; VoteDelegCert : typeof VoteDelegCert ; VoteRegDelegCert : typeof VoteRegDelegCert ; Voter : typeof Voter ; VotingProcedure : typeof VotingProcedure ; VotingProcedures : typeof VotingProcedures ; Withdrawals : typeof Withdrawals  }
 * }>}
 */
export function instantiateWithInstance(opts?: InstantiateOptions | undefined): Promise<{
    instance: WebAssembly.Instance;
    exports: {
        encrypt_with_password: typeof encrypt_with_password;
        decrypt_with_password: typeof decrypt_with_password;
        min_fee: typeof min_fee;
        encode_arbitrary_bytes_as_metadatum: typeof encode_arbitrary_bytes_as_metadatum;
        decode_arbitrary_bytes_from_metadatum: typeof decode_arbitrary_bytes_from_metadatum;
        encode_json_str_to_metadatum: typeof encode_json_str_to_metadatum;
        decode_metadatum_to_json_str: typeof decode_metadatum_to_json_str;
        encode_json_str_to_plutus_datum: typeof encode_json_str_to_plutus_datum;
        decode_plutus_datum_to_json_str: typeof decode_plutus_datum_to_json_str;
        make_daedalus_bootstrap_witness: typeof make_daedalus_bootstrap_witness;
        make_icarus_bootstrap_witness: typeof make_icarus_bootstrap_witness;
        make_vkey_witness: typeof make_vkey_witness;
        hash_auxiliary_data: typeof hash_auxiliary_data;
        hash_transaction: typeof hash_transaction;
        hash_plutus_data: typeof hash_plutus_data;
        hash_blake2b256: typeof hash_blake2b256;
        hash_blake2b224: typeof hash_blake2b224;
        hash_script_data: typeof hash_script_data;
        get_implicit_input: typeof get_implicit_input;
        get_deposit: typeof get_deposit;
        min_ada_required: typeof min_ada_required;
        encode_json_str_to_native_script: typeof encode_json_str_to_native_script;
        apply_params_to_plutus_script: typeof apply_params_to_plutus_script;
        Address: typeof Address;
        Anchor: typeof Anchor;
        AssetName: typeof AssetName;
        AssetNames: typeof AssetNames;
        Assets: typeof Assets;
        AuxiliaryData: typeof AuxiliaryData;
        AuxiliaryDataHash: typeof AuxiliaryDataHash;
        AuxiliaryDataSet: typeof AuxiliaryDataSet;
        BaseAddress: typeof BaseAddress;
        BigInt: typeof BigInt;
        BigNum: typeof BigNum;
        Bip32PrivateKey: typeof Bip32PrivateKey;
        Bip32PublicKey: typeof Bip32PublicKey;
        Block: typeof Block;
        BlockHash: typeof BlockHash;
        Blockfrost: typeof Blockfrost;
        BootstrapWitness: typeof BootstrapWitness;
        BootstrapWitnesses: typeof BootstrapWitnesses;
        ByronAddress: typeof ByronAddress;
        Certificate: typeof Certificate;
        Certificates: typeof Certificates;
        ConstrPlutusData: typeof ConstrPlutusData;
        CostModel: typeof CostModel;
        Costmdls: typeof Costmdls;
        DNSRecordAorAAAA: typeof DNSRecordAorAAAA;
        DNSRecordSRV: typeof DNSRecordSRV;
        Data: typeof Data;
        DataHash: typeof DataHash;
        Datum: typeof Datum;
        Drep: typeof Drep;
        DrepVotingThresholds: typeof DrepVotingThresholds;
        Ed25519KeyHash: typeof Ed25519KeyHash;
        Ed25519KeyHashes: typeof Ed25519KeyHashes;
        Ed25519Signature: typeof Ed25519Signature;
        EnterpriseAddress: typeof EnterpriseAddress;
        ExUnitPrices: typeof ExUnitPrices;
        ExUnits: typeof ExUnits;
        GeneralTransactionMetadata: typeof GeneralTransactionMetadata;
        GenesisDelegateHash: typeof GenesisDelegateHash;
        GenesisHash: typeof GenesisHash;
        GenesisHashes: typeof GenesisHashes;
        GenesisKeyDelegation: typeof GenesisKeyDelegation;
        GovernanceAction: typeof GovernanceAction;
        GovernanceActionId: typeof GovernanceActionId;
        HardForkInitiationAction: typeof HardForkInitiationAction;
        Header: typeof Header;
        HeaderBody: typeof HeaderBody;
        Int: typeof Int;
        Ipv4: typeof Ipv4;
        Ipv6: typeof Ipv6;
        KESSignature: typeof KESSignature;
        KESVKey: typeof KESVKey;
        Language: typeof Language;
        Languages: typeof Languages;
        LegacyDaedalusPrivateKey: typeof LegacyDaedalusPrivateKey;
        LinearFee: typeof LinearFee;
        MIRToStakeCredentials: typeof MIRToStakeCredentials;
        MetadataList: typeof MetadataList;
        MetadataMap: typeof MetadataMap;
        Mint: typeof Mint;
        MintAssets: typeof MintAssets;
        MoveInstantaneousReward: typeof MoveInstantaneousReward;
        MoveInstantaneousRewardsCert: typeof MoveInstantaneousRewardsCert;
        MultiAsset: typeof MultiAsset;
        MultiHostName: typeof MultiHostName;
        NativeScript: typeof NativeScript;
        NativeScripts: typeof NativeScripts;
        NetworkId: typeof NetworkId;
        NetworkInfo: typeof NetworkInfo;
        NewCommittee: typeof NewCommittee;
        NewConstitution: typeof NewConstitution;
        Nonce: typeof Nonce;
        OperationalCert: typeof OperationalCert;
        ParameterChangeAction: typeof ParameterChangeAction;
        PlutusData: typeof PlutusData;
        PlutusList: typeof PlutusList;
        PlutusMap: typeof PlutusMap;
        PlutusScript: typeof PlutusScript;
        PlutusScripts: typeof PlutusScripts;
        PlutusWitness: typeof PlutusWitness;
        Pointer: typeof Pointer;
        PointerAddress: typeof PointerAddress;
        PoolMetadata: typeof PoolMetadata;
        PoolMetadataHash: typeof PoolMetadataHash;
        PoolParams: typeof PoolParams;
        PoolRegistration: typeof PoolRegistration;
        PoolRetirement: typeof PoolRetirement;
        PoolVotingThresholds: typeof PoolVotingThresholds;
        PrivateKey: typeof PrivateKey;
        ProposalProcedure: typeof ProposalProcedure;
        ProposalProcedures: typeof ProposalProcedures;
        ProposedProtocolParameterUpdates: typeof ProposedProtocolParameterUpdates;
        ProtocolParamUpdate: typeof ProtocolParamUpdate;
        ProtocolVersion: typeof ProtocolVersion;
        PublicKey: typeof PublicKey;
        PublicKeys: typeof PublicKeys;
        Redeemer: typeof Redeemer;
        RedeemerTag: typeof RedeemerTag;
        RedeemerWitnessKey: typeof RedeemerWitnessKey;
        Redeemers: typeof Redeemers;
        RegCert: typeof RegCert;
        RegCommitteeHotKeyCert: typeof RegCommitteeHotKeyCert;
        RegDrepCert: typeof RegDrepCert;
        Relay: typeof Relay;
        Relays: typeof Relays;
        RequiredWitnessSet: typeof RequiredWitnessSet;
        RewardAddress: typeof RewardAddress;
        RewardAddresses: typeof RewardAddresses;
        Script: typeof Script;
        ScriptAll: typeof ScriptAll;
        ScriptAny: typeof ScriptAny;
        ScriptDataHash: typeof ScriptDataHash;
        ScriptHash: typeof ScriptHash;
        ScriptHashes: typeof ScriptHashes;
        ScriptNOfK: typeof ScriptNOfK;
        ScriptPubkey: typeof ScriptPubkey;
        ScriptRef: typeof ScriptRef;
        ScriptWitness: typeof ScriptWitness;
        SingleHostAddr: typeof SingleHostAddr;
        SingleHostName: typeof SingleHostName;
        StakeCredential: typeof StakeCredential;
        StakeCredentials: typeof StakeCredentials;
        StakeDelegation: typeof StakeDelegation;
        StakeDeregistration: typeof StakeDeregistration;
        StakeRegDelegCert: typeof StakeRegDelegCert;
        StakeRegistration: typeof StakeRegistration;
        StakeVoteDelegCert: typeof StakeVoteDelegCert;
        StakeVoteRegDelegCert: typeof StakeVoteRegDelegCert;
        Strings: typeof Strings;
        TimelockExpiry: typeof TimelockExpiry;
        TimelockStart: typeof TimelockStart;
        Transaction: typeof Transaction;
        TransactionBodies: typeof TransactionBodies;
        TransactionBody: typeof TransactionBody;
        TransactionBuilder: typeof TransactionBuilder;
        TransactionBuilderConfig: typeof TransactionBuilderConfig;
        TransactionBuilderConfigBuilder: typeof TransactionBuilderConfigBuilder;
        TransactionHash: typeof TransactionHash;
        TransactionIndexes: typeof TransactionIndexes;
        TransactionInput: typeof TransactionInput;
        TransactionInputs: typeof TransactionInputs;
        TransactionMetadatum: typeof TransactionMetadatum;
        TransactionMetadatumLabels: typeof TransactionMetadatumLabels;
        TransactionOutput: typeof TransactionOutput;
        TransactionOutputAmountBuilder: typeof TransactionOutputAmountBuilder;
        TransactionOutputBuilder: typeof TransactionOutputBuilder;
        TransactionOutputs: typeof TransactionOutputs;
        TransactionUnspentOutput: typeof TransactionUnspentOutput;
        TransactionUnspentOutputs: typeof TransactionUnspentOutputs;
        TransactionWitnessSet: typeof TransactionWitnessSet;
        TransactionWitnessSetBuilder: typeof TransactionWitnessSetBuilder;
        TransactionWitnessSets: typeof TransactionWitnessSets;
        TreasuryWithdrawals: typeof TreasuryWithdrawals;
        TreasuryWithdrawalsAction: typeof TreasuryWithdrawalsAction;
        UnitInterval: typeof UnitInterval;
        UnregCert: typeof UnregCert;
        UnregCommitteeHotKeyCert: typeof UnregCommitteeHotKeyCert;
        UnregDrepCert: typeof UnregDrepCert;
        Update: typeof Update;
        Url: typeof Url;
        VRFCert: typeof VRFCert;
        VRFKeyHash: typeof VRFKeyHash;
        VRFVKey: typeof VRFVKey;
        Value: typeof Value;
        Vkey: typeof Vkey;
        Vkeys: typeof Vkeys;
        Vkeywitness: typeof Vkeywitness;
        Vkeywitnesses: typeof Vkeywitnesses;
        Vote: typeof Vote;
        VoteDelegCert: typeof VoteDelegCert;
        VoteRegDelegCert: typeof VoteRegDelegCert;
        Voter: typeof Voter;
        VotingProcedure: typeof VotingProcedure;
        VotingProcedures: typeof VotingProcedures;
        Withdrawals: typeof Withdrawals;
    };
}>;
/** Gets if the Wasm module has been instantiated. */
export function isInstantiated(): boolean;
/** */
export const StakeCredKind: Readonly<{
    Key: 0;
    "0": "Key";
    Script: 1;
    "1": "Script";
}>;
/** */
export const GovernanceActionKind: Readonly<{
    ParameterChangeAction: 0;
    "0": "ParameterChangeAction";
    HardForkInitiationAction: 1;
    "1": "HardForkInitiationAction";
    TreasuryWithdrawalsAction: 2;
    "2": "TreasuryWithdrawalsAction";
    NoConfidence: 3;
    "3": "NoConfidence";
    NewCommittee: 4;
    "4": "NewCommittee";
    NewConstitution: 5;
    "5": "NewConstitution";
    InfoAction: 6;
    "6": "InfoAction";
}>;
/** */
export const VoterKind: Readonly<{
    CommitteeHotKeyHash: 0;
    "0": "CommitteeHotKeyHash";
    CommitteeHotScriptHash: 1;
    "1": "CommitteeHotScriptHash";
    DrepKeyHash: 2;
    "2": "DrepKeyHash";
    DrepScriptHash: 3;
    "3": "DrepScriptHash";
    StakingPoolKeyHash: 4;
    "4": "StakingPoolKeyHash";
}>;
/** */
export const VoteKind: Readonly<{
    No: 0;
    "0": "No";
    Yes: 1;
    "1": "Yes";
    Abstain: 2;
    "2": "Abstain";
}>;
/** */
export const DrepKind: Readonly<{
    KeyHash: 0;
    "0": "KeyHash";
    ScriptHash: 1;
    "1": "ScriptHash";
    Abstain: 2;
    "2": "Abstain";
    NoConfidence: 3;
    "3": "NoConfidence";
}>;
/** */
export const TransactionMetadatumKind: Readonly<{
    MetadataMap: 0;
    "0": "MetadataMap";
    MetadataList: 1;
    "1": "MetadataList";
    Int: 2;
    "2": "Int";
    Bytes: 3;
    "3": "Bytes";
    Text: 4;
    "4": "Text";
}>;
/** */
export const MetadataJsonSchema: Readonly<{
    NoConversions: 0;
    "0": "NoConversions";
    BasicConversions: 1;
    "1": "BasicConversions";
    DetailedSchema: 2;
    "2": "DetailedSchema";
}>;
/** */
export const LanguageKind: Readonly<{
    PlutusV1: 0;
    "0": "PlutusV1";
    PlutusV2: 1;
    "1": "PlutusV2";
    PlutusV3: 2;
    "2": "PlutusV3";
}>;
/** */
export const PlutusDataKind: Readonly<{
    ConstrPlutusData: 0;
    "0": "ConstrPlutusData";
    Map: 1;
    "1": "Map";
    List: 2;
    "2": "List";
    Integer: 3;
    "3": "Integer";
    Bytes: 4;
    "4": "Bytes";
}>;
/** */
export const RedeemerTagKind: Readonly<{
    Spend: 0;
    "0": "Spend";
    Mint: 1;
    "1": "Mint";
    Cert: 2;
    "2": "Cert";
    Reward: 3;
    "3": "Reward";
    Drep: 4;
    "4": "Drep";
}>;
/**
 * JSON <-> PlutusData conversion schemas.
 * Follows ScriptDataJsonSchema in cardano-cli defined at:
 * https://github.com/input-output-hk/cardano-node/blob/master/cardano-api/src/Cardano/Api/ScriptData.hs#L254
 *
 * All methods here have the following restrictions due to limitations on dependencies:
 * * JSON numbers above u64::MAX (positive) or below i64::MIN (negative) will throw errors
 * * Hex strings for bytes don't accept odd-length (half-byte) strings.
 *      cardano-cli seems to support these however but it seems to be different than just 0-padding
 *      on either side when tested so proceed with caution
 */
export const PlutusDatumSchema: Readonly<{
    /**
     * ScriptDataJsonNoSchema in cardano-node.
     *
     * This is the format used by --script-data-value in cardano-cli
     * This tries to accept most JSON but does not support the full spectrum of Plutus datums.
     * From JSON:
     * * null/true/false/floats NOT supported
     * * strings starting with 0x are treated as hex bytes. All other strings are encoded as their utf8 bytes.
     * To JSON:
     * * ConstrPlutusData not supported in ANY FORM (neither keys nor values)
     * * Lists not supported in keys
     * * Maps not supported in keys
     */
    BasicConversions: 0;
    "0": "BasicConversions";
    /**
     * ScriptDataJsonDetailedSchema in cardano-node.
     *
     * This is the format used by --script-data-file in cardano-cli
     * This covers almost all (only minor exceptions) Plutus datums, but the JSON must conform to a strict schema.
     * The schema specifies that ALL keys and ALL values must be contained in a JSON map with 2 cases:
     * 1. For ConstrPlutusData there must be two fields "constructor" contianing a number and "fields" containing its fields
     *    e.g. { "constructor": 2, "fields": [{"int": 2}, {"list": [{"bytes": "CAFEF00D"}]}]}
     * 2. For all other cases there must be only one field named "int", "bytes", "list" or "map"
     *    Integer's value is a JSON number e.g. {"int": 100}
     *    Bytes' value is a hex string representing the bytes WITHOUT any prefix e.g. {"bytes": "CAFEF00D"}
     *    Lists' value is a JSON list of its elements encoded via the same schema e.g. {"list": [{"bytes": "CAFEF00D"}]}
     *    Maps' value is a JSON list of objects, one for each key-value pair in the map, with keys "k" and "v"
     *          respectively with their values being the plutus datum encoded via this same schema
     *          e.g. {"map": [
     *              {"k": {"int": 2}, "v": {"int": 5}},
     *              {"k": {"map": [{"k": {"list": [{"int": 1}]}, "v": {"bytes": "FF03"}}]}, "v": {"list": []}}
     *          ]}
     * From JSON:
     * * null/true/false/floats NOT supported
     * * the JSON must conform to a very specific schema
     * To JSON:
     * * all Plutus datums should be fully supported outside of the integer range limitations outlined above.
     */
    DetailedSchema: 1;
    "1": "DetailedSchema";
}>;
/** */
export const ScriptKind: Readonly<{
    NativeScript: 0;
    "0": "NativeScript";
    PlutusScriptV1: 1;
    "1": "PlutusScriptV1";
    PlutusScriptV2: 2;
    "2": "PlutusScriptV2";
    PlutusScriptV3: 3;
    "3": "PlutusScriptV3";
}>;
/** */
export const DatumKind: Readonly<{
    Hash: 0;
    "0": "Hash";
    Data: 1;
    "1": "Data";
}>;
/**
 * Each new language uses a different namespace for hashing its script
 * This is because you could have a language where the same bytes have different semantics
 * So this avoids scripts in different languages mapping to the same hash
 * Note that the enum value here is different than the enum value for deciding the cost model of a script
 * https://github.com/input-output-hk/cardano-ledger/blob/9c3b4737b13b30f71529e76c5330f403165e28a6/eras/alonzo/impl/src/Cardano/Ledger/Alonzo.hs#L127
 */
export const ScriptHashNamespace: Readonly<{
    NativeScript: 0;
    "0": "NativeScript";
    PlutusV1: 1;
    "1": "PlutusV1";
    PlutusV2: 2;
    "2": "PlutusV2";
}>;
/**
 * Used to choose the schema for a script JSON string
 */
export const ScriptSchema: Readonly<{
    Wallet: 0;
    "0": "Wallet";
    Node: 1;
    "1": "Node";
}>;
/** */
export const ScriptWitnessKind: Readonly<{
    NativeWitness: 0;
    "0": "NativeWitness";
    PlutusWitness: 1;
    "1": "PlutusWitness";
}>;
/** */
export const CertificateKind: Readonly<{
    StakeRegistration: 0;
    "0": "StakeRegistration";
    StakeDeregistration: 1;
    "1": "StakeDeregistration";
    StakeDelegation: 2;
    "2": "StakeDelegation";
    PoolRegistration: 3;
    "3": "PoolRegistration";
    PoolRetirement: 4;
    "4": "PoolRetirement";
    GenesisKeyDelegation: 5;
    "5": "GenesisKeyDelegation";
    MoveInstantaneousRewardsCert: 6;
    "6": "MoveInstantaneousRewardsCert";
    RegCert: 7;
    "7": "RegCert";
    UnregCert: 8;
    "8": "UnregCert";
    VoteDelegCert: 9;
    "9": "VoteDelegCert";
    StakeVoteDelegCert: 10;
    "10": "StakeVoteDelegCert";
    StakeRegDelegCert: 11;
    "11": "StakeRegDelegCert";
    VoteRegDelegCert: 12;
    "12": "VoteRegDelegCert";
    StakeVoteRegDelegCert: 13;
    "13": "StakeVoteRegDelegCert";
    RegCommitteeHotKeyCert: 14;
    "14": "RegCommitteeHotKeyCert";
    UnregCommitteeHotKeyCert: 15;
    "15": "UnregCommitteeHotKeyCert";
    RegDrepCert: 16;
    "16": "RegDrepCert";
    UnregDrepCert: 17;
    "17": "UnregDrepCert";
}>;
/** */
export const MIRPot: Readonly<{
    Reserves: 0;
    "0": "Reserves";
    Treasury: 1;
    "1": "Treasury";
}>;
/** */
export const MIRKind: Readonly<{
    ToOtherPot: 0;
    "0": "ToOtherPot";
    ToStakeCredentials: 1;
    "1": "ToStakeCredentials";
}>;
/** */
export const RelayKind: Readonly<{
    SingleHostAddr: 0;
    "0": "SingleHostAddr";
    SingleHostName: 1;
    "1": "SingleHostName";
    MultiHostName: 2;
    "2": "MultiHostName";
}>;
/** */
export const NativeScriptKind: Readonly<{
    ScriptPubkey: 0;
    "0": "ScriptPubkey";
    ScriptAll: 1;
    "1": "ScriptAll";
    ScriptAny: 2;
    "2": "ScriptAny";
    ScriptNOfK: 3;
    "3": "ScriptNOfK";
    TimelockStart: 4;
    "4": "TimelockStart";
    TimelockExpiry: 5;
    "5": "TimelockExpiry";
}>;
/** */
export const NetworkIdKind: Readonly<{
    Testnet: 0;
    "0": "Testnet";
    Mainnet: 1;
    "1": "Mainnet";
}>;
/** */
export class Address {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} data
     * @returns {Address}
     */
    static from_bytes(data: Uint8Array): Address;
    /**
     * @param {string} json
     * @returns {Address}
     */
    static from_json(json: string): Address;
    /**
     * @param {string} bech_str
     * @returns {Address}
     */
    static from_bech32(bech_str: string): Address;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string | undefined} prefix
     * @returns {string}
     */
    to_bech32(prefix: string | undefined): string;
    /**
     * @returns {number}
     */
    network_id(): number;
    /**
     * @returns {ByronAddress | undefined}
     */
    as_byron(): ByronAddress | undefined;
    /**
     * @returns {RewardAddress | undefined}
     */
    as_reward(): RewardAddress | undefined;
    /**
     * @returns {PointerAddress | undefined}
     */
    as_pointer(): PointerAddress | undefined;
    /**
     * @returns {EnterpriseAddress | undefined}
     */
    as_enterprise(): EnterpriseAddress | undefined;
    /**
     * @returns {BaseAddress | undefined}
     */
    as_base(): BaseAddress | undefined;
}
/** */
export class Anchor {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Anchor}
     */
    static from_bytes(bytes: Uint8Array): Anchor;
    /**
     * @param {string} json
     * @returns {Anchor}
     */
    static from_json(json: string): Anchor;
    /**
     * @param {Url} anchor_url
     * @param {DataHash} anchor_data_hash
     * @returns {Anchor}
     */
    static new(anchor_url: Url, anchor_data_hash: DataHash): Anchor;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Url}
     */
    anchor_url(): Url;
    /**
     * @returns {DataHash}
     */
    anchor_data_hash(): DataHash;
}
/** */
export class AssetName {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {AssetName}
     */
    static from_bytes(bytes: Uint8Array): AssetName;
    /**
     * @param {string} json
     * @returns {AssetName}
     */
    static from_json(json: string): AssetName;
    /**
     * @param {Uint8Array} name
     * @returns {AssetName}
     */
    static new(name: Uint8Array): AssetName;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Uint8Array}
     */
    name(): Uint8Array;
}
/** */
export class AssetNames {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {AssetNames}
     */
    static from_bytes(bytes: Uint8Array): AssetNames;
    /**
     * @param {string} json
     * @returns {AssetNames}
     */
    static from_json(json: string): AssetNames;
    /**
     * @returns {AssetNames}
     */
    static new(): AssetNames;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {AssetName}
     */
    get(index: number): AssetName;
    /**
     * @param {AssetName} elem
     */
    add(elem: AssetName): void;
}
/** */
export class Assets {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Assets}
     */
    static from_bytes(bytes: Uint8Array): Assets;
    /**
     * @param {string} json
     * @returns {Assets}
     */
    static from_json(json: string): Assets;
    /**
     * @returns {Assets}
     */
    static new(): Assets;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {AssetName} key
     * @param {BigNum} value
     * @returns {BigNum | undefined}
     */
    insert(key: AssetName, value: BigNum): BigNum | undefined;
    /**
     * @param {AssetName} key
     * @returns {BigNum | undefined}
     */
    get(key: AssetName): BigNum | undefined;
    /**
     * @returns {AssetNames}
     */
    keys(): AssetNames;
}
/** */
export class AuxiliaryData {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {AuxiliaryData}
     */
    static from_bytes(bytes: Uint8Array): AuxiliaryData;
    /**
     * @param {string} json
     * @returns {AuxiliaryData}
     */
    static from_json(json: string): AuxiliaryData;
    /**
     * @returns {AuxiliaryData}
     */
    static new(): AuxiliaryData;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {GeneralTransactionMetadata | undefined}
     */
    metadata(): GeneralTransactionMetadata | undefined;
    /**
     * @param {GeneralTransactionMetadata} metadata
     */
    set_metadata(metadata: GeneralTransactionMetadata): void;
    /**
     * @returns {NativeScripts | undefined}
     */
    native_scripts(): NativeScripts | undefined;
    /**
     * @param {NativeScripts} native_scripts
     */
    set_native_scripts(native_scripts: NativeScripts): void;
    /**
     * @returns {PlutusScripts | undefined}
     */
    plutus_scripts(): PlutusScripts | undefined;
    /**
     * @returns {PlutusScripts | undefined}
     */
    plutus_v2_scripts(): PlutusScripts | undefined;
    /**
     * @returns {PlutusScripts | undefined}
     */
    plutus_v3_scripts(): PlutusScripts | undefined;
    /**
     * @param {PlutusScripts} plutus_scripts
     */
    set_plutus_scripts(plutus_scripts: PlutusScripts): void;
    /**
     * @param {PlutusScripts} plutus_scripts
     */
    set_plutus_v2_scripts(plutus_scripts: PlutusScripts): void;
    /**
     * @param {PlutusScripts} plutus_scripts
     */
    set_plutus_v3_scripts(plutus_scripts: PlutusScripts): void;
}
/** */
export class AuxiliaryDataHash {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {AuxiliaryDataHash}
     */
    static from_bytes(bytes: Uint8Array): AuxiliaryDataHash;
    /**
     * @param {string} bech_str
     * @returns {AuxiliaryDataHash}
     */
    static from_bech32(bech_str: string): AuxiliaryDataHash;
    /**
     * @param {string} hex
     * @returns {AuxiliaryDataHash}
     */
    static from_hex(hex: string): AuxiliaryDataHash;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string} prefix
     * @returns {string}
     */
    to_bech32(prefix: string): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class AuxiliaryDataSet {
    static __wrap(ptr: any): any;
    /**
     * @returns {AuxiliaryDataSet}
     */
    static new(): AuxiliaryDataSet;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {BigNum} tx_index
     * @param {AuxiliaryData} data
     * @returns {AuxiliaryData | undefined}
     */
    insert(tx_index: BigNum, data: AuxiliaryData): AuxiliaryData | undefined;
    /**
     * @param {BigNum} tx_index
     * @returns {AuxiliaryData | undefined}
     */
    get(tx_index: BigNum): AuxiliaryData | undefined;
    /**
     * @returns {TransactionIndexes}
     */
    indices(): TransactionIndexes;
}
/** */
export class BaseAddress {
    static __wrap(ptr: any): any;
    /**
     * @param {number} network
     * @param {StakeCredential} payment
     * @param {StakeCredential} stake
     * @returns {BaseAddress}
     */
    static new(network: number, payment: StakeCredential, stake: StakeCredential): BaseAddress;
    /**
     * @param {Address} addr
     * @returns {BaseAddress | undefined}
     */
    static from_address(addr: Address): BaseAddress | undefined;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {StakeCredential}
     */
    payment_cred(): StakeCredential;
    /**
     * @returns {StakeCredential}
     */
    stake_cred(): StakeCredential;
    /**
     * @returns {Address}
     */
    to_address(): Address;
}
/** */
export class BigInt {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {BigInt}
     */
    static from_bytes(bytes: Uint8Array): BigInt;
    /**
     * @param {string} text
     * @returns {BigInt}
     */
    static from_str(text: string): BigInt;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {BigNum | undefined}
     */
    as_u64(): BigNum | undefined;
    /**
     * @returns {Int | undefined}
     */
    as_int(): Int | undefined;
    /**
     * @returns {string}
     */
    to_str(): string;
}
/** */
export class BigNum {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {BigNum}
     */
    static from_bytes(bytes: Uint8Array): BigNum;
    /**
     * @param {string} string
     * @returns {BigNum}
     */
    static from_str(string: string): BigNum;
    /**
     * @returns {BigNum}
     */
    static zero(): BigNum;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_str(): string;
    /**
     * @returns {boolean}
     */
    is_zero(): boolean;
    /**
     * @param {BigNum} other
     * @returns {BigNum}
     */
    checked_mul(other: BigNum): BigNum;
    /**
     * @param {BigNum} other
     * @returns {BigNum}
     */
    checked_add(other: BigNum): BigNum;
    /**
     * @param {BigNum} other
     * @returns {BigNum}
     */
    checked_sub(other: BigNum): BigNum;
    /**
     * @param {BigNum} other
     * @returns {BigNum}
     */
    checked_div(other: BigNum): BigNum;
    /**
     * @param {BigNum} other
     * @returns {BigNum}
     */
    checked_div_ceil(other: BigNum): BigNum;
    /**
     * returns 0 if it would otherwise underflow
     * @param {BigNum} other
     * @returns {BigNum}
     */
    clamped_sub(other: BigNum): BigNum;
    /**
     * @param {BigNum} rhs_value
     * @returns {number}
     */
    compare(rhs_value: BigNum): number;
}
/** */
export class Bip32PrivateKey {
    static __wrap(ptr: any): any;
    /**
     * 128-byte xprv a key format in Cardano that some software still uses or requires
     * the traditional 96-byte xprv is simply encoded as
     * prv | chaincode
     * however, because some software may not know how to compute a public key from a private key,
     * the 128-byte inlines the public key in the following format
     * prv | pub | chaincode
     * so be careful if you see the term "xprv" as it could refer to either one
     * our library does not require the pub (instead we compute the pub key when needed)
     * @param {Uint8Array} bytes
     * @returns {Bip32PrivateKey}
     */
    static from_128_xprv(bytes: Uint8Array): Bip32PrivateKey;
    /**
     * @returns {Bip32PrivateKey}
     */
    static generate_ed25519_bip32(): Bip32PrivateKey;
    /**
     * @param {Uint8Array} bytes
     * @returns {Bip32PrivateKey}
     */
    static from_bytes(bytes: Uint8Array): Bip32PrivateKey;
    /**
     * @param {string} bech32_str
     * @returns {Bip32PrivateKey}
     */
    static from_bech32(bech32_str: string): Bip32PrivateKey;
    /**
     * @param {Uint8Array} entropy
     * @param {Uint8Array} password
     * @returns {Bip32PrivateKey}
     */
    static from_bip39_entropy(entropy: Uint8Array, password: Uint8Array): Bip32PrivateKey;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * derive this private key with the given index.
     *
     * # Security considerations
     *
     * * hard derivation index cannot be soft derived with the public key
     *
     * # Hard derivation vs Soft derivation
     *
     * If you pass an index below 0x80000000 then it is a soft derivation.
     * The advantage of soft derivation is that it is possible to derive the
     * public key too. I.e. derivation the private key with a soft derivation
     * index and then retrieving the associated public key is equivalent to
     * deriving the public key associated to the parent private key.
     *
     * Hard derivation index does not allow public key derivation.
     *
     * This is why deriving the private key should not fail while deriving
     * the public key may fail (if the derivation index is invalid).
     * @param {number} index
     * @returns {Bip32PrivateKey}
     */
    derive(index: number): Bip32PrivateKey;
    /**
     * see from_128_xprv
     * @returns {Uint8Array}
     */
    to_128_xprv(): Uint8Array;
    /**
     * @returns {PrivateKey}
     */
    to_raw_key(): PrivateKey;
    /**
     * @returns {Bip32PublicKey}
     */
    to_public(): Bip32PublicKey;
    /**
     * @returns {Uint8Array}
     */
    as_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_bech32(): string;
    /**
     * @returns {Uint8Array}
     */
    chaincode(): Uint8Array;
}
/** */
export class Bip32PublicKey {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Bip32PublicKey}
     */
    static from_bytes(bytes: Uint8Array): Bip32PublicKey;
    /**
     * @param {string} bech32_str
     * @returns {Bip32PublicKey}
     */
    static from_bech32(bech32_str: string): Bip32PublicKey;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * derive this public key with the given index.
     *
     * # Errors
     *
     * If the index is not a soft derivation index (< 0x80000000) then
     * calling this method will fail.
     *
     * # Security considerations
     *
     * * hard derivation index cannot be soft derived with the public key
     *
     * # Hard derivation vs Soft derivation
     *
     * If you pass an index below 0x80000000 then it is a soft derivation.
     * The advantage of soft derivation is that it is possible to derive the
     * public key too. I.e. derivation the private key with a soft derivation
     * index and then retrieving the associated public key is equivalent to
     * deriving the public key associated to the parent private key.
     *
     * Hard derivation index does not allow public key derivation.
     *
     * This is why deriving the private key should not fail while deriving
     * the public key may fail (if the derivation index is invalid).
     * @param {number} index
     * @returns {Bip32PublicKey}
     */
    derive(index: number): Bip32PublicKey;
    /**
     * @returns {PublicKey}
     */
    to_raw_key(): PublicKey;
    /**
     * @returns {Uint8Array}
     */
    as_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_bech32(): string;
    /**
     * @returns {Uint8Array}
     */
    chaincode(): Uint8Array;
}
/** */
export class Block {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Block}
     */
    static from_bytes(bytes: Uint8Array): Block;
    /**
     * @param {string} json
     * @returns {Block}
     */
    static from_json(json: string): Block;
    /**
     * @param {Header} header
     * @param {TransactionBodies} transaction_bodies
     * @param {TransactionWitnessSets} transaction_witness_sets
     * @param {AuxiliaryDataSet} auxiliary_data_set
     * @param {TransactionIndexes} invalid_transactions
     * @returns {Block}
     */
    static new(header: Header, transaction_bodies: TransactionBodies, transaction_witness_sets: TransactionWitnessSets, auxiliary_data_set: AuxiliaryDataSet, invalid_transactions: TransactionIndexes): Block;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Header}
     */
    header(): Header;
    /**
     * @returns {TransactionBodies}
     */
    transaction_bodies(): TransactionBodies;
    /**
     * @returns {TransactionWitnessSets}
     */
    transaction_witness_sets(): TransactionWitnessSets;
    /**
     * @returns {AuxiliaryDataSet}
     */
    auxiliary_data_set(): AuxiliaryDataSet;
    /**
     * @returns {TransactionIndexes}
     */
    invalid_transactions(): TransactionIndexes;
}
/** */
export class BlockHash {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {BlockHash}
     */
    static from_bytes(bytes: Uint8Array): BlockHash;
    /**
     * @param {string} bech_str
     * @returns {BlockHash}
     */
    static from_bech32(bech_str: string): BlockHash;
    /**
     * @param {string} hex
     * @returns {BlockHash}
     */
    static from_hex(hex: string): BlockHash;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string} prefix
     * @returns {string}
     */
    to_bech32(prefix: string): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class Blockfrost {
    static __wrap(ptr: any): any;
    /**
     * @param {string} url
     * @param {string} project_id
     * @returns {Blockfrost}
     */
    static new(url: string, project_id: string): Blockfrost;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {string}
     */
    url(): string;
    /**
     * @returns {string}
     */
    project_id(): string;
}
/** */
export class BootstrapWitness {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {BootstrapWitness}
     */
    static from_bytes(bytes: Uint8Array): BootstrapWitness;
    /**
     * @param {string} json
     * @returns {BootstrapWitness}
     */
    static from_json(json: string): BootstrapWitness;
    /**
     * @param {Vkey} vkey
     * @param {Ed25519Signature} signature
     * @param {Uint8Array} chain_code
     * @param {Uint8Array} attributes
     * @returns {BootstrapWitness}
     */
    static new(vkey: Vkey, signature: Ed25519Signature, chain_code: Uint8Array, attributes: Uint8Array): BootstrapWitness;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Vkey}
     */
    vkey(): Vkey;
    /**
     * @returns {Ed25519Signature}
     */
    signature(): Ed25519Signature;
    /**
     * @returns {Uint8Array}
     */
    chain_code(): Uint8Array;
    /**
     * @returns {Uint8Array}
     */
    attributes(): Uint8Array;
}
/** */
export class BootstrapWitnesses {
    static __wrap(ptr: any): any;
    /**
     * @returns {BootstrapWitnesses}
     */
    static new(): BootstrapWitnesses;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {BootstrapWitness}
     */
    get(index: number): BootstrapWitness;
    /**
     * @param {BootstrapWitness} elem
     */
    add(elem: BootstrapWitness): void;
}
/** */
export class ByronAddress {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ByronAddress}
     */
    static from_bytes(bytes: Uint8Array): ByronAddress;
    /**
     * @param {string} s
     * @returns {ByronAddress}
     */
    static from_base58(s: string): ByronAddress;
    /**
     * @param {Bip32PublicKey} key
     * @param {number} protocol_magic
     * @returns {ByronAddress}
     */
    static icarus_from_key(key: Bip32PublicKey, protocol_magic: number): ByronAddress;
    /**
     * @param {string} s
     * @returns {boolean}
     */
    static is_valid(s: string): boolean;
    /**
     * @param {Address} addr
     * @returns {ByronAddress | undefined}
     */
    static from_address(addr: Address): ByronAddress | undefined;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {string}
     */
    to_base58(): string;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * returns the byron protocol magic embedded in the address, or mainnet id if none is present
     * note: for bech32 addresses, you need to use network_id instead
     * @returns {number}
     */
    byron_protocol_magic(): number;
    /**
     * @returns {Uint8Array}
     */
    attributes(): Uint8Array;
    /**
     * @returns {number}
     */
    network_id(): number;
    /**
     * @returns {Address}
     */
    to_address(): Address;
}
/** */
export class Certificate {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Certificate}
     */
    static from_bytes(bytes: Uint8Array): Certificate;
    /**
     * @param {string} json
     * @returns {Certificate}
     */
    static from_json(json: string): Certificate;
    /**
     * @param {StakeRegistration} stake_registration
     * @returns {Certificate}
     */
    static new_stake_registration(stake_registration: StakeRegistration): Certificate;
    /**
     * @param {StakeDeregistration} stake_deregistration
     * @returns {Certificate}
     */
    static new_stake_deregistration(stake_deregistration: StakeDeregistration): Certificate;
    /**
     * @param {StakeDelegation} stake_delegation
     * @returns {Certificate}
     */
    static new_stake_delegation(stake_delegation: StakeDelegation): Certificate;
    /**
     * @param {PoolRegistration} pool_registration
     * @returns {Certificate}
     */
    static new_pool_registration(pool_registration: PoolRegistration): Certificate;
    /**
     * @param {PoolRetirement} pool_retirement
     * @returns {Certificate}
     */
    static new_pool_retirement(pool_retirement: PoolRetirement): Certificate;
    /**
     * @param {GenesisKeyDelegation} genesis_key_delegation
     * @returns {Certificate}
     */
    static new_genesis_key_delegation(genesis_key_delegation: GenesisKeyDelegation): Certificate;
    /**
     * @param {MoveInstantaneousRewardsCert} move_instantaneous_rewards_cert
     * @returns {Certificate}
     */
    static new_move_instantaneous_rewards_cert(move_instantaneous_rewards_cert: MoveInstantaneousRewardsCert): Certificate;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {StakeRegistration | undefined}
     */
    as_stake_registration(): StakeRegistration | undefined;
    /**
     * @returns {StakeDeregistration | undefined}
     */
    as_stake_deregistration(): StakeDeregistration | undefined;
    /**
     * @returns {StakeDelegation | undefined}
     */
    as_stake_delegation(): StakeDelegation | undefined;
    /**
     * @returns {PoolRegistration | undefined}
     */
    as_pool_registration(): PoolRegistration | undefined;
    /**
     * @returns {PoolRetirement | undefined}
     */
    as_pool_retirement(): PoolRetirement | undefined;
    /**
     * @returns {GenesisKeyDelegation | undefined}
     */
    as_genesis_key_delegation(): GenesisKeyDelegation | undefined;
    /**
     * @returns {MoveInstantaneousRewardsCert | undefined}
     */
    as_move_instantaneous_rewards_cert(): MoveInstantaneousRewardsCert | undefined;
    /**
     * @returns {RegCert | undefined}
     */
    as_reg_cert(): RegCert | undefined;
    /**
     * @returns {UnregCert | undefined}
     */
    as_unreg_cert(): UnregCert | undefined;
    /**
     * @returns {VoteDelegCert | undefined}
     */
    as_vote_deleg_cert(): VoteDelegCert | undefined;
    /**
     * @returns {StakeVoteDelegCert | undefined}
     */
    as_stake_vote_deleg_cert(): StakeVoteDelegCert | undefined;
    /**
     * @returns {StakeRegDelegCert | undefined}
     */
    as_stake_reg_deleg_cert(): StakeRegDelegCert | undefined;
    /**
     * @returns {VoteRegDelegCert | undefined}
     */
    as_vote_reg_deleg_cert(): VoteRegDelegCert | undefined;
    /**
     * @returns {StakeVoteRegDelegCert | undefined}
     */
    as_stake_vote_reg_deleg_cert(): StakeVoteRegDelegCert | undefined;
    /**
     * @returns {RegCommitteeHotKeyCert | undefined}
     */
    as_reg_committee_hot_key_cert(): RegCommitteeHotKeyCert | undefined;
    /**
     * @returns {UnregCommitteeHotKeyCert | undefined}
     */
    as_unreg_committee_hot_key_cert(): UnregCommitteeHotKeyCert | undefined;
    /**
     * @returns {RegDrepCert | undefined}
     */
    as_reg_drep_cert(): RegDrepCert | undefined;
    /**
     * @returns {UnregDrepCert | undefined}
     */
    as_unreg_drep_cert(): UnregDrepCert | undefined;
}
/** */
export class Certificates {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Certificates}
     */
    static from_bytes(bytes: Uint8Array): Certificates;
    /**
     * @param {string} json
     * @returns {Certificates}
     */
    static from_json(json: string): Certificates;
    /**
     * @returns {Certificates}
     */
    static new(): Certificates;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {Certificate}
     */
    get(index: number): Certificate;
    /**
     * @param {Certificate} elem
     */
    add(elem: Certificate): void;
}
/** */
export class ConstrPlutusData {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ConstrPlutusData}
     */
    static from_bytes(bytes: Uint8Array): ConstrPlutusData;
    /**
     * @param {BigNum} alternative
     * @param {PlutusList} data
     * @returns {ConstrPlutusData}
     */
    static new(alternative: BigNum, data: PlutusList): ConstrPlutusData;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {BigNum}
     */
    alternative(): BigNum;
    /**
     * @returns {PlutusList}
     */
    data(): PlutusList;
}
/** */
export class CostModel {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {CostModel}
     */
    static from_bytes(bytes: Uint8Array): CostModel;
    /**
     * @returns {CostModel}
     */
    static new(): CostModel;
    /**
     * @returns {CostModel}
     */
    static new_plutus_v2(): CostModel;
    /**
     * @returns {CostModel}
     */
    static new_plutus_v3(): CostModel;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {number} operation
     * @param {Int} cost
     * @returns {Int}
     */
    set(operation: number, cost: Int): Int;
    /**
     * @param {number} operation
     * @returns {Int}
     */
    get(operation: number): Int;
    /**
     * @returns {number}
     */
    len(): number;
}
/** */
export class Costmdls {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Costmdls}
     */
    static from_bytes(bytes: Uint8Array): Costmdls;
    /**
     * @returns {Costmdls}
     */
    static new(): Costmdls;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {Language} key
     * @param {CostModel} value
     * @returns {CostModel | undefined}
     */
    insert(key: Language, value: CostModel): CostModel | undefined;
    /**
     * @param {Language} key
     * @returns {CostModel | undefined}
     */
    get(key: Language): CostModel | undefined;
    /**
     * @returns {Languages}
     */
    keys(): Languages;
}
/** */
export class DNSRecordAorAAAA {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {DNSRecordAorAAAA}
     */
    static from_bytes(bytes: Uint8Array): DNSRecordAorAAAA;
    /**
     * @param {string} dns_name
     * @returns {DNSRecordAorAAAA}
     */
    static new(dns_name: string): DNSRecordAorAAAA;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    record(): string;
}
/** */
export class DNSRecordSRV {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {DNSRecordSRV}
     */
    static from_bytes(bytes: Uint8Array): DNSRecordSRV;
    /**
     * @param {string} dns_name
     * @returns {DNSRecordSRV}
     */
    static new(dns_name: string): DNSRecordSRV;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    record(): string;
}
/** */
export class Data {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Data}
     */
    static from_bytes(bytes: Uint8Array): Data;
    /**
     * @param {string} json
     * @returns {Data}
     */
    static from_json(json: string): Data;
    /**
     * @param {PlutusData} plutus_data
     * @returns {Data}
     */
    static new(plutus_data: PlutusData): Data;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {PlutusData}
     */
    get(): PlutusData;
}
/** */
export class DataHash {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {DataHash}
     */
    static from_bytes(bytes: Uint8Array): DataHash;
    /**
     * @param {string} bech_str
     * @returns {DataHash}
     */
    static from_bech32(bech_str: string): DataHash;
    /**
     * @param {string} hex
     * @returns {DataHash}
     */
    static from_hex(hex: string): DataHash;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string} prefix
     * @returns {string}
     */
    to_bech32(prefix: string): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class Datum {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Datum}
     */
    static from_bytes(bytes: Uint8Array): Datum;
    /**
     * @param {string} json
     * @returns {Datum}
     */
    static from_json(json: string): Datum;
    /**
     * @param {DataHash} data_hash
     * @returns {Datum}
     */
    static new_data_hash(data_hash: DataHash): Datum;
    /**
     * @param {Data} data
     * @returns {Datum}
     */
    static new_data(data: Data): Datum;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {DataHash | undefined}
     */
    as_data_hash(): DataHash | undefined;
    /**
     * @returns {Data | undefined}
     */
    as_data(): Data | undefined;
}
/** */
export class Drep {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Drep}
     */
    static from_bytes(bytes: Uint8Array): Drep;
    /**
     * @param {string} json
     * @returns {Drep}
     */
    static from_json(json: string): Drep;
    /**
     * @param {Ed25519KeyHash} keyhash
     * @returns {Drep}
     */
    static new_keyhash(keyhash: Ed25519KeyHash): Drep;
    /**
     * @param {ScriptHash} scripthash
     * @returns {Drep}
     */
    static new_scripthash(scripthash: ScriptHash): Drep;
    /**
     * @returns {Drep}
     */
    static new_abstain(): Drep;
    /**
     * @returns {Drep}
     */
    static new_no_confidence(): Drep;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {Ed25519KeyHash | undefined}
     */
    as_keyhash(): Ed25519KeyHash | undefined;
    /**
     * @returns {ScriptHash | undefined}
     */
    as_scripthash(): ScriptHash | undefined;
}
/** */
export class DrepVotingThresholds {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {DrepVotingThresholds}
     */
    static from_bytes(bytes: Uint8Array): DrepVotingThresholds;
    /**
     * @param {string} json
     * @returns {DrepVotingThresholds}
     */
    static from_json(json: string): DrepVotingThresholds;
    /**
     * @param {UnitInterval} motion_no_confidence
     * @param {UnitInterval} committee_normal
     * @param {UnitInterval} committee_no_confidence
     * @param {UnitInterval} update_constitution
     * @param {UnitInterval} hard_fork_initiation
     * @param {UnitInterval} pp_network_group
     * @param {UnitInterval} pp_economic_group
     * @param {UnitInterval} pp_technical_group
     * @param {UnitInterval} pp_governance_group
     * @param {UnitInterval} treasury_withdrawal
     * @returns {DrepVotingThresholds}
     */
    static new(motion_no_confidence: UnitInterval, committee_normal: UnitInterval, committee_no_confidence: UnitInterval, update_constitution: UnitInterval, hard_fork_initiation: UnitInterval, pp_network_group: UnitInterval, pp_economic_group: UnitInterval, pp_technical_group: UnitInterval, pp_governance_group: UnitInterval, treasury_withdrawal: UnitInterval): DrepVotingThresholds;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {UnitInterval}
     */
    motion_no_confidence(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    committee_normal(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    committee_no_confidence(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    update_constitution(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    hard_fork_initiation(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    pp_network_group(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    pp_economic_group(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    pp_technical_group(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    pp_governance_group(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    treasury_withdrawal(): UnitInterval;
}
/** */
export class Ed25519KeyHash {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Ed25519KeyHash}
     */
    static from_bytes(bytes: Uint8Array): Ed25519KeyHash;
    /**
     * @param {string} bech_str
     * @returns {Ed25519KeyHash}
     */
    static from_bech32(bech_str: string): Ed25519KeyHash;
    /**
     * @param {string} hex
     * @returns {Ed25519KeyHash}
     */
    static from_hex(hex: string): Ed25519KeyHash;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string} prefix
     * @returns {string}
     */
    to_bech32(prefix: string): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class Ed25519KeyHashes {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Ed25519KeyHashes}
     */
    static from_bytes(bytes: Uint8Array): Ed25519KeyHashes;
    /**
     * @param {string} json
     * @returns {Ed25519KeyHashes}
     */
    static from_json(json: string): Ed25519KeyHashes;
    /**
     * @returns {Ed25519KeyHashes}
     */
    static new(): Ed25519KeyHashes;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {Ed25519KeyHash}
     */
    get(index: number): Ed25519KeyHash;
    /**
     * @param {Ed25519KeyHash} elem
     */
    add(elem: Ed25519KeyHash): void;
}
/** */
export class Ed25519Signature {
    static __wrap(ptr: any): any;
    /**
     * @param {string} bech32_str
     * @returns {Ed25519Signature}
     */
    static from_bech32(bech32_str: string): Ed25519Signature;
    /**
     * @param {string} input
     * @returns {Ed25519Signature}
     */
    static from_hex(input: string): Ed25519Signature;
    /**
     * @param {Uint8Array} bytes
     * @returns {Ed25519Signature}
     */
    static from_bytes(bytes: Uint8Array): Ed25519Signature;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_bech32(): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class EnterpriseAddress {
    static __wrap(ptr: any): any;
    /**
     * @param {number} network
     * @param {StakeCredential} payment
     * @returns {EnterpriseAddress}
     */
    static new(network: number, payment: StakeCredential): EnterpriseAddress;
    /**
     * @param {Address} addr
     * @returns {EnterpriseAddress | undefined}
     */
    static from_address(addr: Address): EnterpriseAddress | undefined;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {StakeCredential}
     */
    payment_cred(): StakeCredential;
    /**
     * @returns {Address}
     */
    to_address(): Address;
}
/** */
export class ExUnitPrices {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ExUnitPrices}
     */
    static from_bytes(bytes: Uint8Array): ExUnitPrices;
    /**
     * @param {UnitInterval} mem_price
     * @param {UnitInterval} step_price
     * @returns {ExUnitPrices}
     */
    static new(mem_price: UnitInterval, step_price: UnitInterval): ExUnitPrices;
    /**
     * @param {number} mem_price
     * @param {number} step_price
     * @returns {ExUnitPrices}
     */
    static from_float(mem_price: number, step_price: number): ExUnitPrices;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {UnitInterval}
     */
    mem_price(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    step_price(): UnitInterval;
}
/** */
export class ExUnits {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ExUnits}
     */
    static from_bytes(bytes: Uint8Array): ExUnits;
    /**
     * @param {BigNum} mem
     * @param {BigNum} steps
     * @returns {ExUnits}
     */
    static new(mem: BigNum, steps: BigNum): ExUnits;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {BigNum}
     */
    mem(): BigNum;
    /**
     * @returns {BigNum}
     */
    steps(): BigNum;
}
/** */
export class GeneralTransactionMetadata {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {GeneralTransactionMetadata}
     */
    static from_bytes(bytes: Uint8Array): GeneralTransactionMetadata;
    /**
     * @param {string} json
     * @returns {GeneralTransactionMetadata}
     */
    static from_json(json: string): GeneralTransactionMetadata;
    /**
     * @returns {GeneralTransactionMetadata}
     */
    static new(): GeneralTransactionMetadata;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {BigNum} key
     * @param {TransactionMetadatum} value
     * @returns {TransactionMetadatum | undefined}
     */
    insert(key: BigNum, value: TransactionMetadatum): TransactionMetadatum | undefined;
    /**
     * @param {BigNum} key
     * @returns {TransactionMetadatum | undefined}
     */
    get(key: BigNum): TransactionMetadatum | undefined;
    /**
     * @returns {TransactionMetadatumLabels}
     */
    keys(): TransactionMetadatumLabels;
}
/** */
export class GenesisDelegateHash {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {GenesisDelegateHash}
     */
    static from_bytes(bytes: Uint8Array): GenesisDelegateHash;
    /**
     * @param {string} bech_str
     * @returns {GenesisDelegateHash}
     */
    static from_bech32(bech_str: string): GenesisDelegateHash;
    /**
     * @param {string} hex
     * @returns {GenesisDelegateHash}
     */
    static from_hex(hex: string): GenesisDelegateHash;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string} prefix
     * @returns {string}
     */
    to_bech32(prefix: string): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class GenesisHash {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {GenesisHash}
     */
    static from_bytes(bytes: Uint8Array): GenesisHash;
    /**
     * @param {string} bech_str
     * @returns {GenesisHash}
     */
    static from_bech32(bech_str: string): GenesisHash;
    /**
     * @param {string} hex
     * @returns {GenesisHash}
     */
    static from_hex(hex: string): GenesisHash;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string} prefix
     * @returns {string}
     */
    to_bech32(prefix: string): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class GenesisHashes {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {GenesisHashes}
     */
    static from_bytes(bytes: Uint8Array): GenesisHashes;
    /**
     * @param {string} json
     * @returns {GenesisHashes}
     */
    static from_json(json: string): GenesisHashes;
    /**
     * @returns {GenesisHashes}
     */
    static new(): GenesisHashes;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {GenesisHash}
     */
    get(index: number): GenesisHash;
    /**
     * @param {GenesisHash} elem
     */
    add(elem: GenesisHash): void;
}
/** */
export class GenesisKeyDelegation {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {GenesisKeyDelegation}
     */
    static from_bytes(bytes: Uint8Array): GenesisKeyDelegation;
    /**
     * @param {string} json
     * @returns {GenesisKeyDelegation}
     */
    static from_json(json: string): GenesisKeyDelegation;
    /**
     * @param {GenesisHash} genesishash
     * @param {GenesisDelegateHash} genesis_delegate_hash
     * @param {VRFKeyHash} vrf_keyhash
     * @returns {GenesisKeyDelegation}
     */
    static new(genesishash: GenesisHash, genesis_delegate_hash: GenesisDelegateHash, vrf_keyhash: VRFKeyHash): GenesisKeyDelegation;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {GenesisHash}
     */
    genesishash(): GenesisHash;
    /**
     * @returns {GenesisDelegateHash}
     */
    genesis_delegate_hash(): GenesisDelegateHash;
    /**
     * @returns {VRFKeyHash}
     */
    vrf_keyhash(): VRFKeyHash;
}
/** */
export class GovernanceAction {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {GovernanceAction}
     */
    static from_bytes(bytes: Uint8Array): GovernanceAction;
    /**
     * @param {string} json
     * @returns {GovernanceAction}
     */
    static from_json(json: string): GovernanceAction;
    /**
     * @param {ParameterChangeAction} parameter_change_action
     * @returns {GovernanceAction}
     */
    static new_parameter_change_action(parameter_change_action: ParameterChangeAction): GovernanceAction;
    /**
     * @param {HardForkInitiationAction} hard_fork_initiation_action
     * @returns {GovernanceAction}
     */
    static new_hard_fork_initiation_action(hard_fork_initiation_action: HardForkInitiationAction): GovernanceAction;
    /**
     * @param {TreasuryWithdrawalsAction} treasury_withdrawals_action
     * @returns {GovernanceAction}
     */
    static new_treasury_withdrawals_action(treasury_withdrawals_action: TreasuryWithdrawalsAction): GovernanceAction;
    /**
     * @returns {GovernanceAction}
     */
    static new_no_confidence(): GovernanceAction;
    /**
     * @param {NewCommittee} new_committe
     * @returns {GovernanceAction}
     */
    static new_new_committee(new_committe: NewCommittee): GovernanceAction;
    /**
     * @param {NewConstitution} new_constitution
     * @returns {GovernanceAction}
     */
    static new_new_constitution(new_constitution: NewConstitution): GovernanceAction;
    /**
     * @returns {GovernanceAction}
     */
    static new_info_action(): GovernanceAction;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {ParameterChangeAction | undefined}
     */
    as_parameter_change_action(): ParameterChangeAction | undefined;
    /**
     * @returns {HardForkInitiationAction | undefined}
     */
    as_hard_fork_initiation_action(): HardForkInitiationAction | undefined;
    /**
     * @returns {TreasuryWithdrawalsAction | undefined}
     */
    as_treasury_withdrawals_action(): TreasuryWithdrawalsAction | undefined;
    /**
     * @returns {NewCommittee | undefined}
     */
    as_new_committee(): NewCommittee | undefined;
    /**
     * @returns {NewConstitution | undefined}
     */
    as_new_constitution(): NewConstitution | undefined;
}
/** */
export class GovernanceActionId {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {GovernanceActionId}
     */
    static from_bytes(bytes: Uint8Array): GovernanceActionId;
    /**
     * @param {string} json
     * @returns {GovernanceActionId}
     */
    static from_json(json: string): GovernanceActionId;
    /**
     * @param {TransactionHash} transaction_id
     * @param {BigNum} governance_action_index
     * @returns {GovernanceActionId}
     */
    static new(transaction_id: TransactionHash, governance_action_index: BigNum): GovernanceActionId;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {TransactionHash}
     */
    transaction_id(): TransactionHash;
    /**
     * @returns {BigNum}
     */
    governance_action_index(): BigNum;
}
/** */
export class HardForkInitiationAction {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {HardForkInitiationAction}
     */
    static from_bytes(bytes: Uint8Array): HardForkInitiationAction;
    /**
     * @param {string} json
     * @returns {HardForkInitiationAction}
     */
    static from_json(json: string): HardForkInitiationAction;
    /**
     * @param {ProtocolVersion} protocol_version
     * @returns {HardForkInitiationAction}
     */
    static new(protocol_version: ProtocolVersion): HardForkInitiationAction;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {ProtocolVersion}
     */
    protocol_version(): ProtocolVersion;
}
/** */
export class Header {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Header}
     */
    static from_bytes(bytes: Uint8Array): Header;
    /**
     * @param {string} json
     * @returns {Header}
     */
    static from_json(json: string): Header;
    /**
     * @param {HeaderBody} header_body
     * @param {KESSignature} body_signature
     * @returns {Header}
     */
    static new(header_body: HeaderBody, body_signature: KESSignature): Header;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {HeaderBody}
     */
    header_body(): HeaderBody;
    /**
     * @returns {KESSignature}
     */
    body_signature(): KESSignature;
}
/** */
export class HeaderBody {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {HeaderBody}
     */
    static from_bytes(bytes: Uint8Array): HeaderBody;
    /**
     * @param {string} json
     * @returns {HeaderBody}
     */
    static from_json(json: string): HeaderBody;
    /**
     * @param {number} block_number
     * @param {BigNum} slot
     * @param {BlockHash | undefined} prev_hash
     * @param {Vkey} issuer_vkey
     * @param {VRFVKey} vrf_vkey
     * @param {VRFCert} nonce_vrf
     * @param {VRFCert} leader_vrf
     * @param {number} block_body_size
     * @param {BlockHash} block_body_hash
     * @param {OperationalCert} operational_cert
     * @param {ProtocolVersion} protocol_version
     * @returns {HeaderBody}
     */
    static new(block_number: number, slot: BigNum, prev_hash: BlockHash | undefined, issuer_vkey: Vkey, vrf_vkey: VRFVKey, nonce_vrf: VRFCert, leader_vrf: VRFCert, block_body_size: number, block_body_hash: BlockHash, operational_cert: OperationalCert, protocol_version: ProtocolVersion): HeaderBody;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    block_number(): number;
    /**
     * @returns {BigNum}
     */
    slot(): BigNum;
    /**
     * @returns {BlockHash | undefined}
     */
    prev_hash(): BlockHash | undefined;
    /**
     * @returns {Vkey}
     */
    issuer_vkey(): Vkey;
    /**
     * @returns {VRFVKey}
     */
    vrf_vkey(): VRFVKey;
    /**
     * @returns {VRFCert}
     */
    nonce_vrf(): VRFCert;
    /**
     * @returns {VRFCert}
     */
    leader_vrf(): VRFCert;
    /**
     * @returns {number}
     */
    block_body_size(): number;
    /**
     * @returns {BlockHash}
     */
    block_body_hash(): BlockHash;
    /**
     * @returns {OperationalCert}
     */
    operational_cert(): OperationalCert;
    /**
     * @returns {ProtocolVersion}
     */
    protocol_version(): ProtocolVersion;
}
/** */
export class Int {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Int}
     */
    static from_bytes(bytes: Uint8Array): Int;
    /**
     * @param {BigNum} x
     * @returns {Int}
     */
    static new(x: BigNum): Int;
    /**
     * @param {BigNum} x
     * @returns {Int}
     */
    static new_negative(x: BigNum): Int;
    /**
     * @param {number} x
     * @returns {Int}
     */
    static new_i32(x: number): Int;
    /**
     * @param {string} string
     * @returns {Int}
     */
    static from_str(string: string): Int;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {boolean}
     */
    is_positive(): boolean;
    /**
     * BigNum can only contain unsigned u64 values
     *
     * This function will return the BigNum representation
     * only in case the underlying i128 value is positive.
     *
     * Otherwise nothing will be returned (undefined).
     * @returns {BigNum | undefined}
     */
    as_positive(): BigNum | undefined;
    /**
     * BigNum can only contain unsigned u64 values
     *
     * This function will return the *absolute* BigNum representation
     * only in case the underlying i128 value is negative.
     *
     * Otherwise nothing will be returned (undefined).
     * @returns {BigNum | undefined}
     */
    as_negative(): BigNum | undefined;
    /**
     * !!! DEPRECATED !!!
     * Returns an i32 value in case the underlying original i128 value is within the limits.
     * Otherwise will just return an empty value (undefined).
     * @returns {number | undefined}
     */
    as_i32(): number | undefined;
    /**
     * Returns the underlying value converted to i32 if possible (within limits)
     * Otherwise will just return an empty value (undefined).
     * @returns {number | undefined}
     */
    as_i32_or_nothing(): number | undefined;
    /**
     * Returns the underlying value converted to i32 if possible (within limits)
     * JsError in case of out of boundary overflow
     * @returns {number}
     */
    as_i32_or_fail(): number;
    /**
     * Returns string representation of the underlying i128 value directly.
     * Might contain the minus sign (-) in case of negative value.
     * @returns {string}
     */
    to_str(): string;
}
/** */
export class Ipv4 {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Ipv4}
     */
    static from_bytes(bytes: Uint8Array): Ipv4;
    /**
     * @param {string} json
     * @returns {Ipv4}
     */
    static from_json(json: string): Ipv4;
    /**
     * @param {Uint8Array} data
     * @returns {Ipv4}
     */
    static new(data: Uint8Array): Ipv4;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Uint8Array}
     */
    ip(): Uint8Array;
}
/** */
export class Ipv6 {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Ipv6}
     */
    static from_bytes(bytes: Uint8Array): Ipv6;
    /**
     * @param {string} json
     * @returns {Ipv6}
     */
    static from_json(json: string): Ipv6;
    /**
     * @param {Uint8Array} data
     * @returns {Ipv6}
     */
    static new(data: Uint8Array): Ipv6;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Uint8Array}
     */
    ip(): Uint8Array;
}
/** */
export class KESSignature {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {KESSignature}
     */
    static from_bytes(bytes: Uint8Array): KESSignature;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
}
/** */
export class KESVKey {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {KESVKey}
     */
    static from_bytes(bytes: Uint8Array): KESVKey;
    /**
     * @param {string} bech_str
     * @returns {KESVKey}
     */
    static from_bech32(bech_str: string): KESVKey;
    /**
     * @param {string} hex
     * @returns {KESVKey}
     */
    static from_hex(hex: string): KESVKey;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string} prefix
     * @returns {string}
     */
    to_bech32(prefix: string): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class Language {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Language}
     */
    static from_bytes(bytes: Uint8Array): Language;
    /**
     * @returns {Language}
     */
    static new_plutus_v1(): Language;
    /**
     * @returns {Language}
     */
    static new_plutus_v2(): Language;
    /**
     * @returns {Language}
     */
    static new_plutus_v3(): Language;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    kind(): number;
}
/** */
export class Languages {
    static __wrap(ptr: any): any;
    /**
     * @returns {Languages}
     */
    static new(): Languages;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {Language}
     */
    get(index: number): Language;
    /**
     * @param {Language} elem
     */
    add(elem: Language): void;
}
/** */
export class LegacyDaedalusPrivateKey {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {LegacyDaedalusPrivateKey}
     */
    static from_bytes(bytes: Uint8Array): LegacyDaedalusPrivateKey;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    as_bytes(): Uint8Array;
    /**
     * @returns {Uint8Array}
     */
    chaincode(): Uint8Array;
}
/** */
export class LinearFee {
    static __wrap(ptr: any): any;
    /**
     * @param {BigNum} coefficient
     * @param {BigNum} constant
     * @returns {LinearFee}
     */
    static new(coefficient: BigNum, constant: BigNum): LinearFee;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {BigNum}
     */
    constant(): BigNum;
    /**
     * @returns {BigNum}
     */
    coefficient(): BigNum;
}
/** */
export class MIRToStakeCredentials {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {MIRToStakeCredentials}
     */
    static from_bytes(bytes: Uint8Array): MIRToStakeCredentials;
    /**
     * @param {string} json
     * @returns {MIRToStakeCredentials}
     */
    static from_json(json: string): MIRToStakeCredentials;
    /**
     * @returns {MIRToStakeCredentials}
     */
    static new(): MIRToStakeCredentials;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {StakeCredential} cred
     * @param {Int} delta
     * @returns {Int | undefined}
     */
    insert(cred: StakeCredential, delta: Int): Int | undefined;
    /**
     * @param {StakeCredential} cred
     * @returns {Int | undefined}
     */
    get(cred: StakeCredential): Int | undefined;
    /**
     * @returns {StakeCredentials}
     */
    keys(): StakeCredentials;
}
/** */
export class MetadataList {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {MetadataList}
     */
    static from_bytes(bytes: Uint8Array): MetadataList;
    /**
     * @returns {MetadataList}
     */
    static new(): MetadataList;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {TransactionMetadatum}
     */
    get(index: number): TransactionMetadatum;
    /**
     * @param {TransactionMetadatum} elem
     */
    add(elem: TransactionMetadatum): void;
}
/** */
export class MetadataMap {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {MetadataMap}
     */
    static from_bytes(bytes: Uint8Array): MetadataMap;
    /**
     * @returns {MetadataMap}
     */
    static new(): MetadataMap;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {TransactionMetadatum} key
     * @param {TransactionMetadatum} value
     * @returns {TransactionMetadatum | undefined}
     */
    insert(key: TransactionMetadatum, value: TransactionMetadatum): TransactionMetadatum | undefined;
    /**
     * @param {string} key
     * @param {TransactionMetadatum} value
     * @returns {TransactionMetadatum | undefined}
     */
    insert_str(key: string, value: TransactionMetadatum): TransactionMetadatum | undefined;
    /**
     * @param {number} key
     * @param {TransactionMetadatum} value
     * @returns {TransactionMetadatum | undefined}
     */
    insert_i32(key: number, value: TransactionMetadatum): TransactionMetadatum | undefined;
    /**
     * @param {TransactionMetadatum} key
     * @returns {TransactionMetadatum}
     */
    get(key: TransactionMetadatum): TransactionMetadatum;
    /**
     * @param {string} key
     * @returns {TransactionMetadatum}
     */
    get_str(key: string): TransactionMetadatum;
    /**
     * @param {number} key
     * @returns {TransactionMetadatum}
     */
    get_i32(key: number): TransactionMetadatum;
    /**
     * @param {TransactionMetadatum} key
     * @returns {boolean}
     */
    has(key: TransactionMetadatum): boolean;
    /**
     * @returns {MetadataList}
     */
    keys(): MetadataList;
}
/** */
export class Mint {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Mint}
     */
    static from_bytes(bytes: Uint8Array): Mint;
    /**
     * @param {string} json
     * @returns {Mint}
     */
    static from_json(json: string): Mint;
    /**
     * @returns {Mint}
     */
    static new(): Mint;
    /**
     * @param {ScriptHash} key
     * @param {MintAssets} value
     * @returns {Mint}
     */
    static new_from_entry(key: ScriptHash, value: MintAssets): Mint;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {ScriptHash} key
     * @param {MintAssets} value
     * @returns {MintAssets | undefined}
     */
    insert(key: ScriptHash, value: MintAssets): MintAssets | undefined;
    /**
     * @param {ScriptHash} key
     * @returns {MintAssets | undefined}
     */
    get(key: ScriptHash): MintAssets | undefined;
    /**
     * @returns {ScriptHashes}
     */
    keys(): ScriptHashes;
    /**
     * Returns the multiasset where only positive (minting) entries are present
     * @returns {MultiAsset}
     */
    as_positive_multiasset(): MultiAsset;
    /**
     * Returns the multiasset where only negative (burning) entries are present
     * @returns {MultiAsset}
     */
    as_negative_multiasset(): MultiAsset;
}
/** */
export class MintAssets {
    static __wrap(ptr: any): any;
    /**
     * @returns {MintAssets}
     */
    static new(): MintAssets;
    /**
     * @param {AssetName} key
     * @param {Int} value
     * @returns {MintAssets}
     */
    static new_from_entry(key: AssetName, value: Int): MintAssets;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {AssetName} key
     * @param {Int} value
     * @returns {Int | undefined}
     */
    insert(key: AssetName, value: Int): Int | undefined;
    /**
     * @param {AssetName} key
     * @returns {Int | undefined}
     */
    get(key: AssetName): Int | undefined;
    /**
     * @returns {AssetNames}
     */
    keys(): AssetNames;
}
/** */
export class MoveInstantaneousReward {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {MoveInstantaneousReward}
     */
    static from_bytes(bytes: Uint8Array): MoveInstantaneousReward;
    /**
     * @param {string} json
     * @returns {MoveInstantaneousReward}
     */
    static from_json(json: string): MoveInstantaneousReward;
    /**
     * @param {number} pot
     * @param {BigNum} amount
     * @returns {MoveInstantaneousReward}
     */
    static new_to_other_pot(pot: number, amount: BigNum): MoveInstantaneousReward;
    /**
     * @param {number} pot
     * @param {MIRToStakeCredentials} amounts
     * @returns {MoveInstantaneousReward}
     */
    static new_to_stake_creds(pot: number, amounts: MIRToStakeCredentials): MoveInstantaneousReward;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    pot(): number;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {BigNum | undefined}
     */
    as_to_other_pot(): BigNum | undefined;
    /**
     * @returns {MIRToStakeCredentials | undefined}
     */
    as_to_stake_creds(): MIRToStakeCredentials | undefined;
}
/** */
export class MoveInstantaneousRewardsCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {MoveInstantaneousRewardsCert}
     */
    static from_bytes(bytes: Uint8Array): MoveInstantaneousRewardsCert;
    /**
     * @param {string} json
     * @returns {MoveInstantaneousRewardsCert}
     */
    static from_json(json: string): MoveInstantaneousRewardsCert;
    /**
     * @param {MoveInstantaneousReward} move_instantaneous_reward
     * @returns {MoveInstantaneousRewardsCert}
     */
    static new(move_instantaneous_reward: MoveInstantaneousReward): MoveInstantaneousRewardsCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {MoveInstantaneousReward}
     */
    move_instantaneous_reward(): MoveInstantaneousReward;
}
/** */
export class MultiAsset {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {MultiAsset}
     */
    static from_bytes(bytes: Uint8Array): MultiAsset;
    /**
     * @param {string} json
     * @returns {MultiAsset}
     */
    static from_json(json: string): MultiAsset;
    /**
     * @returns {MultiAsset}
     */
    static new(): MultiAsset;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * the number of unique policy IDs in the multiasset
     * @returns {number}
     */
    len(): number;
    /**
     * set (and replace if it exists) all assets with policy {policy_id} to a copy of {assets}
     * @param {ScriptHash} policy_id
     * @param {Assets} assets
     * @returns {Assets | undefined}
     */
    insert(policy_id: ScriptHash, assets: Assets): Assets | undefined;
    /**
     * all assets under {policy_id}, if any exist, or else None (undefined in JS)
     * @param {ScriptHash} policy_id
     * @returns {Assets | undefined}
     */
    get(policy_id: ScriptHash): Assets | undefined;
    /**
     * sets the asset {asset_name} to {value} under policy {policy_id}
     * returns the previous amount if it was set, or else None (undefined in JS)
     * @param {ScriptHash} policy_id
     * @param {AssetName} asset_name
     * @param {BigNum} value
     * @returns {BigNum | undefined}
     */
    set_asset(policy_id: ScriptHash, asset_name: AssetName, value: BigNum): BigNum | undefined;
    /**
     * returns the amount of asset {asset_name} under policy {policy_id}
     * If such an asset does not exist, 0 is returned.
     * @param {ScriptHash} policy_id
     * @param {AssetName} asset_name
     * @returns {BigNum}
     */
    get_asset(policy_id: ScriptHash, asset_name: AssetName): BigNum;
    /**
     * returns all policy IDs used by assets in this multiasset
     * @returns {ScriptHashes}
     */
    keys(): ScriptHashes;
    /**
     * removes an asset from the list if the result is 0 or less
     * does not modify this object, instead the result is returned
     * @param {MultiAsset} rhs_ma
     * @returns {MultiAsset}
     */
    sub(rhs_ma: MultiAsset): MultiAsset;
}
/** */
export class MultiHostName {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {MultiHostName}
     */
    static from_bytes(bytes: Uint8Array): MultiHostName;
    /**
     * @param {string} json
     * @returns {MultiHostName}
     */
    static from_json(json: string): MultiHostName;
    /**
     * @param {DNSRecordSRV} dns_name
     * @returns {MultiHostName}
     */
    static new(dns_name: DNSRecordSRV): MultiHostName;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {DNSRecordSRV}
     */
    dns_name(): DNSRecordSRV;
}
/** */
export class NativeScript {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {NativeScript}
     */
    static from_bytes(bytes: Uint8Array): NativeScript;
    /**
     * @param {string} json
     * @returns {NativeScript}
     */
    static from_json(json: string): NativeScript;
    /**
     * @param {ScriptPubkey} script_pubkey
     * @returns {NativeScript}
     */
    static new_script_pubkey(script_pubkey: ScriptPubkey): NativeScript;
    /**
     * @param {ScriptAll} script_all
     * @returns {NativeScript}
     */
    static new_script_all(script_all: ScriptAll): NativeScript;
    /**
     * @param {ScriptAny} script_any
     * @returns {NativeScript}
     */
    static new_script_any(script_any: ScriptAny): NativeScript;
    /**
     * @param {ScriptNOfK} script_n_of_k
     * @returns {NativeScript}
     */
    static new_script_n_of_k(script_n_of_k: ScriptNOfK): NativeScript;
    /**
     * @param {TimelockStart} timelock_start
     * @returns {NativeScript}
     */
    static new_timelock_start(timelock_start: TimelockStart): NativeScript;
    /**
     * @param {TimelockExpiry} timelock_expiry
     * @returns {NativeScript}
     */
    static new_timelock_expiry(timelock_expiry: TimelockExpiry): NativeScript;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @param {number} namespace
     * @returns {ScriptHash}
     */
    hash(namespace: number): ScriptHash;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {ScriptPubkey | undefined}
     */
    as_script_pubkey(): ScriptPubkey | undefined;
    /**
     * @returns {ScriptAll | undefined}
     */
    as_script_all(): ScriptAll | undefined;
    /**
     * @returns {ScriptAny | undefined}
     */
    as_script_any(): ScriptAny | undefined;
    /**
     * @returns {ScriptNOfK | undefined}
     */
    as_script_n_of_k(): ScriptNOfK | undefined;
    /**
     * @returns {TimelockStart | undefined}
     */
    as_timelock_start(): TimelockStart | undefined;
    /**
     * @returns {TimelockExpiry | undefined}
     */
    as_timelock_expiry(): TimelockExpiry | undefined;
    /**
     * Returns an array of unique Ed25519KeyHashes
     * contained within this script recursively on any depth level.
     * The order of the keys in the result is not determined in any way.
     * @returns {Ed25519KeyHashes}
     */
    get_required_signers(): Ed25519KeyHashes;
    /**
     * @param {BigNum | undefined} lower_bound
     * @param {BigNum | undefined} upper_bound
     * @param {Ed25519KeyHashes} key_hashes
     * @returns {boolean}
     */
    verify(lower_bound: BigNum | undefined, upper_bound: BigNum | undefined, key_hashes: Ed25519KeyHashes): boolean;
}
/** */
export class NativeScripts {
    static __wrap(ptr: any): any;
    /**
     * @returns {NativeScripts}
     */
    static new(): NativeScripts;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {NativeScript}
     */
    get(index: number): NativeScript;
    /**
     * @param {NativeScript} elem
     */
    add(elem: NativeScript): void;
}
/** */
export class NetworkId {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {NetworkId}
     */
    static from_bytes(bytes: Uint8Array): NetworkId;
    /**
     * @param {string} json
     * @returns {NetworkId}
     */
    static from_json(json: string): NetworkId;
    /**
     * @returns {NetworkId}
     */
    static testnet(): NetworkId;
    /**
     * @returns {NetworkId}
     */
    static mainnet(): NetworkId;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    kind(): number;
}
/** */
export class NetworkInfo {
    static __wrap(ptr: any): any;
    /**
     * @param {number} network_id
     * @param {number} protocol_magic
     * @returns {NetworkInfo}
     */
    static new(network_id: number, protocol_magic: number): NetworkInfo;
    /**
     * @returns {NetworkInfo}
     */
    static testnet(): NetworkInfo;
    /**
     * @returns {NetworkInfo}
     */
    static mainnet(): NetworkInfo;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {number}
     */
    network_id(): number;
    /**
     * @returns {number}
     */
    protocol_magic(): number;
}
/** */
export class NewCommittee {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {NewCommittee}
     */
    static from_bytes(bytes: Uint8Array): NewCommittee;
    /**
     * @param {string} json
     * @returns {NewCommittee}
     */
    static from_json(json: string): NewCommittee;
    /**
     * @param {Ed25519KeyHashes} committee
     * @param {UnitInterval} rational
     * @returns {NewCommittee}
     */
    static new(committee: Ed25519KeyHashes, rational: UnitInterval): NewCommittee;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Ed25519KeyHashes}
     */
    committee(): Ed25519KeyHashes;
    /**
     * @returns {UnitInterval}
     */
    rational(): UnitInterval;
}
/** */
export class NewConstitution {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {NewConstitution}
     */
    static from_bytes(bytes: Uint8Array): NewConstitution;
    /**
     * @param {string} json
     * @returns {NewConstitution}
     */
    static from_json(json: string): NewConstitution;
    /**
     * @param {DataHash} hash
     * @returns {NewConstitution}
     */
    static new(hash: DataHash): NewConstitution;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {DataHash}
     */
    hash(): DataHash;
}
/** */
export class Nonce {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Nonce}
     */
    static from_bytes(bytes: Uint8Array): Nonce;
    /**
     * @returns {Nonce}
     */
    static new_identity(): Nonce;
    /**
     * @param {Uint8Array} hash
     * @returns {Nonce}
     */
    static new_from_hash(hash: Uint8Array): Nonce;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {Uint8Array | undefined}
     */
    get_hash(): Uint8Array | undefined;
}
/** */
export class OperationalCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {OperationalCert}
     */
    static from_bytes(bytes: Uint8Array): OperationalCert;
    /**
     * @param {string} json
     * @returns {OperationalCert}
     */
    static from_json(json: string): OperationalCert;
    /**
     * @param {KESVKey} hot_vkey
     * @param {number} sequence_number
     * @param {number} kes_period
     * @param {Ed25519Signature} sigma
     * @returns {OperationalCert}
     */
    static new(hot_vkey: KESVKey, sequence_number: number, kes_period: number, sigma: Ed25519Signature): OperationalCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {KESVKey}
     */
    hot_vkey(): KESVKey;
    /**
     * @returns {number}
     */
    sequence_number(): number;
    /**
     * @returns {number}
     */
    kes_period(): number;
    /**
     * @returns {Ed25519Signature}
     */
    sigma(): Ed25519Signature;
}
/** */
export class ParameterChangeAction {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ParameterChangeAction}
     */
    static from_bytes(bytes: Uint8Array): ParameterChangeAction;
    /**
     * @param {string} json
     * @returns {ParameterChangeAction}
     */
    static from_json(json: string): ParameterChangeAction;
    /**
     * @param {ProtocolParamUpdate} protocol_param_update
     * @returns {ParameterChangeAction}
     */
    static new(protocol_param_update: ProtocolParamUpdate): ParameterChangeAction;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {ProtocolParamUpdate}
     */
    protocol_param_update(): ProtocolParamUpdate;
}
/** */
export class PlutusData {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PlutusData}
     */
    static from_bytes(bytes: Uint8Array): PlutusData;
    /**
     * @param {ConstrPlutusData} constr_plutus_data
     * @returns {PlutusData}
     */
    static new_constr_plutus_data(constr_plutus_data: ConstrPlutusData): PlutusData;
    /**
     * @param {PlutusMap} map
     * @returns {PlutusData}
     */
    static new_map(map: PlutusMap): PlutusData;
    /**
     * @param {PlutusList} list
     * @returns {PlutusData}
     */
    static new_list(list: PlutusList): PlutusData;
    /**
     * @param {BigInt} integer
     * @returns {PlutusData}
     */
    static new_integer(integer: BigInt): PlutusData;
    /**
     * @param {Uint8Array} bytes
     * @returns {PlutusData}
     */
    static new_bytes(bytes: Uint8Array): PlutusData;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {ConstrPlutusData | undefined}
     */
    as_constr_plutus_data(): ConstrPlutusData | undefined;
    /**
     * @returns {PlutusMap | undefined}
     */
    as_map(): PlutusMap | undefined;
    /**
     * @returns {PlutusList | undefined}
     */
    as_list(): PlutusList | undefined;
    /**
     * @returns {BigInt | undefined}
     */
    as_integer(): BigInt | undefined;
    /**
     * @returns {Uint8Array | undefined}
     */
    as_bytes(): Uint8Array | undefined;
}
/** */
export class PlutusList {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PlutusList}
     */
    static from_bytes(bytes: Uint8Array): PlutusList;
    /**
     * @returns {PlutusList}
     */
    static new(): PlutusList;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {PlutusData}
     */
    get(index: number): PlutusData;
    /**
     * @param {PlutusData} elem
     */
    add(elem: PlutusData): void;
}
/** */
export class PlutusMap {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PlutusMap}
     */
    static from_bytes(bytes: Uint8Array): PlutusMap;
    /**
     * @returns {PlutusMap}
     */
    static new(): PlutusMap;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {PlutusData} key
     * @param {PlutusData} value
     * @returns {PlutusData | undefined}
     */
    insert(key: PlutusData, value: PlutusData): PlutusData | undefined;
    /**
     * @param {PlutusData} key
     * @returns {PlutusData | undefined}
     */
    get(key: PlutusData): PlutusData | undefined;
    /**
     * @returns {PlutusList}
     */
    keys(): PlutusList;
}
/** */
export class PlutusScript {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PlutusScript}
     */
    static from_bytes(bytes: Uint8Array): PlutusScript;
    /**
     *     * Creates a new Plutus script from the RAW bytes of the compiled script.
     *     * This does NOT include any CBOR encoding around these bytes (e.g. from "cborBytes" in cardano-cli)
     *     * If you creating this from those you should use PlutusScript::from_bytes() instead.
     *
     * @param {Uint8Array} bytes
     * @returns {PlutusScript}
     */
    static new(bytes: Uint8Array): PlutusScript;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {number} namespace
     * @returns {ScriptHash}
     */
    hash(namespace: number): ScriptHash;
    /**
     *     * The raw bytes of this compiled Plutus script.
     *     * If you need "cborBytes" for cardano-cli use PlutusScript::to_bytes() instead.
     *
     * @returns {Uint8Array}
     */
    bytes(): Uint8Array;
}
/** */
export class PlutusScripts {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PlutusScripts}
     */
    static from_bytes(bytes: Uint8Array): PlutusScripts;
    /**
     * @returns {PlutusScripts}
     */
    static new(): PlutusScripts;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {PlutusScript}
     */
    get(index: number): PlutusScript;
    /**
     * @param {PlutusScript} elem
     */
    add(elem: PlutusScript): void;
}
/** */
export class PlutusWitness {
    static __wrap(ptr: any): any;
    /**
     * Plutus V1 witness or witness where no script is attached and so version doesn't matter
     * @param {PlutusData} redeemer
     * @param {PlutusData | undefined} plutus_data
     * @param {PlutusScript | undefined} script
     * @returns {PlutusWitness}
     */
    static new(redeemer: PlutusData, plutus_data: PlutusData | undefined, script: PlutusScript | undefined): PlutusWitness;
    /**
     * @param {PlutusData} redeemer
     * @param {PlutusData | undefined} plutus_data
     * @param {PlutusScript | undefined} script
     * @returns {PlutusWitness}
     */
    static new_plutus_v2(redeemer: PlutusData, plutus_data: PlutusData | undefined, script: PlutusScript | undefined): PlutusWitness;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {PlutusData | undefined}
     */
    plutus_data(): PlutusData | undefined;
    /**
     * @returns {PlutusData}
     */
    redeemer(): PlutusData;
    /**
     * @returns {PlutusScript | undefined}
     */
    script(): PlutusScript | undefined;
    /**
     * @returns {number}
     */
    version(): number;
}
/** */
export class Pointer {
    static __wrap(ptr: any): any;
    /**
     * @param {BigNum} slot
     * @param {BigNum} tx_index
     * @param {BigNum} cert_index
     * @returns {Pointer}
     */
    static new(slot: BigNum, tx_index: BigNum, cert_index: BigNum): Pointer;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {BigNum}
     */
    slot(): BigNum;
    /**
     * @returns {BigNum}
     */
    tx_index(): BigNum;
    /**
     * @returns {BigNum}
     */
    cert_index(): BigNum;
}
/** */
export class PointerAddress {
    static __wrap(ptr: any): any;
    /**
     * @param {number} network
     * @param {StakeCredential} payment
     * @param {Pointer} stake
     * @returns {PointerAddress}
     */
    static new(network: number, payment: StakeCredential, stake: Pointer): PointerAddress;
    /**
     * @param {Address} addr
     * @returns {PointerAddress | undefined}
     */
    static from_address(addr: Address): PointerAddress | undefined;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {StakeCredential}
     */
    payment_cred(): StakeCredential;
    /**
     * @returns {Pointer}
     */
    stake_pointer(): Pointer;
    /**
     * @returns {Address}
     */
    to_address(): Address;
}
/** */
export class PoolMetadata {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PoolMetadata}
     */
    static from_bytes(bytes: Uint8Array): PoolMetadata;
    /**
     * @param {string} json
     * @returns {PoolMetadata}
     */
    static from_json(json: string): PoolMetadata;
    /**
     * @param {Url} url
     * @param {PoolMetadataHash} pool_metadata_hash
     * @returns {PoolMetadata}
     */
    static new(url: Url, pool_metadata_hash: PoolMetadataHash): PoolMetadata;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Url}
     */
    url(): Url;
    /**
     * @returns {PoolMetadataHash}
     */
    pool_metadata_hash(): PoolMetadataHash;
}
/** */
export class PoolMetadataHash {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PoolMetadataHash}
     */
    static from_bytes(bytes: Uint8Array): PoolMetadataHash;
    /**
     * @param {string} bech_str
     * @returns {PoolMetadataHash}
     */
    static from_bech32(bech_str: string): PoolMetadataHash;
    /**
     * @param {string} hex
     * @returns {PoolMetadataHash}
     */
    static from_hex(hex: string): PoolMetadataHash;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string} prefix
     * @returns {string}
     */
    to_bech32(prefix: string): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class PoolParams {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PoolParams}
     */
    static from_bytes(bytes: Uint8Array): PoolParams;
    /**
     * @param {string} json
     * @returns {PoolParams}
     */
    static from_json(json: string): PoolParams;
    /**
     * @param {Ed25519KeyHash} operator
     * @param {VRFKeyHash} vrf_keyhash
     * @param {BigNum} pledge
     * @param {BigNum} cost
     * @param {UnitInterval} margin
     * @param {RewardAddress} reward_account
     * @param {Ed25519KeyHashes} pool_owners
     * @param {Relays} relays
     * @param {PoolMetadata | undefined} pool_metadata
     * @returns {PoolParams}
     */
    static new(operator: Ed25519KeyHash, vrf_keyhash: VRFKeyHash, pledge: BigNum, cost: BigNum, margin: UnitInterval, reward_account: RewardAddress, pool_owners: Ed25519KeyHashes, relays: Relays, pool_metadata: PoolMetadata | undefined): PoolParams;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Ed25519KeyHash}
     */
    operator(): Ed25519KeyHash;
    /**
     * @returns {VRFKeyHash}
     */
    vrf_keyhash(): VRFKeyHash;
    /**
     * @returns {BigNum}
     */
    pledge(): BigNum;
    /**
     * @returns {BigNum}
     */
    cost(): BigNum;
    /**
     * @returns {UnitInterval}
     */
    margin(): UnitInterval;
    /**
     * @returns {RewardAddress}
     */
    reward_account(): RewardAddress;
    /**
     * @returns {Ed25519KeyHashes}
     */
    pool_owners(): Ed25519KeyHashes;
    /**
     * @returns {Relays}
     */
    relays(): Relays;
    /**
     * @returns {PoolMetadata | undefined}
     */
    pool_metadata(): PoolMetadata | undefined;
}
/** */
export class PoolRegistration {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PoolRegistration}
     */
    static from_bytes(bytes: Uint8Array): PoolRegistration;
    /**
     * @param {string} json
     * @returns {PoolRegistration}
     */
    static from_json(json: string): PoolRegistration;
    /**
     * @param {PoolParams} pool_params
     * @returns {PoolRegistration}
     */
    static new(pool_params: PoolParams): PoolRegistration;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {PoolParams}
     */
    pool_params(): PoolParams;
    /**
     * @param {boolean} update
     */
    set_is_update(update: boolean): void;
}
/** */
export class PoolRetirement {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PoolRetirement}
     */
    static from_bytes(bytes: Uint8Array): PoolRetirement;
    /**
     * @param {string} json
     * @returns {PoolRetirement}
     */
    static from_json(json: string): PoolRetirement;
    /**
     * @param {Ed25519KeyHash} pool_keyhash
     * @param {number} epoch
     * @returns {PoolRetirement}
     */
    static new(pool_keyhash: Ed25519KeyHash, epoch: number): PoolRetirement;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Ed25519KeyHash}
     */
    pool_keyhash(): Ed25519KeyHash;
    /**
     * @returns {number}
     */
    epoch(): number;
}
/** */
export class PoolVotingThresholds {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {PoolVotingThresholds}
     */
    static from_bytes(bytes: Uint8Array): PoolVotingThresholds;
    /**
     * @param {string} json
     * @returns {PoolVotingThresholds}
     */
    static from_json(json: string): PoolVotingThresholds;
    /**
     * @param {UnitInterval} motion_no_confidence
     * @param {UnitInterval} committee_normal
     * @param {UnitInterval} committee_no_confidence
     * @param {UnitInterval} hard_fork_initiation
     * @returns {PoolVotingThresholds}
     */
    static new(motion_no_confidence: UnitInterval, committee_normal: UnitInterval, committee_no_confidence: UnitInterval, hard_fork_initiation: UnitInterval): PoolVotingThresholds;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {UnitInterval}
     */
    motion_no_confidence(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    committee_normal(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    committee_no_confidence(): UnitInterval;
    /**
     * @returns {UnitInterval}
     */
    hard_fork_initiation(): UnitInterval;
}
/** */
export class PrivateKey {
    static __wrap(ptr: any): any;
    /**
     * @returns {PrivateKey}
     */
    static generate_ed25519(): PrivateKey;
    /**
     * @returns {PrivateKey}
     */
    static generate_ed25519extended(): PrivateKey;
    /**
     * Get private key from its bech32 representation
     * ```javascript
     * PrivateKey.from_bech32(&#39;ed25519_sk1ahfetf02qwwg4dkq7mgp4a25lx5vh9920cr5wnxmpzz9906qvm8qwvlts0&#39;);
     * ```
     * For an extended 25519 key
     * ```javascript
     * PrivateKey.from_bech32(&#39;ed25519e_sk1gqwl4szuwwh6d0yk3nsqcc6xxc3fpvjlevgwvt60df59v8zd8f8prazt8ln3lmz096ux3xvhhvm3ca9wj2yctdh3pnw0szrma07rt5gl748fp&#39;);
     * ```
     * @param {string} bech32_str
     * @returns {PrivateKey}
     */
    static from_bech32(bech32_str: string): PrivateKey;
    /**
     * @param {Uint8Array} bytes
     * @returns {PrivateKey}
     */
    static from_extended_bytes(bytes: Uint8Array): PrivateKey;
    /**
     * @param {Uint8Array} bytes
     * @returns {PrivateKey}
     */
    static from_normal_bytes(bytes: Uint8Array): PrivateKey;
    /**
     * @param {Uint8Array} bytes
     * @returns {PrivateKey}
     */
    static from_bytes(bytes: Uint8Array): PrivateKey;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {PublicKey}
     */
    to_public(): PublicKey;
    /**
     * @returns {string}
     */
    to_bech32(): string;
    /**
     * @returns {Uint8Array}
     */
    as_bytes(): Uint8Array;
    /**
     * @param {Uint8Array} message
     * @returns {Ed25519Signature}
     */
    sign(message: Uint8Array): Ed25519Signature;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
}
/** */
export class ProposalProcedure {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ProposalProcedure}
     */
    static from_bytes(bytes: Uint8Array): ProposalProcedure;
    /**
     * @param {string} json
     * @returns {ProposalProcedure}
     */
    static from_json(json: string): ProposalProcedure;
    /**
     * @param {BigNum} deposit
     * @param {ScriptHash} hash
     * @param {GovernanceAction} governance_action
     * @param {Anchor} anchor
     * @returns {ProposalProcedure}
     */
    static new(deposit: BigNum, hash: ScriptHash, governance_action: GovernanceAction, anchor: Anchor): ProposalProcedure;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {BigNum}
     */
    deposit(): BigNum;
    /**
     * @returns {ScriptHash}
     */
    hash(): ScriptHash;
    /**
     * @returns {GovernanceAction}
     */
    governance_action(): GovernanceAction;
    /**
     * @returns {Anchor}
     */
    anchor(): Anchor;
}
/** */
export class ProposalProcedures {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ProposalProcedures}
     */
    static from_bytes(bytes: Uint8Array): ProposalProcedures;
    /**
     * @returns {ProposalProcedures}
     */
    static new(): ProposalProcedures;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {ProposalProcedure}
     */
    get(index: number): ProposalProcedure;
    /**
     * @param {ProposalProcedure} elem
     */
    add(elem: ProposalProcedure): void;
}
/** */
export class ProposedProtocolParameterUpdates {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ProposedProtocolParameterUpdates}
     */
    static from_bytes(bytes: Uint8Array): ProposedProtocolParameterUpdates;
    /**
     * @param {string} json
     * @returns {ProposedProtocolParameterUpdates}
     */
    static from_json(json: string): ProposedProtocolParameterUpdates;
    /**
     * @returns {ProposedProtocolParameterUpdates}
     */
    static new(): ProposedProtocolParameterUpdates;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {GenesisHash} key
     * @param {ProtocolParamUpdate} value
     * @returns {ProtocolParamUpdate | undefined}
     */
    insert(key: GenesisHash, value: ProtocolParamUpdate): ProtocolParamUpdate | undefined;
    /**
     * @param {GenesisHash} key
     * @returns {ProtocolParamUpdate | undefined}
     */
    get(key: GenesisHash): ProtocolParamUpdate | undefined;
    /**
     * @returns {GenesisHashes}
     */
    keys(): GenesisHashes;
}
/** */
export class ProtocolParamUpdate {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ProtocolParamUpdate}
     */
    static from_bytes(bytes: Uint8Array): ProtocolParamUpdate;
    /**
     * @param {string} json
     * @returns {ProtocolParamUpdate}
     */
    static from_json(json: string): ProtocolParamUpdate;
    /**
     * @returns {ProtocolParamUpdate}
     */
    static new(): ProtocolParamUpdate;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @param {BigNum} minfee_a
     */
    set_minfee_a(minfee_a: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    minfee_a(): BigNum | undefined;
    /**
     * @param {BigNum} minfee_b
     */
    set_minfee_b(minfee_b: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    minfee_b(): BigNum | undefined;
    /**
     * @param {number} max_block_body_size
     */
    set_max_block_body_size(max_block_body_size: number): void;
    /**
     * @returns {number | undefined}
     */
    max_block_body_size(): number | undefined;
    /**
     * @param {number} max_tx_size
     */
    set_max_tx_size(max_tx_size: number): void;
    /**
     * @returns {number | undefined}
     */
    max_tx_size(): number | undefined;
    /**
     * @param {number} max_block_header_size
     */
    set_max_block_header_size(max_block_header_size: number): void;
    /**
     * @returns {number | undefined}
     */
    max_block_header_size(): number | undefined;
    /**
     * @param {BigNum} key_deposit
     */
    set_key_deposit(key_deposit: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    key_deposit(): BigNum | undefined;
    /**
     * @param {BigNum} pool_deposit
     */
    set_pool_deposit(pool_deposit: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    pool_deposit(): BigNum | undefined;
    /**
     * @param {number} max_epoch
     */
    set_max_epoch(max_epoch: number): void;
    /**
     * @returns {number | undefined}
     */
    max_epoch(): number | undefined;
    /**
     * @param {number} n_opt
     */
    set_n_opt(n_opt: number): void;
    /**
     * @returns {number | undefined}
     */
    n_opt(): number | undefined;
    /**
     * @param {UnitInterval} pool_pledge_influence
     */
    set_pool_pledge_influence(pool_pledge_influence: UnitInterval): void;
    /**
     * @returns {UnitInterval | undefined}
     */
    pool_pledge_influence(): UnitInterval | undefined;
    /**
     * @param {UnitInterval} expansion_rate
     */
    set_expansion_rate(expansion_rate: UnitInterval): void;
    /**
     * @returns {UnitInterval | undefined}
     */
    expansion_rate(): UnitInterval | undefined;
    /**
     * @param {UnitInterval} treasury_growth_rate
     */
    set_treasury_growth_rate(treasury_growth_rate: UnitInterval): void;
    /**
     * @returns {UnitInterval | undefined}
     */
    treasury_growth_rate(): UnitInterval | undefined;
    /**
     * @param {UnitInterval} d
     */
    set_d(d: UnitInterval): void;
    /**
     * @returns {UnitInterval | undefined}
     */
    d(): UnitInterval | undefined;
    /**
     * @param {Nonce} extra_entropy
     */
    set_extra_entropy(extra_entropy: Nonce): void;
    /**
     * @returns {Nonce | undefined}
     */
    extra_entropy(): Nonce | undefined;
    /**
     * @param {ProtocolVersion} protocol_version
     */
    set_protocol_version(protocol_version: ProtocolVersion): void;
    /**
     * @returns {ProtocolVersion | undefined}
     */
    protocol_version(): ProtocolVersion | undefined;
    /**
     * @param {BigNum} min_pool_cost
     */
    set_min_pool_cost(min_pool_cost: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    min_pool_cost(): BigNum | undefined;
    /**
     * @param {BigNum} ada_per_utxo_byte
     */
    set_ada_per_utxo_byte(ada_per_utxo_byte: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    ada_per_utxo_byte(): BigNum | undefined;
    /**
     * @param {Costmdls} cost_models
     */
    set_cost_models(cost_models: Costmdls): void;
    /**
     * @returns {Costmdls | undefined}
     */
    cost_models(): Costmdls | undefined;
    /**
     * @param {ExUnitPrices} execution_costs
     */
    set_execution_costs(execution_costs: ExUnitPrices): void;
    /**
     * @returns {ExUnitPrices | undefined}
     */
    execution_costs(): ExUnitPrices | undefined;
    /**
     * @param {ExUnits} max_tx_ex_units
     */
    set_max_tx_ex_units(max_tx_ex_units: ExUnits): void;
    /**
     * @returns {ExUnits | undefined}
     */
    max_tx_ex_units(): ExUnits | undefined;
    /**
     * @param {ExUnits} max_block_ex_units
     */
    set_max_block_ex_units(max_block_ex_units: ExUnits): void;
    /**
     * @returns {ExUnits | undefined}
     */
    max_block_ex_units(): ExUnits | undefined;
    /**
     * @param {number} max_value_size
     */
    set_max_value_size(max_value_size: number): void;
    /**
     * @returns {number | undefined}
     */
    max_value_size(): number | undefined;
    /**
     * @param {number} collateral_percentage
     */
    set_collateral_percentage(collateral_percentage: number): void;
    /**
     * @returns {number | undefined}
     */
    collateral_percentage(): number | undefined;
    /**
     * @param {number} max_collateral_inputs
     */
    set_max_collateral_inputs(max_collateral_inputs: number): void;
    /**
     * @returns {number | undefined}
     */
    max_collateral_inputs(): number | undefined;
    /**
     * @param {PoolVotingThresholds} pool_voting_thresholds
     */
    set_pool_voting_thresholds(pool_voting_thresholds: PoolVotingThresholds): void;
    /**
     * @returns {PoolVotingThresholds | undefined}
     */
    pool_voting_thresholds(): PoolVotingThresholds | undefined;
    /**
     * @param {DrepVotingThresholds} drep_voting_thresholds
     */
    set_drep_voting_thresholds(drep_voting_thresholds: DrepVotingThresholds): void;
    /**
     * @returns {DrepVotingThresholds | undefined}
     */
    drep_voting_thresholds(): DrepVotingThresholds | undefined;
    /**
     * @param {BigNum} min_committee_size
     */
    set_min_committee_size(min_committee_size: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    min_committee_size(): BigNum | undefined;
    /**
     * @param {BigNum} committee_term_limit
     */
    set_committee_term_limit(committee_term_limit: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    committee_term_limit(): BigNum | undefined;
    /**
     * @param {BigNum} governance_action_expiration
     */
    set_governance_action_expiration(governance_action_expiration: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    governance_action_expiration(): BigNum | undefined;
    /**
     * @param {BigNum} governance_action_deposit
     */
    set_governance_action_deposit(governance_action_deposit: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    governance_action_deposit(): BigNum | undefined;
    /**
     * @param {BigNum} drep_deposit
     */
    set_drep_deposit(drep_deposit: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    drep_deposit(): BigNum | undefined;
    /**
     * @param {number} drep_inactivity_period
     */
    set_drep_inactivity_period(drep_inactivity_period: number): void;
    /**
     * @returns {number | undefined}
     */
    drep_inactivity_period(): number | undefined;
}
/** */
export class ProtocolVersion {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ProtocolVersion}
     */
    static from_bytes(bytes: Uint8Array): ProtocolVersion;
    /**
     * @param {string} json
     * @returns {ProtocolVersion}
     */
    static from_json(json: string): ProtocolVersion;
    /**
     * @param {number} major
     * @param {number} minor
     * @returns {ProtocolVersion}
     */
    static new(major: number, minor: number): ProtocolVersion;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    major(): number;
    /**
     * @returns {number}
     */
    minor(): number;
}
/**
 * ED25519 key used as public key
 */
export class PublicKey {
    static __wrap(ptr: any): any;
    /**
     * Get public key from its bech32 representation
     * Example:
     * ```javascript
     * const pkey = PublicKey.from_bech32(&#39;ed25519_pk1dgaagyh470y66p899txcl3r0jaeaxu6yd7z2dxyk55qcycdml8gszkxze2&#39;);
     * ```
     * @param {string} bech32_str
     * @returns {PublicKey}
     */
    static from_bech32(bech32_str: string): PublicKey;
    /**
     * @param {Uint8Array} bytes
     * @returns {PublicKey}
     */
    static from_bytes(bytes: Uint8Array): PublicKey;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {string}
     */
    to_bech32(): string;
    /**
     * @returns {Uint8Array}
     */
    as_bytes(): Uint8Array;
    /**
     * @param {Uint8Array} data
     * @param {Ed25519Signature} signature
     * @returns {boolean}
     */
    verify(data: Uint8Array, signature: Ed25519Signature): boolean;
    /**
     * @returns {Ed25519KeyHash}
     */
    hash(): Ed25519KeyHash;
}
/** */
export class PublicKeys {
    static __wrap(ptr: any): any;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {number}
     */
    size(): number;
    /**
     * @param {number} index
     * @returns {PublicKey}
     */
    get(index: number): PublicKey;
    /**
     * @param {PublicKey} key
     */
    add(key: PublicKey): void;
}
/** */
export class Redeemer {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Redeemer}
     */
    static from_bytes(bytes: Uint8Array): Redeemer;
    /**
     * @param {RedeemerTag} tag
     * @param {BigNum} index
     * @param {PlutusData} data
     * @param {ExUnits} ex_units
     * @returns {Redeemer}
     */
    static new(tag: RedeemerTag, index: BigNum, data: PlutusData, ex_units: ExUnits): Redeemer;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {RedeemerTag}
     */
    tag(): RedeemerTag;
    /**
     * @returns {BigNum}
     */
    index(): BigNum;
    /**
     * @returns {PlutusData}
     */
    data(): PlutusData;
    /**
     * @returns {ExUnits}
     */
    ex_units(): ExUnits;
}
/** */
export class RedeemerTag {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {RedeemerTag}
     */
    static from_bytes(bytes: Uint8Array): RedeemerTag;
    /**
     * @returns {RedeemerTag}
     */
    static new_spend(): RedeemerTag;
    /**
     * @returns {RedeemerTag}
     */
    static new_mint(): RedeemerTag;
    /**
     * @returns {RedeemerTag}
     */
    static new_cert(): RedeemerTag;
    /**
     * @returns {RedeemerTag}
     */
    static new_reward(): RedeemerTag;
    /**
     * @returns {RedeemerTag}
     */
    static new_drep(): RedeemerTag;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    kind(): number;
}
/** */
export class RedeemerWitnessKey {
    static __wrap(ptr: any): any;
    /**
     * @param {RedeemerTag} tag
     * @param {BigNum} index
     * @returns {RedeemerWitnessKey}
     */
    static new(tag: RedeemerTag, index: BigNum): RedeemerWitnessKey;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {RedeemerTag}
     */
    tag(): RedeemerTag;
    /**
     * @returns {BigNum}
     */
    index(): BigNum;
}
/** */
export class Redeemers {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Redeemers}
     */
    static from_bytes(bytes: Uint8Array): Redeemers;
    /**
     * @returns {Redeemers}
     */
    static new(): Redeemers;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {Redeemer}
     */
    get(index: number): Redeemer;
    /**
     * @param {Redeemer} elem
     */
    add(elem: Redeemer): void;
}
/** */
export class RegCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {RegCert}
     */
    static from_bytes(bytes: Uint8Array): RegCert;
    /**
     * @param {string} json
     * @returns {RegCert}
     */
    static from_json(json: string): RegCert;
    /**
     * @param {StakeCredential} stake_credential
     * @param {BigNum} coin
     * @returns {RegCert}
     */
    static new(stake_credential: StakeCredential, coin: BigNum): RegCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {StakeCredential}
     */
    stake_credential(): StakeCredential;
    /**
     * @returns {BigNum}
     */
    coin(): BigNum;
}
/** */
export class RegCommitteeHotKeyCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {RegCommitteeHotKeyCert}
     */
    static from_bytes(bytes: Uint8Array): RegCommitteeHotKeyCert;
    /**
     * @param {string} json
     * @returns {RegCommitteeHotKeyCert}
     */
    static from_json(json: string): RegCommitteeHotKeyCert;
    /**
     * @param {Ed25519KeyHash} committee_cold_keyhash
     * @param {Ed25519KeyHash} committee_hot_keyhash
     * @returns {RegCommitteeHotKeyCert}
     */
    static new(committee_cold_keyhash: Ed25519KeyHash, committee_hot_keyhash: Ed25519KeyHash): RegCommitteeHotKeyCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Ed25519KeyHash}
     */
    committee_cold_keyhash(): Ed25519KeyHash;
    /**
     * @returns {Ed25519KeyHash}
     */
    committee_hot_keyhash(): Ed25519KeyHash;
}
/** */
export class RegDrepCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {RegDrepCert}
     */
    static from_bytes(bytes: Uint8Array): RegDrepCert;
    /**
     * @param {string} json
     * @returns {RegDrepCert}
     */
    static from_json(json: string): RegDrepCert;
    /**
     * @param {StakeCredential} voting_credential
     * @param {BigNum} coin
     * @returns {RegDrepCert}
     */
    static new(voting_credential: StakeCredential, coin: BigNum): RegDrepCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {StakeCredential}
     */
    voting_credential(): StakeCredential;
    /**
     * @returns {BigNum}
     */
    coin(): BigNum;
}
/** */
export class Relay {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Relay}
     */
    static from_bytes(bytes: Uint8Array): Relay;
    /**
     * @param {string} json
     * @returns {Relay}
     */
    static from_json(json: string): Relay;
    /**
     * @param {SingleHostAddr} single_host_addr
     * @returns {Relay}
     */
    static new_single_host_addr(single_host_addr: SingleHostAddr): Relay;
    /**
     * @param {SingleHostName} single_host_name
     * @returns {Relay}
     */
    static new_single_host_name(single_host_name: SingleHostName): Relay;
    /**
     * @param {MultiHostName} multi_host_name
     * @returns {Relay}
     */
    static new_multi_host_name(multi_host_name: MultiHostName): Relay;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {SingleHostAddr | undefined}
     */
    as_single_host_addr(): SingleHostAddr | undefined;
    /**
     * @returns {SingleHostName | undefined}
     */
    as_single_host_name(): SingleHostName | undefined;
    /**
     * @returns {MultiHostName | undefined}
     */
    as_multi_host_name(): MultiHostName | undefined;
}
/** */
export class Relays {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Relays}
     */
    static from_bytes(bytes: Uint8Array): Relays;
    /**
     * @param {string} json
     * @returns {Relays}
     */
    static from_json(json: string): Relays;
    /**
     * @returns {Relays}
     */
    static new(): Relays;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {Relay}
     */
    get(index: number): Relay;
    /**
     * @param {Relay} elem
     */
    add(elem: Relay): void;
}
/** */
export class RequiredWitnessSet {
    static __wrap(ptr: any): any;
    /**
     * @returns {RequiredWitnessSet}
     */
    static new(): RequiredWitnessSet;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @param {Vkeywitness} vkey
     */
    add_vkey(vkey: Vkeywitness): void;
    /**
     * @param {Vkey} vkey
     */
    add_vkey_key(vkey: Vkey): void;
    /**
     * @param {Ed25519KeyHash} hash
     */
    add_vkey_key_hash(hash: Ed25519KeyHash): void;
    /**
     * @param {BootstrapWitness} bootstrap
     */
    add_bootstrap(bootstrap: BootstrapWitness): void;
    /**
     * @param {Vkey} bootstrap
     */
    add_bootstrap_key(bootstrap: Vkey): void;
    /**
     * @param {Ed25519KeyHash} hash
     */
    add_bootstrap_key_hash(hash: Ed25519KeyHash): void;
    /**
     * @param {NativeScript} native_script
     */
    add_native_script(native_script: NativeScript): void;
    /**
     * @param {ScriptHash} native_script
     */
    add_native_script_hash(native_script: ScriptHash): void;
    /**
     * @param {PlutusScript} plutus_script
     */
    add_plutus_script(plutus_script: PlutusScript): void;
    /**
     * @param {PlutusScript} plutus_script
     */
    add_plutus_v2_script(plutus_script: PlutusScript): void;
    /**
     * @param {ScriptHash} plutus_script
     */
    add_plutus_hash(plutus_script: ScriptHash): void;
    /**
     * @param {PlutusData} plutus_datum
     */
    add_plutus_datum(plutus_datum: PlutusData): void;
    /**
     * @param {DataHash} plutus_datum
     */
    add_plutus_datum_hash(plutus_datum: DataHash): void;
    /**
     * @param {Redeemer} redeemer
     */
    add_redeemer(redeemer: Redeemer): void;
    /**
     * @param {RedeemerWitnessKey} redeemer
     */
    add_redeemer_tag(redeemer: RedeemerWitnessKey): void;
    /**
     * @param {RequiredWitnessSet} requirements
     */
    add_all(requirements: RequiredWitnessSet): void;
}
/** */
export class RewardAddress {
    static __wrap(ptr: any): any;
    /**
     * @param {number} network
     * @param {StakeCredential} payment
     * @returns {RewardAddress}
     */
    static new(network: number, payment: StakeCredential): RewardAddress;
    /**
     * @param {Address} addr
     * @returns {RewardAddress | undefined}
     */
    static from_address(addr: Address): RewardAddress | undefined;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {StakeCredential}
     */
    payment_cred(): StakeCredential;
    /**
     * @returns {Address}
     */
    to_address(): Address;
}
/** */
export class RewardAddresses {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {RewardAddresses}
     */
    static from_bytes(bytes: Uint8Array): RewardAddresses;
    /**
     * @param {string} json
     * @returns {RewardAddresses}
     */
    static from_json(json: string): RewardAddresses;
    /**
     * @returns {RewardAddresses}
     */
    static new(): RewardAddresses;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {RewardAddress}
     */
    get(index: number): RewardAddress;
    /**
     * @param {RewardAddress} elem
     */
    add(elem: RewardAddress): void;
}
/** */
export class Script {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Script}
     */
    static from_bytes(bytes: Uint8Array): Script;
    /**
     * @param {string} json
     * @returns {Script}
     */
    static from_json(json: string): Script;
    /**
     * @param {NativeScript} native_script
     * @returns {Script}
     */
    static new_native(native_script: NativeScript): Script;
    /**
     * @param {PlutusScript} plutus_script
     * @returns {Script}
     */
    static new_plutus_v1(plutus_script: PlutusScript): Script;
    /**
     * @param {PlutusScript} plutus_script
     * @returns {Script}
     */
    static new_plutus_v2(plutus_script: PlutusScript): Script;
    /**
     * @param {PlutusScript} plutus_script
     * @returns {Script}
     */
    static new_plutus_v3(plutus_script: PlutusScript): Script;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {NativeScript | undefined}
     */
    as_native(): NativeScript | undefined;
    /**
     * @returns {PlutusScript | undefined}
     */
    as_plutus_v1(): PlutusScript | undefined;
    /**
     * @returns {PlutusScript | undefined}
     */
    as_plutus_v2(): PlutusScript | undefined;
    /**
     * @returns {PlutusScript | undefined}
     */
    as_plutus_v3(): PlutusScript | undefined;
}
/** */
export class ScriptAll {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ScriptAll}
     */
    static from_bytes(bytes: Uint8Array): ScriptAll;
    /**
     * @param {string} json
     * @returns {ScriptAll}
     */
    static from_json(json: string): ScriptAll;
    /**
     * @param {NativeScripts} native_scripts
     * @returns {ScriptAll}
     */
    static new(native_scripts: NativeScripts): ScriptAll;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {NativeScripts}
     */
    native_scripts(): NativeScripts;
}
/** */
export class ScriptAny {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ScriptAny}
     */
    static from_bytes(bytes: Uint8Array): ScriptAny;
    /**
     * @param {string} json
     * @returns {ScriptAny}
     */
    static from_json(json: string): ScriptAny;
    /**
     * @param {NativeScripts} native_scripts
     * @returns {ScriptAny}
     */
    static new(native_scripts: NativeScripts): ScriptAny;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {NativeScripts}
     */
    native_scripts(): NativeScripts;
}
/** */
export class ScriptDataHash {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ScriptDataHash}
     */
    static from_bytes(bytes: Uint8Array): ScriptDataHash;
    /**
     * @param {string} bech_str
     * @returns {ScriptDataHash}
     */
    static from_bech32(bech_str: string): ScriptDataHash;
    /**
     * @param {string} hex
     * @returns {ScriptDataHash}
     */
    static from_hex(hex: string): ScriptDataHash;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string} prefix
     * @returns {string}
     */
    to_bech32(prefix: string): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class ScriptHash {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ScriptHash}
     */
    static from_bytes(bytes: Uint8Array): ScriptHash;
    /**
     * @param {string} bech_str
     * @returns {ScriptHash}
     */
    static from_bech32(bech_str: string): ScriptHash;
    /**
     * @param {string} hex
     * @returns {ScriptHash}
     */
    static from_hex(hex: string): ScriptHash;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string} prefix
     * @returns {string}
     */
    to_bech32(prefix: string): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class ScriptHashes {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ScriptHashes}
     */
    static from_bytes(bytes: Uint8Array): ScriptHashes;
    /**
     * @param {string} json
     * @returns {ScriptHashes}
     */
    static from_json(json: string): ScriptHashes;
    /**
     * @returns {ScriptHashes}
     */
    static new(): ScriptHashes;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {ScriptHash}
     */
    get(index: number): ScriptHash;
    /**
     * @param {ScriptHash} elem
     */
    add(elem: ScriptHash): void;
}
/** */
export class ScriptNOfK {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ScriptNOfK}
     */
    static from_bytes(bytes: Uint8Array): ScriptNOfK;
    /**
     * @param {string} json
     * @returns {ScriptNOfK}
     */
    static from_json(json: string): ScriptNOfK;
    /**
     * @param {number} n
     * @param {NativeScripts} native_scripts
     * @returns {ScriptNOfK}
     */
    static new(n: number, native_scripts: NativeScripts): ScriptNOfK;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    n(): number;
    /**
     * @returns {NativeScripts}
     */
    native_scripts(): NativeScripts;
}
/** */
export class ScriptPubkey {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ScriptPubkey}
     */
    static from_bytes(bytes: Uint8Array): ScriptPubkey;
    /**
     * @param {string} json
     * @returns {ScriptPubkey}
     */
    static from_json(json: string): ScriptPubkey;
    /**
     * @param {Ed25519KeyHash} addr_keyhash
     * @returns {ScriptPubkey}
     */
    static new(addr_keyhash: Ed25519KeyHash): ScriptPubkey;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Ed25519KeyHash}
     */
    addr_keyhash(): Ed25519KeyHash;
}
/** */
export class ScriptRef {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {ScriptRef}
     */
    static from_bytes(bytes: Uint8Array): ScriptRef;
    /**
     * @param {string} json
     * @returns {ScriptRef}
     */
    static from_json(json: string): ScriptRef;
    /**
     * @param {Script} script
     * @returns {ScriptRef}
     */
    static new(script: Script): ScriptRef;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Script}
     */
    get(): Script;
}
/** */
export class ScriptWitness {
    static __wrap(ptr: any): any;
    /**
     * @param {string} json
     * @returns {ScriptWitness}
     */
    static from_json(json: string): ScriptWitness;
    /**
     * @param {NativeScript} native_script
     * @returns {ScriptWitness}
     */
    static new_native_witness(native_script: NativeScript): ScriptWitness;
    /**
     * @param {PlutusWitness} plutus_witness
     * @returns {ScriptWitness}
     */
    static new_plutus_witness(plutus_witness: PlutusWitness): ScriptWitness;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {NativeScript | undefined}
     */
    as_native_witness(): NativeScript | undefined;
    /**
     * @returns {PlutusWitness | undefined}
     */
    as_plutus_witness(): PlutusWitness | undefined;
}
/** */
export class SingleHostAddr {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {SingleHostAddr}
     */
    static from_bytes(bytes: Uint8Array): SingleHostAddr;
    /**
     * @param {string} json
     * @returns {SingleHostAddr}
     */
    static from_json(json: string): SingleHostAddr;
    /**
     * @param {number | undefined} port
     * @param {Ipv4 | undefined} ipv4
     * @param {Ipv6 | undefined} ipv6
     * @returns {SingleHostAddr}
     */
    static new(port: number | undefined, ipv4: Ipv4 | undefined, ipv6: Ipv6 | undefined): SingleHostAddr;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number | undefined}
     */
    port(): number | undefined;
    /**
     * @returns {Ipv4 | undefined}
     */
    ipv4(): Ipv4 | undefined;
    /**
     * @returns {Ipv6 | undefined}
     */
    ipv6(): Ipv6 | undefined;
}
/** */
export class SingleHostName {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {SingleHostName}
     */
    static from_bytes(bytes: Uint8Array): SingleHostName;
    /**
     * @param {string} json
     * @returns {SingleHostName}
     */
    static from_json(json: string): SingleHostName;
    /**
     * @param {number | undefined} port
     * @param {DNSRecordAorAAAA} dns_name
     * @returns {SingleHostName}
     */
    static new(port: number | undefined, dns_name: DNSRecordAorAAAA): SingleHostName;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number | undefined}
     */
    port(): number | undefined;
    /**
     * @returns {DNSRecordAorAAAA}
     */
    dns_name(): DNSRecordAorAAAA;
}
/** */
export class StakeCredential {
    static __wrap(ptr: any): any;
    /**
     * @param {Ed25519KeyHash} hash
     * @returns {StakeCredential}
     */
    static from_keyhash(hash: Ed25519KeyHash): StakeCredential;
    /**
     * @param {ScriptHash} hash
     * @returns {StakeCredential}
     */
    static from_scripthash(hash: ScriptHash): StakeCredential;
    /**
     * @param {Uint8Array} bytes
     * @returns {StakeCredential}
     */
    static from_bytes(bytes: Uint8Array): StakeCredential;
    /**
     * @param {string} json
     * @returns {StakeCredential}
     */
    static from_json(json: string): StakeCredential;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Ed25519KeyHash | undefined}
     */
    to_keyhash(): Ed25519KeyHash | undefined;
    /**
     * @returns {ScriptHash | undefined}
     */
    to_scripthash(): ScriptHash | undefined;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
}
/** */
export class StakeCredentials {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {StakeCredentials}
     */
    static from_bytes(bytes: Uint8Array): StakeCredentials;
    /**
     * @param {string} json
     * @returns {StakeCredentials}
     */
    static from_json(json: string): StakeCredentials;
    /**
     * @returns {StakeCredentials}
     */
    static new(): StakeCredentials;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {StakeCredential}
     */
    get(index: number): StakeCredential;
    /**
     * @param {StakeCredential} elem
     */
    add(elem: StakeCredential): void;
}
/** */
export class StakeDelegation {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {StakeDelegation}
     */
    static from_bytes(bytes: Uint8Array): StakeDelegation;
    /**
     * @param {string} json
     * @returns {StakeDelegation}
     */
    static from_json(json: string): StakeDelegation;
    /**
     * @param {StakeCredential} stake_credential
     * @param {Ed25519KeyHash} pool_keyhash
     * @returns {StakeDelegation}
     */
    static new(stake_credential: StakeCredential, pool_keyhash: Ed25519KeyHash): StakeDelegation;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {StakeCredential}
     */
    stake_credential(): StakeCredential;
    /**
     * @returns {Ed25519KeyHash}
     */
    pool_keyhash(): Ed25519KeyHash;
}
/** */
export class StakeDeregistration {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {StakeDeregistration}
     */
    static from_bytes(bytes: Uint8Array): StakeDeregistration;
    /**
     * @param {string} json
     * @returns {StakeDeregistration}
     */
    static from_json(json: string): StakeDeregistration;
    /**
     * @param {StakeCredential} stake_credential
     * @returns {StakeDeregistration}
     */
    static new(stake_credential: StakeCredential): StakeDeregistration;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {StakeCredential}
     */
    stake_credential(): StakeCredential;
}
/** */
export class StakeRegDelegCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {StakeRegDelegCert}
     */
    static from_bytes(bytes: Uint8Array): StakeRegDelegCert;
    /**
     * @param {string} json
     * @returns {StakeRegDelegCert}
     */
    static from_json(json: string): StakeRegDelegCert;
    /**
     * @param {StakeCredential} stake_credential
     * @param {Ed25519KeyHash} pool_keyhash
     * @param {BigNum} coin
     * @returns {StakeRegDelegCert}
     */
    static new(stake_credential: StakeCredential, pool_keyhash: Ed25519KeyHash, coin: BigNum): StakeRegDelegCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {StakeCredential}
     */
    stake_credential(): StakeCredential;
    /**
     * @returns {Ed25519KeyHash}
     */
    pool_keyhash(): Ed25519KeyHash;
    /**
     * @returns {BigNum}
     */
    coin(): BigNum;
}
/** */
export class StakeRegistration {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {StakeRegistration}
     */
    static from_bytes(bytes: Uint8Array): StakeRegistration;
    /**
     * @param {string} json
     * @returns {StakeRegistration}
     */
    static from_json(json: string): StakeRegistration;
    /**
     * @param {StakeCredential} stake_credential
     * @returns {StakeRegistration}
     */
    static new(stake_credential: StakeCredential): StakeRegistration;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {StakeCredential}
     */
    stake_credential(): StakeCredential;
}
/** */
export class StakeVoteDelegCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {StakeVoteDelegCert}
     */
    static from_bytes(bytes: Uint8Array): StakeVoteDelegCert;
    /**
     * @param {string} json
     * @returns {StakeVoteDelegCert}
     */
    static from_json(json: string): StakeVoteDelegCert;
    /**
     * @param {StakeCredential} stake_credential
     * @param {Ed25519KeyHash} pool_keyhash
     * @param {Drep} drep
     * @returns {StakeVoteDelegCert}
     */
    static new(stake_credential: StakeCredential, pool_keyhash: Ed25519KeyHash, drep: Drep): StakeVoteDelegCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {StakeCredential}
     */
    stake_credential(): StakeCredential;
    /**
     * @returns {Ed25519KeyHash}
     */
    pool_keyhash(): Ed25519KeyHash;
    /**
     * @returns {Drep}
     */
    drep(): Drep;
}
/** */
export class StakeVoteRegDelegCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {StakeVoteRegDelegCert}
     */
    static from_bytes(bytes: Uint8Array): StakeVoteRegDelegCert;
    /**
     * @param {string} json
     * @returns {StakeVoteRegDelegCert}
     */
    static from_json(json: string): StakeVoteRegDelegCert;
    /**
     * @param {StakeCredential} stake_credential
     * @param {Ed25519KeyHash} pool_keyhash
     * @param {Drep} drep
     * @param {BigNum} coin
     * @returns {StakeVoteRegDelegCert}
     */
    static new(stake_credential: StakeCredential, pool_keyhash: Ed25519KeyHash, drep: Drep, coin: BigNum): StakeVoteRegDelegCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {StakeCredential}
     */
    stake_credential(): StakeCredential;
    /**
     * @returns {Ed25519KeyHash}
     */
    pool_keyhash(): Ed25519KeyHash;
    /**
     * @returns {Drep}
     */
    drep(): Drep;
    /**
     * @returns {BigNum}
     */
    coin(): BigNum;
}
/** */
export class Strings {
    static __wrap(ptr: any): any;
    /**
     * @returns {Strings}
     */
    static new(): Strings;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {string}
     */
    get(index: number): string;
    /**
     * @param {string} elem
     */
    add(elem: string): void;
}
/** */
export class TimelockExpiry {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TimelockExpiry}
     */
    static from_bytes(bytes: Uint8Array): TimelockExpiry;
    /**
     * @param {string} json
     * @returns {TimelockExpiry}
     */
    static from_json(json: string): TimelockExpiry;
    /**
     * @param {BigNum} slot
     * @returns {TimelockExpiry}
     */
    static new(slot: BigNum): TimelockExpiry;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {BigNum}
     */
    slot(): BigNum;
}
/** */
export class TimelockStart {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TimelockStart}
     */
    static from_bytes(bytes: Uint8Array): TimelockStart;
    /**
     * @param {string} json
     * @returns {TimelockStart}
     */
    static from_json(json: string): TimelockStart;
    /**
     * @param {BigNum} slot
     * @returns {TimelockStart}
     */
    static new(slot: BigNum): TimelockStart;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {BigNum}
     */
    slot(): BigNum;
}
/** */
export class Transaction {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Transaction}
     */
    static from_bytes(bytes: Uint8Array): Transaction;
    /**
     * @param {string} json
     * @returns {Transaction}
     */
    static from_json(json: string): Transaction;
    /**
     * @param {TransactionBody} body
     * @param {TransactionWitnessSet} witness_set
     * @param {AuxiliaryData | undefined} auxiliary_data
     * @returns {Transaction}
     */
    static new(body: TransactionBody, witness_set: TransactionWitnessSet, auxiliary_data: AuxiliaryData | undefined): Transaction;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {TransactionBody}
     */
    body(): TransactionBody;
    /**
     * @returns {TransactionWitnessSet}
     */
    witness_set(): TransactionWitnessSet;
    /**
     * @returns {boolean}
     */
    is_valid(): boolean;
    /**
     * @returns {AuxiliaryData | undefined}
     */
    auxiliary_data(): AuxiliaryData | undefined;
    /**
     * @param {boolean} valid
     */
    set_is_valid(valid: boolean): void;
}
/** */
export class TransactionBodies {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionBodies}
     */
    static from_bytes(bytes: Uint8Array): TransactionBodies;
    /**
     * @param {string} json
     * @returns {TransactionBodies}
     */
    static from_json(json: string): TransactionBodies;
    /**
     * @returns {TransactionBodies}
     */
    static new(): TransactionBodies;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {TransactionBody}
     */
    get(index: number): TransactionBody;
    /**
     * @param {TransactionBody} elem
     */
    add(elem: TransactionBody): void;
}
/** */
export class TransactionBody {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionBody}
     */
    static from_bytes(bytes: Uint8Array): TransactionBody;
    /**
     * @param {string} json
     * @returns {TransactionBody}
     */
    static from_json(json: string): TransactionBody;
    /**
     * @param {TransactionInputs} inputs
     * @param {TransactionOutputs} outputs
     * @param {BigNum} fee
     * @param {BigNum | undefined} ttl
     * @returns {TransactionBody}
     */
    static new(inputs: TransactionInputs, outputs: TransactionOutputs, fee: BigNum, ttl: BigNum | undefined): TransactionBody;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {TransactionInputs}
     */
    inputs(): TransactionInputs;
    /**
     * @returns {TransactionOutputs}
     */
    outputs(): TransactionOutputs;
    /**
     * @returns {BigNum}
     */
    fee(): BigNum;
    /**
     * @returns {BigNum | undefined}
     */
    ttl(): BigNum | undefined;
    /**
     * @param {Certificates} certs
     */
    set_certs(certs: Certificates): void;
    /**
     * @returns {Certificates | undefined}
     */
    certs(): Certificates | undefined;
    /**
     * @param {Withdrawals} withdrawals
     */
    set_withdrawals(withdrawals: Withdrawals): void;
    /**
     * @returns {Withdrawals | undefined}
     */
    withdrawals(): Withdrawals | undefined;
    /**
     * @param {Update} update
     */
    set_update(update: Update): void;
    /**
     * @returns {Update | undefined}
     */
    update(): Update | undefined;
    /**
     * @returns {VotingProcedures | undefined}
     */
    voting_procedures(): VotingProcedures | undefined;
    /**
     * @returns {ProposalProcedures | undefined}
     */
    proposal_procedures(): ProposalProcedures | undefined;
    /**
     * @param {AuxiliaryDataHash} auxiliary_data_hash
     */
    set_auxiliary_data_hash(auxiliary_data_hash: AuxiliaryDataHash): void;
    /**
     * @returns {AuxiliaryDataHash | undefined}
     */
    auxiliary_data_hash(): AuxiliaryDataHash | undefined;
    /**
     * @param {BigNum} validity_start_interval
     */
    set_validity_start_interval(validity_start_interval: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    validity_start_interval(): BigNum | undefined;
    /**
     * @param {Mint} mint
     */
    set_mint(mint: Mint): void;
    /**
     * @returns {Mint | undefined}
     */
    mint(): Mint | undefined;
    /**
     * @param {ScriptDataHash} script_data_hash
     */
    set_script_data_hash(script_data_hash: ScriptDataHash): void;
    /**
     * @returns {ScriptDataHash | undefined}
     */
    script_data_hash(): ScriptDataHash | undefined;
    /**
     * @param {TransactionInputs} collateral
     */
    set_collateral(collateral: TransactionInputs): void;
    /**
     * @returns {TransactionInputs | undefined}
     */
    collateral(): TransactionInputs | undefined;
    /**
     * @param {Ed25519KeyHashes} required_signers
     */
    set_required_signers(required_signers: Ed25519KeyHashes): void;
    /**
     * @returns {Ed25519KeyHashes | undefined}
     */
    required_signers(): Ed25519KeyHashes | undefined;
    /**
     * @param {NetworkId} network_id
     */
    set_network_id(network_id: NetworkId): void;
    /**
     * @returns {NetworkId | undefined}
     */
    network_id(): NetworkId | undefined;
    /**
     * @param {TransactionOutput} collateral_return
     */
    set_collateral_return(collateral_return: TransactionOutput): void;
    /**
     * @returns {TransactionOutput | undefined}
     */
    collateral_return(): TransactionOutput | undefined;
    /**
     * @param {BigNum} total_collateral
     */
    set_total_collateral(total_collateral: BigNum): void;
    /**
     * @returns {BigNum | undefined}
     */
    total_collateral(): BigNum | undefined;
    /**
     * @param {TransactionInputs} reference_inputs
     */
    set_reference_inputs(reference_inputs: TransactionInputs): void;
    /**
     * @returns {TransactionInputs | undefined}
     */
    reference_inputs(): TransactionInputs | undefined;
    /**
     * @param {VotingProcedures} voting_procedures
     */
    set_voting_procedures(voting_procedures: VotingProcedures): void;
    /**
     * @param {ProposalProcedures} proposal_procedures
     */
    set_proposal_procedures(proposal_procedures: ProposalProcedures): void;
    /**
     * @returns {Uint8Array | undefined}
     */
    raw(): Uint8Array | undefined;
}
/** */
export class TransactionBuilder {
    static __wrap(ptr: any): any;
    /**
     * @param {TransactionBuilderConfig} cfg
     * @returns {TransactionBuilder}
     */
    static new(cfg: TransactionBuilderConfig): TransactionBuilder;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * This automatically selects and adds inputs from {inputs} consisting of just enough to cover
     * the outputs that have already been added.
     * This should be called after adding all certs/outputs/etc and will be an error otherwise.
     * Adding a change output must be called after via TransactionBuilder::balance()
     * inputs to cover the minimum fees. This does not, however, set the txbuilder's fee.
     *
     * change_address is required here in order to determine the min ada requirement precisely
     * @param {TransactionUnspentOutputs} inputs
     * @param {Address} change_address
     * @param {Uint32Array} weights
     */
    add_inputs_from(inputs: TransactionUnspentOutputs, change_address: Address, weights: Uint32Array): void;
    /**
     * @param {TransactionUnspentOutput} utxo
     * @param {ScriptWitness | undefined} script_witness
     */
    add_input(utxo: TransactionUnspentOutput, script_witness: ScriptWitness | undefined): void;
    /**
     * @param {TransactionUnspentOutput} utxo
     */
    add_reference_input(utxo: TransactionUnspentOutput): void;
    /**
     * calculates how much the fee would increase if you added a given output
     * @param {Address} address
     * @param {TransactionInput} input
     * @param {Value} amount
     * @returns {BigNum}
     */
    fee_for_input(address: Address, input: TransactionInput, amount: Value): BigNum;
    /**
     * Add explicit output via a TransactionOutput object
     * @param {TransactionOutput} output
     */
    add_output(output: TransactionOutput): void;
    /**
     * Add plutus scripts via a PlutusScripts object
     * @param {PlutusScript} plutus_script
     */
    add_plutus_script(plutus_script: PlutusScript): void;
    /**
     * Add plutus v2 scripts via a PlutusScripts object
     * @param {PlutusScript} plutus_script
     */
    add_plutus_v2_script(plutus_script: PlutusScript): void;
    /**
     * Add plutus data via a PlutusData object
     * @param {PlutusData} plutus_data
     */
    add_plutus_data(plutus_data: PlutusData): void;
    /**
     * Add native scripts via a NativeScripts object
     * @param {NativeScript} native_script
     */
    add_native_script(native_script: NativeScript): void;
    /**
     * Add certificate via a Certificates object
     * @param {Certificate} certificate
     * @param {ScriptWitness | undefined} script_witness
     */
    add_certificate(certificate: Certificate, script_witness: ScriptWitness | undefined): void;
    /**
     * calculates how much the fee would increase if you added a given output
     * @param {TransactionOutput} output
     * @returns {BigNum}
     */
    fee_for_output(output: TransactionOutput): BigNum;
    /**
     * @param {BigNum} ttl
     */
    set_ttl(ttl: BigNum): void;
    /**
     * @param {BigNum} validity_start_interval
     */
    set_validity_start_interval(validity_start_interval: BigNum): void;
    /**
     * @param {RewardAddress} reward_address
     * @param {BigNum} coin
     * @param {ScriptWitness | undefined} script_witness
     */
    add_withdrawal(reward_address: RewardAddress, coin: BigNum, script_witness: ScriptWitness | undefined): void;
    /**
     * @returns {AuxiliaryData | undefined}
     */
    auxiliary_data(): AuxiliaryData | undefined;
    /**
     * Set explicit auxiliary data via an AuxiliaryData object
     * It might contain some metadata plus native or Plutus scripts
     * @param {AuxiliaryData} auxiliary_data
     */
    set_auxiliary_data(auxiliary_data: AuxiliaryData): void;
    /**
     * Set metadata using a GeneralTransactionMetadata object
     * It will be set to the existing or new auxiliary data in this builder
     * @param {GeneralTransactionMetadata} metadata
     */
    set_metadata(metadata: GeneralTransactionMetadata): void;
    /**
     * Add a single metadatum using TransactionMetadatumLabel and TransactionMetadatum objects
     * It will be securely added to existing or new metadata in this builder
     * @param {BigNum} key
     * @param {TransactionMetadatum} val
     */
    add_metadatum(key: BigNum, val: TransactionMetadatum): void;
    /**
     * Add a single JSON metadatum using a TransactionMetadatumLabel and a String
     * It will be securely added to existing or new metadata in this builder
     * @param {BigNum} key
     * @param {string} val
     */
    add_json_metadatum(key: BigNum, val: string): void;
    /**
     * Add a single JSON metadatum using a TransactionMetadatumLabel, a String, and a MetadataJsonSchema object
     * It will be securely added to existing or new metadata in this builder
     * @param {BigNum} key
     * @param {string} val
     * @param {number} schema
     */
    add_json_metadatum_with_schema(key: BigNum, val: string, schema: number): void;
    /**
     * Returns a copy of the current mint state in the builder
     * @returns {Mint | undefined}
     */
    mint(): Mint | undefined;
    /**
     * @returns {Certificates | undefined}
     */
    certificates(): Certificates | undefined;
    /**
     * @returns {Withdrawals | undefined}
     */
    withdrawals(): Withdrawals | undefined;
    /**
     * Returns a copy of the current witness native scripts in the builder
     * @returns {NativeScripts | undefined}
     */
    native_scripts(): NativeScripts | undefined;
    /**
     * Add a mint entry to this builder using a PolicyID and MintAssets object
     * It will be securely added to existing or new Mint in this builder
     * It will securely add assets to an existing PolicyID
     * But it will replace/overwrite any existing mint assets with the same PolicyID
     * first redeemer applied to a PolicyID is taken for all further assets added to the same PolicyID
     * @param {ScriptHash} policy_id
     * @param {MintAssets} mint_assets
     * @param {ScriptWitness | undefined} script_witness
     */
    add_mint(policy_id: ScriptHash, mint_assets: MintAssets, script_witness: ScriptWitness | undefined): void;
    /**
     * @returns {ScriptDataHash | undefined}
     */
    script_data_hash(): ScriptDataHash | undefined;
    /**
     * @param {TransactionUnspentOutput} utxo
     */
    add_collateral(utxo: TransactionUnspentOutput): void;
    /**
     * @returns {TransactionInputs | undefined}
     */
    get_collateral(): TransactionInputs | undefined;
    /**
     * @param {Ed25519KeyHash} required_signer
     */
    add_required_signer(required_signer: Ed25519KeyHash): void;
    /**
     * @returns {Ed25519KeyHashes | undefined}
     */
    required_signers(): Ed25519KeyHashes | undefined;
    /**
     * @param {NetworkId} network_id
     */
    set_network_id(network_id: NetworkId): void;
    /**
     * @returns {NetworkId | undefined}
     */
    network_id(): NetworkId | undefined;
    /**
     * @returns {Redeemers | undefined}
     */
    redeemers(): Redeemers | undefined;
    /**
     * does not include refunds or withdrawals
     * @returns {Value}
     */
    get_explicit_input(): Value;
    /**
     * withdrawals and refunds
     * @returns {Value}
     */
    get_implicit_input(): Value;
    /**
     * Return explicit input plus implicit input plus mint
     * @returns {Value}
     */
    get_total_input(): Value;
    /**
     * Return explicit output plus implicit output plus burn (does not consider fee directly)
     * @returns {Value}
     */
    get_total_output(): Value;
    /**
     * does not include fee
     * @returns {Value}
     */
    get_explicit_output(): Value;
    /**
     * @returns {BigNum}
     */
    get_deposit(): BigNum;
    /**
     * @returns {BigNum | undefined}
     */
    get_fee_if_set(): BigNum | undefined;
    /**
     * Warning: this function will mutate the /fee/ field
     * Make sure to call this function last after setting all other tx-body properties
     * Editing inputs, outputs, mint, etc. after change been calculated
     * might cause a mismatch in calculated fee versus the required fee
     * @param {Address} change_address
     * @param {Datum | undefined} datum
     */
    balance(change_address: Address, datum: Datum | undefined): void;
    /**
     * Returns the TransactionBody.
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    full_size(): number;
    /**
     * @returns {Uint32Array}
     */
    output_sizes(): Uint32Array;
    /**
     * @returns {TransactionOutputs}
     */
    outputs(): TransactionOutputs;
    /**
     * Returns full Transaction object with the body and the auxiliary data
     *
     * NOTE: witness_set will contain all mint_scripts if any been added or set
     *
     * takes fetched ex units into consideration
     *
     * add collateral utxos and collateral change receiver in case you redeem from plutus script utxos
     *
     * async call
     *
     * NOTE: is_valid set to true
     * @param {TransactionUnspentOutputs | undefined} collateral_utxos
     * @param {Address | undefined} collateral_change_address
     * @param {boolean | undefined} native_uplc
     * @returns {Promise<Transaction>}
     */
    construct(collateral_utxos: TransactionUnspentOutputs | undefined, collateral_change_address: Address | undefined, native_uplc: boolean | undefined): Promise<Transaction>;
    /**
     * Returns full Transaction object with the body and the auxiliary data
     * NOTE: witness_set will contain all mint_scripts if any been added or set
     * NOTE: is_valid set to true
     * @returns {Transaction}
     */
    build_tx(): Transaction;
    /**
     * warning: sum of all parts of a transaction must equal 0. You cannot just set the fee to the min value and forget about it
     * warning: min_fee may be slightly larger than the actual minimum fee (ex: a few lovelaces)
     * this is done to simplify the library code, but can be fixed later
     * @returns {BigNum}
     */
    min_fee(): BigNum;
}
/** */
export class TransactionBuilderConfig {
    static __wrap(ptr: any): any;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
}
/** */
export class TransactionBuilderConfigBuilder {
    static __wrap(ptr: any): any;
    /**
     * @returns {TransactionBuilderConfigBuilder}
     */
    static new(): TransactionBuilderConfigBuilder;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @param {LinearFee} fee_algo
     * @returns {TransactionBuilderConfigBuilder}
     */
    fee_algo(fee_algo: LinearFee): TransactionBuilderConfigBuilder;
    /**
     * @param {BigNum} coins_per_utxo_byte
     * @returns {TransactionBuilderConfigBuilder}
     */
    coins_per_utxo_byte(coins_per_utxo_byte: BigNum): TransactionBuilderConfigBuilder;
    /**
     * @param {BigNum} pool_deposit
     * @returns {TransactionBuilderConfigBuilder}
     */
    pool_deposit(pool_deposit: BigNum): TransactionBuilderConfigBuilder;
    /**
     * @param {BigNum} key_deposit
     * @returns {TransactionBuilderConfigBuilder}
     */
    key_deposit(key_deposit: BigNum): TransactionBuilderConfigBuilder;
    /**
     * @param {number} max_value_size
     * @returns {TransactionBuilderConfigBuilder}
     */
    max_value_size(max_value_size: number): TransactionBuilderConfigBuilder;
    /**
     * @param {number} max_tx_size
     * @returns {TransactionBuilderConfigBuilder}
     */
    max_tx_size(max_tx_size: number): TransactionBuilderConfigBuilder;
    /**
     * @param {ExUnitPrices} ex_unit_prices
     * @returns {TransactionBuilderConfigBuilder}
     */
    ex_unit_prices(ex_unit_prices: ExUnitPrices): TransactionBuilderConfigBuilder;
    /**
     * @param {ExUnits} max_tx_ex_units
     * @returns {TransactionBuilderConfigBuilder}
     */
    max_tx_ex_units(max_tx_ex_units: ExUnits): TransactionBuilderConfigBuilder;
    /**
     * @param {Costmdls} costmdls
     * @returns {TransactionBuilderConfigBuilder}
     */
    costmdls(costmdls: Costmdls): TransactionBuilderConfigBuilder;
    /**
     * @param {number} collateral_percentage
     * @returns {TransactionBuilderConfigBuilder}
     */
    collateral_percentage(collateral_percentage: number): TransactionBuilderConfigBuilder;
    /**
     * @param {number} max_collateral_inputs
     * @returns {TransactionBuilderConfigBuilder}
     */
    max_collateral_inputs(max_collateral_inputs: number): TransactionBuilderConfigBuilder;
    /**
     * @param {BigNum} zero_time
     * @param {BigNum} zero_slot
     * @param {number} slot_length
     * @returns {TransactionBuilderConfigBuilder}
     */
    slot_config(zero_time: BigNum, zero_slot: BigNum, slot_length: number): TransactionBuilderConfigBuilder;
    /**
     * @param {Blockfrost} blockfrost
     * @returns {TransactionBuilderConfigBuilder}
     */
    blockfrost(blockfrost: Blockfrost): TransactionBuilderConfigBuilder;
    /**
     * @returns {TransactionBuilderConfig}
     */
    build(): TransactionBuilderConfig;
}
/** */
export class TransactionHash {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionHash}
     */
    static from_bytes(bytes: Uint8Array): TransactionHash;
    /**
     * @param {string} bech_str
     * @returns {TransactionHash}
     */
    static from_bech32(bech_str: string): TransactionHash;
    /**
     * @param {string} hex
     * @returns {TransactionHash}
     */
    static from_hex(hex: string): TransactionHash;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string} prefix
     * @returns {string}
     */
    to_bech32(prefix: string): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class TransactionIndexes {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionIndexes}
     */
    static from_bytes(bytes: Uint8Array): TransactionIndexes;
    /**
     * @returns {TransactionIndexes}
     */
    static new(): TransactionIndexes;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {BigNum}
     */
    get(index: number): BigNum;
    /**
     * @param {BigNum} elem
     */
    add(elem: BigNum): void;
}
/** */
export class TransactionInput {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionInput}
     */
    static from_bytes(bytes: Uint8Array): TransactionInput;
    /**
     * @param {string} json
     * @returns {TransactionInput}
     */
    static from_json(json: string): TransactionInput;
    /**
     * @param {TransactionHash} transaction_id
     * @param {BigNum} index
     * @returns {TransactionInput}
     */
    static new(transaction_id: TransactionHash, index: BigNum): TransactionInput;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {TransactionHash}
     */
    transaction_id(): TransactionHash;
    /**
     * @returns {BigNum}
     */
    index(): BigNum;
}
/** */
export class TransactionInputs {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionInputs}
     */
    static from_bytes(bytes: Uint8Array): TransactionInputs;
    /**
     * @param {string} json
     * @returns {TransactionInputs}
     */
    static from_json(json: string): TransactionInputs;
    /**
     * @returns {TransactionInputs}
     */
    static new(): TransactionInputs;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {TransactionInput}
     */
    get(index: number): TransactionInput;
    /**
     * @param {TransactionInput} elem
     */
    add(elem: TransactionInput): void;
    /** */
    sort(): void;
}
/** */
export class TransactionMetadatum {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionMetadatum}
     */
    static from_bytes(bytes: Uint8Array): TransactionMetadatum;
    /**
     * @param {MetadataMap} map
     * @returns {TransactionMetadatum}
     */
    static new_map(map: MetadataMap): TransactionMetadatum;
    /**
     * @param {MetadataList} list
     * @returns {TransactionMetadatum}
     */
    static new_list(list: MetadataList): TransactionMetadatum;
    /**
     * @param {Int} int
     * @returns {TransactionMetadatum}
     */
    static new_int(int: Int): TransactionMetadatum;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionMetadatum}
     */
    static new_bytes(bytes: Uint8Array): TransactionMetadatum;
    /**
     * @param {string} text
     * @returns {TransactionMetadatum}
     */
    static new_text(text: string): TransactionMetadatum;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {MetadataMap}
     */
    as_map(): MetadataMap;
    /**
     * @returns {MetadataList}
     */
    as_list(): MetadataList;
    /**
     * @returns {Int}
     */
    as_int(): Int;
    /**
     * @returns {Uint8Array}
     */
    as_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    as_text(): string;
}
/** */
export class TransactionMetadatumLabels {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionMetadatumLabels}
     */
    static from_bytes(bytes: Uint8Array): TransactionMetadatumLabels;
    /**
     * @returns {TransactionMetadatumLabels}
     */
    static new(): TransactionMetadatumLabels;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {BigNum}
     */
    get(index: number): BigNum;
    /**
     * @param {BigNum} elem
     */
    add(elem: BigNum): void;
}
/** */
export class TransactionOutput {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionOutput}
     */
    static from_bytes(bytes: Uint8Array): TransactionOutput;
    /**
     * @param {string} json
     * @returns {TransactionOutput}
     */
    static from_json(json: string): TransactionOutput;
    /**
     * @param {Address} address
     * @param {Value} amount
     * @returns {TransactionOutput}
     */
    static new(address: Address, amount: Value): TransactionOutput;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Address}
     */
    address(): Address;
    /**
     * @returns {Value}
     */
    amount(): Value;
    /**
     * @returns {Datum | undefined}
     */
    datum(): Datum | undefined;
    /**
     * @returns {ScriptRef | undefined}
     */
    script_ref(): ScriptRef | undefined;
    /**
     * @param {Datum} datum
     */
    set_datum(datum: Datum): void;
    /**
     * @param {ScriptRef} script_ref
     */
    set_script_ref(script_ref: ScriptRef): void;
    /**
     * @returns {number}
     */
    format(): number;
    /**
     * legacy support: serialize output as array array
     *
     * does not support inline datum and script_ref!
     * @returns {Uint8Array}
     */
    to_legacy_bytes(): Uint8Array;
}
/** */
export class TransactionOutputAmountBuilder {
    static __wrap(ptr: any): any;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @param {Value} amount
     * @returns {TransactionOutputAmountBuilder}
     */
    with_value(amount: Value): TransactionOutputAmountBuilder;
    /**
     * @param {BigNum} coin
     * @returns {TransactionOutputAmountBuilder}
     */
    with_coin(coin: BigNum): TransactionOutputAmountBuilder;
    /**
     * @param {BigNum} coin
     * @param {MultiAsset} multiasset
     * @returns {TransactionOutputAmountBuilder}
     */
    with_coin_and_asset(coin: BigNum, multiasset: MultiAsset): TransactionOutputAmountBuilder;
    /**
     * @param {MultiAsset} multiasset
     * @param {BigNum} coins_per_utxo_word
     * @returns {TransactionOutputAmountBuilder}
     */
    with_asset_and_min_required_coin(multiasset: MultiAsset, coins_per_utxo_word: BigNum): TransactionOutputAmountBuilder;
    /**
     * @returns {TransactionOutput}
     */
    build(): TransactionOutput;
}
/**
 * We introduce a builder-pattern format for creating transaction outputs
 * This is because:
 * 1. Some fields (i.e. data hash) are optional, and we can't easily expose Option<> in WASM
 * 2. Some fields like amounts have many ways it could be set (some depending on other field values being known)
 * 3. Easier to adapt as the output format gets more complicated in future Cardano releases
 */
export class TransactionOutputBuilder {
    static __wrap(ptr: any): any;
    /**
     * @returns {TransactionOutputBuilder}
     */
    static new(): TransactionOutputBuilder;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @param {Address} address
     * @returns {TransactionOutputBuilder}
     */
    with_address(address: Address): TransactionOutputBuilder;
    /**
     * @param {Datum} data_hash
     * @returns {TransactionOutputBuilder}
     */
    with_datum(data_hash: Datum): TransactionOutputBuilder;
    /**
     * @returns {TransactionOutputAmountBuilder}
     */
    next(): TransactionOutputAmountBuilder;
}
/** */
export class TransactionOutputs {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionOutputs}
     */
    static from_bytes(bytes: Uint8Array): TransactionOutputs;
    /**
     * @param {string} json
     * @returns {TransactionOutputs}
     */
    static from_json(json: string): TransactionOutputs;
    /**
     * @returns {TransactionOutputs}
     */
    static new(): TransactionOutputs;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {TransactionOutput}
     */
    get(index: number): TransactionOutput;
    /**
     * @param {TransactionOutput} elem
     */
    add(elem: TransactionOutput): void;
}
/** */
export class TransactionUnspentOutput {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionUnspentOutput}
     */
    static from_bytes(bytes: Uint8Array): TransactionUnspentOutput;
    /**
     * @param {TransactionInput} input
     * @param {TransactionOutput} output
     * @returns {TransactionUnspentOutput}
     */
    static new(input: TransactionInput, output: TransactionOutput): TransactionUnspentOutput;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {TransactionInput}
     */
    input(): TransactionInput;
    /**
     * @returns {TransactionOutput}
     */
    output(): TransactionOutput;
    /**
     * @returns {Uint8Array}
     */
    to_legacy_bytes(): Uint8Array;
}
/** */
export class TransactionUnspentOutputs {
    static __wrap(ptr: any): any;
    /**
     * @returns {TransactionUnspentOutputs}
     */
    static new(): TransactionUnspentOutputs;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {TransactionUnspentOutput}
     */
    get(index: number): TransactionUnspentOutput;
    /**
     * @param {TransactionUnspentOutput} elem
     */
    add(elem: TransactionUnspentOutput): void;
}
/** */
export class TransactionWitnessSet {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionWitnessSet}
     */
    static from_bytes(bytes: Uint8Array): TransactionWitnessSet;
    /**
     * @param {string} json
     * @returns {TransactionWitnessSet}
     */
    static from_json(json: string): TransactionWitnessSet;
    /**
     * @returns {TransactionWitnessSet}
     */
    static new(): TransactionWitnessSet;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @param {Vkeywitnesses} vkeys
     */
    set_vkeys(vkeys: Vkeywitnesses): void;
    /**
     * @returns {Vkeywitnesses | undefined}
     */
    vkeys(): Vkeywitnesses | undefined;
    /**
     * @param {NativeScripts} native_scripts
     */
    set_native_scripts(native_scripts: NativeScripts): void;
    /**
     * @returns {NativeScripts | undefined}
     */
    native_scripts(): NativeScripts | undefined;
    /**
     * @param {BootstrapWitnesses} bootstraps
     */
    set_bootstraps(bootstraps: BootstrapWitnesses): void;
    /**
     * @returns {BootstrapWitnesses | undefined}
     */
    bootstraps(): BootstrapWitnesses | undefined;
    /**
     * @param {PlutusScripts} plutus_scripts
     */
    set_plutus_scripts(plutus_scripts: PlutusScripts): void;
    /**
     * @returns {PlutusScripts | undefined}
     */
    plutus_scripts(): PlutusScripts | undefined;
    /**
     * @param {PlutusList} plutus_data
     */
    set_plutus_data(plutus_data: PlutusList): void;
    /**
     * @returns {PlutusList | undefined}
     */
    plutus_data(): PlutusList | undefined;
    /**
     * @param {Redeemers} redeemers
     */
    set_redeemers(redeemers: Redeemers): void;
    /**
     * @param {PlutusScripts} plutus_scripts
     */
    set_plutus_v2_scripts(plutus_scripts: PlutusScripts): void;
    /**
     * @param {PlutusScripts} plutus_scripts
     */
    set_plutus_v3_scripts(plutus_scripts: PlutusScripts): void;
    /**
     * @returns {Redeemers | undefined}
     */
    redeemers(): Redeemers | undefined;
    /**
     * @returns {PlutusScripts | undefined}
     */
    plutus_v2_scripts(): PlutusScripts | undefined;
    /**
     * @returns {PlutusScripts | undefined}
     */
    plutus_v3_scripts(): PlutusScripts | undefined;
}
/**
 * Builder de-duplicates witnesses as they are added
 */
export class TransactionWitnessSetBuilder {
    static __wrap(ptr: any): any;
    /**
     * @returns {TransactionWitnessSetBuilder}
     */
    static new(): TransactionWitnessSetBuilder;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @param {Vkeywitness} vkey
     */
    add_vkey(vkey: Vkeywitness): void;
    /**
     * @param {BootstrapWitness} bootstrap
     */
    add_bootstrap(bootstrap: BootstrapWitness): void;
    /**
     * @param {NativeScript} native_script
     */
    add_native_script(native_script: NativeScript): void;
    /**
     * @param {PlutusScript} plutus_script
     */
    add_plutus_script(plutus_script: PlutusScript): void;
    /**
     * @param {PlutusScript} plutus_script
     */
    add_plutus_v2_script(plutus_script: PlutusScript): void;
    /**
     * @param {PlutusData} plutus_datum
     */
    add_plutus_datum(plutus_datum: PlutusData): void;
    /**
     * @param {Redeemer} redeemer
     */
    add_redeemer(redeemer: Redeemer): void;
    /**
     * @param {RequiredWitnessSet} required_wits
     */
    add_required_wits(required_wits: RequiredWitnessSet): void;
    /**
     * @param {TransactionWitnessSet} wit_set
     */
    add_existing(wit_set: TransactionWitnessSet): void;
    /**
     * @returns {TransactionWitnessSet}
     */
    build(): TransactionWitnessSet;
}
/** */
export class TransactionWitnessSets {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TransactionWitnessSets}
     */
    static from_bytes(bytes: Uint8Array): TransactionWitnessSets;
    /**
     * @param {string} json
     * @returns {TransactionWitnessSets}
     */
    static from_json(json: string): TransactionWitnessSets;
    /**
     * @returns {TransactionWitnessSets}
     */
    static new(): TransactionWitnessSets;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {TransactionWitnessSet}
     */
    get(index: number): TransactionWitnessSet;
    /**
     * @param {TransactionWitnessSet} elem
     */
    add(elem: TransactionWitnessSet): void;
}
/** */
export class TreasuryWithdrawals {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TreasuryWithdrawals}
     */
    static from_bytes(bytes: Uint8Array): TreasuryWithdrawals;
    /**
     * @param {string} json
     * @returns {TreasuryWithdrawals}
     */
    static from_json(json: string): TreasuryWithdrawals;
    /**
     * @returns {TreasuryWithdrawals}
     */
    static new(): TreasuryWithdrawals;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {Ed25519KeyHash} key
     * @param {BigNum} value
     * @returns {BigNum | undefined}
     */
    insert(key: Ed25519KeyHash, value: BigNum): BigNum | undefined;
    /**
     * @param {Ed25519KeyHash} key
     * @returns {BigNum | undefined}
     */
    get(key: Ed25519KeyHash): BigNum | undefined;
    /**
     * @returns {Ed25519KeyHashes}
     */
    keys(): Ed25519KeyHashes;
}
/** */
export class TreasuryWithdrawalsAction {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {TreasuryWithdrawalsAction}
     */
    static from_bytes(bytes: Uint8Array): TreasuryWithdrawalsAction;
    /**
     * @param {string} json
     * @returns {TreasuryWithdrawalsAction}
     */
    static from_json(json: string): TreasuryWithdrawalsAction;
    /**
     * @param {TreasuryWithdrawals} withdrawals
     * @returns {TreasuryWithdrawalsAction}
     */
    static new(withdrawals: TreasuryWithdrawals): TreasuryWithdrawalsAction;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {TreasuryWithdrawals}
     */
    withdrawals(): TreasuryWithdrawals;
}
/** */
export class UnitInterval {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {UnitInterval}
     */
    static from_bytes(bytes: Uint8Array): UnitInterval;
    /**
     * @param {string} json
     * @returns {UnitInterval}
     */
    static from_json(json: string): UnitInterval;
    /**
     * @param {BigNum} numerator
     * @param {BigNum} denominator
     * @returns {UnitInterval}
     */
    static new(numerator: BigNum, denominator: BigNum): UnitInterval;
    /**
     * @param {number} float_number
     * @returns {UnitInterval}
     */
    static from_float(float_number: number): UnitInterval;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {BigNum}
     */
    numerator(): BigNum;
    /**
     * @returns {BigNum}
     */
    denominator(): BigNum;
}
/** */
export class UnregCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {UnregCert}
     */
    static from_bytes(bytes: Uint8Array): UnregCert;
    /**
     * @param {string} json
     * @returns {UnregCert}
     */
    static from_json(json: string): UnregCert;
    /**
     * @param {StakeCredential} stake_credential
     * @param {BigNum} coin
     * @returns {UnregCert}
     */
    static new(stake_credential: StakeCredential, coin: BigNum): UnregCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {StakeCredential}
     */
    stake_credential(): StakeCredential;
    /**
     * @returns {BigNum}
     */
    coin(): BigNum;
}
/** */
export class UnregCommitteeHotKeyCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {UnregCommitteeHotKeyCert}
     */
    static from_bytes(bytes: Uint8Array): UnregCommitteeHotKeyCert;
    /**
     * @param {string} json
     * @returns {UnregCommitteeHotKeyCert}
     */
    static from_json(json: string): UnregCommitteeHotKeyCert;
    /**
     * @param {Ed25519KeyHash} committee_cold_keyhash
     * @returns {UnregCommitteeHotKeyCert}
     */
    static new(committee_cold_keyhash: Ed25519KeyHash): UnregCommitteeHotKeyCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Ed25519KeyHash}
     */
    committee_cold_keyhash(): Ed25519KeyHash;
}
/** */
export class UnregDrepCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {UnregDrepCert}
     */
    static from_bytes(bytes: Uint8Array): UnregDrepCert;
    /**
     * @param {string} json
     * @returns {UnregDrepCert}
     */
    static from_json(json: string): UnregDrepCert;
    /**
     * @param {StakeCredential} voting_credential
     * @param {BigNum} coin
     * @returns {UnregDrepCert}
     */
    static new(voting_credential: StakeCredential, coin: BigNum): UnregDrepCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {StakeCredential}
     */
    voting_credential(): StakeCredential;
    /**
     * @returns {BigNum}
     */
    coin(): BigNum;
}
/** */
export class Update {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Update}
     */
    static from_bytes(bytes: Uint8Array): Update;
    /**
     * @param {string} json
     * @returns {Update}
     */
    static from_json(json: string): Update;
    /**
     * @param {ProposedProtocolParameterUpdates} proposed_protocol_parameter_updates
     * @param {number} epoch
     * @returns {Update}
     */
    static new(proposed_protocol_parameter_updates: ProposedProtocolParameterUpdates, epoch: number): Update;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {ProposedProtocolParameterUpdates}
     */
    proposed_protocol_parameter_updates(): ProposedProtocolParameterUpdates;
    /**
     * @returns {number}
     */
    epoch(): number;
}
/** */
export class Url {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Url}
     */
    static from_bytes(bytes: Uint8Array): Url;
    /**
     * @param {string} url
     * @returns {Url}
     */
    static new(url: string): Url;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    url(): string;
}
/** */
export class VRFCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {VRFCert}
     */
    static from_bytes(bytes: Uint8Array): VRFCert;
    /**
     * @param {string} json
     * @returns {VRFCert}
     */
    static from_json(json: string): VRFCert;
    /**
     * @param {Uint8Array} output
     * @param {Uint8Array} proof
     * @returns {VRFCert}
     */
    static new(output: Uint8Array, proof: Uint8Array): VRFCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Uint8Array}
     */
    output(): Uint8Array;
    /**
     * @returns {Uint8Array}
     */
    proof(): Uint8Array;
}
/** */
export class VRFKeyHash {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {VRFKeyHash}
     */
    static from_bytes(bytes: Uint8Array): VRFKeyHash;
    /**
     * @param {string} bech_str
     * @returns {VRFKeyHash}
     */
    static from_bech32(bech_str: string): VRFKeyHash;
    /**
     * @param {string} hex
     * @returns {VRFKeyHash}
     */
    static from_hex(hex: string): VRFKeyHash;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @param {string} prefix
     * @returns {string}
     */
    to_bech32(prefix: string): string;
    /**
     * @returns {string}
     */
    to_hex(): string;
}
/** */
export class VRFVKey {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {VRFVKey}
     */
    static from_bytes(bytes: Uint8Array): VRFVKey;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {VRFKeyHash}
     */
    hash(): VRFKeyHash;
    /**
     * @returns {Uint8Array}
     */
    to_raw_key(): Uint8Array;
}
/** */
export class Value {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Value}
     */
    static from_bytes(bytes: Uint8Array): Value;
    /**
     * @param {string} json
     * @returns {Value}
     */
    static from_json(json: string): Value;
    /**
     * @param {BigNum} coin
     * @returns {Value}
     */
    static new(coin: BigNum): Value;
    /**
     * @param {MultiAsset} multiasset
     * @returns {Value}
     */
    static new_from_assets(multiasset: MultiAsset): Value;
    /**
     * @returns {Value}
     */
    static zero(): Value;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {boolean}
     */
    is_zero(): boolean;
    /**
     * @returns {BigNum}
     */
    coin(): BigNum;
    /**
     * @param {BigNum} coin
     */
    set_coin(coin: BigNum): void;
    /**
     * @returns {MultiAsset | undefined}
     */
    multiasset(): MultiAsset | undefined;
    /**
     * @param {MultiAsset} multiasset
     */
    set_multiasset(multiasset: MultiAsset): void;
    /**
     * @param {Value} rhs
     * @returns {Value}
     */
    checked_add(rhs: Value): Value;
    /**
     * @param {Value} rhs_value
     * @returns {Value}
     */
    checked_sub(rhs_value: Value): Value;
    /**
     * @param {Value} rhs_value
     * @returns {Value}
     */
    clamped_sub(rhs_value: Value): Value;
    /**
     * note: values are only partially comparable
     * @param {Value} rhs_value
     * @returns {number | undefined}
     */
    compare(rhs_value: Value): number | undefined;
}
/** */
export class Vkey {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Vkey}
     */
    static from_bytes(bytes: Uint8Array): Vkey;
    /**
     * @param {PublicKey} pk
     * @returns {Vkey}
     */
    static new(pk: PublicKey): Vkey;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {PublicKey}
     */
    public_key(): PublicKey;
}
/** */
export class Vkeys {
    static __wrap(ptr: any): any;
    /**
     * @returns {Vkeys}
     */
    static new(): Vkeys;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {Vkey}
     */
    get(index: number): Vkey;
    /**
     * @param {Vkey} elem
     */
    add(elem: Vkey): void;
}
/** */
export class Vkeywitness {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Vkeywitness}
     */
    static from_bytes(bytes: Uint8Array): Vkeywitness;
    /**
     * @param {string} json
     * @returns {Vkeywitness}
     */
    static from_json(json: string): Vkeywitness;
    /**
     * @param {Vkey} vkey
     * @param {Ed25519Signature} signature
     * @returns {Vkeywitness}
     */
    static new(vkey: Vkey, signature: Ed25519Signature): Vkeywitness;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {Vkey}
     */
    vkey(): Vkey;
    /**
     * @returns {Ed25519Signature}
     */
    signature(): Ed25519Signature;
}
/** */
export class Vkeywitnesses {
    static __wrap(ptr: any): any;
    /**
     * @returns {Vkeywitnesses}
     */
    static new(): Vkeywitnesses;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {Vkeywitness}
     */
    get(index: number): Vkeywitness;
    /**
     * @param {Vkeywitness} elem
     */
    add(elem: Vkeywitness): void;
}
/** */
export class Vote {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Vote}
     */
    static from_bytes(bytes: Uint8Array): Vote;
    /**
     * @param {string} json
     * @returns {Vote}
     */
    static from_json(json: string): Vote;
    /**
     * @returns {Vote}
     */
    static new_no(): Vote;
    /**
     * @returns {Vote}
     */
    static new_yes(): Vote;
    /**
     * @returns {Vote}
     */
    static new_abstain(): Vote;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    kind(): number;
}
/** */
export class VoteDelegCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {VoteDelegCert}
     */
    static from_bytes(bytes: Uint8Array): VoteDelegCert;
    /**
     * @param {string} json
     * @returns {VoteDelegCert}
     */
    static from_json(json: string): VoteDelegCert;
    /**
     * @param {StakeCredential} stake_credential
     * @param {Drep} drep
     * @returns {VoteDelegCert}
     */
    static new(stake_credential: StakeCredential, drep: Drep): VoteDelegCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {StakeCredential}
     */
    stake_credential(): StakeCredential;
    /**
     * @returns {Drep}
     */
    drep(): Drep;
}
/** */
export class VoteRegDelegCert {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {VoteRegDelegCert}
     */
    static from_bytes(bytes: Uint8Array): VoteRegDelegCert;
    /**
     * @param {string} json
     * @returns {VoteRegDelegCert}
     */
    static from_json(json: string): VoteRegDelegCert;
    /**
     * @param {StakeCredential} stake_credential
     * @param {Drep} drep
     * @param {BigNum} coin
     * @returns {VoteRegDelegCert}
     */
    static new(stake_credential: StakeCredential, drep: Drep, coin: BigNum): VoteRegDelegCert;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {StakeCredential}
     */
    stake_credential(): StakeCredential;
    /**
     * @returns {Drep}
     */
    drep(): Drep;
    /**
     * @returns {BigNum}
     */
    coin(): BigNum;
}
/** */
export class Voter {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Voter}
     */
    static from_bytes(bytes: Uint8Array): Voter;
    /**
     * @param {string} json
     * @returns {Voter}
     */
    static from_json(json: string): Voter;
    /**
     * @param {Ed25519KeyHash} keyhash
     * @returns {Voter}
     */
    static new_committee_hot_keyhash(keyhash: Ed25519KeyHash): Voter;
    /**
     * @param {ScriptHash} scripthash
     * @returns {Voter}
     */
    static new_committee_hot_scripthash(scripthash: ScriptHash): Voter;
    /**
     * @param {Ed25519KeyHash} keyhash
     * @returns {Voter}
     */
    static new_drep_keyhash(keyhash: Ed25519KeyHash): Voter;
    /**
     * @param {ScriptHash} scripthash
     * @returns {Voter}
     */
    static new_drep_scripthash(scripthash: ScriptHash): Voter;
    /**
     * @param {Ed25519KeyHash} keyhash
     * @returns {Voter}
     */
    static new_staking_pool_keyhash(keyhash: Ed25519KeyHash): Voter;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    kind(): number;
    /**
     * @returns {Ed25519KeyHash | undefined}
     */
    as_committee_hot_keyhash(): Ed25519KeyHash | undefined;
    /**
     * @returns {ScriptHash | undefined}
     */
    as_committee_hot_scripthash(): ScriptHash | undefined;
    /**
     * @returns {Ed25519KeyHash | undefined}
     */
    as_drep_keyhash(): Ed25519KeyHash | undefined;
    /**
     * @returns {ScriptHash | undefined}
     */
    as_drep_scripthash(): ScriptHash | undefined;
    /**
     * @returns {Ed25519KeyHash | undefined}
     */
    as_staking_pool_keyhash(): Ed25519KeyHash | undefined;
}
/** */
export class VotingProcedure {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {VotingProcedure}
     */
    static from_bytes(bytes: Uint8Array): VotingProcedure;
    /**
     * @param {string} json
     * @returns {VotingProcedure}
     */
    static from_json(json: string): VotingProcedure;
    /**
     * @param {GovernanceActionId} governance_action_id
     * @param {Voter} voter
     * @param {Vote} vote
     * @param {Anchor} anchor
     * @returns {VotingProcedure}
     */
    static new(governance_action_id: GovernanceActionId, voter: Voter, vote: Vote, anchor: Anchor): VotingProcedure;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {GovernanceActionId}
     */
    governance_action_id(): GovernanceActionId;
    /**
     * @returns {Voter}
     */
    voter(): Voter;
    /**
     * @returns {number}
     */
    vote(): number;
    /**
     * @returns {Anchor}
     */
    anchor(): Anchor;
}
/** */
export class VotingProcedures {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {VotingProcedures}
     */
    static from_bytes(bytes: Uint8Array): VotingProcedures;
    /**
     * @returns {VotingProcedures}
     */
    static new(): VotingProcedures;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {number} index
     * @returns {VotingProcedure}
     */
    get(index: number): VotingProcedure;
    /**
     * @param {VotingProcedure} elem
     */
    add(elem: VotingProcedure): void;
}
/** */
export class Withdrawals {
    static __wrap(ptr: any): any;
    /**
     * @param {Uint8Array} bytes
     * @returns {Withdrawals}
     */
    static from_bytes(bytes: Uint8Array): Withdrawals;
    /**
     * @param {string} json
     * @returns {Withdrawals}
     */
    static from_json(json: string): Withdrawals;
    /**
     * @returns {Withdrawals}
     */
    static new(): Withdrawals;
    __destroy_into_raw(): number | undefined;
    ptr: number | undefined;
    free(): void;
    /**
     * @returns {Uint8Array}
     */
    to_bytes(): Uint8Array;
    /**
     * @returns {string}
     */
    to_json(): string;
    /**
     * @returns {any}
     */
    to_js_value(): any;
    /**
     * @returns {number}
     */
    len(): number;
    /**
     * @param {RewardAddress} key
     * @param {BigNum} value
     * @returns {BigNum | undefined}
     */
    insert(key: RewardAddress, value: BigNum): BigNum | undefined;
    /**
     * @param {RewardAddress} key
     * @returns {BigNum | undefined}
     */
    get(key: RewardAddress): BigNum | undefined;
    /**
     * @returns {RewardAddresses}
     */
    keys(): RewardAddresses;
}
/**
 * Decompression callback
 */
export type DecompressCallback = (compressed: Uint8Array) => Uint8Array;
/**
 * Options for instantiating a Wasm instance.
 */
export type InstantiateOptions = {
    /**
     * - Optional url to the Wasm file to instantiate.
     */
    url?: URL | undefined;
    /**
     * - Callback to decompress the
     * raw Wasm file bytes before instantiating.
     */
    decompress?: DecompressCallback | undefined;
};
