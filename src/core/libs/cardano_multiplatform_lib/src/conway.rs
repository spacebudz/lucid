use super::*;

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct VotingProcedure {
    governance_action_id: GovernanceActionId,
    voter: Voter,
    vote: Vote,
    anchor: Option<Anchor>,
}

to_from_bytes!(VotingProcedure);

to_from_json!(VotingProcedure);

#[wasm_bindgen]
impl VotingProcedure {
    pub fn governance_action_id(&self) -> GovernanceActionId {
        self.governance_action_id.clone()
    }

    pub fn voter(&self) -> Voter {
        self.voter.clone()
    }

    pub fn vote(&self) -> Vote {
        self.vote.clone()
    }

    pub fn anchor(&self) -> Option<Anchor> {
        self.anchor.clone()
    }

    pub fn new(
        governance_action_id: &GovernanceActionId,
        voter: &Voter,
        vote: &Vote,
        anchor: &Option<Anchor>,
    ) -> Self {
        Self {
            governance_action_id: governance_action_id.clone(),
            voter: voter.clone(),
            vote: vote.clone(),
            anchor: anchor.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct VotingProcedures(pub(crate) Vec<VotingProcedure>);

to_from_bytes!(VotingProcedures);

#[wasm_bindgen]
impl VotingProcedures {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, index: usize) -> VotingProcedure {
        self.0[index].clone()
    }

    pub fn add(&mut self, elem: &VotingProcedure) {
        self.0.push(elem.clone());
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct ProposalProcedure {
    deposit: Coin,
    hash: ScriptHash,
    governance_action: GovernanceAction,
    anchor: Option<Anchor>,
}

to_from_bytes!(ProposalProcedure);

to_from_json!(ProposalProcedure);

#[wasm_bindgen]
impl ProposalProcedure {
    pub fn deposit(&self) -> Coin {
        self.deposit.clone()
    }

    pub fn hash(&self) -> ScriptHash {
        self.hash.clone()
    }

    pub fn governance_action(&self) -> GovernanceAction {
        self.governance_action.clone()
    }

    pub fn anchor(&self) -> Option<Anchor> {
        self.anchor.clone()
    }

    pub fn new(
        deposit: &Coin,
        hash: &ScriptHash,
        governance_action: &GovernanceAction,
        anchor: &Option<Anchor>,
    ) -> Self {
        Self {
            deposit: deposit.clone(),
            hash: hash.clone(),
            governance_action: governance_action.clone(),
            anchor: anchor.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct ProposalProcedures(pub(crate) Vec<ProposalProcedure>);

to_from_bytes!(ProposalProcedures);

#[wasm_bindgen]
impl ProposalProcedures {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, index: usize) -> ProposalProcedure {
        self.0[index].clone()
    }

    pub fn add(&mut self, elem: &ProposalProcedure) {
        self.0.push(elem.clone());
    }
}

#[wasm_bindgen]
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum GovernanceActionKind {
    ParameterChangeAction,
    HardForkInitiationAction,
    TreasuryWithdrawalsAction,
    NoConfidence,
    NewCommittee,
    NewConstitution,
    InfoAction,
}

#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub enum GovernanceActionEnum {
    ParameterChangeAction(ParameterChangeAction),
    HardForkInitiationAction(HardForkInitiationAction),
    TreasuryWithdrawalsAction(TreasuryWithdrawalsAction),
    NoConfidence,
    NewCommittee(NewCommittee),
    NewConstitution(NewConstitution),
    InfoAction,
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct GovernanceAction(GovernanceActionEnum);

to_from_bytes!(GovernanceAction);

to_from_json!(GovernanceAction);

#[wasm_bindgen]
impl GovernanceAction {
    pub fn new_parameter_change_action(parameter_change_action: &ParameterChangeAction) -> Self {
        Self(GovernanceActionEnum::ParameterChangeAction(
            parameter_change_action.clone(),
        ))
    }

    pub fn new_hard_fork_initiation_action(
        hard_fork_initiation_action: &HardForkInitiationAction,
    ) -> Self {
        Self(GovernanceActionEnum::HardForkInitiationAction(
            hard_fork_initiation_action.clone(),
        ))
    }

    pub fn new_treasury_withdrawals_action(
        treasury_withdrawals_action: &TreasuryWithdrawalsAction,
    ) -> Self {
        Self(GovernanceActionEnum::TreasuryWithdrawalsAction(
            treasury_withdrawals_action.clone(),
        ))
    }

    pub fn new_no_confidence() -> Self {
        Self(GovernanceActionEnum::NoConfidence)
    }

    pub fn new_new_committee(new_committe: &NewCommittee) -> Self {
        Self(GovernanceActionEnum::NewCommittee(new_committe.clone()))
    }

    pub fn new_new_constitution(new_constitution: &NewConstitution) -> Self {
        Self(GovernanceActionEnum::NewConstitution(
            new_constitution.clone(),
        ))
    }

    pub fn new_info_action() -> Self {
        Self(GovernanceActionEnum::InfoAction)
    }

    pub fn kind(&self) -> GovernanceActionKind {
        match &self.0 {
            GovernanceActionEnum::ParameterChangeAction(_) => {
                GovernanceActionKind::ParameterChangeAction
            }
            GovernanceActionEnum::HardForkInitiationAction(_) => {
                GovernanceActionKind::HardForkInitiationAction
            }
            GovernanceActionEnum::TreasuryWithdrawalsAction(_) => {
                GovernanceActionKind::TreasuryWithdrawalsAction
            }
            GovernanceActionEnum::NoConfidence => GovernanceActionKind::NoConfidence,
            GovernanceActionEnum::NewCommittee(_) => GovernanceActionKind::NewCommittee,
            GovernanceActionEnum::NewConstitution(_) => GovernanceActionKind::NewConstitution,
            GovernanceActionEnum::InfoAction => GovernanceActionKind::InfoAction,
        }
    }

    pub fn as_parameter_change_action(&self) -> Option<ParameterChangeAction> {
        match &self.0 {
            GovernanceActionEnum::ParameterChangeAction(x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn as_hard_fork_initiation_action(&self) -> Option<HardForkInitiationAction> {
        match &self.0 {
            GovernanceActionEnum::HardForkInitiationAction(x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn as_treasury_withdrawals_action(&self) -> Option<TreasuryWithdrawalsAction> {
        match &self.0 {
            GovernanceActionEnum::TreasuryWithdrawalsAction(x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn as_new_committee(&self) -> Option<NewCommittee> {
        match &self.0 {
            GovernanceActionEnum::NewCommittee(x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn as_new_constitution(&self) -> Option<NewConstitution> {
        match &self.0 {
            GovernanceActionEnum::NewConstitution(x) => Some(x.clone()),
            _ => None,
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct ParameterChangeAction {
    protocol_param_update: ProtocolParamUpdate,
}

to_from_bytes!(ParameterChangeAction);

to_from_json!(ParameterChangeAction);

#[wasm_bindgen]
impl ParameterChangeAction {
    pub fn protocol_param_update(&self) -> ProtocolParamUpdate {
        self.protocol_param_update.clone()
    }

    pub fn new(protocol_param_update: &ProtocolParamUpdate) -> Self {
        Self {
            protocol_param_update: protocol_param_update.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct HardForkInitiationAction {
    protocol_version: ProtocolVersion,
}

to_from_bytes!(HardForkInitiationAction);

to_from_json!(HardForkInitiationAction);

#[wasm_bindgen]
impl HardForkInitiationAction {
    pub fn protocol_version(&self) -> ProtocolVersion {
        self.protocol_version.clone()
    }

    pub fn new(protocol_version: &ProtocolVersion) -> Self {
        Self {
            protocol_version: protocol_version.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct TreasuryWithdrawals(std::collections::BTreeMap<Ed25519KeyHash, Coin>);

to_from_bytes!(TreasuryWithdrawals);

to_from_json!(TreasuryWithdrawals);

#[wasm_bindgen]
impl TreasuryWithdrawals {
    pub fn new() -> Self {
        Self(std::collections::BTreeMap::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn insert(&mut self, key: &Ed25519KeyHash, value: &Coin) -> Option<Coin> {
        self.0.insert(key.clone(), value.clone())
    }

    pub fn get(&self, key: &Ed25519KeyHash) -> Option<Coin> {
        self.0.get(key).map(|v| v.clone())
    }

    pub fn keys(&self) -> Ed25519KeyHashes {
        Ed25519KeyHashes(
            self.0
                .iter()
                .map(|(k, _v)| k.clone())
                .collect::<Vec<Ed25519KeyHash>>(),
        )
    }
}

impl serde::Serialize for TreasuryWithdrawals {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let map = self.0.iter().collect::<std::collections::BTreeMap<_, _>>();
        map.serialize(serializer)
    }
}

impl<'de> serde::de::Deserialize<'de> for TreasuryWithdrawals {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let map = <std::collections::BTreeMap<_, _> as serde::de::Deserialize>::deserialize(
            deserializer,
        )?;
        Ok(Self(map.into_iter().collect()))
    }
}

impl JsonSchema for TreasuryWithdrawals {
    fn schema_name() -> String {
        String::from("TreasuryWithdrawals")
    }
    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        std::collections::BTreeMap::<GenesisHash, ProtocolParamUpdate>::json_schema(gen)
    }
    fn is_referenceable() -> bool {
        std::collections::BTreeMap::<GenesisHash, ProtocolParamUpdate>::is_referenceable()
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct TreasuryWithdrawalsAction {
    withdrawals: TreasuryWithdrawals,
}

to_from_bytes!(TreasuryWithdrawalsAction);

to_from_json!(TreasuryWithdrawalsAction);

#[wasm_bindgen]
impl TreasuryWithdrawalsAction {
    pub fn withdrawals(&self) -> TreasuryWithdrawals {
        self.withdrawals.clone()
    }

    pub fn new(withdrawals: &TreasuryWithdrawals) -> Self {
        Self {
            withdrawals: withdrawals.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct NewCommittee {
    committee: Ed25519KeyHashes,
    rational: Rational,
}

to_from_bytes!(NewCommittee);

to_from_json!(NewCommittee);

#[wasm_bindgen]
impl NewCommittee {
    pub fn committee(&self) -> Ed25519KeyHashes {
        self.committee.clone()
    }

    pub fn rational(&self) -> Rational {
        self.rational.clone()
    }

    pub fn new(committee: &Ed25519KeyHashes, rational: &Rational) -> Self {
        Self {
            committee: committee.clone(),
            rational: rational.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct NewConstitution {
    hash: DataHash,
}

to_from_bytes!(NewConstitution);

to_from_json!(NewConstitution);

#[wasm_bindgen]
impl NewConstitution {
    pub fn hash(&self) -> DataHash {
        self.hash.clone()
    }

    pub fn new(hash: &DataHash) -> Self {
        Self { hash: hash.clone() }
    }
}

#[wasm_bindgen]
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum VoterKind {
    CommitteeHotKeyHash,
    CommitteeHotScriptHash,
    DrepKeyHash,
    DrepScriptHash,
    StakingPoolKeyHash,
}

#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub enum VoterEnum {
    CommitteeHotKeyHash(Ed25519KeyHash),
    CommitteeHotScriptHash(ScriptHash),
    DrepKeyHash(Ed25519KeyHash),
    DrepScriptHash(ScriptHash),
    StakingPoolKeyHash(Ed25519KeyHash),
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct Voter(VoterEnum);

to_from_bytes!(Voter);

to_from_json!(Voter);

#[wasm_bindgen]
impl Voter {
    pub fn new_committee_hot_keyhash(keyhash: &Ed25519KeyHash) -> Self {
        Self(VoterEnum::CommitteeHotKeyHash(keyhash.clone()))
    }

    pub fn new_committee_hot_scripthash(scripthash: &ScriptHash) -> Self {
        Self(VoterEnum::CommitteeHotScriptHash(scripthash.clone()))
    }

    pub fn new_drep_keyhash(keyhash: &Ed25519KeyHash) -> Self {
        Self(VoterEnum::DrepKeyHash(keyhash.clone()))
    }

    pub fn new_drep_scripthash(scripthash: &ScriptHash) -> Self {
        Self(VoterEnum::DrepScriptHash(scripthash.clone()))
    }

    pub fn new_staking_pool_keyhash(keyhash: &Ed25519KeyHash) -> Self {
        Self(VoterEnum::StakingPoolKeyHash(keyhash.clone()))
    }

    pub fn kind(&self) -> VoterKind {
        match &self.0 {
            VoterEnum::CommitteeHotKeyHash(_) => VoterKind::CommitteeHotKeyHash,
            VoterEnum::CommitteeHotScriptHash(_) => VoterKind::CommitteeHotScriptHash,
            VoterEnum::DrepKeyHash(_) => VoterKind::DrepKeyHash,
            VoterEnum::DrepScriptHash(_) => VoterKind::DrepScriptHash,
            VoterEnum::StakingPoolKeyHash(_) => VoterKind::StakingPoolKeyHash,
        }
    }

    pub fn as_committee_hot_keyhash(&self) -> Option<Ed25519KeyHash> {
        match &self.0 {
            VoterEnum::CommitteeHotKeyHash(x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn as_committee_hot_scripthash(&self) -> Option<ScriptHash> {
        match &self.0 {
            VoterEnum::CommitteeHotScriptHash(x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn as_drep_keyhash(&self) -> Option<Ed25519KeyHash> {
        match &self.0 {
            VoterEnum::DrepKeyHash(x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn as_drep_scripthash(&self) -> Option<ScriptHash> {
        match &self.0 {
            VoterEnum::DrepScriptHash(x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn as_staking_pool_keyhash(&self) -> Option<Ed25519KeyHash> {
        match &self.0 {
            VoterEnum::StakingPoolKeyHash(x) => Some(x.clone()),
            _ => None,
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct Anchor {
    anchor_url: Url,
    anchor_data_hash: DataHash,
}

to_from_bytes!(Anchor);

to_from_json!(Anchor);

#[wasm_bindgen]
impl Anchor {
    pub fn anchor_url(&self) -> Url {
        self.anchor_url.clone()
    }

    pub fn anchor_data_hash(&self) -> DataHash {
        self.anchor_data_hash.clone()
    }

    pub fn new(anchor_url: &Url, anchor_data_hash: &DataHash) -> Self {
        Self {
            anchor_url: anchor_url.clone(),
            anchor_data_hash: anchor_data_hash.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub enum Vote {
    No,
    Yes,
    Abstain,
}

to_from_bytes!(Vote);

to_from_json!(Vote);

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct GovernanceActionId {
    transaction_id: TransactionHash,
    governance_action_index: BigNum,
}

to_from_bytes!(GovernanceActionId);

to_from_json!(GovernanceActionId);

#[wasm_bindgen]
impl GovernanceActionId {
    pub fn transaction_id(&self) -> TransactionHash {
        self.transaction_id.clone()
    }

    pub fn governance_action_index(&self) -> BigNum {
        self.governance_action_index.clone()
    }

    pub fn new(transaction_id: &TransactionHash, governance_action_index: &BigNum) -> Self {
        Self {
            transaction_id: transaction_id.clone(),
            governance_action_index: governance_action_index.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum DrepKind {
    KeyHash,
    ScriptHash,
    Abstain,
    NoConfidence,
}

#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub enum DrepEnum {
    KeyHash(Ed25519KeyHash),
    ScriptHash(ScriptHash),
    Abstain,
    NoConfidence,
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct Drep(DrepEnum);

to_from_bytes!(Drep);

to_from_json!(Drep);

#[wasm_bindgen]
impl Drep {
    pub fn new_keyhash(keyhash: &Ed25519KeyHash) -> Self {
        Self(DrepEnum::KeyHash(keyhash.clone()))
    }

    pub fn new_scripthash(scripthash: &ScriptHash) -> Self {
        Self(DrepEnum::ScriptHash(scripthash.clone()))
    }

    pub fn new_abstain() -> Self {
        Self(DrepEnum::Abstain)
    }

    pub fn new_no_confidence() -> Self {
        Self(DrepEnum::NoConfidence)
    }

    pub fn kind(&self) -> DrepKind {
        match &self.0 {
            DrepEnum::KeyHash(_) => DrepKind::KeyHash,
            DrepEnum::ScriptHash(_) => DrepKind::ScriptHash,
            DrepEnum::Abstain => DrepKind::Abstain,
            DrepEnum::NoConfidence => DrepKind::NoConfidence,
        }
    }

    pub fn as_keyhash(&self) -> Option<Ed25519KeyHash> {
        match &self.0 {
            DrepEnum::KeyHash(x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn as_scripthash(&self) -> Option<ScriptHash> {
        match &self.0 {
            DrepEnum::ScriptHash(x) => Some(x.clone()),
            _ => None,
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct RegCert {
    stake_credential: StakeCredential,
    coin: Coin,
}

to_from_bytes!(RegCert);

to_from_json!(RegCert);

#[wasm_bindgen]
impl RegCert {
    pub fn stake_credential(&self) -> StakeCredential {
        self.stake_credential.clone()
    }

    pub fn coin(&self) -> Coin {
        self.coin.clone()
    }

    pub fn new(stake_credential: &StakeCredential, coin: &Coin) -> Self {
        Self {
            stake_credential: stake_credential.clone(),
            coin: coin.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct VoteDelegCert {
    stake_credential: StakeCredential,
    drep: Drep,
}

to_from_bytes!(VoteDelegCert);

to_from_json!(VoteDelegCert);

#[wasm_bindgen]
impl VoteDelegCert {
    pub fn stake_credential(&self) -> StakeCredential {
        self.stake_credential.clone()
    }

    pub fn drep(&self) -> Drep {
        self.drep.clone()
    }

    pub fn new(stake_credential: &StakeCredential, drep: &Drep) -> Self {
        Self {
            stake_credential: stake_credential.clone(),
            drep: drep.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct StakeVoteDelegCert {
    stake_credential: StakeCredential,
    pool_keyhash: Ed25519KeyHash,
    drep: Drep,
}

to_from_bytes!(StakeVoteDelegCert);

to_from_json!(StakeVoteDelegCert);

#[wasm_bindgen]
impl StakeVoteDelegCert {
    pub fn stake_credential(&self) -> StakeCredential {
        self.stake_credential.clone()
    }

    pub fn pool_keyhash(&self) -> Ed25519KeyHash {
        self.pool_keyhash.clone()
    }

    pub fn drep(&self) -> Drep {
        self.drep.clone()
    }

    pub fn new(
        stake_credential: &StakeCredential,
        pool_keyhash: &Ed25519KeyHash,
        drep: &Drep,
    ) -> Self {
        Self {
            stake_credential: stake_credential.clone(),
            pool_keyhash: pool_keyhash.clone(),
            drep: drep.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct StakeRegDelegCert {
    stake_credential: StakeCredential,
    pool_keyhash: Ed25519KeyHash,
    coin: Coin,
}

to_from_bytes!(StakeRegDelegCert);

to_from_json!(StakeRegDelegCert);

#[wasm_bindgen]
impl StakeRegDelegCert {
    pub fn stake_credential(&self) -> StakeCredential {
        self.stake_credential.clone()
    }

    pub fn pool_keyhash(&self) -> Ed25519KeyHash {
        self.pool_keyhash.clone()
    }

    pub fn coin(&self) -> Coin {
        self.coin.clone()
    }

    pub fn new(
        stake_credential: &StakeCredential,
        pool_keyhash: &Ed25519KeyHash,
        coin: &Coin,
    ) -> Self {
        Self {
            stake_credential: stake_credential.clone(),
            pool_keyhash: pool_keyhash.clone(),
            coin: coin.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct VoteRegDelegCert {
    stake_credential: StakeCredential,
    drep: Drep,
    coin: Coin,
}

to_from_bytes!(VoteRegDelegCert);

to_from_json!(VoteRegDelegCert);

#[wasm_bindgen]
impl VoteRegDelegCert {
    pub fn stake_credential(&self) -> StakeCredential {
        self.stake_credential.clone()
    }

    pub fn drep(&self) -> Drep {
        self.drep.clone()
    }

    pub fn coin(&self) -> Coin {
        self.coin.clone()
    }

    pub fn new(stake_credential: &StakeCredential, drep: &Drep, coin: &Coin) -> Self {
        Self {
            stake_credential: stake_credential.clone(),
            drep: drep.clone(),
            coin: coin.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct StakeVoteRegDelegCert {
    stake_credential: StakeCredential,
    pool_keyhash: Ed25519KeyHash,
    drep: Drep,
    coin: Coin,
}

to_from_bytes!(StakeVoteRegDelegCert);

to_from_json!(StakeVoteRegDelegCert);

#[wasm_bindgen]
impl StakeVoteRegDelegCert {
    pub fn stake_credential(&self) -> StakeCredential {
        self.stake_credential.clone()
    }

    pub fn pool_keyhash(&self) -> Ed25519KeyHash {
        self.pool_keyhash.clone()
    }

    pub fn drep(&self) -> Drep {
        self.drep.clone()
    }

    pub fn coin(&self) -> Coin {
        self.coin.clone()
    }

    pub fn new(
        stake_credential: &StakeCredential,
        pool_keyhash: &Ed25519KeyHash,
        drep: &Drep,
        coin: &Coin,
    ) -> Self {
        Self {
            stake_credential: stake_credential.clone(),
            pool_keyhash: pool_keyhash.clone(),
            drep: drep.clone(),
            coin: coin.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct RegCommitteeHotKeyCert {
    committee_cold_keyhash: Ed25519KeyHash,
    committee_hot_keyhash: Ed25519KeyHash,
}

to_from_bytes!(RegCommitteeHotKeyCert);

to_from_json!(RegCommitteeHotKeyCert);

#[wasm_bindgen]
impl RegCommitteeHotKeyCert {
    pub fn committee_cold_keyhash(&self) -> Ed25519KeyHash {
        self.committee_cold_keyhash.clone()
    }

    pub fn committee_hot_keyhash(&self) -> Ed25519KeyHash {
        self.committee_hot_keyhash.clone()
    }

    pub fn new(
        committee_cold_keyhash: &Ed25519KeyHash,
        committee_hot_keyhash: &Ed25519KeyHash,
    ) -> Self {
        Self {
            committee_cold_keyhash: committee_cold_keyhash.clone(),
            committee_hot_keyhash: committee_hot_keyhash.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct UnregCommitteeHotKeyCert {
    committee_cold_keyhash: Ed25519KeyHash,
}

to_from_bytes!(UnregCommitteeHotKeyCert);

to_from_json!(UnregCommitteeHotKeyCert);

#[wasm_bindgen]
impl UnregCommitteeHotKeyCert {
    pub fn committee_cold_keyhash(&self) -> Ed25519KeyHash {
        self.committee_cold_keyhash.clone()
    }

    pub fn new(committee_cold_keyhash: &Ed25519KeyHash) -> Self {
        Self {
            committee_cold_keyhash: committee_cold_keyhash.clone(),
        }
    }
}

pub type VotingCredential = StakeCredential;

#[wasm_bindgen]
#[derive(
    Clone, Debug, Eq, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize, JsonSchema,
)]
pub struct RegDrepCert {
    voting_credential: VotingCredential,
    coin: Coin,
}

to_from_bytes!(RegDrepCert);

to_from_json!(RegDrepCert);

#[wasm_bindgen]
impl RegDrepCert {
    pub fn voting_credential(&self) -> VotingCredential {
        self.voting_credential.clone()
    }

    pub fn coin(&self) -> Coin {
        self.coin.clone()
    }

    pub fn new(voting_credential: &VotingCredential, coin: &Coin) -> Self {
        Self {
            voting_credential: voting_credential.clone(),
            coin: coin.clone(),
        }
    }
}
