use chrono::NaiveDate;
use fuzzy_date::FuzzyDate;
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use serde::de::{self, Deserialize, Deserializer};
use serde::Serialize;
use serde_derive::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fmt;

lazy_static! {
    static ref DEFAULT_START_DATE: FuzzyDate = FuzzyDate::fromisoformat("0001-01-01").unwrap();
    static ref DEFAULT_END_DATE: FuzzyDate = FuzzyDate::fromisoformat("9999-12-31").unwrap();
    static ref PERSON_ID_REGEX: Regex = Regex::new(r"uk\.org\.publicwhip/person/\d+$").unwrap();
    static ref POST_ID_REGEX: Regex = Regex::new(r"uk\.org\.publicwhip/cons/\d+(-NI)?$").unwrap();
    static ref MEMBER_ID_REGEX: Regex = Regex::new(r".*/-?\d+$").unwrap();
    static ref ORG_ID: Regex = Regex::new(r"^[a-z0-9-]+$").unwrap();
}

type ValidatorErrorCollection = Vec<ValidatorError>;

fn default_start_date() -> FuzzyDate {
    DEFAULT_START_DATE.clone()
}

fn default_end_date() -> FuzzyDate {
    DEFAULT_END_DATE.clone()
}

fn is_default_start_date(date: &FuzzyDate) -> bool {
    date == &*DEFAULT_START_DATE
}

fn is_default_end_date(date: &FuzzyDate) -> bool {
    date == &*DEFAULT_END_DATE
}

#[derive(Serialize, Debug, Clone)]
#[serde(untagged, deny_unknown_fields)]
pub enum NameOptions {
    BasicPersonName(BasicPersonName),
    LordName(LordName),
    AltName(AltName),
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub enum ValidatorType {
    InvalidJson,
    NonUniqueId,
    InvalidForeignKey,
    DateRangeOverlap,
    DateRangeOverlapSameDate,
    DateRange,
    RegexError,
}

#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
#[serde(deny_unknown_fields, rename_all = "lowercase")]
pub enum OrgType {
    Party,
    Chamber,
    Metro,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
pub enum NameType {
    Main,
    Alternate,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
#[serde(rename_all = "snake_case")]
pub enum MembershipReason {
    #[serde(rename = "")]
    Blank,
    Accession,
    Appointed,
    BecamePeer,
    BecamePresidingOfficer,
    ByElection,
    ChangedParty,
    DeclaredVoid,
    Devolution,
    Died,
    Disqualified,
    Dissolution,
    Election,
    GeneralElection,
    GeneralElectionNotStanding,
    GeneralElectionProbably,
    GeneralElectionStanding,
    RecallPetition,
    RegionalElection,
    Reinstated,
    ReplacedInRegion,
    Resigned,
    Retired,
    WhipRemoved,
    WhipRestored,
    Unknown,
}

impl MembershipReason {
    pub fn as_str(&self) -> &str {
        match self {
            MembershipReason::Blank => "",
            MembershipReason::Accession => "accession",
            MembershipReason::Appointed => "appointed",
            MembershipReason::BecamePeer => "became_peer",
            MembershipReason::BecamePresidingOfficer => "became_presiding_officer",
            MembershipReason::ByElection => "by_election",
            MembershipReason::ChangedParty => "changed_party",
            MembershipReason::DeclaredVoid => "declared_void",
            MembershipReason::Devolution => "devolution",
            MembershipReason::Died => "died",
            MembershipReason::Disqualified => "disqualified",
            MembershipReason::Dissolution => "dissolution",
            MembershipReason::Election => "election",
            MembershipReason::GeneralElection => "general_election",
            MembershipReason::GeneralElectionNotStanding => "general_election_not_standing",
            MembershipReason::GeneralElectionProbably => "general_election_probably",
            MembershipReason::GeneralElectionStanding => "general_election_standing",
            MembershipReason::RecallPetition => "recall_petition",
            MembershipReason::RegionalElection => "regional_election",
            MembershipReason::Reinstated => "reinstated",
            MembershipReason::ReplacedInRegion => "replaced_in_region",
            MembershipReason::Resigned => "resigned",
            MembershipReason::Retired => "retired",
            MembershipReason::WhipRemoved => "whip_removed",
            MembershipReason::WhipRestored => "whip_restored",
            MembershipReason::Unknown => "unknown",
        }
    }

    pub fn from_str(s: &str) -> Option<MembershipReason> {
        match s {
            "" => Some(MembershipReason::Blank),
            "accession" => Some(MembershipReason::Accession),
            "appointed" => Some(MembershipReason::Appointed),
            "became_peer" => Some(MembershipReason::BecamePeer),
            "became_presiding_officer" => Some(MembershipReason::BecamePresidingOfficer),
            "by_election" => Some(MembershipReason::ByElection),
            "changed_party" => Some(MembershipReason::ChangedParty),
            "declared_void" => Some(MembershipReason::DeclaredVoid),
            "devolution" => Some(MembershipReason::Devolution),
            "died" => Some(MembershipReason::Died),
            "disqualified" => Some(MembershipReason::Disqualified),
            "dissolution" => Some(MembershipReason::Dissolution),
            "election" => Some(MembershipReason::Election),
            "general_election" => Some(MembershipReason::GeneralElection),
            "general_election_not_standing" => Some(MembershipReason::GeneralElectionNotStanding),
            "general_election_probably" => Some(MembershipReason::GeneralElectionProbably),
            "general_election_standing" => Some(MembershipReason::GeneralElectionStanding),
            "recall_petition" => Some(MembershipReason::RecallPetition),
            "regional_election" => Some(MembershipReason::RegionalElection),
            "reinstated" => Some(MembershipReason::Reinstated),
            "replaced_in_region" => Some(MembershipReason::ReplacedInRegion),
            "resigned" => Some(MembershipReason::Resigned),
            "retired" => Some(MembershipReason::Retired),
            "whip_removed" => Some(MembershipReason::WhipRemoved),
            "whip_restored" => Some(MembershipReason::WhipRestored),
            "unknown" => Some(MembershipReason::Unknown),
            _ => None,
        }
    }
}

#[derive(Serialize, Debug, Clone)]
#[serde(untagged, deny_unknown_fields)]
pub enum MembershipType {
    Membership(Membership),
    MembershipRedirect(MembershipRedirect),
}

#[derive(Serialize, Debug, Clone)]
#[serde(untagged, deny_unknown_fields)]
pub enum PersonType {
    Person(Person),
    PersonRedirect(PersonRedirect),
}

#[derive(Serialize, Debug, Clone)]
pub enum Identifier {
    String(String),
    Int(i64),
}

impl Identifier {
    pub fn as_str(&self) -> String {
        match self {
            Identifier::String(s) => s.clone(),
            Identifier::Int(i) => i.to_string(),
        }
    }
}

// Collection interfaces
macro_rules! create_interface {
    ($name: ident, $type: ident) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            pub root: Vec<$type>,
            pub id_lookup: HashMap<String, usize>,
        }

        impl $name {
            pub fn get(&self, id: &str) -> Option<&$type> {
                self.id_lookup.get(id).map(|i| &self.root[*i])
            }

            pub fn get_mut(&mut self, id: &str) -> Option<&mut $type> {
                self.id_lookup.get(id).map(|i| &mut self.root[*i])
            }

            pub fn set_lookup(&mut self) {
                self.id_lookup = self
                    .root
                    .iter()
                    .enumerate()
                    .map(|(i, x)| (x.get_id().clone(), i))
                    .collect();
            }

            pub fn new() -> Self {
                $name {
                    root: Vec::new(),
                    id_lookup: HashMap::new(),
                }
            }

            pub fn len(&self) -> usize {
                self.root.len()
            }

            pub fn push(&mut self, item: $type) {
                self.root.push(item);
            }

            pub fn pop(&mut self) -> Option<$type> {
                self.root.pop()
            }

            pub fn iter(&self) -> std::slice::Iter<'_, $type> {
                self.root.iter()
            }

            pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, $type> {
                self.root.iter_mut()
            }
        }

        impl<'a> IntoIterator for &'a $name {
            type Item = &'a $type;
            type IntoIter = std::slice::Iter<'a, $type>;

            fn into_iter(self) -> Self::IntoIter {
                self.root.iter()
            }
        }

        impl<'de> Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                let vec = Vec::deserialize(deserializer)?;
                Ok($name {
                    root: vec,
                    id_lookup: HashMap::new(),
                })
            }
        }

        impl Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                self.root.serialize(serializer)
            }
        }
    };
}

create_interface!(PersonCollection, PersonType);
create_interface!(MembershipCollection, MembershipType);
create_interface!(OrganizationCollection, Organization);
create_interface!(PostCollection, Post);

pub struct ValidatorError {
    pub error: String,
    pub validator_type: ValidatorType,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct PersonRedirect {
    pub id: String,
    pub redirect: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct LordName {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub county: Option<String>,
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date"
    )]
    pub end_date: FuzzyDate,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub given_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub honorific_prefix: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub honorific_suffix: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lordname: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lordofname: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lordofname_full: Option<String>,
    pub note: NameType,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date",
        deserialize_with = "empty_string_as_default_start_date",
        serialize_with = "serialize_default_start_date"
    )]
    pub start_date: FuzzyDate,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub surname: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Link {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub note: Option<String>,
    pub url: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Shortcuts {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub current_constituency: Option<String>,
    pub current_party: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct BasicPersonName {
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date"
    )]
    pub end_date: FuzzyDate,
    pub family_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub given_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub honorific_prefix: Option<String>,
    pub note: NameType,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date"
    )]
    pub start_date: FuzzyDate,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Person {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub biography: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub birth_date: Option<FuzzyDate>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub death_date: Option<FuzzyDate>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub gender: Option<String>,
    pub id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub identifiers: Option<Vec<SimpleIdentifer>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub image: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub links: Option<Vec<Link>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub other_names: Option<Vec<NameOptions>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub national_identity: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub shortcuts: Option<Shortcuts>,
}

impl Person {
    pub fn names_on_date(&self, date: &NaiveDate) -> Vec<String> {
        self.other_names
            .as_ref()
            .map(|names| {
                names
                    .iter()
                    .filter(|n| {
                        let (start, end) = n.date_range();
                        start.earliest_date <= *date && *date <= end.latest_date
                    })
                    .map(|n| n.nice_name())
                    .collect()
            })
            .unwrap_or_default()
    }

    pub fn get_identifier(&self, scheme: &str) -> Option<String> {
        self.identifiers.as_ref()?.iter().find_map(|id| {
            if id.scheme == scheme {
                Some(id.identifier.as_str())
            } else {
                None
            }
        })
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct SimpleIdentifer {
    pub identifier: Identifier,
    pub scheme: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct MembershipName {
    pub family_name: String,
    pub given_name: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Membership {
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date"
    )]
    pub end_date: FuzzyDate,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_reason: Option<MembershipReason>,
    pub id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub identifiers: Option<Vec<SimpleIdentifer>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<MembershipName>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub on_behalf_of_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub organization_id: Option<String>,
    pub person_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub post_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub role: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date"
    )]
    pub start_date: FuzzyDate,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start_reason: Option<MembershipReason>,
}

// Regex Trait
trait RegexValidator {
    fn test_regex_values(&self) -> ValidatorErrorCollection;
}

macro_rules! regex_property {
    ($field:ident, $regex:ident, $errors:ident, $field_name:expr) => {
        if !$regex.is_match(&$field) {
            $errors.push(ValidatorError {
                error: format!("{}: {} is not a valid {}", $field_name, $field, $field_name),
                validator_type: ValidatorType::RegexError,
            });
        }
    };
}

impl RegexValidator for Popolo {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        for person in &self.persons {
            errors.extend(person.test_regex_values());
        }
        for membership in &self.memberships {
            errors.extend(membership.test_regex_values());
        }
        for post in &self.posts {
            errors.extend(post.test_regex_values());
        }
        for organization in &self.organizations {
            errors.extend(organization.test_regex_values());
        }
        errors
    }
}

impl RegexValidator for NameOptions {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        match self {
            NameOptions::BasicPersonName(_) => Vec::new(),
            NameOptions::LordName(_) => Vec::new(),
            NameOptions::AltName(name) => name.test_regex_values(),
        }
    }
}

impl RegexValidator for AltName {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        if let Some(organization_id) = &self.organization_id {
            regex_property!(organization_id, ORG_ID, errors, "Organization ID");
        }
        errors
    }
}

impl RegexValidator for Organization {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        let org_id = &self.id;
        regex_property!(org_id, ORG_ID, errors, "Organization ID");
        errors
    }
}

impl RegexValidator for Post {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        let post_id = &self.id;
        let organization_id = &self.organization_id;
        regex_property!(post_id, POST_ID_REGEX, errors, "Post ID");
        regex_property!(organization_id, ORG_ID, errors, "Organization ID");
        errors
    }
}

impl RegexValidator for MembershipType {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        match self {
            MembershipType::Membership(m) => m.test_regex_values(),
            MembershipType::MembershipRedirect(r) => r.test_regex_values(),
        }
    }
}

impl RegexValidator for PersonType {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        match self {
            PersonType::Person(p) => p.test_regex_values(),
            PersonType::PersonRedirect(r) => r.test_regex_values(),
        }
    }
}

impl RegexValidator for Person {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        let person_id = &self.id;
        regex_property!(person_id, PERSON_ID_REGEX, errors, "Person ID");
        errors
    }
}

impl RegexValidator for PersonRedirect {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        let person_id = &self.id;
        let redirect_id = &self.redirect;
        regex_property!(person_id, PERSON_ID_REGEX, errors, "Person ID");
        regex_property!(redirect_id, PERSON_ID_REGEX, errors, "Redirect ID");
        errors
    }
}

impl RegexValidator for MembershipRedirect {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        let member_id = &self.id;
        let redirect_id = &self.redirect;
        regex_property!(member_id, MEMBER_ID_REGEX, errors, "Member ID");
        regex_property!(redirect_id, MEMBER_ID_REGEX, errors, "Redirect ID");
        errors
    }
}

impl RegexValidator for Membership {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        let member_id = &self.id;
        let person_id = &self.person_id;
        regex_property!(member_id, MEMBER_ID_REGEX, errors, "Member ID");
        regex_property!(person_id, PERSON_ID_REGEX, errors, "Person ID");
        if let Some(post_id) = &self.post_id {
            regex_property!(post_id, POST_ID_REGEX, errors, "Post ID");
        }
        if let Some(organization_id) = &self.organization_id {
            regex_property!(organization_id, ORG_ID, errors, "Organization ID");
        }
        if let Some(on_behalf_of_id) = &self.on_behalf_of_id {
            regex_property!(on_behalf_of_id, ORG_ID, errors, "On Behalf Of ID");
        }
        errors
    }
}

trait HasId {
    fn get_id(&self) -> &String;
}

pub trait NiceName {
    fn nice_name(&self) -> String;
}

trait ValidDateRange {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection;
}

macro_rules! impl_valid_date_range {
    ($struct_name:ident) => {
        impl ValidDateRange for $struct_name {
            fn is_valid_date_range(&self) -> ValidatorErrorCollection {
                if self.start_date > self.end_date {
                    vec![ValidatorError {
                        error: format!(
                            "start date {} is after end date {}",
                            self.start_date, self.end_date
                        ),
                        validator_type: ValidatorType::DateRange,
                    }]
                } else {
                    vec![]
                }
            }
        }
    };
}

impl fmt::Display for ValidatorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValidatorType::InvalidJson => write!(f, "Invalid JSON"),
            ValidatorType::NonUniqueId => write!(f, "Non-unique ID"),
            ValidatorType::InvalidForeignKey => write!(f, "Invalid foreign key"),
            ValidatorType::DateRangeOverlap => write!(f, "Date range overlap"),
            ValidatorType::DateRange => write!(f, "Invalid date range"),
            ValidatorType::DateRangeOverlapSameDate => write!(f, "Date range overlap same date"),
            ValidatorType::RegexError => write!(f, "Regex error"),
        }
    }
}

fn check_valid_foreign_key(
    id: &String,
    ids: &HashSet<String>,
    item_type: &str,
    membership_id: &str,
) -> Option<ValidatorError> {
    if !ids.contains(id) {
        Some(ValidatorError {
            error: format!(
                "Membership: {} has invalid {}: {}",
                membership_id, item_type, id
            ),
            validator_type: ValidatorType::InvalidForeignKey,
        })
    } else {
        None
    }
}

fn check_unique<T>(
    items: &Vec<T>,
    id_fn: fn(&T) -> &String,
    item_type: &str,
) -> Result<HashSet<String>, ValidatorErrorCollection> {
    let mut ids: HashSet<String> = HashSet::new();
    let mut duplicates: ValidatorErrorCollection = Vec::new();

    for item in items {
        let id = id_fn(&item).clone();
        if !ids.insert(id.clone()) {
            duplicates.push(ValidatorError {
                error: format!("{}: {}", item_type, id),
                validator_type: ValidatorType::NonUniqueId,
            });
        }
    }

    if duplicates.is_empty() {
        Ok(ids)
    } else {
        Err(duplicates)
    }
}

impl HasId for Post {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl HasId for Organization {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl HasId for PersonRedirect {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl NiceName for BasicPersonName {
    fn nice_name(&self) -> String {
        match &self.given_name {
            Some(given_name) => format!("{} {}", given_name, self.family_name),
            None => self.family_name.clone(),
        }
    }
}

fn empty_string_as_default_start_date<'de, D>(deserializer: D) -> Result<FuzzyDate, D::Error>
where
    D: Deserializer<'de>,
{
    let s: String = Deserialize::deserialize(deserializer)?;
    if s.is_empty() {
        FuzzyDate::fromisoformat("0001-01-02").map_err(de::Error::custom)
    } else {
        FuzzyDate::fromisoformat(&s).map_err(de::Error::custom)
    }
}

fn serialize_default_start_date<S>(date: &FuzzyDate, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    if date == &FuzzyDate::fromisoformat("0001-01-02").unwrap() {
        serializer.serialize_str("")
    } else {
        serializer.serialize_str(&date.to_string())
    }
}

impl LordName {
    pub fn name_variants(&self) -> Vec<String> {
        // Peers with a lordname: only the full nice_name (adding the short form
        // would collide with a different person who holds plain "Lord Smith")
        if self.lordname.is_some() {
            return vec![self.nice_name()];
        }
        // Bishops, earls-of-a-place, etc.: multiple transcript spellings exist
        if let (Some(prefix), Some(place)) = (&self.honorific_prefix, &self.lordofname) {
            let mut variants = vec![
                format!("{} of {}", prefix, place),
                format!("The {} of {}", prefix, place),
            ];
            if prefix == "Bishop" {
                variants.push(format!("The Lord Bishop of {}", place));
            }
            return variants;
        }
        vec![self.nice_name()]
    }
}

impl NiceName for LordName {
    fn nice_name(&self) -> String {
        let name = if let Some(lordname) = &self.lordname {
            if let Some(lordofname) = &self.lordofname {
                format!("{} of {}", lordname, lordofname)
            } else {
                lordname.clone()
            }
        } else if let Some(lordofname) = &self.lordofname {
            if self.honorific_prefix.is_some() {
                format!("of {}", lordofname)
            } else if let Some(surname) = &self.surname {
                surname.clone()
            } else {
                return "Unknown".to_string();
            }
        } else if let Some(surname) = &self.surname {
            surname.clone()
        } else {
            return "Unknown".to_string();
        };

        let mut full_name = if let Some(honorific_prefix) = &self.honorific_prefix {
            format!("{} {}", honorific_prefix, name)
        } else {
            name
        };
        if let Some(honorific_suffix) = &self.honorific_suffix {
            full_name = format!("{} {}", full_name, honorific_suffix);
        }
        full_name
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct AltName {
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date"
    )]
    pub end_date: FuzzyDate,
    pub name: String,
    pub note: NameType,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub organization_id: Option<String>,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date"
    )]
    pub start_date: FuzzyDate,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Organization {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub classification: Option<OrgType>,
    pub id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub identifiers: Option<Vec<SimpleIdentifer>>,
    pub name: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct MembershipRedirect {
    pub id: String,
    pub redirect: String,
}

impl NiceName for AltName {
    fn nice_name(&self) -> String {
        self.name.clone()
    }
}

impl NameOptions {
    pub fn name_variants(&self) -> Vec<String> {
        match self {
            NameOptions::LordName(n) => n.name_variants(),
            _ => vec![self.nice_name()],
        }
    }

    pub fn date_range(&self) -> (&FuzzyDate, &FuzzyDate) {
        match self {
            NameOptions::BasicPersonName(n) => (&n.start_date, &n.end_date),
            NameOptions::LordName(n) => (&n.start_date, &n.end_date),
            NameOptions::AltName(n) => (&n.start_date, &n.end_date),
        }
    }

    pub fn note_str(&self) -> &str {
        match self {
            NameOptions::BasicPersonName(n) => match n.note {
                NameType::Main => "Main",
                NameType::Alternate => "Alternate",
            },
            NameOptions::LordName(n) => match n.note {
                NameType::Main => "Main",
                NameType::Alternate => "Alternate",
            },
            NameOptions::AltName(_) => "Alternate",
        }
    }
}

impl NiceName for NameOptions {
    fn nice_name(&self) -> String {
        match self {
            NameOptions::BasicPersonName(n) => n.nice_name(),
            NameOptions::LordName(n) => n.nice_name(),
            NameOptions::AltName(n) => n.nice_name(),
        }
    }
}

impl ValidDateRange for NameOptions {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection {
        match self {
            NameOptions::BasicPersonName(n) => n.is_valid_date_range(),
            NameOptions::LordName(n) => n.is_valid_date_range(),
            NameOptions::AltName(n) => n.is_valid_date_range(),
        }
    }
}

impl<'de> Deserialize<'de> for NameOptions {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value: serde_json::Value = serde::Deserialize::deserialize(deserializer)?;
        if value.get("name").is_some() {
            Ok(NameOptions::AltName(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        } else if value.get("family_name").is_some() {
            Ok(NameOptions::BasicPersonName(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        } else {
            Ok(NameOptions::LordName(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        }
    }
}

impl ValidDateRange for Person {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection {
        if let Some(other_names) = &self.other_names {
            other_names
                .iter()
                .flat_map(|name| name.is_valid_date_range())
                .collect()
        } else {
            Vec::new()
        }
    }
}

impl HasId for Person {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl HasId for MembershipRedirect {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl<'de> Deserialize<'de> for Identifier {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value: serde_json::Value = serde::Deserialize::deserialize(deserializer)?;
        if value.is_i64() {
            Ok(Identifier::Int(value.as_i64().unwrap()))
        } else if value.is_string() {
            Ok(Identifier::String(value.as_str().unwrap().to_string()))
        } else {
            Err(de::Error::custom("Expected string or number"))
        }
    }
}

impl HasId for Membership {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl ValidDateRange for PersonType {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection {
        match self {
            PersonType::Person(p) => p.is_valid_date_range(),
            PersonType::PersonRedirect(_) => Vec::new(),
        }
    }
}

impl<'de> Deserialize<'de> for PersonType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value: serde_json::Value = serde::Deserialize::deserialize(deserializer)?;
        if value.get("redirect").is_some() {
            Ok(PersonType::PersonRedirect(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        } else {
            Ok(PersonType::Person(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        }
    }
}

impl HasId for PersonType {
    fn get_id(&self) -> &String {
        match self {
            PersonType::Person(p) => p.get_id(),
            PersonType::PersonRedirect(p) => p.get_id(),
        }
    }
}

impl ValidDateRange for MembershipType {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection {
        match self {
            MembershipType::Membership(m) => m.is_valid_date_range(),
            MembershipType::MembershipRedirect(_) => Vec::new(),
        }
    }
}

impl<'de> Deserialize<'de> for MembershipType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value: serde_json::Value = serde::Deserialize::deserialize(deserializer)?;
        if value.get("redirect").is_some() {
            Ok(MembershipType::MembershipRedirect(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        } else {
            Ok(MembershipType::Membership(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        }
    }
}

impl HasId for MembershipType {
    fn get_id(&self) -> &String {
        match self {
            MembershipType::Membership(m) => m.get_id(),
            MembershipType::MembershipRedirect(m) => m.get_id(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Area {
    pub name: String,
    #[serde(default = "Vec::new", skip_serializing_if = "Vec::is_empty")]
    pub other_names: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Post {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub area: Option<Area>,
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date"
    )]
    pub end_date: FuzzyDate,
    pub id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub identifiers: Option<Vec<SimpleIdentifer>>,
    pub label: String,
    pub organization_id: String,
    pub role: String,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date"
    )]
    pub start_date: FuzzyDate,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Popolo {
    #[serde(default = "MembershipCollection::new")]
    pub memberships: MembershipCollection,
    #[serde(default = "OrganizationCollection::new")]
    pub organizations: OrganizationCollection,
    #[serde(default = "PersonCollection::new")]
    pub persons: PersonCollection,
    #[serde(default = "PostCollection::new")]
    pub posts: PostCollection,
}

impl ValidDateRange for Popolo {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        for person in &self.persons {
            errors.extend(person.is_valid_date_range());
        }
        for membership in &self.memberships {
            errors.extend(membership.is_valid_date_range());
        }
        for post in &self.posts {
            errors.extend(post.is_valid_date_range());
        }
        errors
    }
}

pub fn reduce_to_slug(s: &str) -> String {
    s.chars()
        .filter(|c| c.is_alphabetic())
        .flat_map(|c| c.to_lowercase())
        .collect()
}

impl Popolo {
    fn _just_persons(&self) -> Vec<&Person> {
        self.persons
            .iter()
            .filter_map(|p| match p {
                PersonType::Person(p) => Some(p),
                PersonType::PersonRedirect(_) => None,
            })
            .collect()
    }

    fn just_memberships(&self) -> Vec<&Membership> {
        self.memberships
            .iter()
            .filter_map(|m| match m {
                MembershipType::Membership(m) => Some(m),
                MembershipType::MembershipRedirect(_) => None,
            })
            .collect()
    }

    pub fn check_valid_date_ranges(&self) -> Result<(), ValidatorErrorCollection> {
        let mut errors: ValidatorErrorCollection = Vec::new();

        let no_org_str = "no_org".to_string();
        let just_memberships: Vec<&Membership> = self
            .just_memberships()
            .into_iter()
            .filter(|m| m.start_date > FuzzyDate::fromisoformat("1900-01-01").unwrap())
            .sorted_by_key(|m| {
                (
                    m.post_id
                        .as_ref()
                        .unwrap_or(m.organization_id.as_ref().unwrap_or(&no_org_str)),
                    &m.person_id,
                    &m.start_date,
                )
            })
            .collect();

        for (_key, chunk) in &just_memberships.into_iter().chunk_by(|m| {
            (
                m.post_id
                    .as_ref()
                    .unwrap_or(m.organization_id.as_ref().unwrap_or(&no_org_str)),
                &m.person_id,
            )
        }) {
            let group: Vec<&Membership> = chunk.collect();
            for i in 1..group.len() {
                let prev = group[i - 1];
                let curr = group[i];
                if prev.start_date.is_approximate() || curr.start_date.is_approximate() {
                    continue;
                }
                if prev.end_date.is_approximate() || curr.end_date.is_approximate() {
                    continue;
                }
                if prev.end_date > curr.start_date {
                    errors.push(ValidatorError {
                        error: format!("Membership {} overlaps with {}", prev.id, curr.id),
                        validator_type: ValidatorType::DateRangeOverlap,
                    });
                } else if prev.end_date == curr.start_date {
                    errors.push(ValidatorError {
                        error: format!(
                            "Membership {} ends on same date as {} starts",
                            prev.id, curr.id
                        ),
                        validator_type: ValidatorType::DateRangeOverlapSameDate,
                    });
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn check_unique_ids(&self) -> Result<(), ValidatorErrorCollection> {
        let mut errors: ValidatorErrorCollection = Vec::new();

        let person_ids = match check_unique(&self.persons.root, |p| p.get_id(), "Person") {
            Ok(ids) => ids,
            Err(e) => {
                errors.extend(e);
                HashSet::new()
            }
        };

        let org_ids = match check_unique(&self.organizations.root, |o| o.get_id(), "Organization") {
            Ok(ids) => ids,
            Err(e) => {
                errors.extend(e);
                HashSet::new()
            }
        };

        let post_ids = match check_unique(&self.posts.root, |p| p.get_id(), "Post") {
            Ok(ids) => ids,
            Err(e) => {
                errors.extend(e);
                HashSet::new()
            }
        };

        match check_unique(&self.memberships.root, |m| m.get_id(), "Membership") {
            Ok(_) => {}
            Err(e) => errors.extend(e),
        };

        for membership in self.just_memberships() {
            let person_id = &membership.person_id;
            if let Some(e) =
                check_valid_foreign_key(person_id, &person_ids, "person_id", &membership.id)
            {
                errors.push(e);
            }
            if let Some(post_id) = &membership.post_id {
                if let Some(e) =
                    check_valid_foreign_key(post_id, &post_ids, "post_id", &membership.id)
                {
                    errors.push(e);
                }
            }
            if let Some(organization_id) = &membership.organization_id {
                if let Some(e) = check_valid_foreign_key(
                    organization_id,
                    &org_ids,
                    "organization_id",
                    &membership.id,
                ) {
                    errors.push(e);
                }
            }
            if let Some(on_behalf_of_id) = &membership.on_behalf_of_id {
                if let Some(e) = check_valid_foreign_key(
                    on_behalf_of_id,
                    &org_ids,
                    "on_behalf_of_id",
                    &membership.id,
                ) {
                    errors.push(e);
                }
            }
        }

        for person_redirect in self.persons.iter().filter_map(|p| match p {
            PersonType::PersonRedirect(r) => Some(r),
            _ => None,
        }) {
            if let Some(e) = check_valid_foreign_key(
                &person_redirect.redirect,
                &person_ids,
                "redirect",
                &person_redirect.id,
            ) {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn model_dump_json(&self) -> String {
        serde_json::to_string_pretty(&self).unwrap()
    }

    pub fn set_lookup(&mut self) {
        self.persons.set_lookup();
        self.memberships.set_lookup();
        self.organizations.set_lookup();
        self.posts.set_lookup();
    }

    pub fn model_validate_json(
        json_str: &str,
        validate: &bool,
    ) -> Result<Self, ValidatorErrorCollection> {
        let mut popolo: Popolo = serde_json::from_str(json_str).map_err(|e| {
            vec![ValidatorError {
                error: e.to_string(),
                validator_type: ValidatorType::InvalidJson,
            }]
        })?;
        popolo.set_lookup();
        if *validate {
            let mut errors: ValidatorErrorCollection = Vec::new();
            if let Err(e) = popolo.check_unique_ids() {
                errors.extend(e);
            }
            if let Err(e) = popolo.check_valid_date_ranges() {
                errors.extend(e);
            }
            errors.extend(popolo.is_valid_date_range());
            errors.extend(popolo.test_regex_values());
            if !errors.is_empty() {
                return Err(errors);
            }
        }
        Ok(popolo)
    }

    /// Find the person_id for a person with a matching name active in the given chamber on the given date.
    pub fn find_person_id_by_name(
        &self,
        name: &str,
        chamber_id: &str,
        date: NaiveDate,
    ) -> Option<String> {
        let slug = reduce_to_slug(name);

        let post_ids: HashSet<&str> = self
            .posts
            .iter()
            .filter(|p| p.organization_id == chamber_id)
            .map(|p| p.id.as_str())
            .collect();

        let active_person_ids: HashSet<&str> = self
            .just_memberships()
            .iter()
            .filter(|m| {
                let via_post = m
                    .post_id
                    .as_deref()
                    .map(|pid| post_ids.contains(pid))
                    .unwrap_or(false);
                // Lords memberships link directly to the org with no post_id
                let via_org_direct =
                    m.post_id.is_none() && m.organization_id.as_deref() == Some(chamber_id);
                (via_post || via_org_direct)
                    && m.start_date.earliest_date <= date
                    && date <= m.end_date.latest_date
            })
            .map(|m| m.person_id.as_str())
            .collect();

        for pt in self.persons.iter() {
            if let PersonType::Person(person) = pt {
                if !active_person_ids.contains(person.id.as_str()) {
                    continue;
                }
                if let Some(names) = &person.other_names {
                    for n in names {
                        let (start, end) = n.date_range();
                        if start.earliest_date <= date && date <= end.latest_date {
                            for variant in n.name_variants() {
                                if reduce_to_slug(&variant) == slug {
                                    return Some(person.id.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Find the person_id for a person with a matching identifier in the given scheme.
    pub fn find_person_id_by_identifier(&self, identifier: &str, scheme: &str) -> Option<String> {
        for pt in self.persons.iter() {
            if let PersonType::Person(person) = pt {
                if let Some(ids) = &person.identifiers {
                    for id in ids {
                        if id.scheme == scheme && id.identifier.as_str() == identifier {
                            return Some(person.id.clone());
                        }
                    }
                }
            }
        }
        None
    }

    /// Return IDs of all non-redirect memberships for a given person_id.
    pub fn person_membership_ids(&self, person_id: &str) -> Vec<String> {
        self.just_memberships()
            .iter()
            .filter(|m| m.person_id == person_id)
            .map(|m| m.id.clone())
            .collect()
    }
}

impl_valid_date_range!(BasicPersonName);
impl_valid_date_range!(LordName);
impl_valid_date_range!(AltName);
impl_valid_date_range!(Membership);
impl_valid_date_range!(Post);
