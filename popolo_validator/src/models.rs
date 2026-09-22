//! Popolo records and their JSON representations.
//!
//! Person/membership enums distinguish full records from redirect records.
//! Custom deserialization selects by fields rather than relying on an untagged
//! union's trial order. Identifiers preserve string versus integer JSON values.
//! `Option<T>` represents optional data; serde field attributes control whether
//! absence is accepted or omitted on output. Model-specific serde code lives
//! beside the model, while cross-record checks live in `validation`.

use crate::dates::{
    default_end_date, default_start_date, is_default_end_date, is_default_start_date,
};
use crate::names::NameOptions;
use crate::names::NiceName;
use chrono::NaiveDate;
use fuzzy_date::FuzzyDate;
use serde::de::{self, Deserialize};
use serde::Serialize;
use serde_derive::Deserialize;

#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
#[serde(deny_unknown_fields, rename_all = "lowercase")]
pub enum OrgType {
    Party,
    Chamber,
    Metro,
    Committee,
    CrossPartyGroup,
    LocalAuthority,
    CombinedAuthority,
    Other,
}

pub type Extra = serde_json::Map<String, serde_json::Value>;

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
#[serde(untagged)]
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

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct PersonRedirect {
    pub id: String,
    pub redirect: String,
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
pub struct Person {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub biography: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub birth_date: Option<FuzzyDate>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub death_date: Option<FuzzyDate>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extra: Option<Extra>,
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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extra: Option<Extra>,
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

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Organization {
    #[serde(rename = "abstract")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub abstract_: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub classification: Option<OrgType>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dissolution_date: Option<FuzzyDate>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extra: Option<Extra>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub founding_date: Option<FuzzyDate>,
    pub id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub identifiers: Option<Vec<SimpleIdentifer>>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub links: Vec<String>,
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_id: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct MembershipRedirect {
    pub id: String,
    pub redirect: String,
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

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Area {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extra: Option<Extra>,
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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extra: Option<Extra>,
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
