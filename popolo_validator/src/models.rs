//! Popolo records and their JSON representations.
//!
//! Person/membership enums distinguish full records from redirect records.
//! Custom deserialization selects by fields rather than relying on an untagged
//! union's trial order. Identifiers preserve string versus integer JSON values.
//! `Option<T>` represents optional data; serde field attributes control whether
//! absence is accepted or omitted on output. Model-specific serde code lives
//! beside the model, while cross-record checks live in `validation`.

use crate::dates::{
    default_end_date, default_start_date, deserialize_end_date, deserialize_start_date,
    is_default_end_date, is_default_start_date,
};
use crate::names::NameOptions;
use crate::names::NiceName;
use chrono::NaiveDate;
use fuzzy_date::FuzzyDate;
use serde::de::{self, Deserialize};
use serde::Serialize;
use serde_derive::Deserialize;

#[derive(
    Debug,
    Clone,
    Eq,
    PartialEq,
    Hash,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::Display,
    serde_with::SerializeDisplay,
    serde_with::DeserializeFromStr,
)]
#[strum(serialize_all = "snake_case")]
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

impl OrgType {
    pub fn as_str(&self) -> &'static str {
        self.into()
    }
}

pub type Extra = serde_json::Map<String, serde_json::Value>;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::Display,
    serde_with::SerializeDisplay,
    serde_with::DeserializeFromStr,
)]
#[strum(serialize_all = "snake_case")]
pub enum MembershipReason {
    #[strum(serialize = "")]
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
    pub fn as_str(&self) -> &'static str {
        self.into()
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
            Self::String(s) => s.clone(),
            Self::Int(i) => i.to_string(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct PersonRedirect {
    pub id: String,
    pub redirect: String,
}

#[serde_with::skip_serializing_none]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Link {
    pub note: Option<String>,
    pub url: String,
}

#[serde_with::skip_serializing_none]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Shortcuts {
    pub current_constituency: Option<String>,
    pub current_party: String,
}

#[serde_with::skip_serializing_none]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Person {
    pub biography: Option<String>,
    pub birth_date: Option<FuzzyDate>,
    pub death_date: Option<FuzzyDate>,
    pub extra: Option<Extra>,
    pub gender: Option<String>,
    pub id: String,
    pub identifiers: Option<Vec<SimpleIdentifer>>,
    pub image: Option<String>,
    pub links: Option<Vec<Link>>,
    pub other_names: Option<Vec<NameOptions>>,
    pub national_identity: Option<String>,
    pub summary: Option<String>,
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

#[serde_with::skip_serializing_none]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Membership {
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date",
        deserialize_with = "deserialize_end_date"
    )]
    pub end_date: FuzzyDate,
    pub end_reason: Option<MembershipReason>,
    pub extra: Option<Extra>,
    pub id: String,
    pub identifiers: Option<Vec<SimpleIdentifer>>,
    pub label: Option<String>,
    pub name: Option<MembershipName>,
    pub on_behalf_of_id: Option<String>,
    pub organization_id: Option<String>,
    pub person_id: String,
    pub post_id: Option<String>,
    pub reason: Option<String>,
    pub role: Option<String>,
    pub source: Option<String>,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date",
        deserialize_with = "deserialize_start_date"
    )]
    pub start_date: FuzzyDate,
    pub start_reason: Option<MembershipReason>,
}

#[serde_with::skip_serializing_none]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Organization {
    #[serde(rename = "abstract")]
    pub abstract_: Option<String>,
    pub classification: Option<OrgType>,
    pub description: Option<String>,
    pub dissolution_date: Option<FuzzyDate>,
    pub extra: Option<Extra>,
    pub founding_date: Option<FuzzyDate>,
    pub id: String,
    pub identifiers: Option<Vec<SimpleIdentifer>>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub links: Vec<String>,
    pub name: String,
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
            Ok(Self::Int(value.as_i64().unwrap()))
        } else if value.is_string() {
            Ok(Self::String(value.as_str().unwrap().to_string()))
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
            Ok(Self::PersonRedirect(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        } else {
            Ok(Self::Person(
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
            Ok(Self::MembershipRedirect(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        } else {
            Ok(Self::Membership(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        }
    }
}

#[serde_with::skip_serializing_none]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Area {
    pub extra: Option<Extra>,
    pub name: String,
    #[serde(default = "Vec::new", skip_serializing_if = "Vec::is_empty")]
    pub other_names: Vec<String>,
}

#[serde_with::skip_serializing_none]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Post {
    pub area: Option<Area>,
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date",
        deserialize_with = "deserialize_end_date"
    )]
    pub end_date: FuzzyDate,
    pub extra: Option<Extra>,
    pub id: String,
    pub identifiers: Option<Vec<SimpleIdentifer>>,
    pub label: String,
    pub organization_id: String,
    pub role: String,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date",
        deserialize_with = "deserialize_start_date"
    )]
    pub start_date: FuzzyDate,
}
