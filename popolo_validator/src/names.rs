//! Name variants, display forms and custom JSON dispatch.
//!
//! `NameOptions` is a Rust enum representing the Python union of name models.
//! JSON has no Rust variant names: custom deserialization chooses the shape from
//! its fields, and untagged serialization writes only the selected model. Keep
//! those rules and `LordNameData` beside their models so accepted input and output
//! spelling can be reviewed together. Serde defaults/omission preserve missing,
//! empty and sentinel date distinctions supplied by `dates`.
//! `NiceName` is shared behavior implemented by each variant, not inheritance.

use crate::dates::{
    default_end_date, default_start_date, empty_string_as_default_start_date, is_default_end_date,
    is_default_start_date, serialize_default_start_date,
};
use fuzzy_date::FuzzyDate;
use serde::de::{self, Deserialize, Deserializer};
use serde::Serialize;
use serde_derive::Deserialize;

#[derive(Serialize, Debug, Clone)]
#[serde(untagged, deny_unknown_fields)]
pub enum NameOptions {
    BasicPersonName(BasicPersonName),
    LordName(LordName),
    AltName(AltName),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
pub enum NameType {
    Main,
    Alternate,
}

#[derive(Serialize, Debug, Clone)]
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
    pub honorific_prefix: String,
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

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct LordNameData {
    #[serde(default)]
    additional_name: Option<String>,
    #[serde(default)]
    county: Option<String>,
    #[serde(default = "default_end_date")]
    end_date: FuzzyDate,
    #[serde(default)]
    given_name: Option<String>,
    honorific_prefix: String,
    #[serde(default)]
    honorific_suffix: Option<String>,
    #[serde(default)]
    lordname: Option<String>,
    #[serde(default)]
    lordofname: Option<String>,
    #[serde(default)]
    lordofname_full: Option<String>,
    note: NameType,
    #[serde(
        default = "default_start_date",
        deserialize_with = "empty_string_as_default_start_date"
    )]
    start_date: FuzzyDate,
    #[serde(default)]
    surname: Option<String>,
}

impl<'de> Deserialize<'de> for LordName {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let data = LordNameData::deserialize(deserializer)?;
        if data.surname.is_some()
            && data.lordname.is_none()
            && (data.lordofname.is_none() || data.honorific_prefix.is_empty())
        {
            return Err(de::Error::custom(
                "LordName with surname also needs a lordname, or a lordofname and honorific_prefix",
            ));
        }
        Ok(Self {
            additional_name: data.additional_name,
            county: data.county,
            end_date: data.end_date,
            given_name: data.given_name,
            honorific_prefix: data.honorific_prefix,
            honorific_suffix: data.honorific_suffix,
            lordname: data.lordname,
            lordofname: data.lordofname,
            lordofname_full: data.lordofname_full,
            note: data.note,
            start_date: data.start_date,
            surname: data.surname,
        })
    }
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

pub trait NiceName {
    fn nice_name(&self) -> String;
}

impl NiceName for BasicPersonName {
    fn nice_name(&self) -> String {
        match &self.given_name {
            Some(given_name) => format!("{} {}", given_name, self.family_name),
            None => self.family_name.clone(),
        }
    }
}

impl LordName {
    pub fn original_full_name(&self) -> Option<String> {
        self.surname.as_ref().map(|surname| match &self.given_name {
            Some(given_name) => format!("{} {}", given_name, surname),
            None => surname.clone(),
        })
    }

    pub fn name_variants(&self, include_original_name: bool) -> Vec<String> {
        let full = self.nice_name();
        let mut variants = vec![full.clone()];

        if self.lordname.is_none() && self.lordofname.is_some() {
            variants.push(format!("The {}", full));
            if self.honorific_prefix == "Bishop" {
                variants.push(format!(
                    "The Lord Bishop of {}",
                    self.lordofname.as_ref().unwrap()
                ));
            }
        }

        if include_original_name {
            if let Some(original_name) = self.original_full_name() {
                variants.push(original_name);
            }
        }

        variants
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
            format!("of {}", lordofname)
        } else {
            return "Unknown".to_string();
        };

        let mut full_name = format!("{} {}", self.honorific_prefix, name);
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

impl NiceName for AltName {
    fn nice_name(&self) -> String {
        self.name.clone()
    }
}

impl NameOptions {
    pub fn name_variants(&self, include_original_name: bool) -> Vec<String> {
        match self {
            NameOptions::LordName(n) => n.name_variants(include_original_name),
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
