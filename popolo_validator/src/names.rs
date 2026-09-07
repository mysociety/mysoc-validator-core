//! Name variants, display forms and custom JSON dispatch.
//!
//! `NameOptions` is a Rust enum representing the Python union of name models.
//! JSON has no Rust variant names: custom deserialization chooses the shape from
//! its fields, and untagged serialization writes only the selected model. Keep
//! those rules beside their models so accepted input and output spelling can be
//! reviewed together. Missing and empty dates normalize to open bounds.
//! A focused declaration macro keeps LordName and its serde input in sync;
//! TryFrom validates the input before exposing the public model.
//! `NiceName` is shared behavior implemented by each variant, not inheritance.

use crate::dates::{
    default_end_date, default_start_date, deserialize_end_date, deserialize_start_date,
    is_default_end_date, is_default_start_date,
};
use fuzzy_date::FuzzyDate;
use serde::de::Deserialize;
use serde::Serialize;
use serde_derive::Deserialize;

#[derive(Serialize, Debug, Clone)]
#[serde(untagged, deny_unknown_fields)]
pub enum NameOptions {
    BasicPersonName(BasicPersonName),
    LordName(LordName),
    AltName(AltName),
}

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
pub enum NameType {
    Main,
    Alternate,
}

/// Declare validated serde models once, keeping the unchecked input private.
macro_rules! lord_name_model {
    ({ $( $(#[$attribute:meta])* pub $field:ident: $type:ty, )* }) => {
        #[serde_with::skip_serializing_none]
        #[derive(Serialize, Deserialize, Debug, Clone)]
        #[serde(try_from = "LordNameData", deny_unknown_fields)]
        pub struct LordName {
            $( $(#[$attribute])* pub $field: $type, )*
        }

        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct LordNameData {
            $( $(#[$attribute])* $field: $type, )*
        }

        impl TryFrom<LordNameData> for LordName {
            type Error = &'static str;

            fn try_from(input: LordNameData) -> Result<Self, Self::Error> {
                let name = Self { $( $field: input.$field, )* };
                name.validate()?;
                Ok(name)
            }
        }
    };
}

lord_name_model!({
    pub additional_name: Option<String>,
    pub county: Option<String>,
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date",
        deserialize_with = "deserialize_end_date"
    )]
    pub end_date: FuzzyDate,
    pub given_name: Option<String>,
    pub honorific_prefix: String,
    pub honorific_suffix: Option<String>,
    pub lordname: Option<String>,
    pub lordofname: Option<String>,
    pub lordofname_full: Option<String>,
    pub note: NameType,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date",
        deserialize_with = "deserialize_start_date"
    )]
    pub start_date: FuzzyDate,
    pub surname: Option<String>,
});

#[serde_with::skip_serializing_none]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct BasicPersonName {
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date",
        deserialize_with = "deserialize_end_date"
    )]
    pub end_date: FuzzyDate,
    pub family_name: String,
    pub given_name: Option<String>,
    pub honorific_prefix: Option<String>,
    pub note: NameType,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date",
        deserialize_with = "deserialize_start_date"
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
    /// Require enough peerage-style data to display a nonempty surname.
    /// Empty strings follow upstream Python truthiness, just like absent values.
    pub fn validate(&self) -> Result<(), &'static str> {
        let nonempty = |value: &Option<String>| value.as_ref().is_some_and(|s| !s.is_empty());
        if nonempty(&self.surname)
            && !nonempty(&self.lordname)
            && (!nonempty(&self.lordofname) || self.honorific_prefix.is_empty())
        {
            return Err(
                "LordName with surname also needs a lordname, or a lordofname and honorific_prefix",
            );
        }
        Ok(())
    }

    pub fn original_full_name(&self) -> Option<String> {
        self.surname
            .as_ref()
            .filter(|surname| !surname.is_empty())
            .map(
                |surname| match self.given_name.as_ref().filter(|given| !given.is_empty()) {
                    Some(given_name) => format!("{} {}", given_name, surname),
                    None => surname.clone(),
                },
            )
    }

    pub fn name_variants(&self, include_original_name: bool) -> Vec<String> {
        let full = self.nice_name();
        let mut variants = vec![full.clone()];

        if self.lordname.as_ref().is_none_or(|name| name.is_empty())
            && self
                .lordofname
                .as_ref()
                .is_some_and(|name| !name.is_empty())
            && !self.honorific_prefix.is_empty()
        {
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
        let name = if let Some(lordname) = self.lordname.as_ref().filter(|name| !name.is_empty()) {
            if let Some(lordofname) = self.lordofname.as_ref().filter(|name| !name.is_empty()) {
                format!("{} of {}", lordname, lordofname)
            } else {
                lordname.clone()
            }
        } else if let Some(lordofname) = self.lordofname.as_ref().filter(|name| !name.is_empty()) {
            format!("of {}", lordofname)
        } else {
            return "Unknown".to_string();
        };

        let mut full_name = if self.honorific_prefix.is_empty() {
            name
        } else {
            format!("{} {}", self.honorific_prefix, name)
        };
        if let Some(honorific_suffix) = self
            .honorific_suffix
            .as_ref()
            .filter(|suffix| !suffix.is_empty())
        {
            full_name = format!("{} {}", full_name, honorific_suffix);
        }
        full_name
    }
}

#[serde_with::skip_serializing_none]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct AltName {
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date",
        deserialize_with = "deserialize_end_date"
    )]
    pub end_date: FuzzyDate,
    pub name: String,
    pub note: NameType,
    pub organization_id: Option<String>,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date",
        deserialize_with = "deserialize_start_date"
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
            Self::LordName(n) => n.name_variants(include_original_name),
            _ => vec![self.nice_name()],
        }
    }

    pub fn date_range(&self) -> (&FuzzyDate, &FuzzyDate) {
        match self {
            Self::BasicPersonName(n) => (&n.start_date, &n.end_date),
            Self::LordName(n) => (&n.start_date, &n.end_date),
            Self::AltName(n) => (&n.start_date, &n.end_date),
        }
    }

    pub fn note_str(&self) -> &str {
        match self {
            Self::BasicPersonName(n) => n.note.as_str(),
            Self::LordName(n) => n.note.as_str(),
            Self::AltName(_) => "Alternate",
        }
    }
}

impl NiceName for NameOptions {
    fn nice_name(&self) -> String {
        match self {
            Self::BasicPersonName(n) => n.nice_name(),
            Self::LordName(n) => n.nice_name(),
            Self::AltName(n) => n.nice_name(),
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
            Ok(Self::AltName(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        } else if value.get("family_name").is_some() {
            Ok(Self::BasicPersonName(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        } else {
            Ok(Self::LordName(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        }
    }
}

impl NameType {
    pub fn as_str(&self) -> &'static str {
        self.into()
    }
}
