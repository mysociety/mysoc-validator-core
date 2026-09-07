//! Detail values, type inference, groups and custom JSON serialization.
//!
//! `DetailValue` models a discriminated union whose tag is the sibling `type`
//! field of `RegmemDetail`, not a tag inside the value. Custom serde logic pairs
//! them, infers missing metadata and preserves decimal/date string encodings.
//! A group is a newtype around `Vec<RegmemDetail>`: serde's `transparent` writes
//! the list directly, matching a Python RootModel instead of an object with a
//! `root` key. Group mutation checks duplicate slugs before committing.

use crate::entries::RegmemAnnotation;
use crate::enums::CommonKey;
use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::de::Error as DeError;
use serde::ser::SerializeMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::str::FromStr;

/// lowercase, spaces -> `_`, only otherwise alphanumeric, no double `__`.
pub fn slugify(s: &str) -> String {
    let filtered: String = s
        .chars()
        .filter(|c| c.is_alphanumeric() || *c == ' ')
        .collect();
    let trimmed = filtered.trim();
    let underscored = trimmed.replace(' ', "_");
    let lower = underscored.to_lowercase();
    lower.replace("__", "_")
}

/// Mirror Python `str.title()` closely enough for display-name inference:
/// capitalise the first letter of each word, lowercase the rest.
fn title_case(s: &str) -> String {
    s.split(' ')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(first) => {
                    first.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase()
                }
                None => String::new(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

#[derive(Debug, Clone, PartialEq)]
pub enum DetailValue {
    Int(i64),
    Str(String),
    Decimal(Decimal),
    Date(NaiveDate),
    Float(f64),
    Bool(bool),
    Container(Vec<RegmemDetailGroup>),
}

impl DetailValue {
    /// The `type` string upstream uses for this variant.
    pub fn type_str(&self) -> &'static str {
        match self {
            Self::Int(_) => "int",
            Self::Str(_) => "string",
            Self::Decimal(_) => "decimal",
            Self::Date(_) => "date",
            Self::Float(_) => "float",
            Self::Bool(_) => "boolean",
            Self::Container(_) => "container",
        }
    }

    /// Build a `DetailValue` from a JSON value, given an optional `type` tag.
    fn from_json(type_: Option<&str>, value: serde_json::Value) -> Result<DetailValue, String> {
        match type_ {
            Some("int") => value
                .as_i64()
                .map(Self::Int)
                .ok_or_else(|| format!("expected int, got {value}")),
            Some("float") => value
                .as_f64()
                .map(Self::Float)
                .ok_or_else(|| format!("expected float, got {value}")),
            Some("boolean") => value
                .as_bool()
                .map(Self::Bool)
                .ok_or_else(|| format!("expected bool, got {value}")),
            Some("string") => value
                .as_str()
                .map(|s| Self::Str(s.to_string()))
                .ok_or_else(|| format!("expected string, got {value}")),
            Some("decimal") => {
                let s = value
                    .as_str()
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| value.to_string());
                Decimal::from_str(&s)
                    .or_else(|_| Decimal::from_str(s.trim()))
                    .map(Self::Decimal)
                    .map_err(|e| format!("invalid decimal {s:?}: {e}"))
            }
            Some("date") => {
                let s = value
                    .as_str()
                    .ok_or_else(|| format!("expected date string, got {value}"))?;
                NaiveDate::parse_from_str(s, "%Y-%m-%d")
                    .map(Self::Date)
                    .map_err(|e| format!("invalid date {s:?}: {e}"))
            }
            Some("container") => {
                let groups: Vec<RegmemDetailGroup> =
                    serde_json::from_value(value).map_err(|e| e.to_string())?;
                Ok(Self::Container(groups))
            }
            _ => Self::infer_from_json(value),
        }
    }

    /// Infer a `DetailValue` purely from the JSON shape (no `type` tag).
    fn infer_from_json(value: serde_json::Value) -> Result<DetailValue, String> {
        match value {
            serde_json::Value::Bool(b) => Ok(Self::Bool(b)),
            serde_json::Value::String(s) => Ok(Self::Str(s)),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Ok(Self::Int(i))
                } else {
                    Ok(Self::Float(n.as_f64().unwrap_or_default()))
                }
            }
            serde_json::Value::Array(_) => {
                let groups: Vec<RegmemDetailGroup> =
                    serde_json::from_value(value).map_err(|e| e.to_string())?;
                Ok(Self::Container(groups))
            }
            other => Err(format!("cannot infer detail value type from {other}")),
        }
    }
}

impl Serialize for DetailValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Int(i) => serializer.serialize_i64(*i),
            Self::Str(s) => serializer.serialize_str(s),
            Self::Decimal(d) => serializer.serialize_str(&d.to_string()),
            Self::Date(d) => serializer.serialize_str(&d.format("%Y-%m-%d").to_string()),
            Self::Float(f) => serializer.serialize_f64(*f),
            Self::Bool(b) => serializer.serialize_bool(*b),
            Self::Container(groups) => groups.serialize(serializer),
        }
    }
}

fn default_source() -> String {
    "official".to_string()
}

fn is_default_source(s: &str) -> bool {
    s == "official"
}

#[derive(Debug, Clone, PartialEq)]
pub struct RegmemDetail {
    pub source: String,
    pub slug: Option<String>,
    pub display_as: Option<String>,
    pub common_key: Option<CommonKey>,
    pub description: Option<String>,
    pub detail_type: Option<String>,
    pub value: Option<DetailValue>,
    pub annotations: Vec<RegmemAnnotation>,
}

impl RegmemDetail {
    /// Construct a detail from a value, running the upstream inference so that
    /// `slug`/`display_as`/`type` are populated consistently.
    pub fn new(
        slug: Option<String>,
        display_as: Option<String>,
        value: Option<DetailValue>,
    ) -> RegmemDetail {
        let mut detail = RegmemDetail {
            source: default_source(),
            slug,
            display_as,
            common_key: None,
            description: None,
            detail_type: None,
            value,
            annotations: Vec::new(),
        };
        detail.infer_slug();
        detail.infer_type();
        detail
    }

    /// if slug is missing, infer from display_as and vice versa.
    fn infer_slug(&mut self) {
        if self.slug.is_none() {
            if let Some(display_as) = &self.display_as {
                self.slug = Some(slugify(display_as));
            }
        }
        if self.display_as.is_none() {
            if let Some(slug) = &self.slug {
                self.display_as = Some(title_case(&slug.replace('_', " ")));
            }
        }
    }

    /// if type information is missing, infer from value.
    fn infer_type(&mut self) {
        if self.detail_type.is_none() {
            if let Some(value) = &self.value {
                self.detail_type = Some(value.type_str().to_string());
            }
        }
    }

    pub fn sub_detail_groups(&self) -> &[RegmemDetailGroup] {
        match &self.value {
            Some(DetailValue::Container(groups)) => groups,
            _ => &[],
        }
    }
}

impl Serialize for RegmemDetail {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(None)?;
        if !is_default_source(&self.source) {
            map.serialize_entry("source", &self.source)?;
        }
        if let Some(slug) = &self.slug {
            map.serialize_entry("slug", slug)?;
        }
        if let Some(display_as) = &self.display_as {
            map.serialize_entry("display_as", display_as)?;
        }
        if let Some(common_key) = &self.common_key {
            map.serialize_entry("common_key", common_key)?;
        }
        if let Some(description) = &self.description {
            map.serialize_entry("description", description)?;
        }
        if let Some(detail_type) = &self.detail_type {
            map.serialize_entry("type", detail_type)?;
        }
        if let Some(value) = &self.value {
            map.serialize_entry("value", value)?;
        }
        if !self.annotations.is_empty() {
            map.serialize_entry("annotations", &self.annotations)?;
        }
        map.end()
    }
}

impl<'de> Deserialize<'de> for RegmemDetail {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Raw {
            #[serde(default = "default_source")]
            source: String,
            #[serde(default)]
            slug: Option<String>,
            #[serde(default)]
            display_as: Option<String>,
            #[serde(default)]
            common_key: Option<CommonKey>,
            #[serde(default)]
            description: Option<String>,
            #[serde(rename = "type", default)]
            detail_type: Option<String>,
            #[serde(default)]
            value: Option<serde_json::Value>,
            #[serde(default)]
            annotations: Vec<RegmemAnnotation>,
        }
        let raw = Raw::deserialize(deserializer)?;
        let value = match raw.value {
            Some(v) if !v.is_null() => Some(
                DetailValue::from_json(raw.detail_type.as_deref(), v).map_err(D::Error::custom)?,
            ),
            _ => None,
        };
        let mut detail = RegmemDetail {
            source: raw.source,
            slug: raw.slug,
            display_as: raw.display_as,
            common_key: raw.common_key,
            description: raw.description,
            detail_type: raw.detail_type,
            value,
            annotations: raw.annotations,
        };
        detail.infer_slug();
        detail.infer_type();
        Ok(detail)
    }
}

#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
#[serde(transparent)]
pub struct RegmemDetailGroup(pub Vec<RegmemDetail>);

impl RegmemDetailGroup {
    pub fn new() -> RegmemDetailGroup {
        RegmemDetailGroup(Vec::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn append(&mut self, mut item: RegmemDetail, source: Option<&str>) -> Result<(), String> {
        if let Some(source) = source {
            item.source = source.to_string();
        }
        self.0.push(item);
        if let Err(e) = self.check_unique_detail_names() {
            self.0.pop();
            return Err(e);
        }
        Ok(())
    }

    pub fn extend(&mut self, items: Vec<RegmemDetail>, source: Option<&str>) -> Result<(), String> {
        let start = self.0.len();
        for mut item in items {
            if let Some(source) = source {
                item.source = source.to_string();
            }
            self.0.push(item);
        }
        if let Err(e) = self.check_unique_detail_names() {
            self.0.truncate(start);
            return Err(e);
        }
        Ok(())
    }

    pub fn check_unique_detail_names(&self) -> Result<(), String> {
        let mut seen = std::collections::HashSet::new();
        let mut duplicates = std::collections::BTreeSet::new();
        for detail in &self.0 {
            let key = detail.slug.clone().unwrap_or_default();
            if !seen.insert(key.clone()) {
                duplicates.insert(key);
            }
        }
        if duplicates.is_empty() {
            Ok(())
        } else {
            let names: Vec<String> = duplicates.into_iter().collect();
            Err(format!("Duplicate detail names in entry: {:?}", names))
        }
    }
}

#[cfg(test)]
mod tests;
