//! Pure-Rust domain library for the mySociety "register of members' interests"
//! JSON format.
//!
//! This mirrors `mysoc_validator.models.interests` (the `RegmemRegister` family)
//! from the upstream Python library. It is deliberately free of any PyO3
//! dependency — the `mysoc_validator_fast` crate wraps these types for Python.
//!
//! Notable behaviours replicated from upstream:
//!  - `RegmemDetail.value` is a discriminated union keyed by the sibling `type`
//!    field (`decimal`/`date` arrive as strings, `container` as a list of
//!    groups, etc.). When `type` is absent it is inferred from the value.
//!  - `infer_slug` / `infer_type` fill in `slug` <-> `display_as` and `type`.
//!  - `RegmemAnnotation.date_added` defaults to today when absent.
//!  - `RegmemRegister` enforces the global-or-per-person chamber/language/date
//!    rule on load.
//!
//! `comparable_id` / `item_hash` are exposed as best-effort computed values.
//! Upstream computes `item_hash` as a Python `md5(str(model_dump dict))`, which
//! is not cleanly reproducible in Rust; the value here is stable within a build
//! but is intentionally *not* byte-identical to upstream, and is not serialised.

use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::de::Error as DeError;
use serde::ser::SerializeMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::str::FromStr;

// ---------------------------------------------------------------------------
// Small enums
// ---------------------------------------------------------------------------

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Chamber {
    #[serde(rename = "house-of-commons")]
    Commons,
    #[serde(rename = "house-of-lords")]
    Lords,
    #[serde(rename = "scottish-parliament")]
    Scotland,
    #[serde(rename = "welsh-parliament")]
    Senedd,
    #[serde(rename = "london-assembly")]
    London,
    #[serde(rename = "northern-ireland-assembly")]
    NorthernIreland,
}

impl Chamber {
    pub fn as_str(&self) -> &'static str {
        match self {
            Chamber::Commons => "house-of-commons",
            Chamber::Lords => "house-of-lords",
            Chamber::Scotland => "scottish-parliament",
            Chamber::Senedd => "welsh-parliament",
            Chamber::London => "london-assembly",
            Chamber::NorthernIreland => "northern-ireland-assembly",
        }
    }

    pub fn from_str_opt(s: &str) -> Option<Chamber> {
        match s {
            "house-of-commons" => Some(Chamber::Commons),
            "house-of-lords" => Some(Chamber::Lords),
            "scottish-parliament" => Some(Chamber::Scotland),
            "welsh-parliament" => Some(Chamber::Senedd),
            "london-assembly" => Some(Chamber::London),
            "northern-ireland-assembly" => Some(Chamber::NorthernIreland),
            _ => None,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Default)]
#[serde(rename_all = "lowercase")]
pub enum Language {
    #[default]
    En,
    Cy,
}

impl Language {
    pub fn as_str(&self) -> &'static str {
        match self {
            Language::En => "en",
            Language::Cy => "cy",
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Default)]
#[serde(rename_all = "lowercase")]
pub enum ContentType {
    #[default]
    String,
    Markdown,
    Xml,
}

impl ContentType {
    pub fn as_str(&self) -> &'static str {
        match self {
            ContentType::String => "string",
            ContentType::Markdown => "markdown",
            ContentType::Xml => "xml",
        }
    }
}

fn is_default_content_type(c: &ContentType) -> bool {
    *c == ContentType::String
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum CommonKey {
    CompaniesHouse,
    Url,
    StandardisedName,
    SicCode,
}

impl CommonKey {
    pub fn as_str(&self) -> &'static str {
        match self {
            CommonKey::CompaniesHouse => "companies_house",
            CommonKey::Url => "url",
            CommonKey::StandardisedName => "standardised_name",
            CommonKey::SicCode => "sic_code",
        }
    }

    pub fn from_str_opt(s: &str) -> Option<CommonKey> {
        match s {
            "companies_house" => Some(CommonKey::CompaniesHouse),
            "url" => Some(CommonKey::Url),
            "standardised_name" => Some(CommonKey::StandardisedName),
            "sic_code" => Some(CommonKey::SicCode),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
// slugify / title-case helpers (mirroring upstream `slugify`)
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// DetailValue — discriminated union keyed by the `type` field
// ---------------------------------------------------------------------------

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
            DetailValue::Int(_) => "int",
            DetailValue::Str(_) => "string",
            DetailValue::Decimal(_) => "decimal",
            DetailValue::Date(_) => "date",
            DetailValue::Float(_) => "float",
            DetailValue::Bool(_) => "boolean",
            DetailValue::Container(_) => "container",
        }
    }

    /// Build a `DetailValue` from a JSON value, given an optional `type` tag.
    fn from_json(type_: Option<&str>, value: serde_json::Value) -> Result<DetailValue, String> {
        match type_ {
            Some("int") => value
                .as_i64()
                .map(DetailValue::Int)
                .ok_or_else(|| format!("expected int, got {value}")),
            Some("float") => value
                .as_f64()
                .map(DetailValue::Float)
                .ok_or_else(|| format!("expected float, got {value}")),
            Some("boolean") => value
                .as_bool()
                .map(DetailValue::Bool)
                .ok_or_else(|| format!("expected bool, got {value}")),
            Some("string") => value
                .as_str()
                .map(|s| DetailValue::Str(s.to_string()))
                .ok_or_else(|| format!("expected string, got {value}")),
            Some("decimal") => {
                let s = value
                    .as_str()
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| value.to_string());
                Decimal::from_str(&s)
                    .or_else(|_| Decimal::from_str(s.trim()))
                    .map(DetailValue::Decimal)
                    .map_err(|e| format!("invalid decimal {s:?}: {e}"))
            }
            Some("date") => {
                let s = value
                    .as_str()
                    .ok_or_else(|| format!("expected date string, got {value}"))?;
                NaiveDate::parse_from_str(s, "%Y-%m-%d")
                    .map(DetailValue::Date)
                    .map_err(|e| format!("invalid date {s:?}: {e}"))
            }
            Some("container") => {
                let groups: Vec<RegmemDetailGroup> =
                    serde_json::from_value(value).map_err(|e| e.to_string())?;
                Ok(DetailValue::Container(groups))
            }
            _ => Self::infer_from_json(value),
        }
    }

    /// Infer a `DetailValue` purely from the JSON shape (no `type` tag).
    fn infer_from_json(value: serde_json::Value) -> Result<DetailValue, String> {
        match value {
            serde_json::Value::Bool(b) => Ok(DetailValue::Bool(b)),
            serde_json::Value::String(s) => Ok(DetailValue::Str(s)),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Ok(DetailValue::Int(i))
                } else {
                    Ok(DetailValue::Float(n.as_f64().unwrap_or_default()))
                }
            }
            serde_json::Value::Array(_) => {
                let groups: Vec<RegmemDetailGroup> =
                    serde_json::from_value(value).map_err(|e| e.to_string())?;
                Ok(DetailValue::Container(groups))
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
            DetailValue::Int(i) => serializer.serialize_i64(*i),
            DetailValue::Str(s) => serializer.serialize_str(s),
            DetailValue::Decimal(d) => serializer.serialize_str(&d.to_string()),
            DetailValue::Date(d) => serializer.serialize_str(&d.format("%Y-%m-%d").to_string()),
            DetailValue::Float(f) => serializer.serialize_f64(*f),
            DetailValue::Bool(b) => serializer.serialize_bool(*b),
            DetailValue::Container(groups) => groups.serialize(serializer),
        }
    }
}

// ---------------------------------------------------------------------------
// RegmemAnnotation
// ---------------------------------------------------------------------------

fn default_annotation_type() -> String {
    "note".to_string()
}

fn is_default_annotation_type(s: &str) -> bool {
    s == "note"
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct RegmemAnnotation {
    pub author: String,
    #[serde(rename = "type", skip_serializing_if = "is_default_annotation_type")]
    pub annotation_type: String,
    pub content: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub date_added: Option<NaiveDate>,
    #[serde(skip_serializing_if = "is_default_content_type")]
    pub content_format: ContentType,
}

impl RegmemAnnotation {
    pub fn new(
        author: String,
        content: String,
        annotation_type: Option<String>,
        date_added: Option<NaiveDate>,
        content_format: Option<ContentType>,
    ) -> RegmemAnnotation {
        RegmemAnnotation {
            author,
            annotation_type: annotation_type.unwrap_or_else(default_annotation_type),
            content,
            // mirror `date_is_today`: default to today when absent.
            date_added: Some(date_added.unwrap_or_else(|| chrono::Local::now().date_naive())),
            content_format: content_format.unwrap_or_default(),
        }
    }
}

impl<'de> Deserialize<'de> for RegmemAnnotation {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Raw {
            author: String,
            #[serde(rename = "type", default = "default_annotation_type")]
            annotation_type: String,
            content: String,
            #[serde(default)]
            date_added: Option<NaiveDate>,
            #[serde(default)]
            content_format: ContentType,
        }
        let raw = Raw::deserialize(deserializer)?;
        Ok(RegmemAnnotation::new(
            raw.author,
            raw.content,
            Some(raw.annotation_type),
            raw.date_added,
            Some(raw.content_format),
        ))
    }
}

// ---------------------------------------------------------------------------
// RegmemDetail
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// RegmemDetailGroup — a RootModel list of details
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// RegmemInfoBase / RegmemEntry / RegmemSummary
// ---------------------------------------------------------------------------

fn is_false(b: &bool) -> bool {
    !*b
}

fn is_empty_string(s: &str) -> bool {
    s.is_empty()
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RegmemInfoBase {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    #[serde(default, skip_serializing_if = "is_empty_string")]
    pub content: String,
    #[serde(default, skip_serializing_if = "is_default_content_type")]
    pub content_format: ContentType,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub date_registered: Option<NaiveDate>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub date_published: Option<NaiveDate>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub date_updated: Option<NaiveDate>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub date_received: Option<NaiveDate>,
    #[serde(default, skip_serializing_if = "is_false")]
    pub null_entry: bool,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub annotations: Vec<RegmemAnnotation>,
    #[serde(default, skip_serializing_if = "RegmemDetailGroup::is_empty")]
    pub details: RegmemDetailGroup,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub sub_entries: Vec<RegmemEntry>,
}

impl RegmemInfoBase {
    pub fn get_detail(&self, name: &str) -> Option<&RegmemDetail> {
        self.details.0.iter().find(|d| {
            d.slug.as_deref() == Some(name)
                || d.display_as.as_deref() == Some(name)
                || d.common_key.map(|c| c.as_str()) == Some(name)
        })
    }

    pub fn get_detail_value(&self, name: &str) -> Option<&DetailValue> {
        self.get_detail(name).and_then(|d| d.value.as_ref())
    }

    /// Add scalar details (no pandas/DataFrame branch). Infers type per value.
    pub fn add_details(
        &mut self,
        source: Option<&str>,
        values: Vec<(String, DetailValue)>,
    ) -> Result<(), String> {
        for (slug, value) in values {
            let detail = RegmemDetail::new(Some(slug), None, Some(value));
            self.details.append(detail, source)?;
        }
        Ok(())
    }

    pub fn comparable_id(&self) -> String {
        match &self.id {
            Some(id) => id.clone(),
            None => self.item_hash(),
        }
    }

    /// Best-effort stable hash. NOT byte-identical to the upstream Python
    /// `md5(str(model_dump))` value (see crate-level docs).
    pub fn item_hash(&self) -> String {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        self.content.hash(&mut hasher);
        format!("{:?}", self.date_registered).hash(&mut hasher);
        format!("{:?}", self.date_published).hash(&mut hasher);
        format!("{:?}", self.date_updated).hash(&mut hasher);
        format!("{:?}", self.date_received).hash(&mut hasher);
        format!("{:?}", self.id).hash(&mut hasher);
        serde_json::to_string(&self.details)
            .unwrap_or_default()
            .hash(&mut hasher);
        for sub in &self.sub_entries {
            sub.base.item_hash().hash(&mut hasher);
        }
        let full = format!("{:016x}", hasher.finish());
        full[..10].to_string()
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Default)]
#[serde(rename_all = "lowercase")]
pub enum EntryType {
    #[default]
    Entry,
    Subentry,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Default)]
#[serde(rename_all = "lowercase")]
pub enum SummaryType {
    #[default]
    Summary,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RegmemEntry {
    #[serde(flatten)]
    pub base: RegmemInfoBase,
    #[serde(default)]
    pub info_type: EntryType,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RegmemSummary {
    #[serde(flatten)]
    pub base: RegmemInfoBase,
    #[serde(default)]
    pub info_type: SummaryType,
}

// ---------------------------------------------------------------------------
// RegmemCategory / RegmemPerson / RegmemRegister
// ---------------------------------------------------------------------------

fn default_category_id() -> String {
    String::new()
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RegmemCategory {
    #[serde(default = "default_category_id")]
    pub category_id: String,
    pub category_name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub category_description: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub legislation_or_rule_name: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub legislation_or_rule_url: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub summaries: Vec<RegmemSummary>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub entries: Vec<RegmemEntry>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RegmemPerson {
    pub person_id: String,
    pub person_name: String,
    pub published_date: NaiveDate,
    pub chamber: Chamber,
    #[serde(default)]
    pub language: Language,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub categories: Vec<RegmemCategory>,
}

impl RegmemPerson {
    pub fn get_category_index_by_id(&self, category_id: &str) -> Option<usize> {
        self.categories
            .iter()
            .position(|c| c.category_id == category_id)
    }
}

fn default_language() -> Option<Language> {
    Some(Language::En)
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RegmemRegister {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub chamber: Option<Chamber>,
    #[serde(default = "default_language", skip_serializing_if = "Option::is_none")]
    pub language: Option<Language>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub published_date: Option<NaiveDate>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub annotations: Vec<RegmemAnnotation>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub summaries: Vec<RegmemSummary>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub persons: Vec<RegmemPerson>,
}

/// Locates an entry within the register tree, as produced by `iter_entries`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EntryLocation {
    pub person: usize,
    pub category: usize,
    pub entry: usize,
    pub sub_entry: Option<usize>,
}

impl RegmemRegister {
    pub fn model_validate_json(json_str: &str, validate: bool) -> Result<Self, String> {
        let register: RegmemRegister = serde_json::from_str(json_str).map_err(|e| e.to_string())?;
        if validate {
            register.either_global_or_person_chambers()?;
        }
        Ok(register)
    }

    pub fn model_dump_json(&self) -> String {
        serde_json::to_string_pretty(self).unwrap()
    }

    /// Mirror upstream `either_global_or_person_chambers`.
    pub fn either_global_or_person_chambers(&self) -> Result<(), String> {
        match self.chamber {
            None => {
                // Each person must have a chamber (always true given the type).
                // Kept for parity with upstream intent.
            }
            Some(chamber) => {
                for person in &self.persons {
                    if person.chamber != chamber {
                        return Err(
                            "All persons must have the same chamber as the register".to_string()
                        );
                    }
                }
            }
        }
        if let Some(language) = self.language {
            for person in &self.persons {
                if person.language != language {
                    return Err(
                        "All persons must have the same language as the register".to_string()
                    );
                }
            }
        }
        // published_date: RegmemPerson.published_date is required, so the
        // upstream "must be set" check is structurally guaranteed here.
        Ok(())
    }

    pub fn get_person_index_by_id(&self, person_id: &str) -> Option<usize> {
        self.persons.iter().position(|p| p.person_id == person_id)
    }

    /// Locations of every entry and sub-entry, in iteration order.
    pub fn iter_entry_locations(&self) -> Vec<EntryLocation> {
        let mut out = Vec::new();
        for (pi, person) in self.persons.iter().enumerate() {
            for (ci, category) in person.categories.iter().enumerate() {
                for (ei, entry) in category.entries.iter().enumerate() {
                    out.push(EntryLocation {
                        person: pi,
                        category: ci,
                        entry: ei,
                        sub_entry: None,
                    });
                    for si in 0..entry.base.sub_entries.len() {
                        out.push(EntryLocation {
                            person: pi,
                            category: ci,
                            entry: ei,
                            sub_entry: Some(si),
                        });
                    }
                }
            }
        }
        out
    }

    /// Resolve a location to the underlying entry (immutable).
    pub fn entry_at(&self, loc: &EntryLocation) -> Option<&RegmemEntry> {
        let entry = self
            .persons
            .get(loc.person)?
            .categories
            .get(loc.category)?
            .entries
            .get(loc.entry)?;
        match loc.sub_entry {
            None => Some(entry),
            Some(si) => entry.base.sub_entries.get(si),
        }
    }

    /// Resolve a location to the underlying entry (mutable).
    pub fn entry_at_mut(&mut self, loc: &EntryLocation) -> Option<&mut RegmemEntry> {
        let entry = self
            .persons
            .get_mut(loc.person)?
            .categories
            .get_mut(loc.category)?
            .entries
            .get_mut(loc.entry)?;
        match loc.sub_entry {
            None => Some(entry),
            Some(si) => entry.base.sub_entries.get_mut(si),
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slugify() {
        assert_eq!(slugify("Donation Source"), "donation_source");
        assert_eq!(slugify("  Value!! "), "value");
        assert_eq!(slugify("A  B"), "a_b");
    }

    #[test]
    fn test_title_case() {
        assert_eq!(title_case("donation source"), "Donation Source");
    }

    #[test]
    fn test_infer_slug_and_type() {
        let d = RegmemDetail::new(None, Some("Donation Source".to_string()), None);
        assert_eq!(d.slug.as_deref(), Some("donation_source"));

        let d = RegmemDetail::new(Some("donor_name".to_string()), None, None);
        assert_eq!(d.display_as.as_deref(), Some("Donor Name"));

        let d = RegmemDetail::new(
            Some("value".to_string()),
            None,
            Some(DetailValue::Decimal(Decimal::from_str("5000.00").unwrap())),
        );
        assert_eq!(d.detail_type.as_deref(), Some("decimal"));
    }

    #[test]
    fn test_detail_value_roundtrip() {
        let json = r#"[
            {"slug": "v", "type": "decimal", "value": "5000.00"},
            {"slug": "d", "type": "date", "value": "2024-07-23"},
            {"slug": "flag", "type": "boolean", "value": true},
            {"slug": "n", "type": "int", "value": 3}
        ]"#;
        let group: RegmemDetailGroup = serde_json::from_str(json).unwrap();
        assert_eq!(group.len(), 4);
        match group.0[0].value.as_ref().unwrap() {
            DetailValue::Decimal(d) => assert_eq!(d.to_string(), "5000.00"),
            other => panic!("expected decimal, got {other:?}"),
        }
        match group.0[1].value.as_ref().unwrap() {
            DetailValue::Date(d) => assert_eq!(d.format("%Y-%m-%d").to_string(), "2024-07-23"),
            other => panic!("expected date, got {other:?}"),
        }
        // round-trip preserves the decimal string form
        let dumped = serde_json::to_string(&group).unwrap();
        let reparsed: RegmemDetailGroup = serde_json::from_str(&dumped).unwrap();
        assert_eq!(group, reparsed);
    }

    #[test]
    fn test_container_detail() {
        let json = r#"{
            "slug": "legs",
            "type": "container",
            "value": [[{"slug": "from", "type": "string", "value": "A"}]]
        }"#;
        let detail: RegmemDetail = serde_json::from_str(json).unwrap();
        assert_eq!(detail.sub_detail_groups().len(), 1);
        assert_eq!(detail.sub_detail_groups()[0].len(), 1);
    }

    #[test]
    fn test_check_unique_detail_names() {
        let mut group = RegmemDetailGroup::new();
        group
            .append(RegmemDetail::new(Some("a".to_string()), None, None), None)
            .unwrap();
        let err = group
            .append(RegmemDetail::new(Some("a".to_string()), None, None), None)
            .unwrap_err();
        assert!(err.contains("Duplicate detail names"));
        // failed append did not grow the group
        assert_eq!(group.len(), 1);
    }

    #[test]
    fn test_add_details_infers_type() {
        let mut base = RegmemInfoBase {
            id: None,
            content: String::new(),
            content_format: ContentType::String,
            date_registered: None,
            date_published: None,
            date_updated: None,
            date_received: None,
            null_entry: false,
            annotations: Vec::new(),
            details: RegmemDetailGroup::new(),
            sub_entries: Vec::new(),
        };
        base.add_details(
            Some("mysociety"),
            vec![("score".to_string(), DetailValue::Int(5))],
        )
        .unwrap();
        let detail = base.get_detail("score").unwrap();
        assert_eq!(detail.source, "mysociety");
        assert_eq!(detail.detail_type.as_deref(), Some("int"));
        assert_eq!(detail.display_as.as_deref(), Some("Score"));
    }

    fn minimal_register_json(global_chamber: bool) -> String {
        let chamber_line = if global_chamber {
            "\"chamber\": \"house-of-commons\","
        } else {
            ""
        };
        format!(
            r#"{{
                {chamber_line}
                "published_date": "2025-01-20",
                "persons": [
                    {{
                        "person_id": "uk.org.publicwhip/person/1",
                        "person_name": "Test Person",
                        "published_date": "2025-01-20",
                        "chamber": "house-of-commons",
                        "categories": [
                            {{
                                "category_id": "1",
                                "category_name": "Cat",
                                "entries": [
                                    {{"id": "1", "content": "An entry"}}
                                ]
                            }}
                        ]
                    }}
                ]
            }}"#
        )
    }

    #[test]
    fn test_register_loads_and_iterates() {
        let register =
            RegmemRegister::model_validate_json(&minimal_register_json(true), true).unwrap();
        assert_eq!(register.persons.len(), 1);
        let locs = register.iter_entry_locations();
        assert_eq!(locs.len(), 1);
        let entry = register.entry_at(&locs[0]).unwrap();
        assert_eq!(entry.base.content, "An entry");
        assert_eq!(entry.base.comparable_id(), "1");
    }

    #[test]
    fn test_chamber_mismatch_fails() {
        let json = r#"{
            "chamber": "house-of-lords",
            "published_date": "2025-01-20",
            "persons": [
                {
                    "person_id": "p1", "person_name": "x",
                    "published_date": "2025-01-20",
                    "chamber": "house-of-commons", "categories": []
                }
            ]
        }"#;
        let err = RegmemRegister::model_validate_json(json, true).unwrap_err();
        assert!(err.contains("same chamber"));
    }

    #[test]
    fn test_language_mismatch_fails() {
        let json = r#"{
            "chamber": "house-of-commons",
            "language": "en",
            "published_date": "2025-01-20",
            "persons": [
                {
                    "person_id": "p1", "person_name": "x",
                    "published_date": "2025-01-20",
                    "chamber": "house-of-commons", "language": "cy", "categories": []
                }
            ]
        }"#;
        let err = RegmemRegister::model_validate_json(json, true).unwrap_err();
        assert!(err.contains("same language"));
    }

    #[test]
    fn test_unknown_fields_ignored() {
        // comparable_id / item_hash are computed fields in the data; ignored on load.
        let json = r#"{
            "chamber": "house-of-commons",
            "published_date": "2025-01-20",
            "persons": [
                {
                    "person_id": "p1", "person_name": "x",
                    "published_date": "2025-01-20",
                    "chamber": "house-of-commons",
                    "categories": [
                        {"category_id": "1", "category_name": "C", "entries": [
                            {"id": "7041", "content": "c", "comparable_id": "7041", "item_hash": "abc"}
                        ]}
                    ]
                }
            ]
        }"#;
        let register = RegmemRegister::model_validate_json(json, true).unwrap();
        let loc = register.iter_entry_locations()[0];
        assert_eq!(
            register.entry_at(&loc).unwrap().base.id.as_deref(),
            Some("7041")
        );
    }

    #[test]
    fn test_register_round_trip_idempotent() {
        let register =
            RegmemRegister::model_validate_json(&minimal_register_json(true), true).unwrap();
        let dumped = register.model_dump_json();
        let reloaded = RegmemRegister::model_validate_json(&dumped, true).unwrap();
        assert_eq!(register, reloaded);
    }
}
