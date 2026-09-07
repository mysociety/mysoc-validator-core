//! Transcript values, identifiers and reference validation. Parsing and files have separate modules.

mod files;
mod xml;
pub use files::validate_folder;

use regex::Regex;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::fmt;
use std::sync::LazyLock;

static GID_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"^uk\.org\.publicwhip/[a-z-]+(/(en|cy))?/\d{4}-\d{2}-\d{2}[a-z]?\.[A-Za-z0-9-]+\.[A-Za-z0-9]+",
    )
    .unwrap()
});
static PERSON_OR_MEMBER_ID_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(uk\.org\.publicwhip/(person|member)/\d+|unknown)$").unwrap());
static MEMBER_ID_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(uk\.org\.publicwhip/member/\d+|unknown)$").unwrap());

pub fn validate_gid(value: &str) -> bool {
    GID_PATTERN.is_match(value)
}

pub fn validate_person_id(value: &str) -> bool {
    PERSON_OR_MEMBER_ID_PATTERN.is_match(value)
}

pub fn validate_member_id(value: &str) -> bool {
    MEMBER_ID_PATTERN.is_match(value)
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TranscriptError {
    pub location: String,
    pub message: String,
}

impl fmt::Display for TranscriptError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}: {}", self.location, self.message)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MixedContent {
    pub raw: String,
    pub text: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TranscriptItem {
    pub tag: String,
    pub attributes: BTreeMap<String, String>,
    pub id: Option<String>,
    pub url: Option<String>,
    pub speakername: Option<String>,
    pub person_id: Option<String>,
    pub member_id: Option<String>,
    pub spid: Option<String>,
    pub nospeaker: Option<String>,
    pub content: MixedContent,
    pub children: Vec<TranscriptItem>,
}

impl TranscriptItem {
    pub fn new(tag: impl Into<String>, attributes: BTreeMap<String, String>, text: String) -> Self {
        let tag = tag.into();
        Self {
            id: attributes.get("id").cloned(),
            url: attributes.get("url").cloned(),
            speakername: attributes.get("speakername").cloned(),
            person_id: attributes.get("person_id").cloned(),
            member_id: attributes
                .get("speakerid")
                .or_else(|| attributes.get("member_id"))
                .cloned(),
            spid: attributes.get("spid").cloned(),
            nospeaker: attributes.get("nospeaker").cloned(),
            content: MixedContent {
                raw: text.clone(),
                text,
            },
            children: Vec::new(),
            attributes,
            tag,
        }
    }

    pub fn string_value(&self) -> String {
        if self.children.is_empty() {
            self.content.text.clone()
        } else {
            self.children
                .iter()
                .map(Self::string_value)
                .filter(|value| !value.is_empty())
                .collect::<Vec<_>>()
                .join(" ")
        }
    }
}

#[derive(Clone, Debug)]
pub struct Transcript {
    raw_xml: String,
    pub scraper_version: Option<String>,
    pub latest: Option<String>,
    pub items: Vec<TranscriptItem>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TranscriptFileResult {
    pub path: std::path::PathBuf,
    pub errors: Vec<TranscriptError>,
}

impl Transcript {
    pub fn model_dump_xml(&self) -> &str {
        &self.raw_xml
    }

    pub fn validate_person_references(&self, person_ids: &HashSet<String>) -> Vec<TranscriptError> {
        let mut errors = Vec::new();
        for item in &self.items {
            validate_item_person_references(item, person_ids, &mut errors);
        }
        errors
    }
}

fn validate_item_person_references(
    item: &TranscriptItem,
    person_ids: &HashSet<String>,
    errors: &mut Vec<TranscriptError>,
) {
    if let Some(person_id) = &item.person_id {
        if person_id != "unknown" && !person_ids.contains(person_id) {
            errors.push(TranscriptError {
                location: item.id.clone().unwrap_or_else(|| item.tag.clone()),
                message: format!("unknown person_id: {person_id}"),
            });
        }
    }
    for child in &item.children {
        validate_item_person_references(child, person_ids, errors);
    }
}

#[cfg(test)]
mod tests;
