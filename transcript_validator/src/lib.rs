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
mod tests {
    use super::{validate_folder, Transcript};
    use std::collections::HashSet;
    use std::fs;

    #[test]
    fn validates_and_round_trips() {
        let xml = r#"<publicwhip scraperversion="1"><major-heading id="uk.org.publicwhip/debates/2026-01-01.1.mh">Heading</major-heading><speech id="uk.org.publicwhip/debates/2026-01-01.1.1" person_id="unknown"><p>Hello</p></speech></publicwhip>"#;
        let transcript = Transcript::model_validate_xml(xml).unwrap();
        assert_eq!(transcript.items.len(), 2);
        assert_eq!(transcript.items[1].string_value(), "Hello");
        assert_eq!(transcript.model_dump_xml(), xml);
    }

    #[test]
    fn rejects_invalid_gid() {
        let xml =
            r#"<publicwhip><speech id="uk.org.publicwhip/Bad_Name/2026-01-01.1.1"/></publicwhip>"#;
        assert!(Transcript::model_validate_xml(xml).is_err());
    }

    #[test]
    fn folder_validation_checks_stated_person_ids() {
        let folder =
            std::env::temp_dir().join(format!("transcript-validator-{}", std::process::id()));
        fs::create_dir_all(&folder).unwrap();
        fs::write(
            folder.join("valid.xml"),
            r#"<publicwhip><speech id="uk.org.publicwhip/debates/2026-01-01.1.1" person_id="unknown"/></publicwhip>"#,
        )
        .unwrap();
        fs::write(
            folder.join("missing.xml"),
            r#"<publicwhip><speech id="uk.org.publicwhip/debates/2026-01-01.1.2" person_id="uk.org.publicwhip/person/2"/></publicwhip>"#,
        )
        .unwrap();
        let people = HashSet::from(["uk.org.publicwhip/person/1".to_string()]);
        let results = validate_folder(&folder, Some(&people)).unwrap();
        assert_eq!(results.len(), 2);
        assert_eq!(
            results
                .iter()
                .filter(|result| !result.errors.is_empty())
                .count(),
            1
        );
        fs::remove_dir_all(folder).unwrap();
    }
}
