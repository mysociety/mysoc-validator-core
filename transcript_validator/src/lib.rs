use regex::Regex;
use roxmltree::{Document, Node};
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::fmt;
use std::fs;
use std::path::Path;
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
static DTD_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?s)<!DOCTYPE\s+[^\[]+\[.*?\]>").unwrap());
static ENTITY_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"<!ENTITY\s+([A-Za-z][A-Za-z0-9_-]*)\s+"([^"]*)"\s*>"#).unwrap());

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
    pub fn model_validate_xml(xml: &str) -> Result<Self, Vec<TranscriptError>> {
        let parseable_xml = expand_internal_entities(xml);
        let document = Document::parse(&parseable_xml).map_err(|error| {
            vec![TranscriptError {
                location: "xml".to_string(),
                message: error.to_string(),
            }]
        })?;
        let root = document.root_element();
        if root.tag_name().name() != "publicwhip" {
            return Err(vec![TranscriptError {
                location: "root".to_string(),
                message: "expected <publicwhip> root element".to_string(),
            }]);
        }

        let mut errors = Vec::new();
        let items = root
            .children()
            .filter(Node::is_element)
            .map(|node| parse_item(node, &parseable_xml, &mut errors))
            .collect();
        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(Self {
            raw_xml: xml.to_string(),
            scraper_version: root
                .attribute("scraperversion")
                .or_else(|| root.attribute("scraper_version"))
                .or_else(|| root.attribute("scrapeversion"))
                .map(str::to_owned),
            latest: root.attribute("latest").map(str::to_owned),
            items,
        })
    }

    pub fn from_xml_path(path: impl AsRef<Path>) -> Result<Self, Vec<TranscriptError>> {
        let path = path.as_ref();
        let xml = fs::read_to_string(path).map_err(|error| {
            vec![TranscriptError {
                location: path.display().to_string(),
                message: error.to_string(),
            }]
        })?;
        Self::model_validate_xml(&xml)
    }

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

pub fn validate_folder(
    folder: impl AsRef<Path>,
    person_ids: Option<&HashSet<String>>,
) -> Result<Vec<TranscriptFileResult>, std::io::Error> {
    let mut paths = Vec::new();
    collect_xml_paths(folder.as_ref(), &mut paths)?;
    paths.sort();
    Ok(paths
        .into_iter()
        .map(|path| {
            let errors = match Transcript::from_xml_path(&path) {
                Ok(transcript) => person_ids
                    .map(|ids| transcript.validate_person_references(ids))
                    .unwrap_or_default(),
                Err(errors) => errors,
            };
            TranscriptFileResult { path, errors }
        })
        .collect())
}

fn collect_xml_paths(folder: &Path, paths: &mut Vec<std::path::PathBuf>) -> std::io::Result<()> {
    for entry in fs::read_dir(folder)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        if file_type.is_dir() {
            collect_xml_paths(&entry.path(), paths)?;
        } else if file_type.is_file()
            && entry
                .path()
                .extension()
                .is_some_and(|extension| extension == "xml")
        {
            paths.push(entry.path());
        }
    }
    Ok(())
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

fn expand_internal_entities(xml: &str) -> String {
    let Some(dtd) = DTD_PATTERN.find(xml) else {
        return xml.to_string();
    };
    let declarations = &xml[dtd.range()];
    let mut parseable = DTD_PATTERN.replace(xml, "").into_owned();
    for captures in ENTITY_PATTERN.captures_iter(declarations) {
        parseable = parseable.replace(&format!("&{};", &captures[1]), &captures[2]);
    }
    parseable
}

fn parse_item(node: Node<'_, '_>, xml: &str, errors: &mut Vec<TranscriptError>) -> TranscriptItem {
    let tag = node.tag_name().name().to_string();
    let id = node.attribute("id").map(str::to_owned);
    if requires_gid(&tag) {
        match &id {
            Some(id) if GID_PATTERN.is_match(id) => {}
            Some(id) => errors.push(TranscriptError {
                location: tag.clone(),
                message: format!("invalid id: {id}"),
            }),
            None => errors.push(TranscriptError {
                location: tag.clone(),
                message: "missing id".to_string(),
            }),
        }
    }

    let person_id = node.attribute("person_id").map(str::to_owned);
    if let Some(person_id) = &person_id {
        if !PERSON_OR_MEMBER_ID_PATTERN.is_match(person_id) {
            errors.push(TranscriptError {
                location: id.clone().unwrap_or_else(|| tag.clone()),
                message: format!("invalid person_id: {person_id}"),
            });
        }
    }
    let member_id = node
        .attribute("speakerid")
        .or_else(|| node.attribute("member_id"))
        .map(str::to_owned);
    if let Some(member_id) = &member_id {
        if !MEMBER_ID_PATTERN.is_match(member_id) {
            errors.push(TranscriptError {
                location: id.clone().unwrap_or_else(|| tag.clone()),
                message: format!("invalid speakerid: {member_id}"),
            });
        }
    }

    let children = node
        .children()
        .filter(Node::is_element)
        .map(|child| parse_item(child, xml, errors))
        .collect();
    let range = node.range();
    TranscriptItem {
        tag,
        attributes: node
            .attributes()
            .map(|attribute| (attribute.name().to_string(), attribute.value().to_string()))
            .collect(),
        id,
        url: node.attribute("url").map(str::to_owned),
        speakername: node.attribute("speakername").map(str::to_owned),
        person_id,
        member_id,
        spid: node.attribute("spid").map(str::to_owned),
        nospeaker: node.attribute("nospeaker").map(str::to_owned),
        content: MixedContent {
            raw: xml[range].to_string(),
            text: collect_text(node),
        },
        children,
    }
}

fn collect_text(node: Node<'_, '_>) -> String {
    node.descendants()
        .filter(Node::is_text)
        .filter_map(|descendant| descendant.text())
        .map(str::trim)
        .filter(|text| !text.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
}

fn requires_gid(tag: &str) -> bool {
    matches!(
        tag,
        "speech" | "ques" | "reply" | "oral-heading" | "major-heading" | "minor-heading"
    )
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
