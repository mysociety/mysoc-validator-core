//! XML parsing uses an expanded copy; original XML remains the round-trip source.
//!
//! roxmltree nodes borrow the temporary parsing string. Parsing copies the fields
//! and mixed content needed by our owned models before that string is dropped.
//! Entity handling supports the internal declarations recognized by these regexes;
//! it is not a general DTD processor and does not fetch external declarations.
//! `raw_xml` stores the unexpanded original, while extracted content comes from
//! the parsing copy. Serialization returns the original rather than rebuilding
//! XML from potentially edited items.

use crate::{
    validate_gid, validate_member_id, validate_person_id, MixedContent, Transcript,
    TranscriptError, TranscriptItem,
};
use regex::Regex;
use roxmltree::{Document, Node};
use std::sync::LazyLock;

static DTD_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?s)<!DOCTYPE\s+[^\[]+\[.*?\]>").unwrap());
static ENTITY_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"<!ENTITY\s+([A-Za-z][A-Za-z0-9_-]*)\s+"([^"]*)"\s*>"#).unwrap());

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
            Some(id) if validate_gid(id) => {}
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
        if !validate_person_id(person_id) {
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
        if !validate_member_id(member_id) {
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
