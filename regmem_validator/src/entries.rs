//! Entry content, annotations and summaries; shared fields use composition.
//!
//! `RegmemInfoBase` holds fields shared by entries and summaries. Serde `flatten`
//! writes them at the outer object level; it is a wire-format choice, not Rust
//! inheritance. Annotation deserialization supplies today's date when absent.
//! Derived hashes are local computed values, not upstream byte-identical hashes.

use crate::details::{DetailValue, RegmemDetail, RegmemDetailGroup};
use crate::enums::is_default_content_type;
use crate::enums::ContentType;
use chrono::NaiveDate;
use serde::{Deserialize, Deserializer, Serialize};

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

#[cfg(test)]
mod tests {
    use super::*;
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
}
