//! Register hierarchy, cross-record validation and entry locations.
//!
//! Serde constructs the hierarchy; `model_validate_json` optionally enforces
//! global/per-person chamber, language and publication-date rules afterwards.
//! `EntryLocation` is an index address, not an entry ID: edits to contents preserve
//! it but removing or reordering ancestors does not. Location traversal currently
//! includes top-level entries and their immediate sub-entries.

use crate::entries::{RegmemAnnotation, RegmemEntry, RegmemSummary};
use crate::enums::{Chamber, Language};
use chrono::NaiveDate;
use serde::{Deserialize, Serialize};

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
/// Index path into a register; invalidated by removing or reordering ancestors.
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

#[cfg(test)]
mod tests {
    use super::*;
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
