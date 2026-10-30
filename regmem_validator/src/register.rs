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

#[serde_with::skip_serializing_none]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RegmemCategory {
    #[serde(default = "default_category_id")]
    pub category_id: String,
    pub category_name: String,
    #[serde(default)]
    pub category_description: Option<String>,
    #[serde(default)]
    pub legislation_or_rule_name: Option<String>,
    #[serde(default)]
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

#[serde_with::skip_serializing_none]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RegmemRegister {
    #[serde(default)]
    pub chamber: Option<Chamber>,
    /// None permits per-person languages; serializing it as null preserves that rule.
    #[serialize_always]
    #[serde(default = "default_language")]
    pub language: Option<Language>,
    #[serde(default)]
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

    /// Require every person to match any global chamber or language.
    /// Missing register language defaults to English; explicit null permits
    /// per-person languages. Required person fields already guarantee chamber,
    /// language and publication date when no global constraint is supplied.
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
mod tests;
