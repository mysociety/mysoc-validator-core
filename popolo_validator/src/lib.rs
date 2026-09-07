//! Popolo data and load/dump orchestration, without Python dependencies.
//!
//! Public re-exports preserve the original crate-root API. Models and custom
//! serde implementations live in `models` and `names`; indexed collections,
//! validation, queries and sentinel dates have separate modules.
//! Serde builds records and indexes. `model_validate_json` additionally runs
//! cross-record validation when requested; `serde_json::from_str` alone does not
//! perform it. This differs from assuming every Pydantic-style construction
//! automatically applies all model validators.

use crate::validation::ValidatorErrorCollection;
use crate::validation::{RegexValidator, ValidDateRange};
use serde::Serialize;
use serde_derive::Deserialize;
mod names;
pub use names::{AltName, BasicPersonName, LordName, NameOptions, NameType, NiceName};
mod models;
pub use models::{
    Area, Extra, Identifier, Link, Membership, MembershipName, MembershipReason,
    MembershipRedirect, MembershipType, OrgType, Organization, Person, PersonRedirect, PersonType,
    Post, Shortcuts, SimpleIdentifer,
};
mod collections;
pub use collections::{
    MembershipCollection, OrganizationCollection, PersonCollection, PostCollection,
};
mod validation;
pub use validation::{ValidatorError, ValidatorType};
mod queries;
pub use queries::reduce_to_slug;
mod dates;

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Popolo {
    #[serde(default = "MembershipCollection::new")]
    pub memberships: MembershipCollection,
    #[serde(default = "OrganizationCollection::new")]
    pub organizations: OrganizationCollection,
    #[serde(default = "PersonCollection::new")]
    pub persons: PersonCollection,
    #[serde(default = "PostCollection::new")]
    pub posts: PostCollection,
}

impl Popolo {
    pub fn model_dump_json(&self) -> String {
        serde_json::to_string_pretty(&self).unwrap()
    }
    /// Rebuild every index after direct edits through the public Rust fields.
    pub fn set_lookup(&mut self) {
        self.persons.set_lookup();
        self.memberships.set_lookup();
        self.organizations.set_lookup();
        self.posts.set_lookup();
    }
    pub fn model_validate_json(
        json_str: &str,
        validate: &bool,
    ) -> Result<Self, ValidatorErrorCollection> {
        let popolo: Popolo = serde_json::from_str(json_str).map_err(|e| {
            vec![ValidatorError {
                error: e.to_string(),
                validator_type: ValidatorType::InvalidJson,
            }]
        })?;
        if *validate {
            let mut errors: ValidatorErrorCollection = Vec::new();
            if let Err(e) = popolo.check_unique_ids() {
                errors.extend(e);
            }
            if let Err(e) = popolo.check_valid_date_ranges() {
                errors.extend(e);
            }
            errors.extend(popolo.is_valid_date_range());
            errors.extend(popolo.test_regex_values());
            if !errors.is_empty() {
                return Err(errors);
            }
        }
        Ok(popolo)
    }
}
