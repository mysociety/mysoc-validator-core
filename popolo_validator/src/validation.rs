//! Dataset validation, insertion policy, error types and record-level traits.
//!
//! Dataset loading accumulates errors in a stable sequence: IDs/references,
//! membership overlaps, individual date ranges, then format checks. Membership
//! insertion reports only its first error and conservatively rejects possible
//! overlaps, including approximate and historical dates. These policies are
//! deliberately separate for compatibility.
//!
//! Dataset overlap checks sort by exact date bounds, then compare adjacent
//! memberships within each person/scope. Pairs with approximate dates and records
//! starting no later than the 1900 threshold are skipped. Approximate records can
//! therefore separate precise neighbors; this is the existing validation policy,
//! not a complete interval-overlap search. Fuzzy date-range tests use named
//! predicates so Rust's total ordering cannot silently change these rules.

use crate::collections::HasId;
use crate::models::{
    Membership, MembershipRedirect, MembershipType, Organization, Person, PersonRedirect,
    PersonType, Post,
};
use crate::names::{AltName, BasicPersonName, LordName, NameOptions};
use crate::Popolo;
use fuzzy_date::FuzzyDate;
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashSet;
use std::fmt;

lazy_static! {
    static ref PERSON_ID_REGEX: Regex = Regex::new(r"uk\.org\.publicwhip/person/\d+$").unwrap();
    static ref POST_ID_REGEX: Regex = Regex::new(r"^.+$").unwrap();
    static ref MEMBER_ID_REGEX: Regex = Regex::new(r".*/-?\d+$").unwrap();
    static ref ORG_ID: Regex = Regex::new(r"^[a-z0-9-]+$").unwrap();
}

pub(crate) type ValidatorErrorCollection = Vec<ValidatorError>;

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
pub enum ValidatorType {
    InvalidJson,
    NonUniqueId,
    InvalidForeignKey,
    DateRangeOverlap,
    DateRangeOverlapSameDate,
    DateRange,
    RegexError,
}

#[derive(Debug)]
pub struct ValidatorError {
    pub error: String,
    pub validator_type: ValidatorType,
}

pub(crate) trait RegexValidator {
    fn test_regex_values(&self) -> ValidatorErrorCollection;
}

macro_rules! regex_property {
    ($field:ident, $regex:ident, $errors:ident, $field_name:expr) => {
        if !$regex.is_match(&$field) {
            $errors.push(ValidatorError {
                error: format!("{}: {} is not a valid {}", $field_name, $field, $field_name),
                validator_type: ValidatorType::RegexError,
            });
        }
    };
}

impl RegexValidator for Popolo {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        for person in &self.persons {
            errors.extend(person.test_regex_values());
        }
        for membership in &self.memberships {
            errors.extend(membership.test_regex_values());
        }
        for post in &self.posts {
            errors.extend(post.test_regex_values());
        }
        for organization in &self.organizations {
            errors.extend(organization.test_regex_values());
        }
        errors
    }
}

impl RegexValidator for NameOptions {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        match self {
            NameOptions::BasicPersonName(_) => Vec::new(),
            NameOptions::LordName(_) => Vec::new(),
            NameOptions::AltName(name) => name.test_regex_values(),
        }
    }
}

impl RegexValidator for AltName {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        if let Some(organization_id) = &self.organization_id {
            regex_property!(organization_id, ORG_ID, errors, "Organization ID");
        }
        errors
    }
}

impl RegexValidator for Organization {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        let org_id = &self.id;
        regex_property!(org_id, ORG_ID, errors, "Organization ID");
        errors
    }
}

impl RegexValidator for Post {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        let post_id = &self.id;
        let organization_id = &self.organization_id;
        regex_property!(post_id, POST_ID_REGEX, errors, "Post ID");
        regex_property!(organization_id, ORG_ID, errors, "Organization ID");
        errors
    }
}

impl RegexValidator for MembershipType {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        match self {
            MembershipType::Membership(m) => m.test_regex_values(),
            MembershipType::MembershipRedirect(r) => r.test_regex_values(),
        }
    }
}

impl RegexValidator for PersonType {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        match self {
            PersonType::Person(p) => p.test_regex_values(),
            PersonType::PersonRedirect(r) => r.test_regex_values(),
        }
    }
}

impl RegexValidator for Person {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        let person_id = &self.id;
        regex_property!(person_id, PERSON_ID_REGEX, errors, "Person ID");
        errors
    }
}

impl RegexValidator for PersonRedirect {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        let person_id = &self.id;
        let redirect_id = &self.redirect;
        regex_property!(person_id, PERSON_ID_REGEX, errors, "Person ID");
        regex_property!(redirect_id, PERSON_ID_REGEX, errors, "Redirect ID");
        errors
    }
}

impl RegexValidator for MembershipRedirect {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        let member_id = &self.id;
        let redirect_id = &self.redirect;
        regex_property!(member_id, MEMBER_ID_REGEX, errors, "Member ID");
        regex_property!(redirect_id, MEMBER_ID_REGEX, errors, "Redirect ID");
        errors
    }
}

impl RegexValidator for Membership {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        let member_id = &self.id;
        let person_id = &self.person_id;
        regex_property!(member_id, MEMBER_ID_REGEX, errors, "Member ID");
        regex_property!(person_id, PERSON_ID_REGEX, errors, "Person ID");
        if let Some(post_id) = &self.post_id {
            regex_property!(post_id, POST_ID_REGEX, errors, "Post ID");
        }
        if let Some(organization_id) = &self.organization_id {
            regex_property!(organization_id, ORG_ID, errors, "Organization ID");
        }
        if let Some(on_behalf_of_id) = &self.on_behalf_of_id {
            regex_property!(on_behalf_of_id, ORG_ID, errors, "On Behalf Of ID");
        }
        errors
    }
}

pub(crate) trait ValidDateRange {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection;
}

macro_rules! impl_valid_date_range {
    ($struct_name:ident) => {
        impl ValidDateRange for $struct_name {
            fn is_valid_date_range(&self) -> ValidatorErrorCollection {
                if self.start_date.is_after(&self.end_date) {
                    vec![ValidatorError {
                        error: format!(
                            "start date {} is after end date {}",
                            self.start_date, self.end_date
                        ),
                        validator_type: ValidatorType::DateRange,
                    }]
                } else {
                    vec![]
                }
            }
        }
    };
}

impl fmt::Display for ValidatorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValidatorType::InvalidJson => write!(f, "Invalid JSON"),
            ValidatorType::NonUniqueId => write!(f, "Non-unique ID"),
            ValidatorType::InvalidForeignKey => write!(f, "Invalid foreign key"),
            ValidatorType::DateRangeOverlap => write!(f, "Date range overlap"),
            ValidatorType::DateRange => write!(f, "Invalid date range"),
            ValidatorType::DateRangeOverlapSameDate => write!(f, "Date range overlap same date"),
            ValidatorType::RegexError => write!(f, "Regex error"),
        }
    }
}

fn check_valid_foreign_key(
    id: &String,
    ids: &HashSet<String>,
    item_type: &str,
    membership_id: &str,
) -> Option<ValidatorError> {
    if !ids.contains(id) {
        Some(ValidatorError {
            error: format!(
                "Membership: {} has invalid {}: {}",
                membership_id, item_type, id
            ),
            validator_type: ValidatorType::InvalidForeignKey,
        })
    } else {
        None
    }
}

fn check_unique<T>(
    items: &Vec<T>,
    id_fn: fn(&T) -> &String,
    item_type: &str,
) -> Result<HashSet<String>, ValidatorErrorCollection> {
    let mut ids: HashSet<String> = HashSet::new();
    let mut duplicates: ValidatorErrorCollection = Vec::new();

    for item in items {
        let id = id_fn(&item).clone();
        if !ids.insert(id.clone()) {
            duplicates.push(ValidatorError {
                error: format!("{}: {}", item_type, id),
                validator_type: ValidatorType::NonUniqueId,
            });
        }
    }

    if duplicates.is_empty() {
        Ok(ids)
    } else {
        Err(duplicates)
    }
}

impl ValidDateRange for NameOptions {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection {
        match self {
            NameOptions::BasicPersonName(n) => n.is_valid_date_range(),
            NameOptions::LordName(n) => n.is_valid_date_range(),
            NameOptions::AltName(n) => n.is_valid_date_range(),
        }
    }
}

impl ValidDateRange for Person {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection {
        if let Some(other_names) = &self.other_names {
            other_names
                .iter()
                .flat_map(|name| name.is_valid_date_range())
                .collect()
        } else {
            Vec::new()
        }
    }
}

impl ValidDateRange for PersonType {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection {
        match self {
            PersonType::Person(p) => p.is_valid_date_range(),
            PersonType::PersonRedirect(_) => Vec::new(),
        }
    }
}

impl ValidDateRange for MembershipType {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection {
        match self {
            MembershipType::Membership(m) => m.is_valid_date_range(),
            MembershipType::MembershipRedirect(_) => Vec::new(),
        }
    }
}

impl ValidDateRange for Popolo {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection {
        let mut errors: ValidatorErrorCollection = Vec::new();
        for person in &self.persons {
            errors.extend(person.is_valid_date_range());
        }
        for membership in &self.memberships {
            errors.extend(membership.is_valid_date_range());
        }
        for post in &self.posts {
            errors.extend(post.is_valid_date_range());
        }
        errors
    }
}

impl Popolo {
    pub fn check_valid_date_ranges(&self) -> Result<(), ValidatorErrorCollection> {
        let mut errors: ValidatorErrorCollection = Vec::new();

        let no_org_str = "no_org".to_string();
        let just_memberships: Vec<&Membership> = self
            .just_memberships()
            .into_iter()
            .filter(|m| {
                m.start_date
                    .is_after(&FuzzyDate::fromisoformat("1900-01-01").unwrap())
            })
            .sorted_by_key(|m| {
                (
                    m.post_id
                        .as_ref()
                        .unwrap_or(m.organization_id.as_ref().unwrap_or(&no_org_str)),
                    &m.person_id,
                    (m.start_date.earliest_date, m.start_date.latest_date),
                )
            })
            .collect();

        for (_key, chunk) in &just_memberships.into_iter().chunk_by(|m| {
            (
                m.post_id
                    .as_ref()
                    .unwrap_or(m.organization_id.as_ref().unwrap_or(&no_org_str)),
                &m.person_id,
            )
        }) {
            let group: Vec<&Membership> = chunk.collect();
            for i in 1..group.len() {
                let prev = group[i - 1];
                let curr = group[i];
                if prev.start_date.is_approximate() || curr.start_date.is_approximate() {
                    continue;
                }
                if prev.end_date.is_approximate() || curr.end_date.is_approximate() {
                    continue;
                }
                if prev.end_date.is_after(&curr.start_date) {
                    errors.push(ValidatorError {
                        error: format!("Membership {} overlaps with {}", prev.id, curr.id),
                        validator_type: ValidatorType::DateRangeOverlap,
                    });
                } else if prev.end_date.overlaps(&curr.start_date) {
                    errors.push(ValidatorError {
                        error: format!(
                            "Membership {} ends on same date as {} starts",
                            prev.id, curr.id
                        ),
                        validator_type: ValidatorType::DateRangeOverlapSameDate,
                    });
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
    pub fn check_unique_ids(&self) -> Result<(), ValidatorErrorCollection> {
        let mut errors: ValidatorErrorCollection = Vec::new();

        let person_ids = match check_unique(&self.persons.root, |p| p.get_id(), "Person") {
            Ok(ids) => ids,
            Err(e) => {
                errors.extend(e);
                HashSet::new()
            }
        };

        let org_ids = match check_unique(&self.organizations.root, |o| o.get_id(), "Organization") {
            Ok(ids) => ids,
            Err(e) => {
                errors.extend(e);
                HashSet::new()
            }
        };

        let post_ids = match check_unique(&self.posts.root, |p| p.get_id(), "Post") {
            Ok(ids) => ids,
            Err(e) => {
                errors.extend(e);
                HashSet::new()
            }
        };

        match check_unique(&self.memberships.root, |m| m.get_id(), "Membership") {
            Ok(_) => {}
            Err(e) => errors.extend(e),
        };

        for membership in self.just_memberships() {
            let person_id = &membership.person_id;
            if let Some(e) =
                check_valid_foreign_key(person_id, &person_ids, "person_id", &membership.id)
            {
                errors.push(e);
            }
            if let Some(post_id) = &membership.post_id {
                if let Some(e) =
                    check_valid_foreign_key(post_id, &post_ids, "post_id", &membership.id)
                {
                    errors.push(e);
                }
            }
            if let Some(organization_id) = &membership.organization_id {
                if let Some(e) = check_valid_foreign_key(
                    organization_id,
                    &org_ids,
                    "organization_id",
                    &membership.id,
                ) {
                    errors.push(e);
                }
            }
            if let Some(on_behalf_of_id) = &membership.on_behalf_of_id {
                if let Some(e) = check_valid_foreign_key(
                    on_behalf_of_id,
                    &org_ids,
                    "on_behalf_of_id",
                    &membership.id,
                ) {
                    errors.push(e);
                }
            }
        }

        for person_redirect in self.persons.iter().filter_map(|p| match p {
            PersonType::PersonRedirect(r) => Some(r),
            _ => None,
        }) {
            if let Some(e) = check_valid_foreign_key(
                &person_redirect.redirect,
                &person_ids,
                "redirect",
                &person_redirect.id,
            ) {
                errors.push(e);
            }
        }

        for organization in self.organizations.iter() {
            if let Some(parent_id) = &organization.parent_id {
                if !org_ids.contains(parent_id) {
                    errors.push(ValidatorError {
                        error: format!(
                            "Organization {} refers to invalid parent organization {}",
                            organization.id, parent_id
                        ),
                        validator_type: ValidatorType::InvalidForeignKey,
                    });
                }
            }
        }

        for post in self.posts.iter() {
            if !org_ids.contains(&post.organization_id) {
                errors.push(ValidatorError {
                    error: format!(
                        "Post {} refers to invalid organization {}",
                        post.id, post.organization_id
                    ),
                    validator_type: ValidatorType::InvalidForeignKey,
                });
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

impl_valid_date_range!(BasicPersonName);

impl_valid_date_range!(LordName);

impl_valid_date_range!(AltName);

impl_valid_date_range!(Membership);

impl_valid_date_range!(Post);

impl Popolo {
    /// Validate one proposed insertion using the Python collection's historical policy.
    ///
    /// Unlike dataset validation, insertion rejects possible overlaps even when
    /// dates are approximate or before 1900. It returns the first error.
    pub fn validate_membership_insertion(&self, membership: &Membership) -> Result<(), String> {
        if self.memberships.id_lookup.contains_key(&membership.id) {
            return Err(format!("Duplicate Membership id {:?}", membership.id));
        }

        if !MEMBER_ID_REGEX.is_match(&membership.id) {
            return Err(format!("Invalid Membership id format: {}", membership.id));
        }

        if !self.persons.id_lookup.contains_key(&membership.person_id) {
            return Err(format!(
                "Membership {} refers to unknown person_id {}",
                membership.id, membership.person_id
            ));
        }

        if membership.post_id.is_some() || membership.organization_id.is_some() {
            for mt in self.memberships.iter() {
                if let MembershipType::Membership(existing) = mt {
                    if existing.person_id != membership.person_id {
                        continue;
                    }
                    let same_scope = match &membership.post_id {
                        Some(post_id) => existing.post_id.as_ref() == Some(post_id),
                        None => {
                            existing.post_id.is_none()
                                && existing.organization_id == membership.organization_id
                        }
                    };
                    if !same_scope {
                        continue;
                    }
                    let new_start = &membership.start_date;
                    let new_end = &membership.end_date;
                    let ex_start = &existing.start_date;
                    let ex_end = &existing.end_date;
                    if new_start.earliest_date <= ex_end.latest_date
                        && new_end.latest_date >= ex_start.earliest_date
                    {
                        return Err(format!(
                            "New Membership {} overlaps with membership {}",
                            membership.id, existing.id
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    pub fn validate_membership_redirect_insertion(
        &self,
        redirect: &MembershipRedirect,
    ) -> Result<(), String> {
        if self.memberships.id_lookup.contains_key(&redirect.id) {
            return Err(format!("Duplicate MembershipRedirect id {:?}", redirect.id));
        }
        Ok(())
    }
}
