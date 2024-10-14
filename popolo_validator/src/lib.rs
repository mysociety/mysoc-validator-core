use fuzzy_date::FuzzyDate;
use itertools::Itertools;
use lazy_static::lazy_static;
use pyo3::prelude::*;
use pyo3::types::{PyIterator, PyType};
use regex::Regex;
use serde::de::{self, Deserialize, Deserializer};
use serde::Serialize;
use serde_derive::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fmt;

lazy_static! {
    static ref DEFAULT_START_DATE: FuzzyDate = FuzzyDate::fromisoformat("0001-01-01").unwrap();
    static ref DEFAULT_END_DATE: FuzzyDate = FuzzyDate::fromisoformat("9999-12-31").unwrap();
    static ref PERSON_ID_REGEX: Regex = Regex::new(r"uk\.org\.publicwhip/person/\d+$").unwrap();
    static ref POST_ID_REGEX: Regex = Regex::new(r"uk\.org\.publicwhip/cons/\d+(-NI)?$").unwrap();
    static ref MEMBER_ID_REGEX: Regex = Regex::new(r".*/-?\d+$").unwrap();
    static ref ORG_ID: Regex = Regex::new(r"^[a-z0-9-]+$").unwrap();
}

type ValidatorErrorCollection = Vec<ValidatorError>;

fn default_start_date() -> FuzzyDate {
    DEFAULT_START_DATE.clone()
}

fn default_end_date() -> FuzzyDate {
    DEFAULT_END_DATE.clone()
}

fn is_default_start_date(date: &FuzzyDate) -> bool {
    date == &*DEFAULT_START_DATE
}

fn is_default_end_date(date: &FuzzyDate) -> bool {
    date == &*DEFAULT_END_DATE
}

#[derive(Serialize, Debug, Clone)]
#[serde(untagged, deny_unknown_fields)]
#[pyclass]
enum NameOptions {
    BasicPersonName(BasicPersonName),
    LordName(LordName),
    AltName(AltName),
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub enum ValidatorType {
    InvalidJson,
    NonUniqueId,
    InvalidForeignKey,
    DateRangeOverlap,
    DateRangeOverlapSameDate,
    DateRange,
    RegexError,
}

#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq, Hash)]
#[pyclass(eq)]
#[serde(deny_unknown_fields, rename_all = "lowercase")]
enum OrgType {
    Party,
    Chamber,
    Metro,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
#[pyclass(eq)]
enum NameType {
    Main,
    Alternate,
}

#[derive(Serialize, Debug, Clone)]
#[serde(untagged, deny_unknown_fields)]
#[pyclass]
pub enum MembershipType {
    Membership(Membership),
    MembershipRedirect(MembershipRedirect),
}

#[derive(Serialize, Debug, Clone)]
#[serde(untagged, deny_unknown_fields)]
#[pyclass]
pub enum PersonType {
    Person(Person),
    PersonRedirect(PersonRedirect),
}

#[derive(Serialize, Debug, Clone)]
#[pyclass(get_all)]
#[serde(untagged)]
enum Identifier {
    String(String),
    Int(i64),
}

// Defining the collection interfaces
// has to be done as a macro because pyclass doesn't like generic types
macro_rules! create_interface {
    ($name: ident, $type: ident) => {
        #[derive(Debug, Clone)]
        #[pyclass(get_all)]
        pub struct $name {
            pub root: Vec<$type>,
            pub id_lookup: HashMap<String, usize>,
        }

        #[pymethods]
        impl $name {
            pub fn __getitem__(&self, py: Python, index: &str) -> Option<PyObject> {
                self.get(index).map(|item| item.clone().into_py(py))
            }

            pub fn __len__(&self) -> usize {
                self.len()
            }

            // print repr and the length
            pub fn __str__(&self) -> String {
                format!("{}: {}", stringify!($repr), self.len())
            }

            pub fn __contains__(&self, id: &str) -> bool {
                self.id_lookup.contains_key(id)
            }
        }

        impl $name {
            // get the item from the id
            pub fn get(&self, id: &str) -> Option<&$type> {
                self.id_lookup.get(id).map(|i| &self.root[*i])
            }

            // id_lookup is a map of id to position in root
            pub fn set_lookup(&mut self) {
                self.id_lookup = self
                    .root
                    .iter()
                    .enumerate()
                    .map(|(i, x)| (x.get_id().clone(), i))
                    .collect();
            }

            pub fn new() -> Self {
                $name {
                    root: Vec::new(),
                    id_lookup: HashMap::new(),
                }
            }

            pub fn len(&self) -> usize {
                self.root.len()
            }

            pub fn push(&mut self, item: $type) {
                self.root.push(item);
            }

            pub fn pop(&mut self) -> Option<$type> {
                self.root.pop()
            }

            pub fn iter(&self) -> std::slice::Iter<$type> {
                self.root.iter()
            }
        }

        impl Iterator for $name {
            type Item = $type;

            fn next(&mut self) -> Option<Self::Item> {
                self.root.pop()
            }
        }

        // for borrowed values
        impl<'a> IntoIterator for &'a $name {
            type Item = &'a $type;
            type IntoIter = std::slice::Iter<'a, $type>;

            fn into_iter(self) -> Self::IntoIter {
                self.root.iter()
            }
        }

        impl<'de> Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                let vec = Vec::deserialize(deserializer)?;
                Ok($name {
                    root: vec,
                    id_lookup: HashMap::new(),
                })
            }
        }

        impl Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                self.root.serialize(serializer)
            }
        }
    };
}

create_interface!(PersonCollection, PersonType);
create_interface!(MembershipCollection, MembershipType);
create_interface!(OrganizationCollection, Organization);
create_interface!(PostCollection, Post);

pub struct ValidatorError {
    pub error: String,
    pub validator_type: ValidatorType,
}

#[pyclass(get_all)]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct PersonRedirect {
    id: String,
    redirect: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
#[pyclass]
pub struct LordName {
    #[serde(skip_serializing_if = "Option::is_none")]
    additional_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    county: Option<String>,
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date"
    )]
    end_date: FuzzyDate,
    #[serde(skip_serializing_if = "Option::is_none")]
    given_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    honorific_prefix: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    honorific_suffix: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    lordname: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    lordofname: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    lordofname_full: Option<String>,
    note: NameType,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date",
        deserialize_with = "empty_string_as_default_start_date",
        serialize_with = "serialize_default_start_date"
    )]
    start_date: FuzzyDate,
    #[serde(skip_serializing_if = "Option::is_none")]
    surname: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[pyclass(get_all)]
#[serde(deny_unknown_fields)]
pub struct Link {
    #[serde(skip_serializing_if = "Option::is_none")]
    note: Option<String>,
    url: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[pyclass(get_all)]
#[serde(deny_unknown_fields)]
pub struct Shortcuts {
    #[serde(skip_serializing_if = "Option::is_none")]
    current_constituency: Option<String>,
    current_party: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
#[pyclass]
pub struct BasicPersonName {
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date"
    )]
    end_date: FuzzyDate,
    family_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    given_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    honorific_prefix: Option<String>,
    note: NameType,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date"
    )]
    start_date: FuzzyDate,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
#[pyclass(get_all)]
pub struct Person {
    #[serde(skip_serializing_if = "Option::is_none")]
    biography: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    birth_date: Option<FuzzyDate>,
    #[serde(skip_serializing_if = "Option::is_none")]
    death_date: Option<FuzzyDate>,
    #[serde(skip_serializing_if = "Option::is_none")]
    gender: Option<String>,
    id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    identifiers: Option<Vec<SimpleIdentifer>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    image: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    links: Option<Vec<Link>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    other_names: Option<Vec<NameOptions>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    national_identity: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    summary: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    shortcuts: Option<Shortcuts>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[pyclass(get_all)]
#[serde(deny_unknown_fields)]
pub struct SimpleIdentifer {
    identifier: Identifier,
    scheme: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
#[pyclass(get_all)]
pub struct MembershipName {
    family_name: String,
    given_name: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
#[pyclass(get_all)]
pub struct Membership {
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date"
    )]
    end_date: FuzzyDate,
    #[serde(skip_serializing_if = "Option::is_none")]
    end_reason: Option<String>,
    id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    identifiers: Option<Vec<SimpleIdentifer>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    label: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<MembershipName>,
    #[serde(skip_serializing_if = "Option::is_none")]
    on_behalf_of_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    organization_id: Option<String>,
    person_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    post_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    reason: Option<String>, // Exactly one end reason that should be a end_reason
    #[serde(skip_serializing_if = "Option::is_none")]
    role: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    source: Option<String>,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date"
    )]
    start_date: FuzzyDate,
    #[serde(skip_serializing_if = "Option::is_none")]
    start_reason: Option<String>,
}

// Regex Trait

trait RegexValidator {
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
            NameOptions::BasicPersonName(_name) => Vec::new(),
            NameOptions::LordName(_name) => Vec::new(),
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
            MembershipType::Membership(membership) => membership.test_regex_values(),
            MembershipType::MembershipRedirect(redirect) => redirect.test_regex_values(),
        }
    }
}

impl RegexValidator for PersonType {
    fn test_regex_values(&self) -> ValidatorErrorCollection {
        match self {
            PersonType::Person(person) => person.test_regex_values(),
            PersonType::PersonRedirect(redirect) => redirect.test_regex_values(),
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

// Trait to access the `id` field
trait HasId {
    fn get_id(&self) -> &String;
}

// Common trait for all name variants
trait NiceName {
    fn nice_name(&self) -> String;
}

// Define date range test
trait ValidDateRange {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection;
}

// Macro to implement the trait for any struct that has start_date and end_date
macro_rules! impl_valid_date_range {
    ($struct_name:ident) => {
        impl ValidDateRange for $struct_name {
            fn is_valid_date_range(&self) -> ValidatorErrorCollection {
                if self.start_date > self.end_date {
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

// Define

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

impl HasId for Post {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl HasId for Organization {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl HasId for PersonRedirect {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl NiceName for BasicPersonName {
    fn nice_name(&self) -> String {
        match &self.given_name {
            Some(given_name) => format!("{} {}", given_name, self.family_name),
            None => self.family_name.clone(),
        }
    }
}

fn empty_string_as_default_start_date<'de, D>(deserializer: D) -> Result<FuzzyDate, D::Error>
where
    D: Deserializer<'de>,
{
    let s: String = Deserialize::deserialize(deserializer)?;
    if s.is_empty() {
        // stupid special case to be able to tell this one apart later
        FuzzyDate::fromisoformat("0001-01-02").map_err(de::Error::custom)
    } else {
        FuzzyDate::fromisoformat(&s).map_err(de::Error::custom)
    }
}

fn serialize_default_start_date<S>(date: &FuzzyDate, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    // stupid special case for that one start date that's an empty string
    if date == &FuzzyDate::fromisoformat("0001-01-02").unwrap() {
        serializer.serialize_str("")
    } else {
        serializer.serialize_str(&date.to_string())
    }
}

impl NiceName for LordName {
    fn nice_name(&self) -> String {
        let name = self.lordname.clone().unwrap_or_else(|| {
            self.surname
                .clone()
                .unwrap_or_else(|| self.lordofname.clone().unwrap_or("Unknown".to_string()))
        });
        let mut full_name = name.clone();
        if let Some(honorific_prefix) = &self.honorific_prefix {
            full_name = format!("{} {}", honorific_prefix, full_name);
        }
        if let Some(honorific_suffix) = &self.honorific_suffix {
            full_name = format!("{} {}", full_name, honorific_suffix);
        }
        full_name
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
#[pyclass]
pub struct AltName {
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date"
    )]
    end_date: FuzzyDate,
    name: String,
    note: NameType,
    #[serde(skip_serializing_if = "Option::is_none")]
    organization_id: Option<String>,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date"
    )]
    start_date: FuzzyDate,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
#[pyclass(get_all)]
pub struct Organization {
    #[serde(skip_serializing_if = "Option::is_none")]
    classification: Option<OrgType>,
    id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    identifiers: Option<Vec<SimpleIdentifer>>,
    name: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
#[pyclass(get_all)]
pub struct MembershipRedirect {
    id: String,
    redirect: String,
}

impl NiceName for AltName {
    fn nice_name(&self) -> String {
        self.name.clone()
    }
}

impl ValidDateRange for NameOptions {
    fn is_valid_date_range(&self) -> ValidatorErrorCollection {
        match self {
            NameOptions::BasicPersonName(name) => name.is_valid_date_range(),
            NameOptions::LordName(name) => name.is_valid_date_range(),
            NameOptions::AltName(name) => name.is_valid_date_range(),
        }
    }
}

impl<'de> Deserialize<'de> for NameOptions {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        // if there's a name key, it's an alt name
        // if there's a family_name key, it's a person name
        // otherwise it's a lord name
        let value: serde_json::Value = serde::Deserialize::deserialize(deserializer)?;
        if value.get("name").is_some() {
            Ok(NameOptions::AltName(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        } else if value.get("family_name").is_some() {
            Ok(NameOptions::BasicPersonName(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        } else {
            Ok(NameOptions::LordName(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        }
    }
}

impl NiceName for NameOptions {
    fn nice_name(&self) -> String {
        match self {
            NameOptions::BasicPersonName(name) => name.nice_name(),
            NameOptions::LordName(name) => name.nice_name(),
            NameOptions::AltName(name) => name.nice_name(),
        }
    }
}

impl ValidDateRange for Person {
    // if there are other_names, check if they have valid date ranges
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

impl HasId for Person {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl HasId for MembershipRedirect {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl<'de> Deserialize<'de> for Identifier {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value: serde_json::Value = serde::Deserialize::deserialize(deserializer)?;

        if value.is_i64() {
            Ok(Identifier::Int(value.as_i64().unwrap()))
        } else if value.is_string() {
            Ok(Identifier::String(value.as_str().unwrap().to_string()))
        } else {
            Err(de::Error::custom("Expected string or number"))
        }
    }
}

impl HasId for Membership {
    fn get_id(&self) -> &String {
        &self.id
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

impl<'de> Deserialize<'de> for PersonType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value: serde_json::Value = serde::Deserialize::deserialize(deserializer)?;
        if value.get("redirect").is_some() {
            Ok(PersonType::PersonRedirect(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        } else {
            Ok(PersonType::Person(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        }
    }
}

impl HasId for PersonType {
    fn get_id(&self) -> &String {
        match self {
            PersonType::Person(p) => p.get_id(),
            PersonType::PersonRedirect(p) => p.get_id(),
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
impl<'de> Deserialize<'de> for MembershipType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value: serde_json::Value = serde::Deserialize::deserialize(deserializer)?;
        if value.get("redirect").is_some() {
            Ok(MembershipType::MembershipRedirect(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        } else {
            Ok(MembershipType::Membership(
                serde_json::from_value(value).map_err(serde::de::Error::custom)?,
            ))
        }
    }
}

impl HasId for MembershipType {
    fn get_id(&self) -> &String {
        match self {
            MembershipType::Membership(m) => m.get_id(),
            MembershipType::MembershipRedirect(m) => m.get_id(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
#[pyclass(get_all)]
pub struct Area {
    name: String,
    #[serde(default = "Vec::new", skip_serializing_if = "Vec::is_empty")]
    other_names: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
#[pyclass(get_all)]
pub struct Post {
    #[serde(skip_serializing_if = "Option::is_none")]
    area: Option<Area>,
    #[serde(
        default = "default_end_date",
        skip_serializing_if = "is_default_end_date"
    )]
    end_date: FuzzyDate,
    id: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    identifiers: Option<Vec<SimpleIdentifer>>,
    label: String,
    organization_id: String,
    role: String,
    #[serde(
        default = "default_start_date",
        skip_serializing_if = "is_default_start_date"
    )]
    start_date: FuzzyDate,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
#[pyclass(subclass)]
pub struct Popolo {
    #[serde(default = "MembershipCollection::new")]
    #[pyo3(get)]
    pub memberships: MembershipCollection,
    #[serde(default = "OrganizationCollection::new")]
    #[pyo3(get)]
    pub organizations: OrganizationCollection,
    #[serde(default = "PersonCollection::new")]
    #[pyo3(get)]
    pub persons: PersonCollection,
    #[serde(default = "PostCollection::new")]
    #[pyo3(get)]
    pub posts: PostCollection,
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

#[pymethods]
impl Popolo {
    #[getter]
    fn persons_direct<'py>(&self, py: Python<'py>) -> Vec<Py<PyAny>> {
        let py_people: Vec<PyObject> = self
            .persons
            .iter()
            .map(|person_type| match person_type {
                PersonType::Person(person) => person.clone().into_py(py),
                PersonType::PersonRedirect(redirect) => redirect.clone().into_py(py),
            })
            .collect();

        py_people
    }

    #[pyo3(name = "model_validate_json", signature = (json_str, validate=true))]
    #[classmethod]
    fn py_fromjsonstr(_cls: &Bound<'_, PyType>, json_str: &str, validate: bool) -> PyResult<Self> {
        match Popolo::model_validate_json(json_str, &validate) {
            Ok(popolo) => Ok(popolo),
            Err(e) => {
                for error in &e {
                    println!("Validation error: {}", error.error);
                }
                Err(pyo3::exceptions::PyValueError::new_err(
                    "Validation errors help",
                ))
            }
        }
    }
}

impl Popolo {
    fn _just_persons(&self) -> Vec<&Person> {
        self.persons
            .iter()
            .filter_map(|p| match p {
                PersonType::Person(p) => Some(p),
                PersonType::PersonRedirect(_) => None,
            })
            .collect()
    }

    fn just_person_redirects(&self) -> Vec<&PersonRedirect> {
        self.persons
            .iter()
            .filter_map(|p| match p {
                PersonType::Person(_p) => None,
                PersonType::PersonRedirect(p) => Some(p),
            })
            .collect()
    }

    fn just_memberships(&self) -> Vec<&Membership> {
        self.memberships
            .iter()
            .filter_map(|m| match m {
                MembershipType::Membership(m) => Some(m),
                MembershipType::MembershipRedirect(_) => None,
            })
            .collect()
    }

    fn just_membership_redirects(&self) -> Vec<&MembershipRedirect> {
        self.memberships
            .iter()
            .filter_map(|m| match m {
                MembershipType::Membership(_m) => None,
                MembershipType::MembershipRedirect(m) => Some(m),
            })
            .collect()
    }

    pub fn check_valid_date_ranges(&self) -> Result<(), ValidatorErrorCollection> {
        let mut errors: ValidatorErrorCollection = Vec::new();

        let no_org_str = "no_org".to_string();
        // sort the memberships by post_id, person_id, and start_date

        let just_memberships: Vec<&Membership> = self
            .just_memberships()
            .into_iter()
            .filter(|m| m.start_date > FuzzyDate::fromisoformat("1900-01-01").unwrap())
            .sorted_by_key(|m| {
                (
                    m.post_id
                        .as_ref()
                        .unwrap_or(m.organization_id.as_ref().unwrap_or(&no_org_str)),
                    &m.person_id,
                    &m.start_date,
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
                let prev_date = &group[i - 1].end_date;
                let this_date = &group[i].start_date;

                if prev_date.is_approximate() || this_date.is_approximate() {
                    continue;
                }

                if prev_date > this_date {
                    errors.push(ValidatorError {
                        error: format!(
                            "Membership {} overlaps with {}",
                            group[i - 1].id,
                            group[i].id
                        ),
                        validator_type: ValidatorType::DateRangeOverlap,
                    });
                } else if prev_date == this_date {
                    errors.push(ValidatorError {
                        error: format!(
                            "Membership {} same date with {}",
                            group[i - 1].id,
                            group[i].id
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
        // first we're checking if have any duplicates in the ids
        let mut errors: ValidatorErrorCollection = Vec::new();
        let person_ids = check_unique(&self.persons.root, |person| &person.get_id(), "Person")?;
        let organization_ids = check_unique(
            &self.organizations.root,
            |organization| &organization.id,
            "Organization",
        )?;
        let membership_ids = check_unique(
            &self.memberships.root,
            |membership| &membership.get_id(),
            "Membership",
        )?;
        let post_ids = check_unique(&self.posts.root, |post| &post.id, "Post")?;

        // we want to check if the person_id, post_id, organization_id, and on_behalf_of_id in the memberships are valid

        let just_memberships: Vec<&Membership> = self.just_memberships();

        for membership in just_memberships {
            if let Some(error) = check_valid_foreign_key(
                &membership.person_id,
                &person_ids,
                "person_id",
                &membership.id,
            ) {
                errors.push(error);
            }
            if let Some(post_id) = &membership.post_id {
                if let Some(error) =
                    check_valid_foreign_key(post_id, &post_ids, "post_id", &membership.id)
                {
                    errors.push(error);
                }
            }
            if let Some(org_id) = &membership.organization_id {
                if let Some(error) = check_valid_foreign_key(
                    org_id,
                    &organization_ids,
                    "organization_id",
                    &membership.id,
                ) {
                    errors.push(error);
                }
            }
            if let Some(on_behalf_of_id) = &membership.on_behalf_of_id {
                if let Some(error) = check_valid_foreign_key(
                    on_behalf_of_id,
                    &organization_ids,
                    "on_behalf_of_id",
                    &membership.id,
                ) {
                    errors.push(error);
                }
            }
        }
        for membership in self.just_membership_redirects() {
            if let Some(error) = check_valid_foreign_key(
                &membership.redirect,
                &membership_ids,
                "redirect",
                &membership.id,
            ) {
                errors.push(error);
            }
        }

        for person in &self.just_person_redirects() {
            if let Some(error) =
                check_valid_foreign_key(&person.redirect, &person_ids, "redirect", &person.id)
            {
                errors.push(error);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn model_dump_json(&self) -> String {
        serde_json::to_string_pretty(&self).unwrap()
    }

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
        let mut popolo: Popolo = serde_json::from_str(json_str).map_err(|e| {
            vec![ValidatorError {
                error: e.to_string(),
                validator_type: ValidatorType::InvalidJson,
            }]
        })?;
        popolo.set_lookup();
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

impl_valid_date_range!(BasicPersonName);
impl_valid_date_range!(LordName);
impl_valid_date_range!(AltName);
impl_valid_date_range!(Membership);
impl_valid_date_range!(Post);
