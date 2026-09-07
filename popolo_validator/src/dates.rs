//! Open-ended Popolo date defaults and their JSON omission rules.
//!
//! 0001-01-01 and 9999-12-31 represent absent bounds. Empty strings and null
//! deserialize to those defaults, matching upstream Popolo date validators.
//! Omission intentionally retains fuzzy matching of these sentinels, even
//! though Rust equality now requires identical bounds. Constant parses unwrap
//! because their literal values are known valid, not because user input is trusted.

use fuzzy_date::FuzzyDate;
use lazy_static::lazy_static;
use serde::de::{self, Deserialize, Deserializer};

lazy_static! {
    static ref DEFAULT_START_DATE: FuzzyDate = FuzzyDate::fromisoformat("0001-01-01").unwrap();
    static ref DEFAULT_END_DATE: FuzzyDate = FuzzyDate::fromisoformat("9999-12-31").unwrap();
}

pub(crate) fn default_start_date() -> FuzzyDate {
    DEFAULT_START_DATE.clone()
}

pub(crate) fn default_end_date() -> FuzzyDate {
    DEFAULT_END_DATE.clone()
}

pub(crate) fn is_default_start_date(date: &FuzzyDate) -> bool {
    date.overlaps(&DEFAULT_START_DATE)
}

pub(crate) fn is_default_end_date(date: &FuzzyDate) -> bool {
    date.overlaps(&DEFAULT_END_DATE)
}

pub(crate) fn deserialize_start_date<'de, D>(deserializer: D) -> Result<FuzzyDate, D::Error>
where
    D: Deserializer<'de>,
{
    deserialize_date_or_default(deserializer, default_start_date)
}

pub(crate) fn deserialize_end_date<'de, D>(deserializer: D) -> Result<FuzzyDate, D::Error>
where
    D: Deserializer<'de>,
{
    deserialize_date_or_default(deserializer, default_end_date)
}

fn deserialize_date_or_default<'de, D>(
    deserializer: D,
    default: impl FnOnce() -> FuzzyDate,
) -> Result<FuzzyDate, D::Error>
where
    D: Deserializer<'de>,
{
    match Option::<String>::deserialize(deserializer)? {
        Some(value) if !value.is_empty() => {
            FuzzyDate::fromisoformat(&value).map_err(de::Error::custom)
        }
        _ => Ok(default()),
    }
}
