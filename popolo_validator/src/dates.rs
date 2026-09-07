//! Open-ended Popolo date defaults and their JSON omission rules.
//!
//! 0001-01-01 and 9999-12-31 represent absent bounds. Lord names additionally use
//! 0001-01-02 to remember an explicitly empty start-date string. Omission and empty
//! serialization intentionally retain fuzzy matching of these sentinels, even
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

pub(crate) fn empty_string_as_default_start_date<'de, D>(
    deserializer: D,
) -> Result<FuzzyDate, D::Error>
where
    D: Deserializer<'de>,
{
    let s: String = Deserialize::deserialize(deserializer)?;
    if s.is_empty() {
        FuzzyDate::fromisoformat("0001-01-02").map_err(de::Error::custom)
    } else {
        FuzzyDate::fromisoformat(&s).map_err(de::Error::custom)
    }
}

pub(crate) fn serialize_default_start_date<S>(
    date: &FuzzyDate,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    if date.contains(
        &FuzzyDate::fromisoformat("0001-01-02")
            .unwrap()
            .earliest_date,
    ) {
        serializer.serialize_str("")
    } else {
        serializer.serialize_str(&date.to_string())
    }
}
