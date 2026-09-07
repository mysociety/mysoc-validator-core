//! Partial dates represented by inclusive earliest/latest bounds.
//!
//! Precision controls the compact JSON spelling (year, month, day or range).
//! Rust equality compares bounds, and ordering sorts by earliest then latest.
//! Overlap is deliberately a named predicate: unlike equality it is not
//! transitive. Python's historical fuzzy comparisons live in `python.rs`.
//! Public bounds are retained for Rust API compatibility; use the setters when
//! changing them so the cached precision used by `isoformat` stays in sync.

use chrono::{Datelike, Duration, NaiveDate};
use std::fmt;

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::str::FromStr;

#[cfg(feature = "python")]
use pyo3::prelude::*;
#[cfg(feature = "python")]
use pyo3_stub_gen::derive::gen_stub_pyclass;
#[cfg(feature = "python")]
mod python;

#[derive(Debug, Clone, PartialEq)]
pub enum DateCompleteness {
    YearOnly,
    YearAndMonth,
    FullDate,
    FullDateRange,
    NotADate,
}

fn is_partial_just_year(earliest_date: &NaiveDate, latest_date: &NaiveDate) -> bool {
    if earliest_date.year() == latest_date.year() {
        return earliest_date.month() == 1
            && earliest_date.day() == 1
            && latest_date.month() == 12
            && latest_date.day() == 31;
    }
    false
}

fn is_partial_just_year_and_month(earliest_date: &NaiveDate, latest_date: &NaiveDate) -> bool {
    if earliest_date.month() == latest_date.month() && earliest_date.year() == latest_date.year() {
        let latest_in_month = last_day_in_month(earliest_date);
        return earliest_date.day() == 1 && latest_date.day() == latest_in_month.day();
    }
    false
}

fn get_paired_completeness(earliest_date: &NaiveDate, latest_date: &NaiveDate) -> DateCompleteness {
    if earliest_date == latest_date {
        DateCompleteness::FullDate
    } else if is_partial_just_year_and_month(earliest_date, latest_date) {
        DateCompleteness::YearAndMonth
    } else if is_partial_just_year(earliest_date, latest_date) {
        DateCompleteness::YearOnly
    } else {
        DateCompleteness::FullDateRange
    }
}

fn last_day_in_month(start_date: &NaiveDate) -> NaiveDate {
    if start_date.month() == 12 {
        NaiveDate::from_ymd_opt(start_date.year(), 12, 31).unwrap()
    } else {
        NaiveDate::from_ymd_opt(start_date.year(), start_date.month() + 1, 1).unwrap()
            - Duration::days(1)
    }
}

fn last_day_in_year(start_date: &NaiveDate) -> NaiveDate {
    NaiveDate::from_ymd_opt(start_date.year(), 12, 31).unwrap()
}

fn parse_single_date(date: &str) -> Result<NaiveDate, String> {
    if !date.chars().all(|c| c.is_ascii_digit() || c == '-') {
        return Err(format!("Invalid date format: {}", date));
    }
    match date.len() {
        4 => {
            let year = i32::from_str(date).map_err(|_| format!("Invalid year format: {}", date))?;
            NaiveDate::from_ymd_opt(year, 1, 1)
                .ok_or_else(|| format!("Failed to create date from year: {}", date))
        }
        7 => {
            let (year, month) = date.split_at(4);
            let year = i32::from_str(year).map_err(|_| format!("Invalid year: {}", year))?;
            let month =
                u32::from_str(&month[1..]).map_err(|_| format!("Invalid month: {}", month))?;
            NaiveDate::from_ymd_opt(year, month, 1)
                .ok_or_else(|| format!("Failed to create date from year-month: {}", date))
        }
        10 => NaiveDate::parse_from_str(date, "%Y-%m-%d")
            .map_err(|_| format!("Invalid full date format: {}", date)),
        _ => Err(format!("Invalid date length: {}", date)),
    }
}

fn determine_completeness(date: &str) -> DateCompleteness {
    match date.len() {
        4 => DateCompleteness::YearOnly,
        7 => DateCompleteness::YearAndMonth,
        10 => DateCompleteness::FullDate,
        _ => DateCompleteness::NotADate,
    }
}

#[cfg_attr(feature = "python", gen_stub_pyclass)]
#[cfg_attr(feature = "python", pyclass(subclass, module = "fuzzy_date"))]
#[derive(Debug, Clone)]
pub struct FuzzyDate {
    pub earliest_date: NaiveDate,
    pub latest_date: NaiveDate,
    completeness: DateCompleteness,
}

impl Serialize for FuzzyDate {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.isoformat())
    }
}

impl<'de> Deserialize<'de> for FuzzyDate {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        FuzzyDate::fromisoformat(&s).map_err(serde::de::Error::custom)
    }
}

impl FuzzyDate {
    pub fn new(earliest_date: NaiveDate, latest_date: NaiveDate) -> Self {
        Self {
            earliest_date,
            latest_date,
            completeness: get_paired_completeness(&earliest_date, &latest_date),
        }
    }

    pub fn is_approximate(&self) -> bool {
        self.completeness != DateCompleteness::FullDate
    }

    pub fn set_earliest(&mut self, earliest_date: NaiveDate) {
        self.earliest_date = earliest_date;
        self.completeness = get_paired_completeness(&self.earliest_date, &self.latest_date);
    }

    pub fn set_latest(&mut self, latest_date: NaiveDate) {
        self.latest_date = latest_date;
        self.completeness = get_paired_completeness(&self.earliest_date, &self.latest_date);
    }

    pub fn isoformat(&self) -> String {
        match self.completeness {
            DateCompleteness::YearOnly => format!("{}", self.earliest_date.year()),
            DateCompleteness::YearAndMonth => format!(
                "{}-{:02}",
                self.earliest_date.year(),
                self.earliest_date.month()
            ),
            DateCompleteness::FullDate => self.earliest_date.to_string(),
            DateCompleteness::FullDateRange => {
                format!("{}/{}", self.earliest_date, self.latest_date)
            }
            DateCompleteness::NotADate => "Invalid date".to_string(),
        }
    }

    pub fn fromisoformat(iso8601_date_string: &str) -> Result<Self, String> {
        let parts: Vec<&str> = iso8601_date_string.split('/').collect();

        let completeness = match parts.len() {
            1 => determine_completeness(parts[0]),
            2 => DateCompleteness::FullDateRange,
            _ => DateCompleteness::NotADate,
        };

        let start_date = match completeness {
            DateCompleteness::NotADate => {
                return Err(format!("Invalid date format: {}", iso8601_date_string))
            }
            DateCompleteness::FullDateRange => parse_single_date(parts[0])?,
            _ => parse_single_date(iso8601_date_string)?,
        };

        let end_date = match completeness {
            DateCompleteness::NotADate => {
                return Err(format!("Invalid date format: {}", iso8601_date_string))
            }
            DateCompleteness::YearOnly => last_day_in_year(&start_date),
            DateCompleteness::YearAndMonth => last_day_in_month(&start_date),
            DateCompleteness::FullDateRange => parse_single_date(parts[1])?,
            DateCompleteness::FullDate => start_date,
        };

        Ok(Self {
            earliest_date: start_date,
            latest_date: end_date,
            completeness,
        })
    }

    /// Whether a precise date falls within these inclusive bounds.
    pub fn contains(&self, date: &NaiveDate) -> bool {
        date >= &self.earliest_date && date <= &self.latest_date
    }

    /// Fuzzy matching is separate from Rust equality, which compares exact bounds.
    pub fn overlaps(&self, other: &Self) -> bool {
        self == other
            || self.contains(&other.earliest_date)
            || self.contains(&other.latest_date)
            || other.contains(&self.earliest_date)
            || other.contains(&self.latest_date)
    }

    /// Whether this entire interval precedes the other interval.
    pub fn is_before(&self, other: &Self) -> bool {
        self.latest_date < other.earliest_date
    }

    /// Whether this entire interval follows the other interval.
    pub fn is_after(&self, other: &Self) -> bool {
        // Bounds are publicly mutable. Preserve the historical comparator's
        // before-first precedence even if a caller has reversed them.
        !self.is_before(other) && self.earliest_date > other.latest_date
    }

    /// Compatibility spelling for precise-date containment.
    pub fn approx_equal(&self, date: &NaiveDate) -> bool {
        self.contains(date)
    }
}

impl From<String> for FuzzyDate {
    fn from(s: String) -> FuzzyDate {
        FuzzyDate::fromisoformat(&s).unwrap()
    }
}

impl From<FuzzyDate> for String {
    fn from(fuzzy_date: FuzzyDate) -> String {
        fuzzy_date.isoformat()
    }
}

impl fmt::Display for FuzzyDate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.isoformat())
    }
}

impl std::ops::Add<Duration> for FuzzyDate {
    type Output = Self;
    fn add(self, other: Duration) -> Self::Output {
        Self::new(self.earliest_date + other, self.latest_date + other)
    }
}

impl PartialEq<FuzzyDate> for FuzzyDate {
    fn eq(&self, other: &FuzzyDate) -> bool {
        self.earliest_date == other.earliest_date && self.latest_date == other.latest_date
    }
}

impl PartialEq<str> for FuzzyDate {
    fn eq(&self, other: &str) -> bool {
        match FuzzyDate::fromisoformat(other) {
            Ok(date) => self == &date,
            Err(_) => false,
        }
    }
}

impl PartialEq<NaiveDate> for FuzzyDate {
    fn eq(&self, other: &NaiveDate) -> bool {
        self.earliest_date == *other && self.latest_date == *other
    }
}

impl PartialOrd for FuzzyDate {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FuzzyDate {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.earliest_date, self.latest_date).cmp(&(other.earliest_date, other.latest_date))
    }
}

impl PartialOrd<NaiveDate> for FuzzyDate {
    fn partial_cmp(&self, other: &NaiveDate) -> Option<std::cmp::Ordering> {
        Some((self.earliest_date, self.latest_date).cmp(&(*other, *other)))
    }
}

impl Eq for FuzzyDate {}

#[cfg(test)]
mod tests;
