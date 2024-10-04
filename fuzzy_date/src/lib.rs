use chrono::{Datelike, Duration, NaiveDate};
use pyo3::exceptions::PyTypeError;
use pyo3::prelude::*;
use pyo3::types::{PyString, PyType};
use std::fmt;

use std::str::FromStr;

#[derive(Debug, Clone)]

enum DateCompleteness {
    YearOnly,
    YearAndMonth,
    FullDate,
    FullDateRange,
    NotADate,
}

/// Check if the date is just start and end of year
fn is_partial_just_year(earliest_date: &NaiveDate, latest_date: &NaiveDate) -> bool {
    if earliest_date.year() == latest_date.year() {
        return earliest_date.month() == 1
            && earliest_date.day() == 1
            && latest_date.month() == 12
            && latest_date.day() == 31;
    }
    false
}

/// Check if the date is just start and end of month
fn is_partial_just_year_and_month(earliest_date: &NaiveDate, latest_date: &NaiveDate) -> bool {
    if earliest_date.month() == latest_date.month() && earliest_date.year() == latest_date.year() {
        let latest_in_month = last_day_in_month(&earliest_date);
        return earliest_date.day() == 1 && latest_date.day() == latest_in_month.day();
    }
    false
}

/// Get the completeness of a date range
fn get_paired_completeness(earliest_date: &NaiveDate, latest_date: &NaiveDate) -> DateCompleteness {
    if earliest_date == latest_date {
        return DateCompleteness::FullDate;
    } else if is_partial_just_year_and_month(&earliest_date, &latest_date) {
        return DateCompleteness::YearAndMonth;
    } else if is_partial_just_year(&earliest_date, &latest_date) {
        return DateCompleteness::YearOnly;
    }
    DateCompleteness::FullDateRange
}

/// given a start date, return the last day of the month
fn last_day_in_month(start_date: &NaiveDate) -> NaiveDate {
    let latest = if start_date.month() == 12 {
        NaiveDate::from_ymd_opt(start_date.year(), 12, 31).unwrap()
    } else {
        NaiveDate::from_ymd_opt(start_date.year(), start_date.month() + 1, 1).unwrap()
            - Duration::days(1)
    };
    latest
}

/// given a start date, return the last day of the year
fn last_day_in_year(start_date: &NaiveDate) -> NaiveDate {
    NaiveDate::from_ymd_opt(start_date.year(), 12, 31).unwrap()
}

// Try and parse an incomplete date string
fn parse_single_date(date: &str) -> Result<NaiveDate, String> {
    // Check is just numbers and -
    if !date.chars().all(|c| c.is_digit(10) || c == '-') {
        return Err(format!("Invalid date format: {}", date));
    }

    match date.len() {
        4 => {
            // Parse the year-only format: YYYY
            let year = i32::from_str(date).map_err(|_| format!("Invalid year format: {}", date))?;
            NaiveDate::from_ymd_opt(year, 1, 1)
                .ok_or_else(|| format!("Failed to create date from year: {}", date))
        }
        7 => {
            // Parse the year-month format: YYYY-MM
            let (year, month) = date.split_at(4);
            let year = i32::from_str(year).map_err(|_| format!("Invalid year format: {}", year))?;
            let month = u32::from_str(&month[1..])
                .map_err(|_| format!("Invalid month format: {}", month))?;
            NaiveDate::from_ymd_opt(year, month, 1)
                .ok_or_else(|| format!("Failed to create date from year-month: {}", date))
        }
        10 => {
            // Parse the full date format: YYYY-MM-DD
            NaiveDate::parse_from_str(date, "%Y-%m-%d")
                .map_err(|_| format!("Invalid full date format: {}", date))
        }
        _ => Err(format!("Invalid date length: {}", date)),
    }
}

// Determine the completeness of a date based on length alone
fn determine_completeness(date: &str) -> DateCompleteness {
    match date.len() {
        4 => DateCompleteness::YearOnly,
        7 => DateCompleteness::YearAndMonth,
        10 => DateCompleteness::FullDate,
        _ => DateCompleteness::NotADate,
    }
}

#[pyclass]
#[derive(Debug, Clone)]
pub struct FuzzyDate {
    #[pyo3(get)]
    earliest_date: NaiveDate,
    #[pyo3(get)]
    latest_date: NaiveDate,
    completeness: DateCompleteness,
}

#[pymethods]
impl FuzzyDate {
    #[new]
    fn new_py(earliest_date: NaiveDate, latest_date: NaiveDate) -> Self {
        Self {
            earliest_date: earliest_date,
            latest_date: latest_date,
            completeness: get_paired_completeness(&earliest_date, &latest_date),
        }
    }

    #[setter]
    fn set_earliest_date(&mut self, earliest_date: &Bound<'_, PyAny>) -> PyResult<()> {
        if let Ok(date) = earliest_date.extract::<NaiveDate>() {
            self.set_earliest(date);
            Ok(())
        } else {
            Err(PyTypeError::new_err("Invalid date format"))
        }
    }

    #[setter]
    fn set_latest_date(&mut self, latest_date: &Bound<'_, PyAny>) -> PyResult<()> {
        if let Ok(date) = latest_date.extract::<NaiveDate>() {
            self.set_latest(date);
            Ok(())
        } else {
            Err(PyTypeError::new_err("Invalid date format"))
        }
    }

    fn __eq__(&self, other: &Bound<'_, PyAny>) -> PyResult<bool> {
        if let Ok(other_date) = other.extract::<NaiveDate>() {
            Ok(self.eq(&other_date))
        } else if let Ok(other_date) = other.extract::<FuzzyDate>() {
            Ok(self.eq(&other_date))
        } else if let Ok(other_date_str) = other.extract::<&str>() {
            Ok(self.isoformat() == other_date_str)
        } else {
            Err(PyTypeError::new_err(
                "Comparison not supported between these types",
            ))
        }
    }

    fn __lt__(&self, other: &Bound<'_, PyAny>) -> PyResult<bool> {
        if let Ok(other_date) = other.extract::<NaiveDate>() {
            Ok(self.partial_cmp(&other_date) == Some(std::cmp::Ordering::Less))
        } else if let Ok(other_date) = other.extract::<FuzzyDate>() {
            Ok(self.partial_cmp(&other_date) == Some(std::cmp::Ordering::Less))
        } else {
            Err(PyTypeError::new_err(
                "Comparison not supported between these types",
            ))
        }
    }

    fn __gt__(&self, other: &Bound<'_, PyAny>) -> PyResult<bool> {
        if let Ok(other_date) = other.extract::<NaiveDate>() {
            Ok(self.partial_cmp(&other_date) == Some(std::cmp::Ordering::Greater))
        } else if let Ok(other_date) = other.extract::<FuzzyDate>() {
            Ok(self.partial_cmp(&other_date) == Some(std::cmp::Ordering::Greater))
        } else {
            Err(PyTypeError::new_err(
                "Comparison not supported between these types",
            ))
        }
    }

    #[pyo3(name = "fromisoformat")]
    #[classmethod]
    fn py_fromisoformat(_cls: &Bound<'_, PyType>, iso8601_date_string: &str) -> PyResult<Self> {
        match FuzzyDate::fromisoformat(iso8601_date_string) {
            Ok(date) => Ok(date),
            Err(e) => Err(pyo3::exceptions::PyValueError::new_err(e)),
        }
    }

    #[pyo3(name = "isoformat")]
    fn py_isoformat(&self) -> String {
        self.isoformat()
    }

    fn __str__(&self) -> String {
        self.isoformat()
    }

    fn __repr__(slf: &Bound<'_, Self>) -> String {
        let class_name: Bound<'_, PyString> = slf.get_type().qualname().unwrap();
        let in_self = slf.borrow();
        format!(
            "{}({}, {})",
            class_name, in_self.earliest_date, in_self.latest_date
        )
    }
}

impl FuzzyDate {
    pub fn new(earliest_date: NaiveDate, latest_date: NaiveDate) -> Self {
        Self {
            earliest_date: earliest_date,
            latest_date: latest_date,
            completeness: get_paired_completeness(&earliest_date, &latest_date),
        }
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
                format!(
                    "{}/{}",
                    self.earliest_date.to_string(),
                    self.latest_date.to_string()
                )
            }
            _ => "Invalid date".to_string(),
        }
    }

    pub fn fromisoformat(iso8601_date_string: &str) -> Result<Self, String> {
        let parts: Vec<&str> = iso8601_date_string.split('/').collect();

        // Determine the completeness
        let completeness = match parts.len() {
            1 => determine_completeness(parts[0]),
            2 => DateCompleteness::FullDateRange,
            _ => DateCompleteness::NotADate,
        };

        // Parse the start date
        let start_date_result = match completeness {
            DateCompleteness::NotADate => {
                Err(format!("Invalid date format: {}", iso8601_date_string))
            }
            DateCompleteness::FullDateRange => parse_single_date(parts[0]),
            _ => parse_single_date(iso8601_date_string),
        };

        let start_date = match start_date_result {
            Ok(date) => date,
            Err(e) => return Err(e),
        };

        let end_date_result = match completeness {
            DateCompleteness::NotADate => {
                Err(format!("Invalid date format: {}", iso8601_date_string))
            }
            DateCompleteness::YearOnly => Ok(last_day_in_year(&start_date)),
            DateCompleteness::YearAndMonth => Ok(last_day_in_month(&start_date)),
            DateCompleteness::FullDateRange => parse_single_date(parts[1]),
            DateCompleteness::FullDate => Ok(start_date),
        };

        let end_date = match end_date_result {
            Ok(date) => date,
            Err(e) => return Err(e),
        };

        Ok(Self {
            earliest_date: start_date,
            latest_date: end_date,
            completeness: completeness,
        })
    }

    pub fn approx_equal(&self, date: &NaiveDate) -> bool {
        // if date is between earliest and latest
        if date >= &self.earliest_date && date <= &self.latest_date {
            return true;
        } else {
            return false;
        }
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
        // Either the earlest date and latest date are an exact match
        // or one of the dates is within the range of the other
        (self.earliest_date == other.earliest_date && self.latest_date == other.latest_date)
            || self.approx_equal(&other.earliest_date)
            || self.approx_equal(&other.latest_date)
            || other.approx_equal(&self.earliest_date)
            || other.approx_equal(&self.latest_date)
    }
}

impl PartialEq<NaiveDate> for FuzzyDate {
    fn eq(&self, other: &NaiveDate) -> bool {
        self.approx_equal(other)
    }
}
impl PartialOrd for FuzzyDate {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // if latest date is before the earliest date of the other - that's less
        // if earliest date is after the latest date of the other - that's greater
        if self.latest_date < other.earliest_date {
            Some(std::cmp::Ordering::Less)
        } else if self.earliest_date > other.latest_date {
            Some(std::cmp::Ordering::Greater)
        } else {
            Some(std::cmp::Ordering::Equal)
        }
    }
}

impl PartialOrd<NaiveDate> for FuzzyDate {
    fn partial_cmp(&self, other: &NaiveDate) -> Option<std::cmp::Ordering> {
        // if the date is after the latest date - that's greater
        // if the date is before the earliest date - that's less
        if other > &self.latest_date {
            Some(std::cmp::Ordering::Less)
        } else if other < &self.earliest_date {
            Some(std::cmp::Ordering::Greater)
        } else {
            Some(std::cmp::Ordering::Equal)
        }
    }
}

impl Eq for FuzzyDate {}
