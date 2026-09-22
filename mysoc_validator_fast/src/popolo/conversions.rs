//! Python/domain conversions and compatibility serialization helpers.
//!
//! `Bound<'py, PyAny>` is a Python object tied to an interpreter-access lifetime;
//! `Python<'py>` carries that access token. `Py<T>` owns a Python reference that
//! can outlive the token and is rebound before use. Extracting `FuzzyDate` clones
//! domain data, while cloning a Python reference retains the same Python object.
//! `PyResult<T>` maps Rust errors to Python exceptions; it does not copy `T`.
//! JSON conversions call Python and must run outside dataset lock guards.

use crate::conversions::py_to_json_value;
use chrono::NaiveDate;
use fuzzy_date::FuzzyDate;
use popolo_validator::MembershipReason;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;

pub(super) fn py_to_naive_date(obj: &Bound<'_, PyAny>) -> PyResult<NaiveDate> {
    if let Ok(d) = obj.extract::<NaiveDate>() {
        return Ok(d);
    }
    if let Ok(fd) = obj.extract::<FuzzyDate>() {
        return Ok(fd.earliest_date);
    }
    if let Ok(s) = obj.extract::<String>() {
        return NaiveDate::parse_from_str(&s, "%Y-%m-%d")
            .map_err(|e| PyValueError::new_err(format!("Invalid date string: {}", e)));
    }
    let year: i32 = obj.getattr("year")?.extract()?;
    let month: u32 = obj.getattr("month")?.extract()?;
    let day: u32 = obj.getattr("day")?.extract()?;
    NaiveDate::from_ymd_opt(year, month, day).ok_or_else(|| PyValueError::new_err("Invalid date"))
}

pub(super) fn py_to_fuzzy_date(obj: &Bound<'_, PyAny>) -> PyResult<FuzzyDate> {
    if let Ok(fd) = obj.extract::<FuzzyDate>() {
        return Ok(fd);
    }
    let d = py_to_naive_date(obj)?;
    Ok(FuzzyDate::new(d, d))
}

pub(super) fn fuzzy_to_py(py: Python<'_>, fd: &FuzzyDate) -> PyResult<Py<FuzzyDate>> {
    Py::new(py, fd.clone())
}

pub(super) fn membership_reason_to_str(r: &MembershipReason) -> String {
    r.as_str().to_string()
}

pub(super) fn str_to_membership_reason(s: &str) -> PyResult<MembershipReason> {
    MembershipReason::from_str(s)
        .ok_or_else(|| PyValueError::new_err(format!("Unknown MembershipReason: {}", s)))
}

pub(super) fn py_to_membership_reason(obj: &Bound<'_, PyAny>) -> PyResult<MembershipReason> {
    let s: String = obj.str()?.extract()?;
    str_to_membership_reason(&s)
}

pub(super) fn py_to_extra(
    value: Option<&Bound<'_, PyAny>>,
) -> PyResult<Option<popolo_validator::Extra>> {
    value
        .map(|value| match py_to_json_value(value)? {
            serde_json::Value::Object(extra) => Ok(extra),
            _ => Err(PyValueError::new_err("extra must be a mapping")),
        })
        .transpose()
}

pub(super) trait ToOptionalString {
    fn to_optional_string(&self) -> Option<String>;
}

impl ToOptionalString for String {
    fn to_optional_string(&self) -> Option<String> {
        Some(self.clone())
    }
}

impl ToOptionalString for Option<String> {
    fn to_optional_string(&self) -> Option<String> {
        self.clone()
    }
}
