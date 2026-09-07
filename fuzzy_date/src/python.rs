//! Python compatibility comparisons, independent of Rust comparison traits.
//!
//! A Python date matches any interval containing it; two fuzzy dates match if
//! they overlap. Strings use the compact ISO spelling. Overlapping intervals
//! are neither less nor greater, so both <= and >= may be true. Do not replace
//! these predicates with Rust operators when changing the core date type.
//! The feature gate on this module keeps PyO3 out of pure-Rust builds.

use super::FuzzyDate;
use chrono::{Datelike, NaiveDate};
use pyo3::exceptions::PyTypeError;
use pyo3::prelude::*;
use pyo3::types::{PyString, PyType};
use pyo3_stub_gen::derive::gen_stub_pymethods;

#[gen_stub_pymethods]
#[pymethods]
impl FuzzyDate {
    #[new]
    fn new_py(earliest_date: NaiveDate, latest_date: NaiveDate) -> Self {
        Self::new(earliest_date, latest_date)
    }

    #[getter]
    fn earliest_date(&self) -> NaiveDate {
        self.earliest_date
    }

    #[getter]
    fn latest_date(&self) -> NaiveDate {
        self.latest_date
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
            Ok(self.contains(&other_date))
        } else if let Ok(other_date) = other.extract::<FuzzyDate>() {
            Ok(self.overlaps(&other_date))
        } else if let Ok(other_date_str) = other.extract::<String>() {
            Ok(self.isoformat() == other_date_str)
        } else {
            Err(PyTypeError::new_err(
                "Comparison not supported between these types",
            ))
        }
    }

    fn __lt__(&self, other: &Bound<'_, PyAny>) -> PyResult<bool> {
        if let Ok(other_date) = other.extract::<NaiveDate>() {
            Ok(self.is_before(&Self::new(other_date, other_date)))
        } else if let Ok(other_date) = other.extract::<FuzzyDate>() {
            Ok(self.is_before(&other_date))
        } else {
            Err(PyTypeError::new_err(
                "Comparison not supported between these types",
            ))
        }
    }

    fn __gt__(&self, other: &Bound<'_, PyAny>) -> PyResult<bool> {
        if let Ok(other_date) = other.extract::<NaiveDate>() {
            Ok(self.is_after(&Self::new(other_date, other_date)))
        } else if let Ok(other_date) = other.extract::<FuzzyDate>() {
            Ok(self.is_after(&other_date))
        } else {
            Err(PyTypeError::new_err(
                "Comparison not supported between these types",
            ))
        }
    }

    fn __le__(&self, other: &Bound<'_, PyAny>) -> PyResult<bool> {
        if let Ok(other_date) = other.extract::<NaiveDate>() {
            Ok(!self.is_after(&Self::new(other_date, other_date)))
        } else if let Ok(other_date) = other.extract::<FuzzyDate>() {
            Ok(!self.is_after(&other_date))
        } else {
            Err(PyTypeError::new_err(
                "Comparison not supported between these types",
            ))
        }
    }

    fn __ge__(&self, other: &Bound<'_, PyAny>) -> PyResult<bool> {
        if let Ok(other_date) = other.extract::<NaiveDate>() {
            Ok(!self.is_before(&Self::new(other_date, other_date)))
        } else if let Ok(other_date) = other.extract::<FuzzyDate>() {
            Ok(!self.is_before(&other_date))
        } else {
            Err(PyTypeError::new_err(
                "Comparison not supported between these types",
            ))
        }
    }

    fn __add__(&self, py: Python, other: &Bound<'_, PyAny>) -> PyResult<PyObject> {
        let days: i64 = other.getattr("days")?.extract()?;
        let result = self.earliest_date + chrono::Duration::days(days);
        let date_cls = py.import_bound("datetime")?.getattr("date")?;
        date_cls
            .call1((result.year(), result.month(), result.day()))
            .map(|d| d.into_py(py))
    }

    #[pyo3(name = "fromisoformat")]
    #[classmethod]
    fn py_fromisoformat(_cls: &Bound<'_, PyType>, iso8601_date_string: &str) -> PyResult<Self> {
        Self::fromisoformat(iso8601_date_string).map_err(pyo3::exceptions::PyValueError::new_err)
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
