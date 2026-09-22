//! Owned Python name values; editing a returned value does not edit a person.

use super::conversions::py_to_fuzzy_date;
use fuzzy_date::FuzzyDate;
use popolo_validator::{NameType, NiceName};
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

#[gen_stub_pyclass]
#[pyclass(name = "LordName")]
#[derive(Clone)]
pub struct PyLordName {
    inner: popolo_validator::LordName,
}

#[gen_stub_pyclass]
#[pyclass(name = "BasicPersonName")]
#[derive(Clone)]
pub struct PyBasicPersonName {
    inner: popolo_validator::BasicPersonName,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyBasicPersonName {
    #[new]
    #[pyo3(signature = (family_name, *, given_name=None, note="Main"))]
    fn py_new(family_name: String, given_name: Option<String>, note: &str) -> PyResult<Self> {
        let note = match note {
            "Main" => NameType::Main,
            "Alternate" => NameType::Alternate,
            _ => return Err(PyValueError::new_err("note must be Main or Alternate")),
        };
        Ok(Self {
            inner: popolo_validator::BasicPersonName {
                end_date: FuzzyDate::fromisoformat("9999-12-31").unwrap(),
                family_name,
                given_name,
                honorific_prefix: None,
                note,
                start_date: FuzzyDate::fromisoformat("0001-01-01").unwrap(),
            },
        })
    }

    fn nice_name(&self) -> String {
        self.inner.nice_name()
    }
}

#[gen_stub_pyclass]
#[pyclass(name = "AltName")]
#[derive(Clone)]
pub struct PyAltName {
    inner: popolo_validator::AltName,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyAltName {
    #[new]
    #[pyo3(signature = (name, *, note="Alternate"))]
    fn py_new(name: String, note: &str) -> PyResult<Self> {
        if !matches!(note, "Main" | "Alternate") {
            return Err(PyValueError::new_err("note must be Main or Alternate"));
        }
        Ok(Self {
            inner: popolo_validator::AltName {
                end_date: FuzzyDate::fromisoformat("9999-12-31").unwrap(),
                name,
                note: NameType::Alternate,
                organization_id: None,
                start_date: FuzzyDate::fromisoformat("0001-01-01").unwrap(),
            },
        })
    }

    fn nice_name(&self) -> String {
        self.inner.nice_name()
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyLordName {
    #[new]
    #[pyo3(signature = (*, note, honorific_prefix, additional_name=None, county=None,
        end_date=None, given_name=None, honorific_suffix=None, lordname=None,
        lordofname=None, lordofname_full=None, start_date=None, surname=None))]
    #[allow(clippy::too_many_arguments)]
    fn py_new(
        note: &str,
        honorific_prefix: String,
        additional_name: Option<String>,
        county: Option<String>,
        end_date: Option<&Bound<'_, PyAny>>,
        given_name: Option<String>,
        honorific_suffix: Option<String>,
        lordname: Option<String>,
        lordofname: Option<String>,
        lordofname_full: Option<String>,
        start_date: Option<&Bound<'_, PyAny>>,
        surname: Option<String>,
    ) -> PyResult<Self> {
        if surname.is_some()
            && lordname.is_none()
            && (lordofname.is_none() || honorific_prefix.is_empty())
        {
            return Err(PyValueError::new_err(
                "LordName with surname also needs a lordname, or a lordofname and honorific_prefix",
            ));
        }
        let note = match note {
            "Main" => NameType::Main,
            "Alternate" => NameType::Alternate,
            _ => return Err(PyValueError::new_err("note must be Main or Alternate")),
        };
        Ok(Self {
            inner: popolo_validator::LordName {
                additional_name,
                county,
                end_date: end_date
                    .map(py_to_fuzzy_date)
                    .transpose()?
                    .unwrap_or_else(|| FuzzyDate::fromisoformat("9999-12-31").unwrap()),
                given_name,
                honorific_prefix,
                honorific_suffix,
                lordname,
                lordofname,
                lordofname_full,
                note,
                start_date: start_date
                    .map(py_to_fuzzy_date)
                    .transpose()?
                    .unwrap_or_else(|| FuzzyDate::fromisoformat("0001-01-01").unwrap()),
                surname,
            },
        })
    }

    fn nice_name(&self) -> String {
        self.inner.nice_name()
    }

    fn original_full_name(&self) -> Option<String> {
        self.inner.original_full_name()
    }

    #[pyo3(signature = (include_original_name=false))]
    fn name_variants(&self, include_original_name: bool) -> Vec<String> {
        self.inner.name_variants(include_original_name)
    }
}

impl PyLordName {
    pub(super) fn from_owned(inner: popolo_validator::LordName) -> Self {
        Self { inner }
    }
}

impl PyBasicPersonName {
    pub(super) fn from_owned(inner: popolo_validator::BasicPersonName) -> Self {
        Self { inner }
    }
}

impl PyAltName {
    pub(super) fn from_owned(inner: popolo_validator::AltName) -> Self {
        Self { inner }
    }
}
