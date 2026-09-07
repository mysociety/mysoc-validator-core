//! Register, person and category wrappers navigating a shared tree.

use chrono::NaiveDate;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::PyType;
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};
use regmem_validator as rv;
use std::sync::{Arc, RwLock};

use crate::conversions::escape_non_ascii;

use super::entries::PyRegmemEntry;
use super::values::{PyRegmemAnnotation, PyRegmemSummary};
use super::SharedRegister;

#[gen_stub_pyclass]
#[pyclass(name = "RegmemCategory")]
#[derive(Clone)]
pub struct PyRegmemCategory {
    shared: SharedRegister,
    person: usize,
    category: usize,
}

impl PyRegmemCategory {
    fn owned_category(&self) -> rv::RegmemCategory {
        self.with_category(Clone::clone)
    }

    fn with_category<R>(&self, f: impl FnOnce(&rv::RegmemCategory) -> R) -> R {
        let guard = self.shared.read().unwrap();
        let category = &guard.persons[self.person].categories[self.category];
        f(category)
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyRegmemCategory {
    #[new]
    #[pyo3(signature = (*, category_id="", category_name, category_description=None,
        legislation_or_rule_name=None, legislation_or_rule_url=None, summaries=None,
        entries=None))]
    #[allow(clippy::too_many_arguments)]
    fn py_new(
        category_id: &str,
        category_name: String,
        category_description: Option<String>,
        legislation_or_rule_name: Option<String>,
        legislation_or_rule_url: Option<String>,
        summaries: Option<Vec<PyRef<'_, PyRegmemSummary>>>,
        entries: Option<Vec<PyRef<'_, PyRegmemEntry>>>,
    ) -> Self {
        let category = rv::RegmemCategory {
            category_id: category_id.to_string(),
            category_name,
            category_description,
            legislation_or_rule_name,
            legislation_or_rule_url,
            summaries: summaries
                .unwrap_or_default()
                .iter()
                .map(|summary| summary.inner.clone())
                .collect(),
            entries: entries
                .unwrap_or_default()
                .iter()
                .map(|entry| entry.owned_entry())
                .collect(),
        };
        let register = rv::RegmemRegister {
            chamber: None,
            language: None,
            published_date: None,
            annotations: Vec::new(),
            summaries: Vec::new(),
            persons: vec![rv::RegmemPerson {
                person_id: String::new(),
                person_name: String::new(),
                published_date: NaiveDate::from_ymd_opt(1, 1, 1).unwrap(),
                chamber: rv::Chamber::Commons,
                language: rv::Language::En,
                categories: vec![category],
            }],
        };
        Self {
            shared: Arc::new(RwLock::new(register)),
            person: 0,
            category: 0,
        }
    }

    #[getter]
    fn category_id(&self) -> String {
        self.with_category(|c| c.category_id.clone())
    }

    #[getter]
    fn category_name(&self) -> String {
        self.with_category(|c| c.category_name.clone())
    }

    #[getter]
    fn category_description(&self) -> Option<String> {
        self.with_category(|c| c.category_description.clone())
    }

    #[getter]
    fn legislation_or_rule_name(&self) -> Option<String> {
        self.with_category(|c| c.legislation_or_rule_name.clone())
    }

    #[getter]
    fn legislation_or_rule_url(&self) -> Option<String> {
        self.with_category(|c| c.legislation_or_rule_url.clone())
    }

    #[getter]
    fn entries(&self, py: Python<'_>) -> PyResult<Vec<Py<PyRegmemEntry>>> {
        let count = self.with_category(|c| c.entries.len());
        (0..count)
            .map(|ei| {
                let loc = rv::EntryLocation {
                    person: self.person,
                    category: self.category,
                    entry: ei,
                    sub_entry: None,
                };
                Py::new(py, PyRegmemEntry::handle(self.shared.clone(), loc))
            })
            .collect()
    }

    fn entries_and_subentries(&self, py: Python<'_>) -> PyResult<Vec<Py<PyRegmemEntry>>> {
        let entries = self.entries(py)?;
        let mut output = Vec::new();
        for entry in entries {
            let sub_entries = entry.borrow(py).sub_entries(py)?;
            output.push(entry);
            output.extend(sub_entries);
        }
        Ok(output)
    }

    #[getter]
    fn summaries(&self) -> Vec<PyRegmemSummary> {
        self.with_category(|c| {
            c.summaries
                .iter()
                .map(|s| PyRegmemSummary { inner: s.clone() })
                .collect()
        })
    }

    fn __repr__(&self) -> String {
        format!(
            "<RegmemCategory {}>",
            self.with_category(|c| c.category_id.clone())
        )
    }
}

#[gen_stub_pyclass]
#[pyclass(name = "RegmemPerson")]
#[derive(Clone)]
pub struct PyRegmemPerson {
    shared: SharedRegister,
    person: usize,
}

impl PyRegmemPerson {
    fn owned_person(&self) -> rv::RegmemPerson {
        self.with_person(Clone::clone)
    }

    fn with_person<R>(&self, f: impl FnOnce(&rv::RegmemPerson) -> R) -> R {
        let guard = self.shared.read().unwrap();
        f(&guard.persons[self.person])
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyRegmemPerson {
    #[new]
    #[pyo3(signature = (*, person_id, person_name, published_date, chamber, language="en",
        categories=None))]
    fn py_new(
        person_id: String,
        person_name: String,
        published_date: &Bound<'_, PyAny>,
        chamber: &Bound<'_, PyAny>,
        language: &str,
        categories: Option<Vec<PyRef<'_, PyRegmemCategory>>>,
    ) -> PyResult<Self> {
        let published_date = published_date.extract::<NaiveDate>().or_else(|_| {
            NaiveDate::parse_from_str(published_date.extract::<String>()?.as_str(), "%Y-%m-%d")
                .map_err(|error| PyValueError::new_err(error.to_string()))
        })?;
        let chamber_value = chamber.str()?.to_str()?.to_string();
        let chamber = rv::Chamber::from_str_opt(&chamber_value)
            .ok_or_else(|| PyValueError::new_err(format!("Unknown chamber: {chamber_value}")))?;
        let person = rv::RegmemPerson {
            person_id,
            person_name,
            published_date,
            chamber,
            language: if language == "cy" {
                rv::Language::Cy
            } else {
                rv::Language::En
            },
            categories: categories
                .unwrap_or_default()
                .iter()
                .map(|category| category.owned_category())
                .collect(),
        };
        Ok(Self {
            shared: Arc::new(RwLock::new(rv::RegmemRegister {
                chamber: None,
                language: None,
                published_date: None,
                annotations: Vec::new(),
                summaries: Vec::new(),
                persons: vec![person],
            })),
            person: 0,
        })
    }

    #[getter]
    fn person_id(&self) -> String {
        self.with_person(|p| p.person_id.clone())
    }

    #[getter]
    fn person_name(&self) -> String {
        self.with_person(|p| p.person_name.clone())
    }

    #[getter]
    fn published_date(&self) -> NaiveDate {
        self.with_person(|p| p.published_date)
    }

    #[getter]
    fn chamber(&self) -> String {
        self.with_person(|p| p.chamber.as_str().to_string())
    }

    #[getter]
    fn language(&self) -> String {
        self.with_person(|p| p.language.as_str().to_string())
    }

    #[getter]
    fn categories(&self, py: Python<'_>) -> PyResult<Vec<Py<PyRegmemCategory>>> {
        let count = self.with_person(|p| p.categories.len());
        (0..count)
            .map(|ci| {
                Py::new(
                    py,
                    PyRegmemCategory {
                        shared: self.shared.clone(),
                        person: self.person,
                        category: ci,
                    },
                )
            })
            .collect()
    }

    fn get_category_from_id(
        &self,
        py: Python<'_>,
        category_id: &str,
    ) -> PyResult<Py<PyRegmemCategory>> {
        let idx = self.with_person(|p| p.get_category_index_by_id(category_id));
        match idx {
            Some(ci) => Py::new(
                py,
                PyRegmemCategory {
                    shared: self.shared.clone(),
                    person: self.person,
                    category: ci,
                },
            ),
            None => Err(PyValueError::new_err(format!(
                "Category {category_id} not found in register"
            ))),
        }
    }

    fn __repr__(&self) -> String {
        format!(
            "<RegmemPerson {}>",
            self.with_person(|p| p.person_id.clone())
        )
    }
}

#[gen_stub_pyclass]
#[pyclass(name = "RegmemRegister", subclass)]
pub struct PyRegmemRegister {
    shared: SharedRegister,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyRegmemRegister {
    #[new]
    #[pyo3(signature = (*, chamber=None, language="en", published_date=None,
        annotations=None, summaries=None, persons=None))]
    fn py_new(
        chamber: Option<&Bound<'_, PyAny>>,
        language: &str,
        published_date: Option<&Bound<'_, PyAny>>,
        annotations: Option<Vec<PyRef<'_, PyRegmemAnnotation>>>,
        summaries: Option<Vec<PyRef<'_, PyRegmemSummary>>>,
        persons: Option<Vec<PyRef<'_, PyRegmemPerson>>>,
    ) -> PyResult<Self> {
        let chamber = chamber
            .map(|value| {
                let value = value.str()?.to_str()?.to_string();
                rv::Chamber::from_str_opt(&value)
                    .ok_or_else(|| PyValueError::new_err(format!("Unknown chamber: {value}")))
            })
            .transpose()?;
        let published_date = published_date
            .map(|value| {
                value.extract::<NaiveDate>().or_else(|_| {
                    NaiveDate::parse_from_str(value.extract::<String>()?.as_str(), "%Y-%m-%d")
                        .map_err(|error| PyValueError::new_err(error.to_string()))
                })
            })
            .transpose()?;
        let register = rv::RegmemRegister {
            chamber,
            language: Some(if language == "cy" {
                rv::Language::Cy
            } else {
                rv::Language::En
            }),
            published_date,
            annotations: annotations
                .unwrap_or_default()
                .iter()
                .map(|annotation| annotation.inner.clone())
                .collect(),
            summaries: summaries
                .unwrap_or_default()
                .iter()
                .map(|summary| summary.inner.clone())
                .collect(),
            persons: persons
                .unwrap_or_default()
                .iter()
                .map(|person| person.owned_person())
                .collect(),
        };
        register
            .either_global_or_person_chambers()
            .map_err(PyValueError::new_err)?;
        Ok(Self {
            shared: Arc::new(RwLock::new(register)),
        })
    }

    #[classmethod]
    #[pyo3(name = "model_validate_json", signature = (json_str, validate = true))]
    fn py_model_validate_json(
        _cls: &Bound<'_, PyType>,
        json_str: &str,
        validate: bool,
    ) -> PyResult<Self> {
        match rv::RegmemRegister::model_validate_json(json_str, validate) {
            Ok(register) => Ok(PyRegmemRegister {
                shared: Arc::new(RwLock::new(register)),
            }),
            Err(e) => Err(PyValueError::new_err(e)),
        }
    }

    fn model_dump_json(&self) -> String {
        let guard = self.shared.read().unwrap();
        escape_non_ascii(guard.model_dump_json())
    }

    fn __eq__(&self, other: PyRef<'_, Self>) -> bool {
        *self.shared.read().unwrap() == *other.shared.read().unwrap()
    }

    #[getter]
    fn chamber(&self) -> Option<String> {
        self.shared
            .read()
            .unwrap()
            .chamber
            .map(|c| c.as_str().to_string())
    }

    #[getter]
    fn language(&self) -> Option<String> {
        self.shared
            .read()
            .unwrap()
            .language
            .map(|l| l.as_str().to_string())
    }

    #[getter]
    fn published_date(&self) -> Option<NaiveDate> {
        self.shared.read().unwrap().published_date
    }

    #[getter]
    fn annotations(&self) -> Vec<PyRegmemAnnotation> {
        self.shared
            .read()
            .unwrap()
            .annotations
            .iter()
            .map(|a| PyRegmemAnnotation { inner: a.clone() })
            .collect()
    }

    #[getter]
    fn summaries(&self) -> Vec<PyRegmemSummary> {
        self.shared
            .read()
            .unwrap()
            .summaries
            .iter()
            .map(|s| PyRegmemSummary { inner: s.clone() })
            .collect()
    }

    #[getter]
    fn persons(&self, py: Python<'_>) -> PyResult<Vec<Py<PyRegmemPerson>>> {
        let count = self.shared.read().unwrap().persons.len();
        (0..count)
            .map(|person| {
                Py::new(
                    py,
                    PyRegmemPerson {
                        shared: self.shared.clone(),
                        person,
                    },
                )
            })
            .collect()
    }

    fn get_person_from_id(&self, py: Python<'_>, person_id: &str) -> PyResult<Py<PyRegmemPerson>> {
        let idx = self
            .shared
            .read()
            .unwrap()
            .get_person_index_by_id(person_id);
        match idx {
            Some(person) => Py::new(
                py,
                PyRegmemPerson {
                    shared: self.shared.clone(),
                    person,
                },
            ),
            None => Err(PyValueError::new_err(format!(
                "Person {person_id} not found in register"
            ))),
        }
    }

    /// Iterate (person, category, entry, parent_entry) for every entry and
    /// sub-entry, mirroring upstream `iter_entries`.
    fn iter_entries(&self, py: Python<'_>) -> PyResult<Vec<PyObject>> {
        let locs = self.shared.read().unwrap().iter_entry_locations();
        let mut out: Vec<PyObject> = Vec::with_capacity(locs.len());
        let mut parents: std::collections::HashMap<(usize, usize, usize), PyObject> =
            std::collections::HashMap::new();
        for loc in locs {
            let person = Py::new(
                py,
                PyRegmemPerson {
                    shared: self.shared.clone(),
                    person: loc.person,
                },
            )?;
            let category = Py::new(
                py,
                PyRegmemCategory {
                    shared: self.shared.clone(),
                    person: loc.person,
                    category: loc.category,
                },
            )?;
            let entry = Py::new(py, PyRegmemEntry::handle(self.shared.clone(), loc))?;
            let key = (loc.person, loc.category, loc.entry);
            let entry_object = entry.into_py(py);
            let parent = loc
                .sub_entry
                .and_then(|_| parents.get(&key).map(|parent| parent.clone_ref(py)))
                .unwrap_or_else(|| py.None());
            if loc.sub_entry.is_none() {
                parents.insert(key, entry_object.clone_ref(py));
            }
            let tuple = pyo3::types::PyTuple::new_bound(
                py,
                &[
                    person.into_py(py),
                    category.into_py(py),
                    entry_object,
                    parent,
                ],
            );
            out.push(tuple.into_py(py));
        }
        Ok(out)
    }
}
