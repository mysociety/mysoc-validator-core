//! Live register entry handles and enrichment operations.
//!
//! `EntryLocation` resolves a fresh borrow on every access. Closure results must
//! own any data escaping the lock guard; `owned_entry` deliberately clones a
//! snapshot when constructors embed an entry elsewhere. Entry/group handles write
//! through, while returned detail and annotation wrappers are snapshots.

use chrono::NaiveDate;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList};
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};
use regmem_validator as rv;
use std::sync::{Arc, RwLock};

use super::values::{content_type_from_str, detail_value_to_py, py_to_detail_value};
use super::values::{PyRegmemAnnotation, PyRegmemDetail, PyRegmemDetailGroup};
use super::SharedRegister;

#[gen_stub_pyclass]
#[pyclass(name = "RegmemEntry")]
#[derive(Clone)]
pub struct PyRegmemEntry {
    shared: SharedRegister,
    loc: rv::EntryLocation,
}

impl PyRegmemEntry {
    pub(super) fn owned_entry(&self) -> rv::RegmemEntry {
        let guard = self.shared.read().unwrap();
        guard
            .entry_at(&self.loc)
            .expect("entry not found in register")
            .clone()
    }

    fn with_base<R>(&self, f: impl FnOnce(&rv::RegmemInfoBase) -> R) -> R {
        let guard = self.shared.read().unwrap();
        let entry = guard
            .entry_at(&self.loc)
            .expect("entry not found in register");
        f(&entry.base)
    }

    fn with_base_mut<R>(&mut self, f: impl FnOnce(&mut rv::RegmemInfoBase) -> R) -> R {
        let mut guard = self.shared.write().unwrap();
        let entry = guard
            .entry_at_mut(&self.loc)
            .expect("entry not found in register");
        f(&mut entry.base)
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyRegmemEntry {
    #[new]
    #[pyo3(signature = (*, id=None, info_type=None, content="", content_format=None,
        date_registered=None, date_published=None, date_updated=None, date_received=None,
        null_entry=false, annotations=None, details=None, sub_entries=None))]
    #[allow(clippy::too_many_arguments)]
    fn py_new(
        id: Option<String>,
        info_type: Option<String>,
        content: &str,
        content_format: Option<String>,
        date_registered: Option<&Bound<'_, PyAny>>,
        date_published: Option<&Bound<'_, PyAny>>,
        date_updated: Option<&Bound<'_, PyAny>>,
        date_received: Option<&Bound<'_, PyAny>>,
        null_entry: bool,
        annotations: Option<Vec<PyRef<'_, PyRegmemAnnotation>>>,
        details: Option<PyRef<'_, PyRegmemDetailGroup>>,
        sub_entries: Option<Vec<PyRef<'_, PyRegmemEntry>>>,
    ) -> PyResult<Self> {
        let parse_date = |value: Option<&Bound<'_, PyAny>>| -> PyResult<Option<NaiveDate>> {
            value
                .map(|value| {
                    value.extract::<NaiveDate>().or_else(|_| {
                        NaiveDate::parse_from_str(value.extract::<String>()?.as_str(), "%Y-%m-%d")
                            .map_err(|error| PyValueError::new_err(error.to_string()))
                    })
                })
                .transpose()
        };
        let entry = rv::RegmemEntry {
            info_type: if info_type.as_deref() == Some("subentry") {
                rv::EntryType::Subentry
            } else {
                rv::EntryType::Entry
            },
            base: rv::RegmemInfoBase {
                id,
                content: content.to_string(),
                content_format: content_type_from_str(
                    content_format.as_deref().unwrap_or("string"),
                ),
                date_registered: parse_date(date_registered)?,
                date_published: parse_date(date_published)?,
                date_updated: parse_date(date_updated)?,
                date_received: parse_date(date_received)?,
                null_entry,
                annotations: annotations
                    .unwrap_or_default()
                    .iter()
                    .map(|annotation| annotation.inner.clone())
                    .collect(),
                details: details
                    .map(|details| details.with_group(Clone::clone))
                    .unwrap_or_else(rv::RegmemDetailGroup::new),
                sub_entries: sub_entries
                    .unwrap_or_default()
                    .iter()
                    .map(|entry| entry.owned_entry())
                    .collect(),
            },
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
                categories: vec![rv::RegmemCategory {
                    category_id: String::new(),
                    category_name: String::new(),
                    category_description: None,
                    legislation_or_rule_name: None,
                    legislation_or_rule_url: None,
                    summaries: Vec::new(),
                    entries: vec![entry],
                }],
            }],
        };
        Ok(Self {
            shared: Arc::new(RwLock::new(register)),
            loc: rv::EntryLocation {
                person: 0,
                category: 0,
                entry: 0,
                sub_entry: None,
            },
        })
    }

    #[getter]
    fn info_type(&self) -> String {
        let guard = self.shared.read().unwrap();
        let entry = guard.entry_at(&self.loc).expect("entry not found");
        match entry.info_type {
            rv::EntryType::Entry => "entry".to_string(),
            rv::EntryType::Subentry => "subentry".to_string(),
        }
    }

    #[getter]
    fn id(&self) -> Option<String> {
        self.with_base(|b| b.id.clone())
    }

    #[getter]
    fn content(&self) -> String {
        self.with_base(|b| b.content.clone())
    }

    #[setter(content)]
    fn set_content(&mut self, value: String) {
        self.with_base_mut(|b| b.content = value);
    }

    #[getter]
    fn content_format(&self) -> String {
        self.with_base(|b| b.content_format.as_str().to_string())
    }

    #[getter]
    fn null_entry(&self) -> bool {
        self.with_base(|b| b.null_entry)
    }

    #[getter]
    fn date_registered(&self) -> Option<NaiveDate> {
        self.with_base(|b| b.date_registered)
    }

    #[getter]
    fn date_published(&self) -> Option<NaiveDate> {
        self.with_base(|b| b.date_published)
    }

    #[getter]
    fn date_updated(&self) -> Option<NaiveDate> {
        self.with_base(|b| b.date_updated)
    }

    #[getter]
    fn date_received(&self) -> Option<NaiveDate> {
        self.with_base(|b| b.date_received)
    }

    #[getter]
    fn comparable_id(&self) -> String {
        self.with_base(|b| b.comparable_id())
    }

    #[getter]
    fn item_hash(&self) -> String {
        self.with_base(|b| b.item_hash())
    }

    #[getter]
    fn details(&self, py: Python<'_>) -> PyResult<Py<PyRegmemDetailGroup>> {
        Py::new(
            py,
            PyRegmemDetailGroup::handle(self.shared.clone(), self.loc),
        )
    }

    #[getter]
    fn annotations(&self) -> Vec<PyRegmemAnnotation> {
        self.with_base(|b| {
            b.annotations
                .iter()
                .map(|a| PyRegmemAnnotation { inner: a.clone() })
                .collect()
        })
    }

    #[getter]
    pub(super) fn sub_entries(&self, py: Python<'_>) -> PyResult<Vec<Py<PyRegmemEntry>>> {
        // Only top-level entries expose sub-entry handles (one level deep).
        if self.loc.sub_entry.is_some() {
            return Ok(Vec::new());
        }
        let count = self.with_base(|b| b.sub_entries.len());
        (0..count)
            .map(|si| {
                let loc = rv::EntryLocation {
                    sub_entry: Some(si),
                    ..self.loc
                };
                Py::new(
                    py,
                    PyRegmemEntry {
                        shared: self.shared.clone(),
                        loc,
                    },
                )
            })
            .collect()
    }

    fn get_detail(&self, name: &str) -> Option<PyRegmemDetail> {
        self.with_base(|b| {
            b.get_detail(name)
                .map(|d| PyRegmemDetail { inner: d.clone() })
        })
    }

    fn get_detail_value(&self, py: Python<'_>, name: &str) -> PyResult<Option<PyObject>> {
        let value = self.with_base(|b| b.get_detail_value(name).cloned());
        match value {
            Some(v) => Ok(Some(detail_value_to_py(py, &v)?)),
            None => Ok(None),
        }
    }

    #[pyo3(signature = (reduce=None))]
    fn details_dict(
        &self,
        py: Python<'_>,
        reduce: Option<&Bound<'_, PyDict>>,
    ) -> PyResult<Py<PyDict>> {
        let base = self.with_base(Clone::clone);
        let output = PyDict::new_bound(py);
        output.set_item("id", base.comparable_id())?;
        output.set_item("content", base.content)?;
        if let Some(date) = base.date_registered {
            output.set_item("date_registered", date.format("%Y-%m-%d").to_string())?;
        }
        if let Some(date) = base.date_published {
            output.set_item("date_published", date.format("%Y-%m-%d").to_string())?;
        }
        for detail in &base.details.0 {
            if let Some(slug) = &detail.slug {
                output.set_item(
                    slug,
                    match &detail.value {
                        Some(value) => detail_value_to_py(py, value)?,
                        None => py.None(),
                    },
                )?;
            }
        }
        if let Some(reduce) = reduce {
            for (key, slugs) in reduce.iter() {
                let key: String = key.extract()?;
                let Some(value) = output.get_item(&key)? else {
                    continue;
                };
                let Ok(groups) = value.downcast::<PyList>() else {
                    continue;
                };
                for slug in slugs.extract::<Vec<String>>()? {
                    let values = PyList::empty_bound(py);
                    for group in groups {
                        if let Ok(group) = group.extract::<PyRef<PyRegmemDetailGroup>>() {
                            if let Some(detail) = group.with_group(|details| {
                                details
                                    .0
                                    .iter()
                                    .find(|d| d.slug.as_deref() == Some(slug.as_str()))
                                    .cloned()
                            }) {
                                if let Some(value) = detail.value {
                                    values.append(detail_value_to_py(py, &value)?)?;
                                }
                            }
                        }
                    }
                    output.set_item(slug, values)?;
                }
                output.del_item(key)?;
            }
        }
        Ok(output.into())
    }

    /// Add scalar key/value details (mirrors upstream `add_details`).
    #[pyo3(signature = (*, source=None, **values))]
    fn add_details(
        &mut self,
        source: Option<String>,
        values: Option<&Bound<'_, PyDict>>,
    ) -> PyResult<()> {
        let mut parsed: Vec<(String, rv::DetailValue)> = Vec::new();
        if let Some(values) = values {
            for (key, value) in values.iter() {
                let slug: String = key.extract()?;
                parsed.push((slug, py_to_detail_value(&value)?));
            }
        }
        self.with_base_mut(|b| b.add_details(source.as_deref(), parsed))
            .map_err(PyValueError::new_err)
    }

    /// Append an annotation, persisting it back into the register.
    fn add_annotation(&mut self, annotation: PyRef<'_, PyRegmemAnnotation>) {
        let ann = annotation.inner.clone();
        self.with_base_mut(|b| b.annotations.push(ann));
    }

    fn __repr__(&self) -> String {
        format!("<RegmemEntry {}>", self.with_base(|b| b.comparable_id()))
    }
}

impl PyRegmemEntry {
    pub(super) fn handle(shared: SharedRegister, loc: rv::EntryLocation) -> Self {
        Self { shared, loc }
    }
}
