//! PyO3 bridge for the register-of-interests JSON format (`regmem_validator`).
//!
//! Mirrors `mysoc_validator.models.interests`. All data lives in a shared
//! `Arc<RwLock<RegmemRegister>>`. Register / Person / Category / Entry are thin
//! *handles* that navigate into the tree by index path (mirroring the Popolo
//! handle pattern in `lib.rs`), so enrichment edits (adding details / notes)
//! persist back into the register and survive a `model_dump_json` round-trip.
//!
//! `RegmemDetail` / `RegmemAnnotation` are owned value wrappers: standalone
//! objects you construct and then `append` / `add_details` into an entry.
//! `RegmemSummary` is exposed read-only as a detached value wrapper.

use chrono::NaiveDate;
use pyo3::exceptions::{PyKeyError, PyValueError};
use pyo3::prelude::*;
use pyo3::types::{PyBool, PyDict, PyType};
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};
use regmem_validator as rv;
use rust_decimal::Decimal;
use std::str::FromStr;
use std::sync::{Arc, RwLock};

use crate::escape_non_ascii;

type SharedRegister = Arc<RwLock<rv::RegmemRegister>>;

// ---------------------------------------------------------------------------
// Value conversions
// ---------------------------------------------------------------------------

fn content_type_from_str(s: &str) -> rv::ContentType {
    match s {
        "markdown" => rv::ContentType::Markdown,
        "xml" => rv::ContentType::Xml,
        _ => rv::ContentType::String,
    }
}

/// Convert a `DetailValue` to a native Python object.
fn detail_value_to_py(py: Python<'_>, value: &rv::DetailValue) -> PyResult<PyObject> {
    Ok(match value {
        rv::DetailValue::Int(i) => i.into_py(py),
        rv::DetailValue::Str(s) => s.into_py(py),
        rv::DetailValue::Float(f) => f.into_py(py),
        rv::DetailValue::Bool(b) => b.into_py(py),
        rv::DetailValue::Date(d) => d.into_py(py),
        rv::DetailValue::Decimal(d) => {
            let decimal_cls = py.import_bound("decimal")?.getattr("Decimal")?;
            decimal_cls.call1((d.to_string(),))?.into_py(py)
        }
        rv::DetailValue::Container(groups) => {
            let mut out: Vec<Py<PyRegmemDetailGroup>> = Vec::with_capacity(groups.len());
            for group in groups {
                out.push(Py::new(py, PyRegmemDetailGroup::standalone(group.clone()))?);
            }
            out.into_py(py)
        }
    })
}

/// Convert a native Python object into a `DetailValue` (scalars only).
fn py_to_detail_value(obj: &Bound<'_, PyAny>) -> PyResult<rv::DetailValue> {
    let py = obj.py();
    // bool must be checked before int (Python bool is an int subclass).
    if let Ok(b) = obj.downcast::<PyBool>() {
        return Ok(rv::DetailValue::Bool(b.is_true()));
    }
    let decimal_cls = py.import_bound("decimal")?.getattr("Decimal")?;
    if obj.is_instance(&decimal_cls)? {
        let s: String = obj.str()?.extract()?;
        return Decimal::from_str(&s)
            .map(rv::DetailValue::Decimal)
            .map_err(|e| PyValueError::new_err(format!("invalid decimal: {e}")));
    }
    // datetime.date before str.
    if let Ok(d) = obj.extract::<NaiveDate>() {
        return Ok(rv::DetailValue::Date(d));
    }
    if let Ok(i) = obj.extract::<i64>() {
        return Ok(rv::DetailValue::Int(i));
    }
    if let Ok(f) = obj.extract::<f64>() {
        return Ok(rv::DetailValue::Float(f));
    }
    if let Ok(s) = obj.extract::<String>() {
        return Ok(rv::DetailValue::Str(s));
    }
    Err(PyValueError::new_err(
        "Unsupported detail value type (expected str, int, float, bool, date or Decimal)",
    ))
}

// ---------------------------------------------------------------------------
// RegmemAnnotation — owned value wrapper
// ---------------------------------------------------------------------------

#[gen_stub_pyclass]
#[pyclass(name = "RegmemAnnotation")]
#[derive(Clone)]
pub struct PyRegmemAnnotation {
    pub(crate) inner: rv::RegmemAnnotation,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyRegmemAnnotation {
    #[new]
    #[pyo3(signature = (author, content, *, r#type=None, date_added=None, content_format=None))]
    fn py_new(
        author: String,
        content: String,
        r#type: Option<String>,
        date_added: Option<NaiveDate>,
        content_format: Option<String>,
    ) -> Self {
        PyRegmemAnnotation {
            inner: rv::RegmemAnnotation::new(
                author,
                content,
                r#type,
                date_added,
                content_format.as_deref().map(content_type_from_str),
            ),
        }
    }

    #[getter]
    fn author(&self) -> String {
        self.inner.author.clone()
    }

    #[getter]
    fn content(&self) -> String {
        self.inner.content.clone()
    }

    #[getter(r#type)]
    fn annotation_type(&self) -> String {
        self.inner.annotation_type.clone()
    }

    #[getter]
    fn date_added(&self) -> Option<NaiveDate> {
        self.inner.date_added
    }

    #[getter]
    fn content_format(&self) -> String {
        self.inner.content_format.as_str().to_string()
    }

    fn __repr__(&self) -> String {
        format!("<RegmemAnnotation by {}>", self.inner.author)
    }
}

// ---------------------------------------------------------------------------
// RegmemDetail — owned value wrapper
// ---------------------------------------------------------------------------

#[gen_stub_pyclass]
#[pyclass(name = "RegmemDetail")]
#[derive(Clone)]
pub struct PyRegmemDetail {
    pub(crate) inner: rv::RegmemDetail,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyRegmemDetail {
    #[new]
    #[pyo3(signature = (*, slug=None, display_as=None, value=None, source=None, description=None, common_key=None))]
    fn py_new(
        slug: Option<String>,
        display_as: Option<String>,
        value: Option<&Bound<'_, PyAny>>,
        source: Option<String>,
        description: Option<String>,
        common_key: Option<String>,
    ) -> PyResult<Self> {
        let val = match value {
            Some(o) if !o.is_none() => Some(py_to_detail_value(o)?),
            _ => None,
        };
        let mut detail = rv::RegmemDetail::new(slug, display_as, val);
        if let Some(source) = source {
            detail.source = source;
        }
        detail.description = description;
        if let Some(ck) = common_key {
            detail.common_key = rv::CommonKey::from_str_opt(&ck);
        }
        Ok(PyRegmemDetail { inner: detail })
    }

    #[getter]
    fn source(&self) -> String {
        self.inner.source.clone()
    }

    #[getter]
    fn slug(&self) -> Option<String> {
        self.inner.slug.clone()
    }

    #[getter]
    fn display_as(&self) -> Option<String> {
        self.inner.display_as.clone()
    }

    #[getter]
    fn common_key(&self) -> Option<String> {
        self.inner.common_key.map(|c| c.as_str().to_string())
    }

    #[getter]
    fn description(&self) -> Option<String> {
        self.inner.description.clone()
    }

    #[getter(r#type)]
    fn detail_type(&self) -> Option<String> {
        self.inner.detail_type.clone()
    }

    #[getter]
    fn value(&self, py: Python<'_>) -> PyResult<Option<PyObject>> {
        match &self.inner.value {
            Some(v) => Ok(Some(detail_value_to_py(py, v)?)),
            None => Ok(None),
        }
    }

    #[setter(value)]
    fn set_value(&mut self, value: &Bound<'_, PyAny>) -> PyResult<()> {
        if value.is_none() {
            self.inner.value = None;
        } else {
            let v = py_to_detail_value(value)?;
            self.inner.detail_type = Some(v.type_str().to_string());
            self.inner.value = Some(v);
        }
        Ok(())
    }

    #[getter]
    fn annotations(&self) -> Vec<PyRegmemAnnotation> {
        self.inner
            .annotations
            .iter()
            .map(|a| PyRegmemAnnotation { inner: a.clone() })
            .collect()
    }

    #[getter]
    fn sub_detail_groups(&self, py: Python<'_>) -> PyResult<Vec<Py<PyRegmemDetailGroup>>> {
        self.inner
            .sub_detail_groups()
            .iter()
            .map(|g| Py::new(py, PyRegmemDetailGroup::standalone(g.clone())))
            .collect()
    }

    fn __repr__(&self) -> String {
        format!(
            "<RegmemDetail {}>",
            self.inner.slug.as_deref().unwrap_or("?")
        )
    }
}

// ---------------------------------------------------------------------------
// RegmemDetailGroup — standalone or handle into an entry's `details`
// ---------------------------------------------------------------------------

enum GroupState {
    Standalone(rv::RegmemDetailGroup),
    Handle {
        shared: SharedRegister,
        loc: rv::EntryLocation,
    },
}

#[gen_stub_pyclass]
#[pyclass(name = "RegmemDetailGroup")]
pub struct PyRegmemDetailGroup {
    inner: GroupState,
}

impl PyRegmemDetailGroup {
    pub(crate) fn standalone(group: rv::RegmemDetailGroup) -> Self {
        PyRegmemDetailGroup {
            inner: GroupState::Standalone(group),
        }
    }

    fn handle(shared: SharedRegister, loc: rv::EntryLocation) -> Self {
        PyRegmemDetailGroup {
            inner: GroupState::Handle { shared, loc },
        }
    }

    fn with_group<R>(&self, f: impl FnOnce(&rv::RegmemDetailGroup) -> R) -> R {
        match &self.inner {
            GroupState::Standalone(g) => f(g),
            GroupState::Handle { shared, loc } => {
                let guard = shared.read().unwrap();
                let entry = guard.entry_at(loc).expect("entry not found in register");
                f(&entry.base.details)
            }
        }
    }

    fn with_group_mut<R>(&mut self, f: impl FnOnce(&mut rv::RegmemDetailGroup) -> R) -> R {
        match &mut self.inner {
            GroupState::Standalone(g) => f(g),
            GroupState::Handle { shared, loc } => {
                let mut guard = shared.write().unwrap();
                let entry = guard
                    .entry_at_mut(loc)
                    .expect("entry not found in register");
                f(&mut entry.base.details)
            }
        }
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyRegmemDetailGroup {
    #[new]
    fn py_new() -> Self {
        PyRegmemDetailGroup::standalone(rv::RegmemDetailGroup::new())
    }

    fn __len__(&self) -> usize {
        self.with_group(|g| g.len())
    }

    fn __getitem__(&self, index: isize) -> PyResult<PyRegmemDetail> {
        self.with_group(|g| {
            let len = g.len() as isize;
            let idx = if index < 0 { index + len } else { index };
            if idx < 0 || idx >= len {
                Err(PyKeyError::new_err("detail index out of range"))
            } else {
                Ok(PyRegmemDetail {
                    inner: g.0[idx as usize].clone(),
                })
            }
        })
    }

    #[getter]
    fn root(&self) -> Vec<PyRegmemDetail> {
        self.with_group(|g| {
            g.0.iter()
                .map(|d| PyRegmemDetail { inner: d.clone() })
                .collect()
        })
    }

    #[pyo3(signature = (item, *, source=None))]
    fn append(&mut self, item: PyRef<'_, PyRegmemDetail>, source: Option<String>) -> PyResult<()> {
        let detail = item.inner.clone();
        self.with_group_mut(|g| g.append(detail, source.as_deref()))
            .map_err(PyValueError::new_err)
    }

    #[pyo3(signature = (items, *, source=None))]
    fn extend(
        &mut self,
        items: Vec<PyRef<'_, PyRegmemDetail>>,
        source: Option<String>,
    ) -> PyResult<()> {
        let details: Vec<rv::RegmemDetail> = items.iter().map(|d| d.inner.clone()).collect();
        self.with_group_mut(|g| g.extend(details, source.as_deref()))
            .map_err(PyValueError::new_err)
    }

    fn detail_dict(&self, py: Python<'_>) -> PyResult<Py<PyDict>> {
        let dict = PyDict::new_bound(py);
        // clone out the details first to avoid holding the lock during conversion
        let details = self.with_group(|g| g.0.clone());
        for detail in &details {
            if let Some(slug) = &detail.slug {
                let value = match &detail.value {
                    Some(v) => detail_value_to_py(py, v)?,
                    None => py.None(),
                };
                dict.set_item(slug, value)?;
            }
        }
        Ok(dict.into())
    }

    fn __repr__(&self) -> String {
        format!("<RegmemDetailGroup len={}>", self.with_group(|g| g.len()))
    }
}

// ---------------------------------------------------------------------------
// RegmemSummary — detached read-only value wrapper
// ---------------------------------------------------------------------------

#[gen_stub_pyclass]
#[pyclass(name = "RegmemSummary")]
#[derive(Clone)]
pub struct PyRegmemSummary {
    pub(crate) inner: rv::RegmemSummary,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyRegmemSummary {
    #[getter]
    fn info_type(&self) -> String {
        "summary".to_string()
    }

    #[getter]
    fn id(&self) -> Option<String> {
        self.inner.base.id.clone()
    }

    #[getter]
    fn content(&self) -> String {
        self.inner.base.content.clone()
    }

    #[getter]
    fn content_format(&self) -> String {
        self.inner.base.content_format.as_str().to_string()
    }

    #[getter]
    fn null_entry(&self) -> bool {
        self.inner.base.null_entry
    }

    #[getter]
    fn comparable_id(&self) -> String {
        self.inner.base.comparable_id()
    }

    #[getter]
    fn item_hash(&self) -> String {
        self.inner.base.item_hash()
    }

    #[getter]
    fn details(&self, py: Python<'_>) -> PyResult<Py<PyRegmemDetailGroup>> {
        Py::new(
            py,
            PyRegmemDetailGroup::standalone(self.inner.base.details.clone()),
        )
    }

    fn get_detail(&self, name: &str) -> Option<PyRegmemDetail> {
        self.inner
            .base
            .get_detail(name)
            .map(|d| PyRegmemDetail { inner: d.clone() })
    }

    fn __repr__(&self) -> String {
        format!("<RegmemSummary {}>", self.inner.base.comparable_id())
    }
}

// ---------------------------------------------------------------------------
// RegmemEntry — handle into the register tree
// ---------------------------------------------------------------------------

#[gen_stub_pyclass]
#[pyclass(name = "RegmemEntry")]
#[derive(Clone)]
pub struct PyRegmemEntry {
    shared: SharedRegister,
    loc: rv::EntryLocation,
}

impl PyRegmemEntry {
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
    fn sub_entries(&self, py: Python<'_>) -> PyResult<Vec<Py<PyRegmemEntry>>> {
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

// ---------------------------------------------------------------------------
// RegmemCategory — handle
// ---------------------------------------------------------------------------

#[gen_stub_pyclass]
#[pyclass(name = "RegmemCategory")]
#[derive(Clone)]
pub struct PyRegmemCategory {
    shared: SharedRegister,
    person: usize,
    category: usize,
}

impl PyRegmemCategory {
    fn with_category<R>(&self, f: impl FnOnce(&rv::RegmemCategory) -> R) -> R {
        let guard = self.shared.read().unwrap();
        let category = &guard.persons[self.person].categories[self.category];
        f(category)
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyRegmemCategory {
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

// ---------------------------------------------------------------------------
// RegmemPerson — handle
// ---------------------------------------------------------------------------

#[gen_stub_pyclass]
#[pyclass(name = "RegmemPerson")]
#[derive(Clone)]
pub struct PyRegmemPerson {
    shared: SharedRegister,
    person: usize,
}

impl PyRegmemPerson {
    fn with_person<R>(&self, f: impl FnOnce(&rv::RegmemPerson) -> R) -> R {
        let guard = self.shared.read().unwrap();
        f(&guard.persons[self.person])
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyRegmemPerson {
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

// ---------------------------------------------------------------------------
// RegmemRegister — top-level wrapper
// ---------------------------------------------------------------------------

#[gen_stub_pyclass]
#[pyclass(name = "RegmemRegister", subclass)]
pub struct PyRegmemRegister {
    shared: SharedRegister,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyRegmemRegister {
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
            let entry = Py::new(
                py,
                PyRegmemEntry {
                    shared: self.shared.clone(),
                    loc,
                },
            )?;
            let parent: PyObject = match loc.sub_entry {
                Some(_) => {
                    let parent_loc = rv::EntryLocation {
                        sub_entry: None,
                        ..loc
                    };
                    Py::new(
                        py,
                        PyRegmemEntry {
                            shared: self.shared.clone(),
                            loc: parent_loc,
                        },
                    )?
                    .into_py(py)
                }
                None => py.None(),
            };
            let tuple = pyo3::types::PyTuple::new_bound(
                py,
                &[
                    person.into_py(py),
                    category.into_py(py),
                    entry.into_py(py),
                    parent,
                ],
            );
            out.push(tuple.into_py(py));
        }
        Ok(out)
    }
}
