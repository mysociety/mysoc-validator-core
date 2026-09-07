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
use pyo3::types::{PyBool, PyDict, PyList, PyType};
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
    if let Ok(items) = obj.downcast::<PyList>() {
        let mut groups = Vec::with_capacity(items.len());
        for item in items {
            let group = item.extract::<PyRef<PyRegmemDetailGroup>>()?;
            groups.push(group.with_group(Clone::clone));
        }
        return Ok(rv::DetailValue::Container(groups));
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

    fn dict_value(&self, py: Python<'_>) -> PyResult<Option<PyObject>> {
        let Some(value) = &self.inner.value else {
            return Ok(None);
        };
        if let rv::DetailValue::Container(groups) = value {
            let rows = PyList::empty_bound(py);
            for group in groups {
                let row = PyDict::new_bound(py);
                for detail in &group.0 {
                    if let Some(slug) = &detail.slug {
                        row.set_item(
                            slug,
                            match &detail.value {
                                Some(value) => detail_value_to_py(py, value)?,
                                None => py.None(),
                            },
                        )?;
                    }
                }
                rows.append(row)?;
            }
            Ok(Some(rows.into()))
        } else {
            Ok(Some(detail_value_to_py(py, value)?))
        }
    }

    fn iter_sub_details(&self) -> Vec<PyRegmemDetail> {
        self.inner
            .sub_detail_groups()
            .iter()
            .flat_map(|group| group.0.iter())
            .cloned()
            .map(|inner| PyRegmemDetail { inner })
            .collect()
    }

    #[classmethod]
    fn parameterized_class_from_str<'py>(
        cls: &Bound<'py, PyType>,
        _type_str: &str,
    ) -> Bound<'py, PyType> {
        cls.clone()
    }

    #[classmethod]
    fn __class_getitem__<'py>(
        cls: &Bound<'py, PyType>,
        _item: &Bound<'_, PyAny>,
    ) -> Bound<'py, PyType> {
        cls.clone()
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
    #[pyo3(signature = (*, root=None))]
    fn py_new(root: Option<Vec<PyRef<'_, PyRegmemDetail>>>) -> Self {
        let details = root
            .unwrap_or_default()
            .iter()
            .map(|detail| detail.inner.clone())
            .collect();
        PyRegmemDetailGroup::standalone(rv::RegmemDetailGroup(details))
    }

    #[staticmethod]
    fn model_validate(value: &Bound<'_, PyAny>) -> PyResult<Self> {
        let value = crate::py_to_json_value(value)?;
        let group: rv::RegmemDetailGroup = serde_json::from_value(value)
            .map_err(|error| PyValueError::new_err(error.to_string()))?;
        Ok(PyRegmemDetailGroup::standalone(group))
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

    fn __iter__(slf: PyRef<'_, Self>) -> PyResult<PyObject> {
        let py = slf.py();
        Ok(slf.root().into_py(py).call_method0(py, "__iter__")?)
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
    fn owned_entry(&self) -> rv::RegmemEntry {
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
            let entry = Py::new(
                py,
                PyRegmemEntry {
                    shared: self.shared.clone(),
                    loc,
                },
            )?;
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
