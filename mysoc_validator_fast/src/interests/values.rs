//! Owned detail/annotation/summary wrappers and standalone or live detail groups.
//!
//! Adding an owned detail or annotation copies it into the register; subsequent
//! changes to that wrapper do not write through. `GroupState` explicitly chooses
//! owned storage or an entry location, allowing the same Python API on both.
//! Detail-value conversion is kept here because Python bool/int extraction order
//! and date/Decimal conversion affect which domain enum variant is selected.

use chrono::NaiveDate;
use pyo3::exceptions::{PyKeyError, PyValueError};
use pyo3::prelude::*;
use pyo3::types::{PyBool, PyDict, PyList, PyType};
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};
use regmem_validator as rv;
use rust_decimal::Decimal;
use std::str::FromStr;

use super::SharedRegister;

pub(super) fn content_type_from_str(s: &str) -> PyResult<rv::ContentType> {
    s.parse()
        .map_err(|_| PyValueError::new_err(format!("Unknown content format: {s}")))
}

/// Convert a `DetailValue` to a native Python object.
pub(super) fn detail_value_to_py(py: Python<'_>, value: &rv::DetailValue) -> PyResult<PyObject> {
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
pub(super) fn py_to_detail_value(obj: &Bound<'_, PyAny>) -> PyResult<rv::DetailValue> {
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
    ) -> PyResult<Self> {
        Ok(Self {
            inner: rv::RegmemAnnotation::new(
                author,
                content,
                r#type,
                date_added,
                content_format
                    .as_deref()
                    .map(content_type_from_str)
                    .transpose()?,
            ),
        })
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
            detail.common_key = Some(
                ck.parse::<rv::CommonKey>()
                    .map_err(|_| PyValueError::new_err(format!("Unknown common key: {ck}")))?,
            );
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

    pub(super) fn handle(shared: SharedRegister, loc: rv::EntryLocation) -> Self {
        PyRegmemDetailGroup {
            inner: GroupState::Handle { shared, loc },
        }
    }

    pub(super) fn with_group<R>(&self, f: impl FnOnce(&rv::RegmemDetailGroup) -> R) -> R {
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
        Self::standalone(rv::RegmemDetailGroup(details))
    }

    #[staticmethod]
    fn model_validate(value: &Bound<'_, PyAny>) -> PyResult<Self> {
        let value = crate::conversions::py_to_json_value(value)?;
        let group: rv::RegmemDetailGroup = serde_json::from_value(value)
            .map_err(|error| PyValueError::new_err(error.to_string()))?;
        Ok(Self::standalone(group))
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
