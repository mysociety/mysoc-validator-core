//! Python proxy forwarding extra-data operations to its owning model.
//!
//! `owner` retains the Python wrapper; it is not a copy of the underlying JSON.
//! Assigning `extra[key]` calls the owner's setter and updates live data. Values
//! returned by `get_extra` or `localised_values` are detached Python containers:
//! editing a nested returned dict requires assigning it back to persist changes.

use crate::conversions::json_value_to_py;
use pyo3::exceptions::PyKeyError;
use pyo3::prelude::*;
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

#[gen_stub_pyclass]
#[pyclass(name = "Extra")]
pub struct PyExtra {
    owner: PyObject,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyExtra {
    fn __getitem__(&self, py: Python<'_>, key: &str) -> PyResult<PyObject> {
        let value = self.owner.call_method1(py, "get_extra", (key,))?;
        if value.is_none(py) {
            Err(PyKeyError::new_err(key.to_string()))
        } else {
            Ok(value)
        }
    }

    fn __setitem__(&self, py: Python<'_>, key: &str, value: PyObject) -> PyResult<()> {
        self.owner.call_method1(py, "set_extra", (key, value))?;
        Ok(())
    }

    #[getter]
    fn localised_values(&self, py: Python<'_>) -> PyResult<PyObject> {
        let value = self
            .owner
            .call_method1(py, "get_extra", ("localised_values",))?;
        if value.is_none(py) {
            json_value_to_py(py, &serde_json::json!({}))
        } else {
            Ok(value)
        }
    }

    fn model_dump(&self, py: Python<'_>) -> PyResult<PyObject> {
        self.owner.call_method0(py, "_extra_dict")
    }

    fn __repr__(&self, py: Python<'_>) -> PyResult<String> {
        Ok(self.model_dump(py)?.bind(py).repr()?.extract()?)
    }
}

impl PyExtra {
    pub(super) fn new(owner: PyObject) -> Self {
        Self { owner }
    }
}
