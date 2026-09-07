//! Shared JSON/Python conversion used by Popolo and interests wrappers.
//!
//! `Bound` borrows an object within a Python interpreter-access lifetime, while
//! `PyObject` owns a Python reference. JSON round-trips create detached containers.
//! These helpers invoke Python functions, which callers can replace; call them
//! after releasing dataset lock guards to allow re-entrant model access.
//! PyO3 also checks borrows of each wrapper: a callback cannot mutably borrow the
//! same wrapper while an `&self` method is active, even after the dataset guard
//! is dropped. A separate handle to the same record has its own wrapper borrow.

use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;

pub(crate) fn escape_non_ascii(s: String) -> String {
    let mut result = String::with_capacity(s.len() + 64);
    for c in s.chars() {
        if (c as u32) > 127 {
            result.push_str(&format!("\\u{:04x}", c as u32));
        } else {
            result.push(c);
        }
    }
    result
}

pub(crate) fn json_value_to_py(py: Python<'_>, value: &serde_json::Value) -> PyResult<PyObject> {
    let json = PyModule::import_bound(py, "json")?;
    let encoded =
        serde_json::to_string(value).map_err(|error| PyValueError::new_err(error.to_string()))?;
    Ok(json.call_method1("loads", (encoded,))?.into_py(py))
}

pub(crate) fn py_to_json_value(value: &Bound<'_, PyAny>) -> PyResult<serde_json::Value> {
    let json = PyModule::import_bound(value.py(), "json")?;
    let serializable = if value.hasattr("model_dump")? {
        value.call_method0("model_dump")?
    } else {
        value.clone()
    };
    let encoded: String = json.call_method1("dumps", (serializable,))?.extract()?;
    serde_json::from_str(&encoded).map_err(|error| PyValueError::new_err(error.to_string()))
}
