//! Filesystem adapters and Python result conversion for folder validation.

use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::PyDict;
use std::collections::HashSet;
use std::fs;

#[pyfunction]
#[pyo3(signature = (folder, people_path=None))]
pub(super) fn validate_transcript_folder(
    py: Python<'_>,
    folder: &Bound<'_, PyAny>,
    people_path: Option<&Bound<'_, PyAny>>,
) -> PyResult<Vec<PyObject>> {
    let folder = filesystem_path(folder)?;
    let person_ids = people_path
        .map(|path| load_person_ids(&filesystem_path(path)?))
        .transpose()?;
    let results = transcript_validator::validate_folder(&folder, person_ids.as_ref())
        .map_err(|error| PyValueError::new_err(format!("{}: {error}", folder)))?;
    results
        .into_iter()
        .map(|result| {
            let value = PyDict::new_bound(py);
            value.set_item("path", result.path.to_string_lossy().as_ref())?;
            value.set_item(
                "errors",
                result
                    .errors
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>(),
            )?;
            Ok(value.into())
        })
        .collect()
}

pub(super) fn filesystem_path(path: &Bound<'_, PyAny>) -> PyResult<String> {
    if let Ok(path) = path.extract::<String>() {
        Ok(path)
    } else {
        path.call_method0("__fspath__")?.extract::<String>()
    }
}

fn load_person_ids(path: &str) -> PyResult<HashSet<String>> {
    let json = fs::read_to_string(path)
        .map_err(|error| PyValueError::new_err(format!("{path}: {error}")))?;
    let popolo =
        popolo_validator::Popolo::model_validate_json(&json, &false).map_err(|errors| {
            PyValueError::new_err(
                errors
                    .iter()
                    .map(|error| error.error.as_str())
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
        })?;
    Ok(popolo
        .persons
        .iter()
        .map(|person| match person {
            popolo_validator::PersonType::Person(person) => person.id.clone(),
            popolo_validator::PersonType::PersonRedirect(redirect) => redirect.id.clone(),
        })
        .collect())
}
