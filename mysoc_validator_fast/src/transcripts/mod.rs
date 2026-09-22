//! Top-level transcript wrapper and Python class registration.

mod files;
mod items;
use files::{filesystem_path, validate_transcript_folder};
use items::{
    item_to_python, PyAgreement, PyDivision, PyDivisionCount, PyGidRedirect, PyMajorHeading,
    PyMinorHeading, PyMotion, PyMspName, PyOralHeading, PyQuestion, PyRepList, PyRepName, PyReply,
    PySource, PySpeech, PySpeechItem,
};
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::PyType;
use transcript_validator::{Transcript, TranscriptError};

fn validation_error(errors: Vec<TranscriptError>) -> PyErr {
    PyValueError::new_err(
        errors
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("\n"),
    )
}

#[pyclass(name = "Transcript")]
pub struct PyTranscript {
    transcript: Transcript,
}

#[pymethods]
impl PyTranscript {
    #[staticmethod]
    fn model_validate_xml(xml: &str) -> PyResult<Self> {
        Ok(Self {
            transcript: Transcript::model_validate_xml(xml).map_err(validation_error)?,
        })
    }

    #[staticmethod]
    fn from_xml_path(path: &Bound<'_, PyAny>) -> PyResult<Self> {
        let path = filesystem_path(path)?;
        Ok(Self {
            transcript: Transcript::from_xml_path(path).map_err(validation_error)?,
        })
    }

    fn model_dump_xml(&self) -> String {
        self.transcript.model_dump_xml().to_string()
    }

    #[getter]
    fn scraper_version(&self) -> Option<String> {
        self.transcript.scraper_version.clone()
    }

    #[getter]
    fn latest(&self) -> Option<String> {
        self.transcript.latest.clone()
    }

    #[getter]
    fn items(&self, py: Python<'_>) -> PyResult<Vec<PyObject>> {
        self.transcript
            .items
            .iter()
            .map(|item| item_to_python(py, item))
            .collect()
    }

    fn iter_speeches(&self, py: Python<'_>) -> PyResult<Vec<PyObject>> {
        self.transcript
            .items
            .iter()
            .filter(|item| item.tag == "speech")
            .map(|item| item_to_python(py, item))
            .collect()
    }

    fn iter_type(&self, py: Python<'_>, item_type: &Bound<'_, PyType>) -> PyResult<Vec<PyObject>> {
        let mut matching = Vec::new();
        for item in &self.transcript.items {
            let object = item_to_python(py, item)?;
            if object.bind(py).is_instance(item_type)? {
                matching.push(object);
            }
        }
        Ok(matching)
    }

    fn iter_has_text(&self, py: Python<'_>) -> PyResult<Vec<PyObject>> {
        self.transcript
            .items
            .iter()
            .filter(|item| !item.string_value().is_empty())
            .map(|item| item_to_python(py, item))
            .collect()
    }

    fn iter_headed_speeches(&self, py: Python<'_>) -> PyResult<Vec<PyObject>> {
        let mut major: Option<PyObject> = None;
        let mut minor: Option<PyObject> = None;
        let mut speech_index: i64 = -1;
        let mut results = Vec::new();
        for item in &self.transcript.items {
            match item.tag.as_str() {
                "major-heading" => {
                    major = Some(item_to_python(py, item)?);
                    minor = None;
                }
                "minor-heading" => {
                    minor = Some(item_to_python(py, item)?);
                    speech_index = -1;
                }
                "speech" => {
                    speech_index += 1;
                    results.push(
                        (
                            major.as_ref().map(|value| value.clone_ref(py)),
                            minor.as_ref().map(|value| value.clone_ref(py)),
                            item_to_python(py, item)?,
                            speech_index,
                        )
                            .into_py(py),
                    );
                }
                _ => {}
            }
        }
        Ok(results)
    }
}

pub fn register_transcript_classes(module: &Bound<'_, PyModule>) -> PyResult<()> {
    module.add_class::<PyTranscript>()?;
    module.add_class::<PySpeech>()?;
    module.add_class::<PyDivision>()?;
    module.add_class::<PyGidRedirect>()?;
    module.add_class::<PyOralHeading>()?;
    module.add_class::<PyMajorHeading>()?;
    module.add_class::<PyMinorHeading>()?;
    module.add_class::<PyAgreement>()?;
    module.add_class::<PySource>()?;
    module.add_class::<PyQuestion>()?;
    module.add_class::<PyReply>()?;
    module.add_class::<PySpeechItem>()?;
    module.add_class::<PyDivisionCount>()?;
    module.add_class::<PyMspName>()?;
    module.add_class::<PyRepName>()?;
    module.add_class::<PyRepList>()?;
    module.add_class::<PyMotion>()?;
    module.add_function(wrap_pyfunction!(validate_transcript_folder, module)?)?;
    Ok(())
}
