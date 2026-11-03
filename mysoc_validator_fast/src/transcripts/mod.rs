//! Top-level transcript wrapper and exports for declarative module registration.

mod files;
mod items;
use files::filesystem_path;
pub(crate) use files::validate_transcript_folder;
use items::{item_to_python, PyTranscriptItem};
pub(crate) use items::{
    PyAgreement, PyDivision, PyDivisionCount, PyGidRedirect, PyMajorHeading, PyMinorHeading,
    PyMotion, PyMspName, PyOralHeading, PyQuestion, PyRepList, PyRepName, PyReply, PySource,
    PySpeech, PySpeechItem,
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
    fn items(&self, py: Python<'_>) -> PyResult<Vec<PyTranscriptItem>> {
        self.transcript
            .items
            .iter()
            .map(|item| item_to_python(py, item))
            .collect()
    }

    fn iter_speeches(&self, py: Python<'_>) -> PyResult<Vec<Py<PySpeech>>> {
        self.transcript
            .items
            .iter()
            .filter(|item| item.tag == "speech")
            .map(|item| Py::new(py, PySpeech { item: item.clone() }))
            .collect()
    }

    #[pyo3(signature = (item_type: "type[Any]"))]
    fn iter_type(
        &self,
        py: Python<'_>,
        item_type: &Bound<'_, PyType>,
    ) -> PyResult<Vec<PyTranscriptItem>> {
        let mut matching = Vec::new();
        for item in &self.transcript.items {
            let object = item_to_python(py, item)?;
            if pyo3::IntoPyObjectExt::into_py_any(&object, py)?
                .bind(py)
                .is_instance(item_type)?
            {
                matching.push(object);
            }
        }
        Ok(matching)
    }

    fn iter_has_text(&self, py: Python<'_>) -> PyResult<Vec<PyTranscriptItem>> {
        self.transcript
            .items
            .iter()
            .filter(|item| !item.string_value().is_empty())
            .map(|item| item_to_python(py, item))
            .collect()
    }

    fn iter_headed_speeches(
        &self,
        py: Python<'_>,
    ) -> PyResult<
        Vec<(
            Option<Py<PyMajorHeading>>,
            Option<Py<PyMinorHeading>>,
            Py<PySpeech>,
            i64,
        )>,
    > {
        let mut major: Option<Py<PyMajorHeading>> = None;
        let mut minor: Option<Py<PyMinorHeading>> = None;
        let mut speech_index: i64 = -1;
        let mut results = Vec::new();
        for item in &self.transcript.items {
            match item.tag.as_str() {
                "major-heading" => {
                    major = Some(Py::new(py, PyMajorHeading { item: item.clone() })?);
                    minor = None;
                }
                "minor-heading" => {
                    minor = Some(Py::new(py, PyMinorHeading { item: item.clone() })?);
                    speech_index = -1;
                }
                "speech" => {
                    speech_index += 1;
                    results.push((
                        major.as_ref().map(|value| value.clone_ref(py)),
                        minor.as_ref().map(|value| value.clone_ref(py)),
                        Py::new(py, PySpeech { item: item.clone() })?,
                        speech_index,
                    ));
                }
                _ => {}
            }
        }
        Ok(results)
    }
}
