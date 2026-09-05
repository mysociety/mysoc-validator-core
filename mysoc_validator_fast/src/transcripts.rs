use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::{PyDict, PyType};
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::fs;
use transcript_validator::{validate_gid, Transcript, TranscriptError, TranscriptItem};

fn validation_error(errors: Vec<TranscriptError>) -> PyErr {
    PyValueError::new_err(
        errors
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("\n"),
    )
}

macro_rules! transcript_item_class {
    ($rust_name:ident, $python_name:literal) => {
        #[pyclass(name = $python_name)]
        #[derive(Clone)]
        pub struct $rust_name {
            item: TranscriptItem,
        }

        #[pymethods]
        impl $rust_name {
            #[new]
            #[pyo3(signature = (id=None, content=None, items=None, **attributes))]
            fn new(
                id: Option<String>,
                content: Option<&Bound<'_, PyAny>>,
                items: Option<Vec<PyObject>>,
                attributes: Option<&Bound<'_, PyDict>>,
            ) -> PyResult<Self> {
                if let Some(id) = &id {
                    if matches!(
                        $python_name,
                        "Speech"
                            | "Question"
                            | "Reply"
                            | "OralHeading"
                            | "MajorHeading"
                            | "MinorHeading"
                    ) && !validate_gid(id)
                    {
                        return Err(PyValueError::new_err(format!("invalid id: {id}")));
                    }
                }
                let mut values = BTreeMap::new();
                if let Some(id) = id {
                    values.insert("id".to_string(), id);
                }
                if let Some(attributes) = attributes {
                    for (key, value) in attributes.iter() {
                        values.insert(key.extract::<String>()?, value.extract::<String>()?);
                    }
                }
                let text = match content {
                    Some(content) if content.is_instance_of::<PyDict>() => content
                        .downcast::<PyDict>()?
                        .get_item("text")?
                        .map(|value| value.extract::<String>())
                        .transpose()?
                        .unwrap_or_default(),
                    Some(content) => content.extract::<String>()?,
                    None => String::new(),
                };
                let mut item = TranscriptItem::new(
                    match $python_name {
                        "Question" => "ques",
                        "OralHeading" => "oral-heading",
                        "MajorHeading" => "major-heading",
                        "MinorHeading" => "minor-heading",
                        name => name,
                    }
                    .to_ascii_lowercase(),
                    values,
                    text,
                );
                if items.is_some_and(|items| !items.is_empty()) {
                    return Err(PyValueError::new_err(
                        "constructed child items are not yet supported",
                    ));
                }
                item.children = Vec::new();
                Ok(Self { item })
            }

            #[getter]
            fn tag(&self) -> String {
                self.item.tag.clone()
            }

            #[getter]
            fn content(&self, py: Python<'_>) -> PyResult<PyObject> {
                let content = PyDict::new_bound(py);
                content.set_item("text", &self.item.content.text)?;
                content.set_item("raw", &self.item.content.raw)?;
                Ok(content.into())
            }

            #[getter]
            fn id(&self) -> Option<String> {
                self.item.id.clone()
            }

            #[getter]
            fn url(&self) -> Option<String> {
                self.item.url.clone()
            }

            #[getter]
            fn speakername(&self) -> Option<String> {
                self.item.speakername.clone()
            }

            #[getter]
            fn person_id(&self) -> Option<String> {
                self.item.person_id.clone()
            }

            #[getter]
            fn member_id(&self) -> Option<String> {
                self.item.member_id.clone()
            }

            #[getter]
            fn spid(&self) -> Option<String> {
                self.item.spid.clone()
            }

            #[getter]
            fn nospeaker(&self) -> Option<String> {
                self.item.nospeaker.clone()
            }

            #[getter(r#type)]
            fn type_(&self) -> Option<String> {
                self.item.attributes.get("type").cloned()
            }

            #[getter]
            fn date(&self) -> Option<String> {
                self.item
                    .attributes
                    .get("divdate")
                    .or_else(|| self.item.attributes.get("date"))
                    .cloned()
            }

            #[getter]
            fn divnumber(&self) -> Option<i64> {
                self.item
                    .attributes
                    .get("divnumber")
                    .and_then(|value| value.parse().ok())
            }

            fn model_dump(&self, py: Python<'_>) -> PyResult<PyObject> {
                let result = PyDict::new_bound(py);
                result.set_item("@tag", &self.item.tag)?;
                for (key, value) in &self.item.attributes {
                    result.set_item(key, value)?;
                }
                result.set_item("content", self.content(py)?)?;
                result.set_item("items", self.items(py)?)?;
                Ok(result.into())
            }

            #[getter]
            fn items(&self, py: Python<'_>) -> PyResult<Vec<PyObject>> {
                self.item
                    .children
                    .iter()
                    .map(|item| item_to_python(py, item))
                    .collect()
            }

            fn __str__(&self) -> String {
                self.item.string_value()
            }

            fn __repr__(&self) -> String {
                format!(
                    "<{}: {}>",
                    $python_name,
                    self.item.id.as_deref().unwrap_or("")
                )
            }
        }
    };
}

transcript_item_class!(PySpeech, "Speech");
transcript_item_class!(PyDivision, "Division");
transcript_item_class!(PyGidRedirect, "GIDRedirect");
transcript_item_class!(PyOralHeading, "OralHeading");
transcript_item_class!(PyMajorHeading, "MajorHeading");
transcript_item_class!(PyMinorHeading, "MinorHeading");
transcript_item_class!(PyAgreement, "Agreement");
transcript_item_class!(PySource, "Source");
transcript_item_class!(PyQuestion, "Question");
transcript_item_class!(PyReply, "Reply");
transcript_item_class!(PySpeechItem, "SpeechItem");

fn item_to_python(py: Python<'_>, item: &TranscriptItem) -> PyResult<PyObject> {
    macro_rules! create_item {
        ($type:ident) => {
            Ok(Py::new(py, $type { item: item.clone() })?.into_py(py))
        };
    }
    match item.tag.as_str() {
        "speech" => create_item!(PySpeech),
        "division" => create_item!(PyDivision),
        "gidredirect" => create_item!(PyGidRedirect),
        "oral-heading" => create_item!(PyOralHeading),
        "major-heading" => create_item!(PyMajorHeading),
        "minor-heading" => create_item!(PyMinorHeading),
        "agreement" => create_item!(PyAgreement),
        "source" => create_item!(PySource),
        "ques" => create_item!(PyQuestion),
        "reply" => create_item!(PyReply),
        _ => create_item!(PySpeechItem),
    }
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
        let path = if let Ok(path) = path.extract::<String>() {
            path
        } else {
            path.call_method0("__fspath__")?.extract::<String>()?
        };
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
    module.add_function(wrap_pyfunction!(validate_transcript_folder, module)?)?;
    Ok(())
}

#[pyfunction]
#[pyo3(signature = (folder, people_path=None))]
fn validate_transcript_folder(
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

fn filesystem_path(path: &Bound<'_, PyAny>) -> PyResult<String> {
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
