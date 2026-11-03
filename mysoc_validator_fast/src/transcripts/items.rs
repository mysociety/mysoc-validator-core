//! Python item classes own copies of parsed items, not live transcript handles.
//!
//! The macro supplies one repeated Python interface for XML tag-specific classes;
//! invocations name the public classes and `item_to_python` dispatches parsed tags.
//! Editing a returned item does not alter the transcript's stored original XML.
//! `clone_ref(py)` retains a Python reference; `item.clone()` copies Rust data.

use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::PyDict;
use std::collections::BTreeMap;
use transcript_validator::{validate_gid, TranscriptItem};

macro_rules! transcript_item_class {
    ($rust_name:ident, $python_name:literal) => {
        transcript_item_class!($rust_name, $python_name, BTreeMap<String, String>, markup_content);
    };
    ($rust_name:ident, $python_name:literal, $content_type:ty, $content:ident) => {
        #[pyclass(name = $python_name, from_py_object)]
        #[derive(Clone)]
        pub struct $rust_name {
            pub(super) item: TranscriptItem,
        }

        #[pymethods]
        impl $rust_name {
            #[new]
            #[pyo3(signature = (id=None, content=None, items=None, **attributes: "Any"))]
            fn new(
                id: Option<String>,
                content: Option<&Bound<'_, PyAny>>,
                items: Option<Vec<Py<PyAny>>>,
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
                        .cast::<PyDict>()?
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
            fn content(&self) -> $content_type {
                $content(&self.item)
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

            #[getter]
            fn oldgid(&self) -> Option<String> {
                self.item.attributes.get("oldgid").cloned()
            }

            #[getter]
            fn newgid(&self) -> Option<String> {
                self.item.attributes.get("newgid").cloned()
            }

            #[getter]
            fn matchtype(&self) -> Option<String> {
                self.item.attributes.get("matchtype").cloned()
            }

            #[getter]
            fn agreement_id(&self) -> Option<String> {
                self.item.attributes.get("agreement_id").cloned()
            }

            #[getter]
            fn speech_id(&self) -> Option<String> {
                self.item.attributes.get("speech_id").cloned()
            }

            #[getter]
            fn agreementnumber(&self) -> Option<i64> {
                self.item
                    .attributes
                    .get("agreementnumber")
                    .and_then(|value| value.parse().ok())
            }

            #[getter]
            fn ayes(&self) -> Option<i64> {
                self.item
                    .attributes
                    .get("ayes")
                    .or_else(|| self.item.attributes.get("for"))
                    .and_then(|value| value.parse().ok())
            }

            #[getter]
            fn noes(&self) -> Option<i64> {
                self.item
                    .attributes
                    .get("noes")
                    .or_else(|| self.item.attributes.get("against"))
                    .and_then(|value| value.parse().ok())
            }

            #[getter]
            fn neutral(&self) -> Option<i64> {
                self.item
                    .attributes
                    .get("neutral")
                    .or_else(|| self.item.attributes.get("abstentions"))
                    .and_then(|value| value.parse().ok())
            }

            #[getter]
            fn absent(&self) -> Option<i64> {
                self.item
                    .attributes
                    .get("absent")
                    .and_then(|value| value.parse().ok())
            }

            #[getter]
            fn spoiled(&self) -> Option<i64> {
                self.item
                    .attributes
                    .get("spoiledvotes")
                    .and_then(|value| value.parse().ok())
            }

            #[getter]
            fn tellerayes(&self) -> Option<i64> {
                self.item
                    .attributes
                    .get("tellerayes")
                    .and_then(|value| value.parse().ok())
            }

            #[getter]
            fn tellernoes(&self) -> Option<i64> {
                self.item
                    .attributes
                    .get("tellernoes")
                    .and_then(|value| value.parse().ok())
            }

            #[getter]
            fn not_content(&self) -> Option<i64> {
                self.item
                    .attributes
                    .get("not-content")
                    .and_then(|value| value.parse().ok())
            }

            #[getter]
            fn motion_status(&self) -> Option<String> {
                self.item.attributes.get("motion_status").cloned()
            }

            #[getter]
            fn vote(&self) -> Option<String> {
                self.item.attributes.get("vote").cloned()
            }

            #[getter]
            fn teller(&self) -> Option<String> {
                self.item.attributes.get("teller").cloned()
            }

            #[getter]
            fn proxy(&self) -> Option<String> {
                self.item.attributes.get("proxy").cloned()
            }

            #[getter]
            fn name(&self) -> Option<String> {
                self.item.attributes.get("name").cloned().or_else(|| {
                    matches!($python_name, "MSPName" | "RepName")
                        .then(|| self.item.content.text.clone())
                })
            }

            #[getter]
            fn count(&self, py: Python<'_>) -> PyResult<Option<Py<PyDivisionCount>>> {
                self.item
                    .children
                    .iter()
                    .find(|child| child.tag == "divisioncount")
                    .map(|child| Py::new(py, PyDivisionCount { item: child.clone() }))
                    .transpose()
            }

            #[getter]
            fn representatives(&self, py: Python<'_>) -> PyResult<Vec<Py<PyRepList>>> {
                self.item
                    .children
                    .iter()
                    .filter(|child| {
                        matches!(
                            child.tag.as_str(),
                            "replist" | "mplist" | "msplist" | "mslist" | "mlalist" | "lordlist"
                        )
                    })
                    .map(|child| Py::new(py, PyRepList { item: child.clone() }))
                    .collect()
            }

            #[getter]
            fn rel_motions(&self, py: Python<'_>) -> PyResult<Vec<Py<PyMotion>>> {
                self.item
                    .children
                    .iter()
                    .filter(|child| child.tag == "motion")
                    .map(|child| Py::new(py, PyMotion { item: child.clone() }))
                    .collect()
            }

            #[pyo3(signature = () -> "dict[str, Any]")]
            fn model_dump(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
                let result = PyDict::new(py);
                result.set_item("@tag", &self.item.tag)?;
                for (key, value) in &self.item.attributes {
                    result.set_item(key, value)?;
                }
                result.set_item("content", self.content())?;
                result.set_item("items", self.items(py)?)?;
                Ok(result.into())
            }

            #[getter]
            fn items(&self, py: Python<'_>) -> PyResult<Vec<PyTranscriptItem>> {
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
transcript_item_class!(PyDivisionCount, "DivisionCount", Option<i64>, count_content);
transcript_item_class!(PyMspName, "MSPName", String, text_content);
transcript_item_class!(PyRepName, "RepName", String, text_content);
transcript_item_class!(PyRepList, "RepList");
transcript_item_class!(PyMotion, "Motion");

/// The concrete Python classes returned by heterogeneous transcript collections.
#[derive(IntoPyObject, IntoPyObjectRef)]
pub(super) enum PyTranscriptItem {
    Speech(Py<PySpeech>),
    Division(Py<PyDivision>),
    GidRedirect(Py<PyGidRedirect>),
    OralHeading(Py<PyOralHeading>),
    MajorHeading(Py<PyMajorHeading>),
    MinorHeading(Py<PyMinorHeading>),
    Agreement(Py<PyAgreement>),
    Source(Py<PySource>),
    Question(Py<PyQuestion>),
    Reply(Py<PyReply>),
    SpeechItem(Py<PySpeechItem>),
    DivisionCount(Py<PyDivisionCount>),
    MspName(Py<PyMspName>),
    RepName(Py<PyRepName>),
    RepList(Py<PyRepList>),
    Motion(Py<PyMotion>),
}

fn markup_content(item: &TranscriptItem) -> BTreeMap<String, String> {
    BTreeMap::from([
        ("text".to_owned(), item.content.text.clone()),
        ("raw".to_owned(), item.content.raw.clone()),
    ])
}

fn count_content(item: &TranscriptItem) -> Option<i64> {
    item.content.text.trim().parse().ok()
}

fn text_content(item: &TranscriptItem) -> String {
    item.content.text.clone()
}

pub(super) fn item_to_python(py: Python<'_>, item: &TranscriptItem) -> PyResult<PyTranscriptItem> {
    macro_rules! create_item {
        ($type:ident, $variant:ident) => {
            Ok(PyTranscriptItem::$variant(Py::new(
                py,
                $type { item: item.clone() },
            )?))
        };
    }
    match item.tag.as_str() {
        "speech" => create_item!(PySpeech, Speech),
        "division" => create_item!(PyDivision, Division),
        "gidredirect" => create_item!(PyGidRedirect, GidRedirect),
        "oral-heading" => create_item!(PyOralHeading, OralHeading),
        "major-heading" => create_item!(PyMajorHeading, MajorHeading),
        "minor-heading" => create_item!(PyMinorHeading, MinorHeading),
        "agreement" => create_item!(PyAgreement, Agreement),
        "source" => create_item!(PySource, Source),
        "ques" => create_item!(PyQuestion, Question),
        "reply" => create_item!(PyReply, Reply),
        "divisioncount" => create_item!(PyDivisionCount, DivisionCount),
        "mspname" => create_item!(PyMspName, MspName),
        "repname" | "mpname" | "msname" | "mlaname" | "lord" => create_item!(PyRepName, RepName),
        "replist" | "mplist" | "msplist" | "mslist" | "mlalist" | "lordlist" => {
            create_item!(PyRepList, RepList)
        }
        "motion" => create_item!(PyMotion, Motion),
        _ => create_item!(PySpeechItem, SpeechItem),
    }
}
