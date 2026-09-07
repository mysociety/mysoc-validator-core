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
                if $python_name == "DivisionCount" {
                    return Ok(self
                        .item
                        .content
                        .text
                        .trim()
                        .parse::<i64>()
                        .ok()
                        .into_py(py));
                }
                if matches!($python_name, "MSPName" | "RepName") {
                    return Ok(self.item.content.text.clone().into_py(py));
                }
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
            fn count(&self, py: Python<'_>) -> PyResult<Option<PyObject>> {
                self.item
                    .children
                    .iter()
                    .find(|child| child.tag == "divisioncount")
                    .map(|child| item_to_python(py, child))
                    .transpose()
            }

            #[getter]
            fn representatives(&self, py: Python<'_>) -> PyResult<Vec<PyObject>> {
                self.item
                    .children
                    .iter()
                    .filter(|child| {
                        matches!(
                            child.tag.as_str(),
                            "replist" | "mplist" | "msplist" | "mslist" | "mlalist" | "lordlist"
                        )
                    })
                    .map(|child| item_to_python(py, child))
                    .collect()
            }

            #[getter]
            fn rel_motions(&self, py: Python<'_>) -> PyResult<Vec<PyObject>> {
                self.item
                    .children
                    .iter()
                    .filter(|child| child.tag == "motion")
                    .map(|child| item_to_python(py, child))
                    .collect()
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
transcript_item_class!(PyDivisionCount, "DivisionCount");
transcript_item_class!(PyMspName, "MSPName");
transcript_item_class!(PyRepName, "RepName");
transcript_item_class!(PyRepList, "RepList");
transcript_item_class!(PyMotion, "Motion");

pub(super) fn item_to_python(py: Python<'_>, item: &TranscriptItem) -> PyResult<PyObject> {
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
        "divisioncount" => create_item!(PyDivisionCount),
        "mspname" => create_item!(PyMspName),
        "repname" | "mpname" | "msname" | "mlaname" | "lord" => create_item!(PyRepName),
        "replist" | "mplist" | "msplist" | "mslist" | "mlalist" | "lordlist" => {
            create_item!(PyRepList)
        }
        "motion" => create_item!(PyMotion),
        _ => create_item!(PySpeechItem),
    }
}
