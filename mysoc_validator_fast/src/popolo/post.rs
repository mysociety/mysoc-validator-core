//! Posts and their areas, with live handles and collection operations.
//!
//! Post handles use IDs; an Area handle navigates to the area of its owning post.
//! Python exposes no post removal or area replacement, so those locations remain
//! valid under supported edits. Standalone posts/areas own their data. Date getters
//! clone date values, while area getters on a live post return a live area view.

use super::accessors::{extra_methods, standalone_model_methods, str_getter, str_setter};
use super::conversions::{py_to_extra, ToOptionalString};
use super::extra::PyExtra;
use super::organization::{PyOrganization, PyOrganizationCollection};
use super::SharedPopolo;
use crate::conversions::{json_value_to_py, py_to_json_value};
use pyo3::exceptions::{PyKeyError, PyValueError};
use pyo3::prelude::*;
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

enum AreaState {
    Standalone(popolo_validator::Area),
    PostHandle {
        shared: SharedPopolo,
        post_id: String,
    },
}

#[gen_stub_pyclass]
#[pyclass(name = "Area")]
pub struct PyArea {
    inner: AreaState,
}

impl PyArea {
    fn get_standalone(&self) -> Option<&popolo_validator::Area> {
        match &self.inner {
            AreaState::Standalone(area) => Some(area),
            AreaState::PostHandle { .. } => None,
        }
    }

    fn with_area<R>(&self, f: impl FnOnce(&popolo_validator::Area) -> R) -> R {
        match &self.inner {
            AreaState::Standalone(area) => f(area),
            AreaState::PostHandle { shared, post_id } => {
                let guard = shared.read().unwrap();
                let area = guard
                    .posts
                    .get(post_id)
                    .and_then(|post| post.area.as_ref())
                    .expect("Area not found in Post");
                f(area)
            }
        }
    }

    fn with_area_mut<R>(&mut self, f: impl FnOnce(&mut popolo_validator::Area) -> R) -> R {
        match &mut self.inner {
            AreaState::Standalone(area) => f(area),
            AreaState::PostHandle { shared, post_id } => {
                let mut guard = shared.write().unwrap();
                let area = guard
                    .posts
                    .get_mut(post_id)
                    .and_then(|post| post.area.as_mut())
                    .expect("Area not found in Post");
                f(area)
            }
        }
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyArea {
    #[new]
    #[pyo3(signature = (name, other_names=Vec::new(), *, extra=None))]
    fn py_new(
        name: String,
        other_names: Vec<String>,
        extra: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        Ok(Self {
            inner: AreaState::Standalone(popolo_validator::Area {
                extra: py_to_extra(extra)?,
                name,
                other_names,
            }),
        })
    }

    #[getter]
    fn other_names(&self) -> Vec<String> {
        self.with_area(|area| area.other_names.clone())
    }
}

str_getter!(PyArea, with_area, name);
str_setter!(PyArea, with_area_mut, name);
extra_methods!(PyArea, with_area, with_area_mut, ["name" => name]);
standalone_model_methods!(
    PyArea,
    AreaState,
    Standalone,
    popolo_validator::Area,
    with_area
);

enum PostState {
    Standalone(popolo_validator::Post),
    Handle { shared: SharedPopolo, id: String },
}

#[gen_stub_pyclass]
#[pyclass(name = "Post")]
pub struct PyPost {
    inner: PostState,
}

impl PyPost {
    pub fn make_handle(shared: SharedPopolo, id: String) -> Self {
        PyPost {
            inner: PostState::Handle { shared, id },
        }
    }

    pub fn get_id(&self) -> &str {
        match &self.inner {
            PostState::Standalone(p) => &p.id,
            PostState::Handle { id, .. } => id,
        }
    }

    pub fn get_standalone(&self) -> Option<&popolo_validator::Post> {
        match &self.inner {
            PostState::Standalone(p) => Some(p),
            _ => None,
        }
    }

    fn with_post<R>(&self, f: impl FnOnce(&popolo_validator::Post) -> R) -> R {
        match &self.inner {
            PostState::Standalone(p) => f(p),
            PostState::Handle { shared, id } => {
                let guard = shared.read().unwrap();
                let post = guard.posts.get(id).expect("Post not found");
                f(post)
            }
        }
    }

    fn with_post_mut<R>(&mut self, f: impl FnOnce(&mut popolo_validator::Post) -> R) -> R {
        match &mut self.inner {
            PostState::Standalone(p) => f(p),
            PostState::Handle { shared, id } => {
                let id = id.clone();
                let mut guard = shared.write().unwrap();
                let post = guard
                    .posts
                    .iter_mut()
                    .find(|p| p.id == id)
                    .expect("Post not found");
                f(post)
            }
        }
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyPost {
    #[new]
    #[pyo3(signature = (id, label = String::new(), role = String::new(),
                        organization_id = String::new(), *, area=None, extra=None))]
    fn py_new(
        id: String,
        label: String,
        role: String,
        organization_id: String,
        area: Option<&Bound<'_, PyAny>>,
        extra: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        let area = area
            .map(|area| {
                area.extract::<PyRef<PyArea>>()?
                    .get_standalone()
                    .cloned()
                    .ok_or_else(|| PyValueError::new_err("area must be standalone"))
            })
            .transpose()?;
        Ok(PyPost {
            inner: PostState::Standalone(popolo_validator::Post {
                id,
                label,
                role,
                organization_id,
                area,
                identifiers: None,
                start_date: fuzzy_date::FuzzyDate::fromisoformat("0001-01-01").unwrap(),
                end_date: fuzzy_date::FuzzyDate::fromisoformat("9999-12-31").unwrap(),
                extra: py_to_extra(extra)?,
            }),
        })
    }

    #[getter]
    fn id(&self) -> &str {
        self.get_id()
    }

    #[getter]
    fn area_name(&self) -> Option<String> {
        self.with_post(|p| p.area.as_ref().map(|a| a.name.clone()))
    }

    #[getter]
    fn area(&self, py: Python<'_>) -> PyResult<Option<Py<PyArea>>> {
        match &self.inner {
            PostState::Standalone(post) => post
                .area
                .clone()
                .map(|area| {
                    Py::new(
                        py,
                        PyArea {
                            inner: AreaState::Standalone(area),
                        },
                    )
                })
                .transpose(),
            PostState::Handle { shared, id } => {
                if self.with_post(|post| post.area.is_some()) {
                    Ok(Some(Py::new(
                        py,
                        PyArea {
                            inner: AreaState::PostHandle {
                                shared: shared.clone(),
                                post_id: id.clone(),
                            },
                        },
                    )?))
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn organization(&self, py: Python<'_>) -> PyResult<Py<PyOrganization>> {
        let PostState::Handle { shared, .. } = &self.inner else {
            return Err(PyValueError::new_err("Post has no parent Popolo"));
        };
        let organization_id = self.with_post(|post| post.organization_id.clone());
        PyOrganizationCollection::new(shared.clone()).__getitem__(py, &organization_id)
    }

    fn __repr__(&self) -> String {
        format!("<Post: {}>", self.get_id())
    }
}

str_getter!(PyPost, with_post, label);
str_getter!(PyPost, with_post, role);
str_getter!(PyPost, with_post, organization_id);
str_setter!(PyPost, with_post_mut, label);
extra_methods!(PyPost, with_post, with_post_mut, ["label" => label, "role" => role]);
standalone_model_methods!(
    PyPost,
    PostState,
    Standalone,
    popolo_validator::Post,
    with_post
);

// PostCollection

#[gen_stub_pyclass]
#[pyclass(name = "PostCollection")]
pub struct PyPostCollection {
    shared: SharedPopolo,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyPostCollection {
    pub(super) fn __getitem__(&self, py: Python<'_>, id: &str) -> PyResult<Py<PyPost>> {
        let guard = self.shared.read().unwrap();
        if guard.posts.id_lookup.contains_key(id) {
            let pid = id.to_string();
            drop(guard);
            let p = PyPost::make_handle(self.shared.clone(), pid);
            Py::new(py, p)
        } else {
            Err(PyKeyError::new_err(id.to_string()))
        }
    }

    fn __len__(&self) -> usize {
        self.shared.read().unwrap().posts.len()
    }

    fn __contains__(&self, id: &str) -> bool {
        self.shared.read().unwrap().posts.id_lookup.contains_key(id)
    }

    #[getter]
    fn root(&self, py: Python<'_>) -> PyResult<Vec<Py<PyPost>>> {
        let ids: Vec<String> = {
            let guard = self.shared.read().unwrap();
            guard.posts.root.iter().map(|p| p.id.clone()).collect()
        };
        ids.iter()
            .map(|id| {
                let p = PyPost::make_handle(self.shared.clone(), id.clone());
                Py::new(py, p)
            })
            .collect()
    }

    fn append(&self, _py: Python<'_>, item: &Bound<'_, PyAny>) -> PyResult<()> {
        if let Ok(p) = item.extract::<PyRef<PyPost>>() {
            let pid = p.get_id().to_string();
            let post_data = p.get_standalone().cloned();
            drop(p);
            let mut guard = self.shared.write().unwrap();
            if guard.posts.id_lookup.contains_key(&pid) {
                return Err(PyValueError::new_err(format!(
                    "Duplicate Post id {:?}",
                    pid
                )));
            }
            let data = post_data.unwrap_or(popolo_validator::Post {
                id: pid,
                label: String::new(),
                role: String::new(),
                organization_id: String::new(),
                area: None,
                identifiers: None,
                start_date: fuzzy_date::FuzzyDate::fromisoformat("0001-01-01").unwrap(),
                end_date: fuzzy_date::FuzzyDate::fromisoformat("9999-12-31").unwrap(),
                extra: None,
            });
            guard.posts.push(data);
        } else {
            return Err(PyValueError::new_err("append() expects a Post"));
        }
        Ok(())
    }
}

impl PyPostCollection {
    pub(super) fn new(shared: SharedPopolo) -> Self {
        Self { shared }
    }
}
