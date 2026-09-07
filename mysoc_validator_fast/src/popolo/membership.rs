//! Membership values, live handles, redirects and collection operations.
//!
//! Standalone constructors own records. `append`/`extend` copy those records into
//! Popolo; the caller's original wrapper stays standalone. Lookups resolve IDs
//! and return live handles. Python has no membership removal or ID setter, so
//! record lookup failures inside handle accessors indicate an internal invariant
//! failure. Insertion validation lives in the domain's `validation` module.
//! `extend` validates and commits each item in order, not as one transaction.

use super::accessors::{
    extra_methods, opt_str_getter, opt_str_setter, standalone_model_methods, str_getter, str_setter,
};
use super::conversions::{
    fuzzy_to_py, membership_reason_to_str, py_to_extra, py_to_fuzzy_date, py_to_membership_reason,
    str_to_membership_reason, ToOptionalString,
};
use super::extra::PyExtra;
use super::organization::{PyOrganization, PyOrganizationCollection};
use super::person::{PyPerson, PyPersonCollection};
use super::post::{PyPost, PyPostCollection};
use super::SharedPopolo;
use crate::conversions::{json_value_to_py, py_to_json_value};
use fuzzy_date::FuzzyDate;
use popolo_validator::MembershipType;
use pyo3::exceptions::{PyKeyError, PyValueError};
use pyo3::prelude::*;
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

#[gen_stub_pyclass]
#[pyclass(name = "MembershipRedirect")]
#[derive(Clone)]
pub struct PyMembershipRedirect {
    pub id: String,
    pub redirect: String,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyMembershipRedirect {
    #[new]
    fn py_new(id: String, redirect: String) -> Self {
        PyMembershipRedirect { id, redirect }
    }

    #[getter]
    fn id(&self) -> &str {
        &self.id
    }

    #[getter]
    fn redirect(&self) -> &str {
        &self.redirect
    }

    fn __repr__(&self) -> String {
        format!("<MembershipRedirect: {} -> {}>", self.id, self.redirect)
    }
}

enum MembershipState {
    Standalone(popolo_validator::Membership),
    Handle { shared: SharedPopolo, id: String },
}

#[gen_stub_pyclass]
#[pyclass(name = "Membership")]
pub struct PyMembership {
    inner: MembershipState,
}

impl PyMembership {
    fn with_membership<R>(&self, f: impl FnOnce(&popolo_validator::Membership) -> R) -> R {
        match &self.inner {
            MembershipState::Standalone(m) => f(m),
            MembershipState::Handle { shared, id } => {
                let guard = shared.read().unwrap();
                let m = guard
                    .memberships
                    .get(id)
                    .and_then(|mt| match mt {
                        MembershipType::Membership(m) => Some(m),
                        _ => None,
                    })
                    .expect("Membership not found in Popolo");
                f(m)
            }
        }
    }

    fn with_membership_mut<R>(
        &mut self,
        f: impl FnOnce(&mut popolo_validator::Membership) -> R,
    ) -> R {
        match &mut self.inner {
            MembershipState::Standalone(m) => f(m),
            MembershipState::Handle { shared, id } => {
                let id = id.clone();
                let mut guard = shared.write().unwrap();
                let m = guard
                    .memberships
                    .iter_mut()
                    .find_map(|mt| match mt {
                        MembershipType::Membership(m) if m.id == id => Some(m),
                        _ => None,
                    })
                    .expect("Membership not found in Popolo");
                f(m)
            }
        }
    }

    pub fn get_standalone(&self) -> Option<&popolo_validator::Membership> {
        match &self.inner {
            MembershipState::Standalone(m) => Some(m),
            _ => None,
        }
    }

    pub fn make_handle(shared: SharedPopolo, id: String) -> Self {
        PyMembership {
            inner: MembershipState::Handle { shared, id },
        }
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyMembership {
    #[new]
    #[pyo3(signature = (
        id, person_id, *,
        start_date = None, end_date = None,
        post_id = None, organization_id = None, on_behalf_of_id = None,
        role = None, label = None, source = None,
        start_reason = None, end_reason = None,
        identifiers = None, name = None, reason = None, extra = None
    ))]
    #[allow(clippy::too_many_arguments)]
    fn py_new(
        id: String,
        person_id: String,
        start_date: Option<&Bound<'_, PyAny>>,
        end_date: Option<&Bound<'_, PyAny>>,
        post_id: Option<String>,
        organization_id: Option<String>,
        on_behalf_of_id: Option<String>,
        role: Option<String>,
        label: Option<String>,
        source: Option<String>,
        start_reason: Option<&Bound<'_, PyAny>>,
        end_reason: Option<&Bound<'_, PyAny>>,
        identifiers: Option<&Bound<'_, PyAny>>,
        name: Option<&Bound<'_, PyAny>>,
        reason: Option<String>,
        extra: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        let start = match start_date {
            Some(v) => py_to_fuzzy_date(v)?,
            None => FuzzyDate::fromisoformat("0001-01-01").unwrap(),
        };
        let end = match end_date {
            Some(v) => py_to_fuzzy_date(v)?,
            None => FuzzyDate::fromisoformat("9999-12-31").unwrap(),
        };
        let sr = match start_reason {
            Some(v) => {
                let s: String = v.str()?.extract()?;
                if s.is_empty() {
                    None
                } else {
                    Some(str_to_membership_reason(&s)?)
                }
            }
            None => None,
        };
        let er = match end_reason {
            Some(v) => {
                let s: String = v.str()?.extract()?;
                if s.is_empty() {
                    None
                } else {
                    Some(str_to_membership_reason(&s)?)
                }
            }
            None => None,
        };
        let _ = identifiers;
        let _ = name;
        Ok(PyMembership {
            inner: MembershipState::Standalone(popolo_validator::Membership {
                id,
                person_id,
                start_date: start,
                end_date: end,
                post_id,
                organization_id,
                on_behalf_of_id,
                role,
                label,
                source,
                start_reason: sr,
                end_reason: er,
                extra: py_to_extra(extra)?,
                identifiers: None,
                name: None,
                reason,
            }),
        })
    }

    #[getter]
    fn start_date(&self, py: Python<'_>) -> PyResult<Py<FuzzyDate>> {
        self.with_membership(|m| fuzzy_to_py(py, &m.start_date))
    }

    #[getter]
    fn end_date(&self, py: Python<'_>) -> PyResult<Py<FuzzyDate>> {
        self.with_membership(|m| fuzzy_to_py(py, &m.end_date))
    }

    #[getter]
    fn start_reason(&self) -> String {
        self.with_membership(|m| {
            m.start_reason
                .as_ref()
                .map(membership_reason_to_str)
                .unwrap_or_default()
        })
    }

    #[getter]
    fn end_reason(&self) -> Option<String> {
        self.with_membership(|m| m.end_reason.as_ref().map(membership_reason_to_str))
    }

    #[setter(end_date)]
    fn set_end_date(&mut self, value: &Bound<'_, PyAny>) -> PyResult<()> {
        let fd = py_to_fuzzy_date(value)?;
        self.with_membership_mut(|m| m.end_date = fd);
        Ok(())
    }

    #[setter(start_date)]
    fn set_start_date(&mut self, value: &Bound<'_, PyAny>) -> PyResult<()> {
        let fd = py_to_fuzzy_date(value)?;
        self.with_membership_mut(|m| m.start_date = fd);
        Ok(())
    }

    #[setter(end_reason)]
    fn set_end_reason(&mut self, value: &Bound<'_, PyAny>) -> PyResult<()> {
        let r = py_to_membership_reason(value)?;
        self.with_membership_mut(|m| m.end_reason = Some(r));
        Ok(())
    }

    #[setter(start_reason)]
    fn set_start_reason(&mut self, value: &Bound<'_, PyAny>) -> PyResult<()> {
        let r = py_to_membership_reason(value)?;
        self.with_membership_mut(|m| m.start_reason = Some(r));
        Ok(())
    }

    fn person(&self, py: Python<'_>) -> PyResult<Option<Py<PyPerson>>> {
        let MembershipState::Handle { shared, .. } = &self.inner else {
            return Ok(None);
        };
        let person_id = self.with_membership(|membership| membership.person_id.clone());
        PyPersonCollection::new(shared.clone())
            .__getitem__(py, &person_id)
            .map(Some)
    }

    fn post(&self, py: Python<'_>) -> PyResult<Option<Py<PyPost>>> {
        let MembershipState::Handle { shared, .. } = &self.inner else {
            return Ok(None);
        };
        let Some(post_id) = self.with_membership(|membership| membership.post_id.clone()) else {
            return Ok(None);
        };
        PyPostCollection::new(shared.clone())
            .__getitem__(py, &post_id)
            .map(Some)
    }

    fn organization(&self, py: Python<'_>) -> PyResult<Option<Py<PyOrganization>>> {
        self.related_organization(py, false)
    }

    fn on_behalf_of(&self, py: Python<'_>) -> PyResult<Option<Py<PyOrganization>>> {
        self.related_organization(py, true)
    }

    fn __repr__(&self) -> String {
        format!("<Membership: {}>", self.with_membership(|m| m.id.clone()))
    }
}

impl PyMembership {
    fn related_organization(
        &self,
        py: Python<'_>,
        on_behalf_of: bool,
    ) -> PyResult<Option<Py<PyOrganization>>> {
        let MembershipState::Handle { shared, .. } = &self.inner else {
            return Ok(None);
        };
        let organization_id = self.with_membership(|membership| {
            if on_behalf_of {
                membership.on_behalf_of_id.clone()
            } else {
                membership.organization_id.clone()
            }
        });
        let Some(organization_id) = organization_id else {
            return Ok(None);
        };
        PyOrganizationCollection::new(shared.clone())
            .__getitem__(py, &organization_id)
            .map(Some)
    }
}

// Simple Membership getters/setters via macro (each generates its own impl block)
str_getter!(PyMembership, with_membership, id);
str_getter!(PyMembership, with_membership, person_id);
opt_str_getter!(PyMembership, with_membership, post_id);
opt_str_getter!(PyMembership, with_membership, organization_id);
opt_str_getter!(PyMembership, with_membership, on_behalf_of_id);
opt_str_getter!(PyMembership, with_membership, role);
opt_str_getter!(PyMembership, with_membership, label);
opt_str_getter!(PyMembership, with_membership, source);
str_setter!(PyMembership, with_membership_mut, person_id);
opt_str_setter!(PyMembership, with_membership_mut, organization_id);
opt_str_setter!(PyMembership, with_membership_mut, on_behalf_of_id);
opt_str_setter!(PyMembership, with_membership_mut, role);
opt_str_setter!(PyMembership, with_membership_mut, label);
extra_methods!(
    PyMembership,
    with_membership,
    with_membership_mut,
    ["label" => label, "role" => role]
);
standalone_model_methods!(
    PyMembership,
    MembershipState,
    Standalone,
    popolo_validator::Membership,
    with_membership
);

#[gen_stub_pyclass]
#[pyclass(name = "MembershipCollection")]
pub struct PyMembershipCollection {
    shared: SharedPopolo,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyMembershipCollection {
    pub(super) fn __getitem__(&self, py: Python<'_>, id: &str) -> PyResult<Py<PyMembership>> {
        let resolved_id = self
            .shared
            .read()
            .unwrap()
            .resolve_membership_id(id)
            .ok_or_else(|| PyKeyError::new_err(id.to_string()))?;
        Py::new(
            py,
            PyMembership::make_handle(self.shared.clone(), resolved_id),
        )
    }

    fn __len__(&self) -> usize {
        self.shared.read().unwrap().memberships.len()
    }

    fn __contains__(&self, id: &str) -> bool {
        self.shared
            .read()
            .unwrap()
            .memberships
            .id_lookup
            .contains_key(id)
    }

    #[getter]
    fn root(&self, py: Python<'_>) -> PyResult<Vec<PyObject>> {
        let ids: Vec<(String, bool)> = {
            let guard = self.shared.read().unwrap();
            guard
                .memberships
                .root
                .iter()
                .map(|mt| match mt {
                    MembershipType::Membership(m) => (m.id.clone(), false),
                    MembershipType::MembershipRedirect(r) => (r.id.clone(), true),
                })
                .collect()
        };
        ids.iter()
            .map(|(id, is_redirect)| {
                if *is_redirect {
                    let guard = self.shared.read().unwrap();
                    let r = guard.memberships.get(id).and_then(|mt| match mt {
                        MembershipType::MembershipRedirect(r) => Some(r.clone()),
                        _ => None,
                    });
                    drop(guard);
                    if let Some(r) = r {
                        let py_r = PyMembershipRedirect {
                            id: r.id,
                            redirect: r.redirect,
                        };
                        Ok(Py::new(py, py_r)?.into_py(py))
                    } else {
                        Err(PyValueError::new_err("Redirect not found"))
                    }
                } else {
                    let m = PyMembership::make_handle(self.shared.clone(), id.clone());
                    Ok(Py::new(py, m)?.into_py(py))
                }
            })
            .collect()
    }

    fn redirects(&self, py: Python<'_>) -> PyResult<Vec<PyObject>> {
        let redirects: Vec<_> = {
            let guard = self.shared.read().unwrap();
            guard
                .memberships
                .root
                .iter()
                .filter_map(|mt| match mt {
                    MembershipType::MembershipRedirect(r) => {
                        Some((r.id.clone(), r.redirect.clone()))
                    }
                    _ => None,
                })
                .collect()
        };
        redirects
            .iter()
            .map(|(id, redirect)| {
                let py_r = PyMembershipRedirect {
                    id: id.clone(),
                    redirect: redirect.clone(),
                };
                Ok(Py::new(py, py_r)?.into_py(py))
            })
            .collect()
    }

    fn append(&self, py: Python<'_>, item: &Bound<'_, PyAny>) -> PyResult<()> {
        self.extend(py, vec![item.clone().unbind()])
    }

    fn extend(&self, py: Python<'_>, items: Vec<PyObject>) -> PyResult<()> {
        for item in &items {
            let bound = item.bind(py);
            if let Ok(m) = bound.extract::<PyRef<PyMembership>>() {
                if let Some(standalone) = m.get_standalone() {
                    let m_clone = standalone.clone();
                    drop(m);
                    let mut guard = self.shared.write().unwrap();
                    guard
                        .validate_membership_insertion(&m_clone)
                        .map_err(PyValueError::new_err)?;
                    guard.memberships.push(MembershipType::Membership(m_clone));
                } else {
                    return Err(PyValueError::new_err(
                        "Cannot extend with a Membership that is already part of a Popolo",
                    ));
                }
            } else if let Ok(r) = bound.extract::<PyRef<PyMembershipRedirect>>() {
                let id = r.id.clone();
                let redirect = r.redirect.clone();
                drop(r);
                let mut guard = self.shared.write().unwrap();
                let redirect = popolo_validator::MembershipRedirect { id, redirect };
                guard
                    .validate_membership_redirect_insertion(&redirect)
                    .map_err(PyValueError::new_err)?;
                guard
                    .memberships
                    .push(MembershipType::MembershipRedirect(redirect));
            } else {
                return Err(PyValueError::new_err(
                    "extend() expects Membership or MembershipRedirect objects",
                ));
            }
        }
        Ok(())
    }
}

impl PyMembershipCollection {
    pub(super) fn new(shared: SharedPopolo) -> Self {
        Self { shared }
    }
}
