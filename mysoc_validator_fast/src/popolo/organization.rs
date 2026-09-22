//! Organization values, live handles and collection operations.
//!
//! Unlike people and memberships, organizations can be removed. A live handle
//! to a removed ID raises `KeyError` on data access; `pop` returns an independent
//! owned value. IDs remain readable on stale handles. Reusing an ID makes an old
//! handle address the new record because identity here is the ID, not a generation.
//! Accessors use `PyResult` to keep ordinary removal from panicking under a lock.

use super::accessors::{
    extra_methods, opt_str_getter, standalone_model_methods, str_getter, str_setter,
};
use super::conversions::{
    fuzzy_to_py, py_to_extra, py_to_fuzzy_date, py_to_membership_reason, ToOptionalString,
};
use super::extra::PyExtra;
use super::SharedPopolo;
use crate::conversions::{json_value_to_py, py_to_json_value};
use chrono::NaiveDate;
use fuzzy_date::FuzzyDate;
use popolo_validator::{MembershipType, OrgType};
use pyo3::exceptions::{PyKeyError, PyValueError};
use pyo3::prelude::*;
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

enum OrganizationState {
    Standalone(popolo_validator::Organization),
    Handle { shared: SharedPopolo, id: String },
}

#[gen_stub_pyclass]
#[pyclass(name = "Organization")]
pub struct PyOrganization {
    inner: OrganizationState,
}

impl PyOrganization {
    pub fn make_handle(shared: SharedPopolo, id: String) -> Self {
        PyOrganization {
            inner: OrganizationState::Handle { shared, id },
        }
    }

    pub fn get_id(&self) -> &str {
        match &self.inner {
            OrganizationState::Standalone(o) => &o.id,
            OrganizationState::Handle { id, .. } => id,
        }
    }

    pub fn get_standalone(&self) -> Option<&popolo_validator::Organization> {
        match &self.inner {
            OrganizationState::Standalone(o) => Some(o),
            _ => None,
        }
    }

    fn with_org<R>(&self, f: impl FnOnce(&popolo_validator::Organization) -> R) -> PyResult<R> {
        match &self.inner {
            OrganizationState::Standalone(o) => Ok(f(o)),
            OrganizationState::Handle { shared, id } => {
                let guard = shared.read().unwrap();
                let org = guard
                    .organizations
                    .get(id)
                    .ok_or_else(|| PyKeyError::new_err(id.clone()))?;
                Ok(f(org))
            }
        }
    }

    fn with_org_mut<R>(
        &mut self,
        f: impl FnOnce(&mut popolo_validator::Organization) -> R,
    ) -> PyResult<R> {
        match &mut self.inner {
            OrganizationState::Standalone(o) => Ok(f(o)),
            OrganizationState::Handle { shared, id } => {
                let id = id.clone();
                let mut guard = shared.write().unwrap();
                let org = guard
                    .organizations
                    .iter_mut()
                    .find(|o| o.id == id)
                    .ok_or_else(|| PyKeyError::new_err(id.clone()))?;
                Ok(f(org))
            }
        }
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyOrganization {
    #[new]
    #[pyo3(signature = (id, name = String::new(), *, classification = None,
        abstract_ = None, description = None, founding_date = None,
        dissolution_date = None, links = Vec::new(), parent_id = None, extra = None))]
    #[allow(clippy::too_many_arguments)]
    fn py_new(
        id: String,
        name: String,
        classification: Option<String>,
        abstract_: Option<String>,
        description: Option<String>,
        founding_date: Option<&Bound<'_, PyAny>>,
        dissolution_date: Option<&Bound<'_, PyAny>>,
        links: Vec<String>,
        parent_id: Option<String>,
        extra: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        let org_type = classification
            .map(|classification| match classification.as_str() {
                "party" => Ok(OrgType::Party),
                "chamber" => Ok(OrgType::Chamber),
                "metro" => Ok(OrgType::Metro),
                "committee" => Ok(OrgType::Committee),
                "cross_party_group" => Ok(OrgType::CrossPartyGroup),
                "local_authority" => Ok(OrgType::LocalAuthority),
                "combined_authority" => Ok(OrgType::CombinedAuthority),
                "other" => Ok(OrgType::Other),
                _ => Err(PyValueError::new_err(format!(
                    "Unknown organization classification: {}",
                    classification
                ))),
            })
            .transpose()?;
        Ok(PyOrganization {
            inner: OrganizationState::Standalone(popolo_validator::Organization {
                id,
                name,
                classification: org_type,
                identifiers: None,
                abstract_,
                description,
                dissolution_date: dissolution_date.map(py_to_fuzzy_date).transpose()?,
                extra: py_to_extra(extra)?,
                founding_date: founding_date.map(py_to_fuzzy_date).transpose()?,
                links,
                parent_id,
            }),
        })
    }

    #[getter]
    fn id(&self) -> &str {
        self.get_id()
    }

    fn delete(&self) -> PyResult<()> {
        let OrganizationState::Handle { shared, id } = &self.inner else {
            return Err(PyValueError::new_err(
                "Cannot delete an Organization with no parent Popolo",
            ));
        };
        let mut guard = shared.write().unwrap();
        guard
            .organizations
            .remove(id)
            .ok_or_else(|| PyKeyError::new_err(id.clone()))?;

        Ok(())
    }

    fn close_open_memberships<'py>(
        slf: PyRef<'py, Self>,
        end_date: &Bound<'_, PyAny>,
        end_reason: &Bound<'_, PyAny>,
    ) -> PyResult<PyRef<'py, Self>> {
        let OrganizationState::Handle { shared, id } = &slf.inner else {
            return Err(PyValueError::new_err("Organization has no parent Popolo"));
        };
        let end_date = py_to_fuzzy_date(end_date)?;
        let end_reason = py_to_membership_reason(end_reason)?;
        let mut guard = shared.write().unwrap();
        for item in &mut guard.memberships.root {
            if let MembershipType::Membership(membership) = item {
                if membership.organization_id.as_deref() == Some(id)
                    && membership.end_date.latest_date
                        == NaiveDate::from_ymd_opt(9999, 12, 31).unwrap()
                {
                    membership.end_date = end_date.clone();
                    membership.end_reason = Some(end_reason.clone());
                }
            }
        }
        drop(guard);
        Ok(slf)
    }

    #[getter]
    fn classification(&self) -> PyResult<Option<String>> {
        self.with_org(|o| {
            o.classification.as_ref().map(|c| match c {
                OrgType::Party => "party".to_string(),
                OrgType::Chamber => "chamber".to_string(),
                OrgType::Metro => "metro".to_string(),
                OrgType::Committee => "committee".to_string(),
                OrgType::CrossPartyGroup => "cross_party_group".to_string(),
                OrgType::LocalAuthority => "local_authority".to_string(),
                OrgType::CombinedAuthority => "combined_authority".to_string(),
                OrgType::Other => "other".to_string(),
            })
        })
    }

    fn __repr__(&self) -> String {
        format!("<Organization: {}>", self.get_id())
    }
}

str_getter!(PyOrganization, with_org, name, ?);
str_setter!(PyOrganization, with_org_mut, name, ?);
extra_methods!(PyOrganization, with_org, with_org_mut, ["name" => name, "abstract" => abstract_, "description" => description], ?);
standalone_model_methods!(
    PyOrganization,
    OrganizationState,
    Standalone,
    popolo_validator::Organization,
    with_org, ?
);

opt_str_getter!(PyOrganization, with_org, description, ?);
opt_str_getter!(PyOrganization, with_org, parent_id, ?);

#[gen_stub_pymethods]
#[pymethods]
impl PyOrganization {
    #[getter]
    fn r#abstract(&self) -> PyResult<Option<String>> {
        self.with_org(|organization| organization.abstract_.clone())
    }

    #[getter]
    fn links(&self) -> PyResult<Vec<String>> {
        self.with_org(|organization| organization.links.clone())
    }

    #[getter]
    fn founding_date(&self, py: Python<'_>) -> PyResult<Option<Py<FuzzyDate>>> {
        self.with_org(|organization| {
            organization
                .founding_date
                .as_ref()
                .map(|date| fuzzy_to_py(py, date))
                .transpose()
        })?
    }

    #[getter]
    fn dissolution_date(&self, py: Python<'_>) -> PyResult<Option<Py<FuzzyDate>>> {
        self.with_org(|organization| {
            organization
                .dissolution_date
                .as_ref()
                .map(|date| fuzzy_to_py(py, date))
                .transpose()
        })?
    }
}

#[gen_stub_pyclass]
#[pyclass(name = "OrganizationCollection")]
pub struct PyOrganizationCollection {
    shared: SharedPopolo,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyOrganizationCollection {
    pub(super) fn __getitem__(&self, py: Python<'_>, id: &str) -> PyResult<Py<PyOrganization>> {
        let guard = self.shared.read().unwrap();
        if guard.organizations.id_lookup.contains_key(id) {
            let oid = id.to_string();
            drop(guard);
            let o = PyOrganization::make_handle(self.shared.clone(), oid);
            Py::new(py, o)
        } else {
            Err(PyKeyError::new_err(id.to_string()))
        }
    }

    fn __len__(&self) -> usize {
        self.shared.read().unwrap().organizations.len()
    }

    fn __contains__(&self, id: &str) -> bool {
        self.shared
            .read()
            .unwrap()
            .organizations
            .id_lookup
            .contains_key(id)
    }

    fn get(&self, py: Python<'_>, id: &str) -> PyResult<Option<Py<PyOrganization>>> {
        if self.__contains__(id) {
            self.__getitem__(py, id).map(Some)
        } else {
            Ok(None)
        }
    }

    fn pop(&self, id: &str) -> PyResult<PyOrganization> {
        let mut guard = self.shared.write().unwrap();
        let organization = guard
            .organizations
            .remove(id)
            .ok_or_else(|| PyKeyError::new_err(id.to_string()))?;

        Ok(PyOrganization {
            inner: OrganizationState::Standalone(organization),
        })
    }

    fn append(&self, _py: Python<'_>, item: &Bound<'_, PyAny>) -> PyResult<()> {
        if let Ok(o) = item.extract::<PyRef<PyOrganization>>() {
            let oid = o.get_id().to_string();
            let org_data = o.get_standalone().cloned();
            drop(o);
            let mut guard = self.shared.write().unwrap();
            if guard.organizations.id_lookup.contains_key(&oid) {
                return Err(PyValueError::new_err(format!(
                    "Duplicate Organization id {:?}",
                    oid
                )));
            }
            let data = org_data.unwrap_or(popolo_validator::Organization {
                id: oid,
                name: String::new(),
                classification: None,
                identifiers: None,
                abstract_: None,
                description: None,
                dissolution_date: None,
                extra: None,
                founding_date: None,
                links: Vec::new(),
                parent_id: None,
            });
            guard.organizations.push(data);
        } else {
            return Err(PyValueError::new_err("append() expects an Organization"));
        }
        Ok(())
    }
}

impl PyOrganizationCollection {
    pub(super) fn new(shared: SharedPopolo) -> Self {
        Self { shared }
    }
}
