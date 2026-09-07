//! Popolo ownership, top-level Python operations and class registration.
//!
//! `Arc` is shared ownership: cloning it keeps the same dataset alive rather
//! than copying the records. `RwLock` grants either readers or one writer; a
//! guard releases that access when it leaves scope. This is independent of
//! Python's object references and interpreter lock.
//!
//! Each entity module keeps its standalone/handle enum beside its methods and
//! collection. An enum variant is an explicit storage state, not a Python
//! subclass. Appending standalone data copies it; collection lookups return
//! live ID-based handles. Names and date getters return detached values.
//!
//! Access closures must finish borrowing records before their guard is dropped.
//! Copy data out before calling Python or acquiring the same lock again: either
//! can re-enter an accessor. `accessors.rs` supplies repeated field methods;
//! conversions and exception mapping remain at this bridge boundary.
//!
//! Remaining lock `unwrap()` calls assume no internal Rust panic poisoned the
//! lock. User validation errors return `PyResult` and must not unwind under a
//! write guard. Organization removal is supported, so its accessors are fallible.

use crate::conversions::escape_non_ascii;
use chrono::NaiveDate;
use popolo_validator::{MembershipType, PersonType, Popolo};
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::PyType;
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};
use std::sync::{Arc, RwLock};

mod accessors;
mod conversions;
mod extra;
mod membership;
mod names;
mod organization;
mod person;
mod post;

type SharedPopolo = Arc<RwLock<Popolo>>;
pub use self::extra::PyExtra;
pub use self::membership::{PyMembership, PyMembershipCollection, PyMembershipRedirect};
pub use self::names::{PyAltName, PyBasicPersonName, PyLordName};
pub use self::organization::{PyOrganization, PyOrganizationCollection};
pub use self::person::{PyPerson, PyPersonCollection, PyPersonRedirect};
pub use self::post::{PyArea, PyPost, PyPostCollection};

#[gen_stub_pyclass]
#[pyclass(name = "Popolo", subclass)]
pub struct PyPopolo {
    shared: SharedPopolo,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyPopolo {
    #[classmethod]
    #[pyo3(name = "model_validate_json", signature = (json_str, validate = true))]
    fn py_model_validate_json(
        _cls: &Bound<'_, PyType>,
        json_str: &str,
        validate: bool,
    ) -> PyResult<Self> {
        match Popolo::model_validate_json(json_str, &validate) {
            Ok(p) => Ok(PyPopolo {
                shared: Arc::new(RwLock::new(p)),
            }),
            Err(errors) => {
                let msgs: Vec<String> = errors.iter().map(|e| e.error.clone()).collect();
                Err(PyValueError::new_err(msgs.join("\n")))
            }
        }
    }

    fn model_dump_json(&self) -> String {
        let guard = self.shared.read().unwrap();
        escape_non_ascii(guard.model_dump_json())
    }

    fn to_json_str(&self) -> String {
        self.model_dump_json()
    }

    fn to_path(&self, path: &Bound<'_, PyAny>) -> PyResult<()> {
        let path = if let Ok(path) = path.extract::<String>() {
            path
        } else {
            path.call_method0("__fspath__")?.extract::<String>()?
        };
        std::fs::write(path, self.model_dump_json())
            .map_err(|error| PyValueError::new_err(error.to_string()))
    }

    fn update<'py>(slf: PyRef<'py, Self>, other: PyRef<'_, Self>) -> PyResult<PyRef<'py, Self>> {
        if Arc::ptr_eq(&slf.shared, &other.shared) {
            return Ok(slf);
        }
        let other_guard = other.shared.read().unwrap();
        let mut guard = slf.shared.write().unwrap();

        // Preserve merge policy: skip IDs present before this merge, retaining
        // any duplicates already present in an unvalidated incoming dataset.
        let membership_ids = guard.memberships.id_lookup.clone();
        let organization_ids = guard.organizations.id_lookup.clone();
        let person_ids = guard.persons.id_lookup.clone();
        let post_ids = guard.posts.id_lookup.clone();
        for membership in &other_guard.memberships.root {
            let id = match membership {
                MembershipType::Membership(membership) => &membership.id,
                MembershipType::MembershipRedirect(redirect) => &redirect.id,
            };
            if !membership_ids.contains_key(id) {
                guard.memberships.push(membership.clone());
            }
        }
        for organization in &other_guard.organizations.root {
            if !organization_ids.contains_key(&organization.id) {
                guard.organizations.push(organization.clone());
            }
        }
        for person in &other_guard.persons.root {
            let id = match person {
                PersonType::Person(person) => &person.id,
                PersonType::PersonRedirect(redirect) => &redirect.id,
            };
            if !person_ids.contains_key(id) {
                guard.persons.push(person.clone());
            }
        }
        for post in &other_guard.posts.root {
            if !post_ids.contains_key(&post.id) {
                guard.posts.push(post.clone());
            }
        }
        drop(guard);
        drop(other_guard);
        Ok(slf)
    }

    #[getter]
    fn persons(&self) -> PyPersonCollection {
        PyPersonCollection::new(self.shared.clone())
    }

    #[getter]
    fn memberships(&self) -> PyMembershipCollection {
        PyMembershipCollection::new(self.shared.clone())
    }

    #[getter]
    fn organizations(&self) -> PyOrganizationCollection {
        PyOrganizationCollection::new(self.shared.clone())
    }

    #[getter]
    fn posts(&self) -> PyPostCollection {
        PyPostCollection::new(self.shared.clone())
    }

    #[pyo3(signature = (name, chamber_id, date_str, include_historical_names=None))]
    fn find_person_id_by_name(
        &self,
        name: &str,
        chamber_id: &str,
        date_str: &str,
        include_historical_names: Option<bool>,
    ) -> PyResult<Option<String>> {
        let d = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
            .map_err(|e| PyValueError::new_err(format!("Invalid date: {}", e)))?;
        self.shared
            .read()
            .unwrap()
            .find_person_id_by_name(
                name,
                chamber_id,
                d,
                include_historical_names.unwrap_or(false),
            )
            .map_err(PyValueError::new_err)
    }

    fn find_person_id_by_identifier(&self, identifier: &str, scheme: &str) -> Option<String> {
        self.shared
            .read()
            .unwrap()
            .find_person_id_by_identifier(identifier, scheme)
    }
}

pub(crate) fn register_popolo_classes(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PyLordName>()?;
    m.add_class::<PyBasicPersonName>()?;
    m.add_class::<PyAltName>()?;
    m.add_class::<PyExtra>()?;
    m.add_class::<PyPopolo>()?;
    m.add_class::<PyMembership>()?;
    m.add_class::<PyMembershipRedirect>()?;
    m.add_class::<PyPerson>()?;
    m.add_class::<PyPersonRedirect>()?;
    m.add_class::<PyOrganization>()?;
    m.add_class::<PyArea>()?;
    m.add_class::<PyPost>()?;
    m.add_class::<PyMembershipCollection>()?;
    m.add_class::<PyPersonCollection>()?;
    m.add_class::<PyOrganizationCollection>()?;
    m.add_class::<PyPostCollection>()?;
    Ok(())
}
