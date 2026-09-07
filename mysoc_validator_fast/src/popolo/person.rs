//! People, redirects, live handles and the person collection.
//!
//! Handles keep their original ID, but resolve merge redirects for both reads
//! and writes. Collections reject missing/cyclic redirects before making handles.
//! Python operations do not remove persons: merging replaces the source with a
//! redirect to an existing target, preserving the invariant used by accessors.
//! `root` is a new Python list of live handles and owned redirect values; changing
//! that list does not change the collection. Name getters return owned copies.

use super::accessors::{extra_methods, opt_str_getter, opt_str_setter, standalone_model_methods};
use super::conversions::{
    fuzzy_to_py, py_to_extra, py_to_fuzzy_date, py_to_membership_reason, py_to_naive_date,
    ToOptionalString,
};
use super::extra::PyExtra;
use super::membership::PyMembership;
use super::names::{PyAltName, PyBasicPersonName, PyLordName};
use super::organization::PyOrganization;
use super::SharedPopolo;
use crate::conversions::{json_value_to_py, py_to_json_value};
use chrono::{Duration, NaiveDate};
use fuzzy_date::FuzzyDate;
use popolo_validator::{MembershipReason, MembershipType, NameType, PersonType};
use pyo3::exceptions::{PyKeyError, PyValueError};
use pyo3::prelude::*;
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};

#[gen_stub_pyclass]
#[pyclass(name = "PersonRedirect")]
#[derive(Clone)]
pub struct PyPersonRedirect {
    pub id: String,
    pub redirect: String,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyPersonRedirect {
    #[new]
    fn py_new(id: String, redirect: String) -> Self {
        PyPersonRedirect { id, redirect }
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
        format!("<PersonRedirect: {} -> {}>", self.id, self.redirect)
    }
}

enum PersonState {
    Standalone(popolo_validator::Person),
    Handle { shared: SharedPopolo, id: String },
}

#[gen_stub_pyclass]
#[pyclass(name = "Person")]
pub struct PyPerson {
    inner: PersonState,
}

impl PyPerson {
    pub fn make_handle(shared: SharedPopolo, id: String) -> Self {
        PyPerson {
            inner: PersonState::Handle { shared, id },
        }
    }

    pub fn get_id(&self) -> &str {
        match &self.inner {
            PersonState::Standalone(p) => &p.id,
            PersonState::Handle { id, .. } => id,
        }
    }

    pub fn get_standalone(&self) -> Option<&popolo_validator::Person> {
        match &self.inner {
            PersonState::Standalone(p) => Some(p),
            _ => None,
        }
    }

    // `R` cannot borrow from the guard: the closure must return an owned value.
    // Python construction/conversion belongs after this helper returns.
    fn with_person<R>(&self, f: impl FnOnce(&popolo_validator::Person) -> R) -> R {
        match &self.inner {
            PersonState::Standalone(p) => f(p),
            PersonState::Handle { shared, id } => {
                let guard = shared.read().unwrap();
                let resolved_id = guard.resolve_person_id(id).expect(
                    "person handles originate from resolved records; merges preserve targets",
                );
                let person = guard
                    .persons
                    .get(&resolved_id)
                    .and_then(|pt| match pt {
                        PersonType::Person(p) => Some(p),
                        _ => None,
                    })
                    .expect("Person not found in Popolo");
                f(person)
            }
        }
    }

    fn with_person_mut<R>(&mut self, f: impl FnOnce(&mut popolo_validator::Person) -> R) -> R {
        match &mut self.inner {
            PersonState::Standalone(p) => f(p),
            PersonState::Handle { shared, id } => {
                let mut guard = shared.write().unwrap();
                let id = guard.resolve_person_id(id).expect(
                    "person handles originate from resolved records; merges preserve targets",
                );
                let person = guard
                    .persons
                    .iter_mut()
                    .find_map(|pt| match pt {
                        PersonType::Person(p) if p.id == id => Some(p),
                        _ => None,
                    })
                    .expect("Person not found in Popolo");
                f(person)
            }
        }
    }

    fn change_party_data(
        &self,
        new_party_id: String,
        change_date: NaiveDate,
        change_reason: MembershipReason,
        source: Option<String>,
    ) -> PyResult<()> {
        let PersonState::Handle { shared, id } = &self.inner else {
            return Err(PyValueError::new_err("Person has no parent Popolo object"));
        };
        let mut guard = shared.write().unwrap();
        let resolved_id = guard
            .resolve_person_id(id)
            .ok_or_else(|| PyKeyError::new_err(id.clone()))?;
        let id = &resolved_id;
        let last_index = guard
            .memberships
            .root
            .iter()
            .enumerate()
            .filter_map(|(index, item)| match item {
                MembershipType::Membership(membership) if membership.person_id == *id => {
                    Some((membership.start_date.earliest_date, index))
                }
                _ => None,
            })
            .max_by_key(|(start_date, _)| *start_date)
            .map(|(_, index)| index)
            .ok_or_else(|| PyValueError::new_err("Person has no memberships"))?;
        let MembershipType::Membership(last) = &mut guard.memberships.root[last_index] else {
            unreachable!()
        };
        last.end_date = FuzzyDate::new(change_date, change_date);
        last.end_reason = Some(change_reason.clone());
        let mut new_membership = last.clone();
        let next_id = guard
            .memberships
            .root
            .iter()
            .filter_map(|item| match item {
                MembershipType::Membership(membership) => membership
                    .id
                    .strip_prefix("uk.org.publicwhip/member/")
                    .and_then(|value| value.parse::<u32>().ok()),
                MembershipType::MembershipRedirect(_) => None,
            })
            .max()
            .unwrap_or(0)
            + 1;
        let start_date = change_date + Duration::days(1);
        new_membership.id = format!("uk.org.publicwhip/member/{next_id}");
        new_membership.start_date = FuzzyDate::new(start_date, start_date);
        new_membership.end_date = FuzzyDate::fromisoformat("9999-12-31").unwrap();
        new_membership.start_reason = Some(change_reason);
        new_membership.end_reason = None;
        new_membership.on_behalf_of_id = Some(new_party_id);
        new_membership.source = source;
        guard
            .memberships
            .push(MembershipType::Membership(new_membership));

        Ok(())
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyPerson {
    #[new]
    #[pyo3(signature = (id, *, biography=None, summary=None, gender=None,
                        national_identity=None, image=None, extra=None))]
    fn py_new(
        id: String,
        biography: Option<String>,
        summary: Option<String>,
        gender: Option<String>,
        national_identity: Option<String>,
        image: Option<String>,
        extra: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Self> {
        Ok(PyPerson {
            inner: PersonState::Standalone(popolo_validator::Person {
                id,
                biography,
                summary,
                gender,
                national_identity,
                image,
                birth_date: None,
                death_date: None,
                extra: py_to_extra(extra)?,
                identifiers: None,
                links: None,
                other_names: None,
                shortcuts: None,
            }),
        })
    }

    #[getter]
    fn id(&self) -> &str {
        self.get_id()
    }

    #[getter]
    fn birth_date(&self, py: Python<'_>) -> PyResult<Option<Py<FuzzyDate>>> {
        self.with_person(|p| {
            p.birth_date
                .as_ref()
                .map(|fd| fuzzy_to_py(py, fd))
                .transpose()
        })
    }

    #[getter]
    fn death_date(&self, py: Python<'_>) -> PyResult<Option<Py<FuzzyDate>>> {
        self.with_person(|p| {
            p.death_date
                .as_ref()
                .map(|fd| fuzzy_to_py(py, fd))
                .transpose()
        })
    }

    fn names_on_date(&self, _py: Python<'_>, date: &Bound<'_, PyAny>) -> PyResult<Vec<String>> {
        let d = py_to_naive_date(date)?;
        Ok(self.with_person(|p| p.names_on_date(&d)))
    }

    fn get_identifier(&self, scheme: &str) -> Option<String> {
        self.with_person(|p| p.get_identifier(scheme))
    }

    #[pyo3(signature = (scheme, identifier, *, if_missing=false))]
    fn add_identifer(&mut self, scheme: String, identifier: String, if_missing: bool) -> bool {
        if if_missing
            && self.with_person(|person| {
                person.get_identifier(&scheme).as_deref() == Some(identifier.as_str())
            })
        {
            return false;
        }
        self.with_person_mut(|person| {
            let identifiers = person.identifiers.get_or_insert_with(Vec::new);
            identifiers.retain(|existing| existing.scheme != scheme);
            identifiers.push(popolo_validator::SimpleIdentifer {
                scheme,
                identifier: popolo_validator::Identifier::String(identifier),
            });
        });
        true
    }

    #[pyo3(signature = (include_original_name=true))]
    fn all_name_variants(&self, include_original_name: bool) -> Vec<String> {
        self.with_person(|person| {
            let mut variants = Vec::new();
            for name in person.other_names.as_deref().unwrap_or_default() {
                for variant in name.name_variants(include_original_name) {
                    if !variants.contains(&variant) {
                        variants.push(variant);
                    }
                }
            }
            variants
        })
    }

    fn get_main_name(&self, py: Python<'_>, date: &Bound<'_, PyAny>) -> PyResult<Option<PyObject>> {
        let date = py_to_naive_date(date)?;
        let name = self.with_person(|person| {
            person
                .other_names
                .as_deref()
                .unwrap_or_default()
                .iter()
                .find(|name| {
                    let (start, end) = name.date_range();
                    name.note_str() == "Main"
                        && start.earliest_date <= date
                        && date <= end.latest_date
                })
                .cloned()
        });
        name.map(|name| match name {
            popolo_validator::NameOptions::BasicPersonName(inner) => {
                Py::new(py, PyBasicPersonName::from_owned(inner)).map(|value| value.into_py(py))
            }
            popolo_validator::NameOptions::LordName(inner) => {
                Py::new(py, PyLordName::from_owned(inner)).map(|value| value.into_py(py))
            }
            popolo_validator::NameOptions::AltName(inner) => {
                Py::new(py, PyAltName::from_owned(inner)).map(|value| value.into_py(py))
            }
        })
        .transpose()
    }

    fn memberships(&self, py: Python<'_>) -> PyResult<Vec<Py<PyMembership>>> {
        let (ids, shared) = match &self.inner {
            PersonState::Standalone(_) => {
                return Err(PyValueError::new_err(
                    "Cannot call memberships() on a standalone Person not attached to a Popolo",
                ))
            }
            PersonState::Handle { shared, id } => {
                let guard = shared.read().unwrap();
                let resolved_id = guard
                    .resolve_person_id(id)
                    .ok_or_else(|| PyKeyError::new_err(id.clone()))?;
                let id = &resolved_id;
                (guard.person_membership_ids(id), shared.clone())
            }
        };
        ids.iter()
            .map(|mid| {
                let m = PyMembership::make_handle(shared.clone(), mid.clone());
                Py::new(py, m)
            })
            .collect()
    }

    fn reduced_id(&self) -> String {
        self.get_id()
            .rsplit('/')
            .next()
            .unwrap_or(self.get_id())
            .to_string()
    }

    #[pyo3(signature = (date, *, chamber))]
    fn membership_on_date(
        &self,
        py: Python<'_>,
        date: &Bound<'_, PyAny>,
        chamber: &Bound<'_, PyAny>,
    ) -> PyResult<Option<Py<PyMembership>>> {
        let date = py_to_naive_date(date)?;
        let chamber = chamber.str()?.to_str()?.to_string();
        let PersonState::Handle { shared, id } = &self.inner else {
            return Err(PyValueError::new_err("Person has no parent Popolo"));
        };
        let membership_id = {
            let guard = shared.read().unwrap();
            let resolved_id = guard
                .resolve_person_id(id)
                .ok_or_else(|| PyKeyError::new_err(id.clone()))?;
            let id = &resolved_id;
            guard.memberships.root.iter().find_map(|item| {
                let MembershipType::Membership(membership) = item else {
                    return None;
                };
                let post_matches = membership
                    .post_id
                    .as_ref()
                    .and_then(|post_id| guard.posts.get(post_id))
                    .is_some_and(|post| post.organization_id == chamber);
                (membership.person_id == *id
                    && post_matches
                    && membership.start_date.earliest_date <= date
                    && date <= membership.end_date.latest_date)
                    .then(|| membership.id.clone())
            })
        };
        membership_id
            .map(|membership_id| {
                Py::new(py, PyMembership::make_handle(shared.clone(), membership_id))
            })
            .transpose()
    }

    fn latest_membership(
        &self,
        py: Python<'_>,
        chamber: &Bound<'_, PyAny>,
    ) -> PyResult<Option<Py<PyMembership>>> {
        let chamber = chamber.str()?.to_str()?.to_string();
        let PersonState::Handle { shared, id } = &self.inner else {
            return Err(PyValueError::new_err("Person has no parent Popolo"));
        };
        let membership_id = {
            let guard = shared.read().unwrap();
            let resolved_id = guard
                .resolve_person_id(id)
                .ok_or_else(|| PyKeyError::new_err(id.clone()))?;
            let id = &resolved_id;
            guard
                .memberships
                .root
                .iter()
                .filter_map(|item| match item {
                    MembershipType::Membership(membership)
                        if membership.person_id == *id
                            && membership
                                .post_id
                                .as_ref()
                                .and_then(|post_id| guard.posts.get(post_id))
                                .is_some_and(|post| post.organization_id == chamber) =>
                    {
                        Some((membership.start_date.earliest_date, membership.id.clone()))
                    }
                    _ => None,
                })
                .max_by_key(|(start_date, _)| *start_date)
                .map(|(_, membership_id)| membership_id)
        };
        membership_id
            .map(|membership_id| {
                Py::new(py, PyMembership::make_handle(shared.clone(), membership_id))
            })
            .transpose()
    }

    #[pyo3(signature = (organization_id, role, start_date, end_date=None,
        post_id="", on_behalf_of_id="", start_reason=None, end_reason=None))]
    #[allow(clippy::too_many_arguments)]
    fn add_membership(
        &self,
        organization_id: &Bound<'_, PyAny>,
        role: String,
        start_date: &Bound<'_, PyAny>,
        end_date: Option<&Bound<'_, PyAny>>,
        post_id: &str,
        on_behalf_of_id: &str,
        start_reason: Option<&Bound<'_, PyAny>>,
        end_reason: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<()> {
        let PersonState::Handle { shared, id } = &self.inner else {
            return Err(PyValueError::new_err("Person has no parent Popolo object"));
        };
        let organization_id = organization_id.str()?.to_str()?.to_string();
        let mut guard = shared.write().unwrap();
        let resolved_id = guard
            .resolve_person_id(id)
            .ok_or_else(|| PyKeyError::new_err(id.clone()))?;
        let id = &resolved_id;
        let next_id = guard
            .memberships
            .root
            .iter()
            .filter_map(|item| match item {
                MembershipType::Membership(membership) => membership
                    .id
                    .strip_prefix("uk.org.publicwhip/member/")
                    .and_then(|value| value.parse::<u32>().ok()),
                MembershipType::MembershipRedirect(_) => None,
            })
            .max()
            .unwrap_or(0)
            + 1;
        guard
            .memberships
            .push(MembershipType::Membership(popolo_validator::Membership {
                id: format!("uk.org.publicwhip/member/{next_id}"),
                person_id: id.clone(),
                organization_id: Some(organization_id),
                role: Some(role),
                start_date: py_to_fuzzy_date(start_date)?,
                end_date: end_date
                    .map(py_to_fuzzy_date)
                    .transpose()?
                    .unwrap_or_else(|| FuzzyDate::fromisoformat("9999-12-31").unwrap()),
                post_id: (!post_id.is_empty()).then(|| post_id.to_string()),
                on_behalf_of_id: (!on_behalf_of_id.is_empty()).then(|| on_behalf_of_id.to_string()),
                start_reason: start_reason.map(py_to_membership_reason).transpose()?,
                end_reason: end_reason.map(py_to_membership_reason).transpose()?,
                extra: None,
                identifiers: None,
                label: None,
                name: None,
                reason: None,
                source: None,
            }));

        Ok(())
    }

    fn end_membership_with_reason(
        &self,
        end_date: &Bound<'_, PyAny>,
        end_reason: &Bound<'_, PyAny>,
    ) -> PyResult<()> {
        let PersonState::Handle { shared, id } = &self.inner else {
            return Err(PyValueError::new_err("Person has no parent Popolo object"));
        };
        let end_date = py_to_fuzzy_date(end_date)?;
        let end_reason = py_to_membership_reason(end_reason)?;
        let mut guard = shared.write().unwrap();
        let resolved_id = guard
            .resolve_person_id(id)
            .ok_or_else(|| PyKeyError::new_err(id.clone()))?;
        let id = &resolved_id;
        let membership = guard
            .memberships
            .root
            .iter_mut()
            .filter_map(|item| match item {
                MembershipType::Membership(membership) if membership.person_id == *id => {
                    Some(membership)
                }
                _ => None,
            })
            .max_by_key(|membership| membership.start_date.earliest_date)
            .ok_or_else(|| PyValueError::new_err("Person has no memberships"))?;
        membership.end_date = end_date;
        membership.end_reason = Some(end_reason);
        Ok(())
    }

    #[pyo3(signature = (new_party, change_date=None, change_reason=None, source_url=None))]
    fn change_party(
        &self,
        new_party: PyRef<'_, PyOrganization>,
        change_date: Option<&Bound<'_, PyAny>>,
        change_reason: Option<&Bound<'_, PyAny>>,
        source_url: Option<String>,
    ) -> PyResult<()> {
        let change_date = match change_date {
            Some(value) => py_to_naive_date(value)?,
            None => chrono::Local::now().date_naive(),
        };
        let reason = match change_reason {
            Some(value) => py_to_membership_reason(value)?,
            None => MembershipReason::ChangedParty,
        };
        self.change_party_data(
            new_party.get_id().to_string(),
            change_date,
            reason,
            source_url,
        )
    }

    #[pyo3(signature = (change_date=None, source_url=None))]
    fn remove_whip(
        &self,
        change_date: Option<&Bound<'_, PyAny>>,
        source_url: Option<String>,
    ) -> PyResult<()> {
        let change_date = match change_date {
            Some(value) => py_to_naive_date(value)?,
            None => chrono::Local::now().date_naive(),
        };
        self.change_party_data(
            "independent".to_string(),
            change_date,
            MembershipReason::WhipRemoved,
            source_url,
        )
    }

    #[pyo3(signature = (change_date=None, source_url=None))]
    fn restore_whip(
        &self,
        change_date: Option<&Bound<'_, PyAny>>,
        source_url: Option<String>,
    ) -> PyResult<()> {
        let change_date = match change_date {
            Some(value) => py_to_naive_date(value)?,
            None => chrono::Local::now().date_naive(),
        };
        let PersonState::Handle { shared, id } = &self.inner else {
            return Err(PyValueError::new_err("Person has no parent Popolo object"));
        };
        let previous_party = {
            let guard = shared.read().unwrap();
            let resolved_id = guard
                .resolve_person_id(id)
                .ok_or_else(|| PyKeyError::new_err(id.clone()))?;
            let id = &resolved_id;
            guard
                .memberships
                .root
                .iter()
                .rev()
                .find_map(|item| match item {
                    MembershipType::Membership(membership)
                        if membership.person_id == *id
                            && membership.on_behalf_of_id.as_deref() != Some("independent") =>
                    {
                        membership.on_behalf_of_id.clone()
                    }
                    _ => None,
                })
        }
        .ok_or_else(|| PyValueError::new_err("No previous party found"))?;
        self.change_party_data(
            previous_party,
            change_date,
            MembershipReason::WhipRestored,
            source_url,
        )
    }

    #[pyo3(signature = (*, given_name=None, family_name=None, one_name=None,
        start_date=None, end_date=None))]
    fn add_alt_name(
        &mut self,
        given_name: Option<String>,
        family_name: Option<String>,
        one_name: Option<String>,
        start_date: Option<&Bound<'_, PyAny>>,
        end_date: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<()> {
        let start_date = start_date
            .map(py_to_fuzzy_date)
            .transpose()?
            .unwrap_or_else(|| FuzzyDate::fromisoformat("0001-01-01").unwrap());
        let end_date = end_date
            .map(py_to_fuzzy_date)
            .transpose()?
            .unwrap_or_else(|| FuzzyDate::fromisoformat("9999-12-31").unwrap());
        let name = match (given_name, family_name, one_name) {
            (Some(given_name), Some(family_name), _)
                if !given_name.is_empty() && !family_name.is_empty() =>
            {
                popolo_validator::NameOptions::BasicPersonName(popolo_validator::BasicPersonName {
                    end_date,
                    family_name,
                    given_name: Some(given_name),
                    honorific_prefix: None,
                    note: NameType::Alternate,
                    start_date,
                })
            }
            (None, None, Some(name)) if !name.is_empty() => {
                popolo_validator::NameOptions::AltName(popolo_validator::AltName {
                    end_date,
                    name,
                    note: NameType::Alternate,
                    organization_id: None,
                    start_date,
                })
            }
            (Some(given_name), Some(family_name), _)
                if given_name.is_empty() && family_name.is_empty() =>
            {
                return Err(PyValueError::new_err(
                    "Either one_name or given and last name must be provided",
                ));
            }
            (Some(_), None, _) | (None, Some(_), _) => {
                return Err(PyValueError::new_err(
                    "Both given and last name must be provided",
                ));
            }
            _ => {
                return Err(PyValueError::new_err(
                    "Either one_name or given and last name must be provided",
                ));
            }
        };
        self.with_person_mut(|person| person.other_names.get_or_insert_with(Vec::new).push(name));
        Ok(())
    }

    fn change_main_name(
        &mut self,
        given_name: String,
        family_name: String,
        change_date: &Bound<'_, PyAny>,
    ) -> PyResult<()> {
        let change_date = py_to_naive_date(change_date)?;
        let previous_day = change_date - Duration::days(1);
        self.with_person_mut(|person| {
            let names = person.other_names.get_or_insert_with(Vec::new);
            for name in names.iter_mut() {
                if name.note_str() == "Main" {
                    let (start, end) = name.date_range();
                    if start.earliest_date <= change_date && change_date <= end.latest_date {
                        let end_date = FuzzyDate::new(previous_day, previous_day);
                        match name {
                            popolo_validator::NameOptions::BasicPersonName(name) => {
                                name.end_date = end_date
                            }
                            popolo_validator::NameOptions::LordName(name) => {
                                name.end_date = end_date
                            }
                            popolo_validator::NameOptions::AltName(name) => {
                                name.end_date = end_date
                            }
                        }
                        break;
                    }
                }
            }
            names.push(popolo_validator::NameOptions::BasicPersonName(
                popolo_validator::BasicPersonName {
                    end_date: FuzzyDate::fromisoformat("9999-12-31").unwrap(),
                    family_name,
                    given_name: Some(given_name),
                    honorific_prefix: None,
                    note: NameType::Main,
                    start_date: FuzzyDate::new(change_date, change_date),
                },
            ));
        });
        Ok(())
    }

    #[pyo3(signature = (*, given_name, county, honorific_prefix, lordname,
        lordofname_full, change_date))]
    #[allow(clippy::too_many_arguments)]
    fn change_main_name_to_lord(
        &mut self,
        given_name: String,
        county: String,
        honorific_prefix: String,
        lordname: String,
        lordofname_full: String,
        change_date: &Bound<'_, PyAny>,
    ) -> PyResult<()> {
        let change_date = py_to_naive_date(change_date)?;
        let previous_day = change_date - Duration::days(1);
        self.with_person_mut(|person| {
            let names = person.other_names.get_or_insert_with(Vec::new);
            for name in names.iter_mut() {
                if name.note_str() == "Main" {
                    let (start, end) = name.date_range();
                    if start.earliest_date <= change_date && change_date <= end.latest_date {
                        let end_date = FuzzyDate::new(previous_day, previous_day);
                        match name {
                            popolo_validator::NameOptions::BasicPersonName(name) => {
                                name.end_date = end_date
                            }
                            popolo_validator::NameOptions::LordName(name) => {
                                name.end_date = end_date
                            }
                            popolo_validator::NameOptions::AltName(name) => {
                                name.end_date = end_date
                            }
                        }
                        break;
                    }
                }
            }
            names.push(popolo_validator::NameOptions::LordName(
                popolo_validator::LordName {
                    additional_name: None,
                    county: Some(county),
                    end_date: FuzzyDate::fromisoformat("9999-12-31").unwrap(),
                    given_name: Some(given_name),
                    honorific_prefix,
                    honorific_suffix: None,
                    lordname: Some(lordname),
                    lordofname: None,
                    lordofname_full: Some(lordofname_full),
                    note: NameType::Main,
                    start_date: FuzzyDate::new(change_date, change_date),
                    surname: None,
                },
            ));
        });
        Ok(())
    }

    fn __repr__(&self) -> String {
        format!("<Person: {}>", self.get_id())
    }
}

// Simple Person getters/setters via macro
opt_str_getter!(PyPerson, with_person, biography);
opt_str_getter!(PyPerson, with_person, gender);
opt_str_getter!(PyPerson, with_person, national_identity);
opt_str_getter!(PyPerson, with_person, summary);
opt_str_getter!(PyPerson, with_person, image);
opt_str_setter!(PyPerson, with_person_mut, biography);
opt_str_setter!(PyPerson, with_person_mut, summary);
opt_str_setter!(PyPerson, with_person_mut, gender);
opt_str_setter!(PyPerson, with_person_mut, national_identity);
opt_str_setter!(PyPerson, with_person_mut, image);
extra_methods!(PyPerson, with_person, with_person_mut, ["biography" => biography, "summary" => summary]);
standalone_model_methods!(
    PyPerson,
    PersonState,
    Standalone,
    popolo_validator::Person,
    with_person
);

#[gen_stub_pyclass]
#[pyclass(name = "PersonCollection")]
pub struct PyPersonCollection {
    shared: SharedPopolo,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyPersonCollection {
    pub(super) fn __getitem__(&self, py: Python<'_>, id: &str) -> PyResult<Py<PyPerson>> {
        let resolved_id = self
            .shared
            .read()
            .unwrap()
            .resolve_person_id(id)
            .ok_or_else(|| PyKeyError::new_err(id.to_string()))?;
        Py::new(py, PyPerson::make_handle(self.shared.clone(), resolved_id))
    }

    fn __len__(&self) -> usize {
        self.shared.read().unwrap().persons.len()
    }

    fn __contains__(&self, id: &str) -> bool {
        self.shared
            .read()
            .unwrap()
            .persons
            .id_lookup
            .contains_key(id)
    }

    #[getter]
    fn root(&self, py: Python<'_>) -> PyResult<Vec<PyObject>> {
        let ids: Vec<(String, bool)> = {
            let guard = self.shared.read().unwrap();
            guard
                .persons
                .root
                .iter()
                .map(|pt| match pt {
                    PersonType::Person(p) => (p.id.clone(), false),
                    PersonType::PersonRedirect(r) => (r.id.clone(), true),
                })
                .collect()
        };
        ids.iter()
            .map(|(id, is_redirect)| {
                if *is_redirect {
                    let guard = self.shared.read().unwrap();
                    let r = guard.persons.get(id).and_then(|pt| match pt {
                        PersonType::PersonRedirect(r) => Some(r.clone()),
                        _ => None,
                    });
                    drop(guard);
                    if let Some(r) = r {
                        let py_r = PyPersonRedirect {
                            id: r.id,
                            redirect: r.redirect,
                        };
                        Ok(Py::new(py, py_r)?.into_py(py))
                    } else {
                        Err(PyValueError::new_err("PersonRedirect not found"))
                    }
                } else {
                    let p = PyPerson::make_handle(self.shared.clone(), id.clone());
                    Ok(Py::new(py, p)?.into_py(py))
                }
            })
            .collect()
    }

    fn redirects(&self, py: Python<'_>) -> PyResult<Vec<PyObject>> {
        let redirects: Vec<_> = {
            let guard = self.shared.read().unwrap();
            guard
                .persons
                .root
                .iter()
                .filter_map(|pt| match pt {
                    PersonType::PersonRedirect(r) => Some((r.id.clone(), r.redirect.clone())),
                    _ => None,
                })
                .collect()
        };
        redirects
            .iter()
            .map(|(id, redirect)| {
                let py_r = PyPersonRedirect {
                    id: id.clone(),
                    redirect: redirect.clone(),
                };
                Ok(Py::new(py, py_r)?.into_py(py))
            })
            .collect()
    }

    fn merge_people(&self, target_id: &str, source_id: &str) -> PyResult<()> {
        if target_id == source_id {
            return Err(PyValueError::new_err("Cannot merge a person into itself"));
        }
        let mut guard = self.shared.write().unwrap();
        let target_index = guard
            .persons
            .id_lookup
            .get(target_id)
            .copied()
            .ok_or_else(|| PyKeyError::new_err(target_id.to_string()))?;
        let source_index = guard
            .persons
            .id_lookup
            .get(source_id)
            .copied()
            .ok_or_else(|| PyKeyError::new_err(source_id.to_string()))?;
        let source = match &guard.persons.root[source_index] {
            PersonType::Person(person) => person.clone(),
            PersonType::PersonRedirect(_) => {
                return Err(PyValueError::new_err("Source person is already a redirect"));
            }
        };
        let target = match &mut guard.persons.root[target_index] {
            PersonType::Person(person) => person,
            PersonType::PersonRedirect(_) => {
                return Err(PyValueError::new_err("Target person is a redirect"));
            }
        };
        if let Some(source_identifiers) = source.identifiers {
            let target_identifiers = target.identifiers.get_or_insert_with(Vec::new);
            for identifier in source_identifiers {
                if !target_identifiers.iter().any(|existing| {
                    existing.scheme == identifier.scheme
                        && existing.identifier.as_str() == identifier.identifier.as_str()
                }) {
                    target_identifiers.push(identifier);
                }
            }
        }
        if let Some(source_names) = source.other_names {
            target
                .other_names
                .get_or_insert_with(Vec::new)
                .extend(source_names);
        }
        for membership in &mut guard.memberships.root {
            if let MembershipType::Membership(membership) = membership {
                if membership.person_id == source_id {
                    membership.person_id = target_id.to_string();
                }
            }
        }
        guard
            .persons
            .replace(
                source_index,
                PersonType::PersonRedirect(popolo_validator::PersonRedirect {
                    id: source_id.to_string(),
                    redirect: target_id.to_string(),
                }),
            )
            .expect("source index was resolved under this write guard");

        Ok(())
    }

    fn from_identifier(
        &self,
        py: Python<'_>,
        identifier: &str,
        scheme: &str,
    ) -> PyResult<Py<PyPerson>> {
        let person_id = {
            let guard = self.shared.read().unwrap();
            guard.find_person_id_by_identifier(identifier, scheme)
        };
        match person_id {
            Some(pid) => self.__getitem__(py, &pid),
            None => Err(PyKeyError::new_err(format!(
                "No person found with {}:{}",
                scheme, identifier
            ))),
        }
    }

    #[pyo3(signature = (name, chamber_id, date, include_historical_names=None))]
    fn from_name(
        &self,
        py: Python<'_>,
        name: &str,
        chamber_id: &str,
        date: &Bound<'_, PyAny>,
        include_historical_names: Option<bool>,
    ) -> PyResult<Option<Py<PyPerson>>> {
        let d = py_to_naive_date(date)?;
        let person_id = {
            let guard = self.shared.read().unwrap();
            guard
                .find_person_id_by_name(
                    name,
                    chamber_id,
                    d,
                    include_historical_names.unwrap_or(false),
                )
                .map_err(PyValueError::new_err)?
        };
        match person_id {
            Some(pid) => Ok(Some(self.__getitem__(py, &pid)?)),
            None => Ok(None),
        }
    }

    fn append(&self, _py: Python<'_>, item: &Bound<'_, PyAny>) -> PyResult<()> {
        if let Ok(p) = item.extract::<PyRef<PyPerson>>() {
            let pid = p.get_id().to_string();
            let person_data = p.get_standalone().cloned();
            drop(p);
            let mut guard = self.shared.write().unwrap();
            if guard.persons.id_lookup.contains_key(&pid) {
                return Err(PyValueError::new_err(format!(
                    "Duplicate Person id {:?}",
                    pid
                )));
            }
            let data = person_data.unwrap_or(popolo_validator::Person {
                id: pid,
                biography: None,
                birth_date: None,
                death_date: None,
                extra: None,
                gender: None,
                identifiers: None,
                image: None,
                links: None,
                other_names: None,
                national_identity: None,
                summary: None,
                shortcuts: None,
            });
            guard.persons.push(PersonType::Person(data));
        } else if let Ok(r) = item.extract::<PyRef<PyPersonRedirect>>() {
            let rid = r.id.clone();
            let redirect = r.redirect.clone();
            drop(r);
            let mut guard = self.shared.write().unwrap();
            if guard.persons.id_lookup.contains_key(&rid) {
                return Err(PyValueError::new_err(format!(
                    "Duplicate PersonRedirect id {:?}",
                    rid
                )));
            }
            guard.persons.push(PersonType::PersonRedirect(
                popolo_validator::PersonRedirect { id: rid, redirect },
            ));
        } else {
            return Err(PyValueError::new_err(
                "append() expects Person or PersonRedirect",
            ));
        }
        Ok(())
    }
}

impl PyPersonCollection {
    pub(super) fn new(shared: SharedPopolo) -> Self {
        Self { shared }
    }
}
