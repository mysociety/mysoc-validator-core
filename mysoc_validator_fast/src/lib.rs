use chrono::NaiveDate;
use fuzzy_date::FuzzyDate;
use popolo_validator::{MembershipReason, MembershipType, OrgType, PersonType, Popolo};
use pyo3::exceptions::{PyKeyError, PyValueError};
use pyo3::prelude::*;
use pyo3::types::PyType;
use pyo3_stub_gen::define_stub_info_gatherer;
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pymethods};
use std::sync::{Arc, RwLock};

mod interests;

// ---------------------------------------------------------------------------
// Shared state
// ---------------------------------------------------------------------------

type SharedPopolo = Arc<RwLock<Popolo>>;

pub(crate) fn escape_non_ascii(s: String) -> String {
    let mut result = String::with_capacity(s.len() + 64);
    for c in s.chars() {
        if (c as u32) > 127 {
            result.push_str(&format!("\\u{:04x}", c as u32));
        } else {
            result.push(c);
        }
    }
    result
}

// ---------------------------------------------------------------------------
// Date / reason helpers
// ---------------------------------------------------------------------------

fn py_to_naive_date(obj: &Bound<'_, PyAny>) -> PyResult<NaiveDate> {
    if let Ok(d) = obj.extract::<NaiveDate>() {
        return Ok(d);
    }
    if let Ok(fd) = obj.extract::<FuzzyDate>() {
        return Ok(fd.earliest_date);
    }
    if let Ok(s) = obj.extract::<String>() {
        return NaiveDate::parse_from_str(&s, "%Y-%m-%d")
            .map_err(|e| PyValueError::new_err(format!("Invalid date string: {}", e)));
    }
    let year: i32 = obj.getattr("year")?.extract()?;
    let month: u32 = obj.getattr("month")?.extract()?;
    let day: u32 = obj.getattr("day")?.extract()?;
    NaiveDate::from_ymd_opt(year, month, day).ok_or_else(|| PyValueError::new_err("Invalid date"))
}

fn py_to_fuzzy_date(obj: &Bound<'_, PyAny>) -> PyResult<FuzzyDate> {
    if let Ok(fd) = obj.extract::<FuzzyDate>() {
        return Ok(fd);
    }
    let d = py_to_naive_date(obj)?;
    Ok(FuzzyDate::new(d, d))
}

fn fuzzy_to_py(py: Python<'_>, fd: &FuzzyDate) -> PyResult<Py<FuzzyDate>> {
    Py::new(py, fd.clone())
}

fn membership_reason_to_str(r: &MembershipReason) -> String {
    r.as_str().to_string()
}

fn str_to_membership_reason(s: &str) -> PyResult<MembershipReason> {
    MembershipReason::from_str(s)
        .ok_or_else(|| PyValueError::new_err(format!("Unknown MembershipReason: {}", s)))
}

fn py_to_membership_reason(obj: &Bound<'_, PyAny>) -> PyResult<MembershipReason> {
    let s: String = obj.str()?.extract()?;
    str_to_membership_reason(&s)
}

// ---------------------------------------------------------------------------
// Macros — each generates a standalone #[pymethods] impl block so they
// expand *before* the proc macro sees them, avoiding macro-inside-proc-macro
// ordering issues.
// ---------------------------------------------------------------------------

/// Generate a `String` getter for `$field` using `$accessor`.
macro_rules! str_getter {
    ($struct:ty, $accessor:ident, $field:ident) => {
        #[pyo3_stub_gen::derive::gen_stub_pymethods]
        #[pymethods]
        impl $struct {
            #[getter]
            fn $field(&self) -> String {
                self.$accessor(|inner| inner.$field.clone())
            }
        }
    };
}

/// Generate an `Option<String>` getter for `$field` using `$accessor`.
macro_rules! opt_str_getter {
    ($struct:ty, $accessor:ident, $field:ident) => {
        #[pyo3_stub_gen::derive::gen_stub_pymethods]
        #[pymethods]
        impl $struct {
            #[getter]
            fn $field(&self) -> Option<String> {
                self.$accessor(|inner| inner.$field.clone())
            }
        }
    };
}

/// Generate a `String` setter for `$field` using `$accessor`.
macro_rules! str_setter {
    ($struct:ty, $accessor:ident, $field:ident) => {
        ::paste::paste! {
            #[pyo3_stub_gen::derive::gen_stub_pymethods]
            #[pymethods]
            impl $struct {
                #[setter($field)]
                fn [<set_ $field>](&mut self, value: String) {
                    self.$accessor(|inner| inner.$field = value);
                }
            }
        }
    };
}

/// Generate an `Option<String>` setter for `$field` using `$accessor`.
macro_rules! opt_str_setter {
    ($struct:ty, $accessor:ident, $field:ident) => {
        ::paste::paste! {
            #[pyo3_stub_gen::derive::gen_stub_pymethods]
            #[pymethods]
            impl $struct {
                #[setter($field)]
                fn [<set_ $field>](&mut self, value: Option<String>) {
                    self.$accessor(|inner| inner.$field = value);
                }
            }
        }
    };
}

// ---------------------------------------------------------------------------
// MembershipRedirect
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// PersonRedirect
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Membership — two-state: Standalone (user-created) or Handle (in Popolo)
// ---------------------------------------------------------------------------

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
        identifiers = None, name = None, reason = None
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

    fn __repr__(&self) -> String {
        format!("<Membership: {}>", self.with_membership(|m| m.id.clone()))
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

// ---------------------------------------------------------------------------
// Person — standalone or handle
// ---------------------------------------------------------------------------

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

    fn with_person<R>(&self, f: impl FnOnce(&popolo_validator::Person) -> R) -> R {
        match &self.inner {
            PersonState::Standalone(p) => f(p),
            PersonState::Handle { shared, id } => {
                let guard = shared.read().unwrap();
                let resolved_id = match guard.persons.get(id) {
                    Some(PersonType::PersonRedirect(r)) => r.redirect.clone(),
                    _ => id.clone(),
                };
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
                let id = id.clone();
                let mut guard = shared.write().unwrap();
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
}

#[gen_stub_pymethods]
#[pymethods]
impl PyPerson {
    #[new]
    #[pyo3(signature = (id, *, biography=None, summary=None, gender=None,
                        national_identity=None, image=None))]
    fn py_new(
        id: String,
        biography: Option<String>,
        summary: Option<String>,
        gender: Option<String>,
        national_identity: Option<String>,
        image: Option<String>,
    ) -> Self {
        PyPerson {
            inner: PersonState::Standalone(popolo_validator::Person {
                id,
                biography,
                summary,
                gender,
                national_identity,
                image,
                birth_date: None,
                death_date: None,
                identifiers: None,
                links: None,
                other_names: None,
                shortcuts: None,
            }),
        }
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

    fn memberships(&self, py: Python<'_>) -> PyResult<Vec<Py<PyMembership>>> {
        let (ids, shared) = match &self.inner {
            PersonState::Standalone(_) => {
                return Err(PyValueError::new_err(
                    "Cannot call memberships() on a standalone Person not attached to a Popolo",
                ))
            }
            PersonState::Handle { shared, id } => {
                let guard = shared.read().unwrap();
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

// ---------------------------------------------------------------------------
// Organization — standalone or handle
// ---------------------------------------------------------------------------

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

    fn with_org<R>(&self, f: impl FnOnce(&popolo_validator::Organization) -> R) -> R {
        match &self.inner {
            OrganizationState::Standalone(o) => f(o),
            OrganizationState::Handle { shared, id } => {
                let guard = shared.read().unwrap();
                let org = guard.organizations.get(id).expect("Organization not found");
                f(org)
            }
        }
    }

    fn with_org_mut<R>(&mut self, f: impl FnOnce(&mut popolo_validator::Organization) -> R) -> R {
        match &mut self.inner {
            OrganizationState::Standalone(o) => f(o),
            OrganizationState::Handle { shared, id } => {
                let id = id.clone();
                let mut guard = shared.write().unwrap();
                let org = guard
                    .organizations
                    .iter_mut()
                    .find(|o| o.id == id)
                    .expect("Organization not found");
                f(org)
            }
        }
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyOrganization {
    #[new]
    #[pyo3(signature = (id, name = String::new(), *, classification = None))]
    fn py_new(id: String, name: String, classification: Option<String>) -> Self {
        let org_type = classification.and_then(|c| match c.as_str() {
            "party" => Some(OrgType::Party),
            "chamber" => Some(OrgType::Chamber),
            "metro" => Some(OrgType::Metro),
            _ => None,
        });
        PyOrganization {
            inner: OrganizationState::Standalone(popolo_validator::Organization {
                id,
                name,
                classification: org_type,
                identifiers: None,
            }),
        }
    }

    #[getter]
    fn id(&self) -> &str {
        self.get_id()
    }

    #[getter]
    fn classification(&self) -> Option<String> {
        self.with_org(|o| {
            o.classification.as_ref().map(|c| match c {
                OrgType::Party => "party".to_string(),
                OrgType::Chamber => "chamber".to_string(),
                OrgType::Metro => "metro".to_string(),
            })
        })
    }

    fn __repr__(&self) -> String {
        format!("<Organization: {}>", self.get_id())
    }
}

str_getter!(PyOrganization, with_org, name);
str_setter!(PyOrganization, with_org_mut, name);

// ---------------------------------------------------------------------------
// Post — standalone or handle
// ---------------------------------------------------------------------------

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
                        organization_id = String::new()))]
    fn py_new(id: String, label: String, role: String, organization_id: String) -> Self {
        PyPost {
            inner: PostState::Standalone(popolo_validator::Post {
                id,
                label,
                role,
                organization_id,
                area: None,
                identifiers: None,
                start_date: fuzzy_date::FuzzyDate::fromisoformat("0001-01-01").unwrap(),
                end_date: fuzzy_date::FuzzyDate::fromisoformat("9999-12-31").unwrap(),
            }),
        }
    }

    #[getter]
    fn id(&self) -> &str {
        self.get_id()
    }

    #[getter]
    fn area_name(&self) -> Option<String> {
        self.with_post(|p| p.area.as_ref().map(|a| a.name.clone()))
    }

    fn __repr__(&self) -> String {
        format!("<Post: {}>", self.get_id())
    }
}

str_getter!(PyPost, with_post, label);
str_getter!(PyPost, with_post, role);
str_getter!(PyPost, with_post, organization_id);
str_setter!(PyPost, with_post_mut, label);

// ---------------------------------------------------------------------------
// MembershipCollection
// ---------------------------------------------------------------------------

#[gen_stub_pyclass]
#[pyclass(name = "MembershipCollection")]
pub struct PyMembershipCollection {
    shared: SharedPopolo,
}

impl PyMembershipCollection {
    fn validate_new_membership(&self, membership: &popolo_validator::Membership) -> PyResult<()> {
        let guard = self.shared.read().unwrap();

        if guard.memberships.id_lookup.contains_key(&membership.id) {
            return Err(PyValueError::new_err(format!(
                "Duplicate Membership id {:?}",
                membership.id
            )));
        }

        let member_re = regex::Regex::new(r".*/-?\d+$").unwrap();
        if !member_re.is_match(&membership.id) {
            return Err(PyValueError::new_err(format!(
                "Invalid Membership id format: {}",
                membership.id
            )));
        }

        if !guard.persons.id_lookup.contains_key(&membership.person_id) {
            return Err(PyValueError::new_err(format!(
                "Membership {} refers to unknown person_id {}",
                membership.id, membership.person_id
            )));
        }

        if membership.post_id.is_some() || membership.organization_id.is_some() {
            for mt in guard.memberships.iter() {
                if let MembershipType::Membership(existing) = mt {
                    if existing.person_id != membership.person_id {
                        continue;
                    }
                    if existing.post_id != membership.post_id {
                        continue;
                    }
                    let new_start = &membership.start_date;
                    let new_end = &membership.end_date;
                    let ex_start = &existing.start_date;
                    let ex_end = &existing.end_date;
                    if new_start.earliest_date <= ex_end.latest_date
                        && new_end.latest_date >= ex_start.earliest_date
                    {
                        return Err(PyValueError::new_err(format!(
                            "New Membership {} overlaps with membership {}",
                            membership.id, existing.id
                        )));
                    }
                }
            }
        }
        Ok(())
    }

    fn validate_new_redirect(&self, redirect: &PyMembershipRedirect) -> PyResult<()> {
        let guard = self.shared.read().unwrap();
        if guard.memberships.id_lookup.contains_key(&redirect.id) {
            return Err(PyValueError::new_err(format!(
                "Duplicate MembershipRedirect id {:?}",
                redirect.id
            )));
        }
        Ok(())
    }
}

#[gen_stub_pymethods]
#[pymethods]
impl PyMembershipCollection {
    fn __getitem__(&self, py: Python<'_>, id: &str) -> PyResult<Py<PyMembership>> {
        let guard = self.shared.read().unwrap();
        match guard.memberships.get(id) {
            Some(MembershipType::Membership(_)) => {
                let mid = id.to_string();
                drop(guard);
                let m = PyMembership::make_handle(self.shared.clone(), mid);
                Py::new(py, m)
            }
            Some(MembershipType::MembershipRedirect(r)) => {
                let redir = r.redirect.clone();
                drop(guard);
                self.__getitem__(py, &redir)
            }
            None => Err(PyKeyError::new_err(id.to_string())),
        }
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
                    self.validate_new_membership(standalone)?;
                    let m_clone = standalone.clone();
                    drop(m);
                    let mut guard = self.shared.write().unwrap();
                    guard.memberships.push(MembershipType::Membership(m_clone));
                    guard.memberships.set_lookup();
                } else {
                    return Err(PyValueError::new_err(
                        "Cannot extend with a Membership that is already part of a Popolo",
                    ));
                }
            } else if let Ok(r) = bound.extract::<PyRef<PyMembershipRedirect>>() {
                self.validate_new_redirect(&r)?;
                let id = r.id.clone();
                let redirect = r.redirect.clone();
                drop(r);
                let mut guard = self.shared.write().unwrap();
                guard.memberships.push(MembershipType::MembershipRedirect(
                    popolo_validator::MembershipRedirect { id, redirect },
                ));
                guard.memberships.set_lookup();
            } else {
                return Err(PyValueError::new_err(
                    "extend() expects Membership or MembershipRedirect objects",
                ));
            }
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// PersonCollection
// ---------------------------------------------------------------------------

#[gen_stub_pyclass]
#[pyclass(name = "PersonCollection")]
pub struct PyPersonCollection {
    shared: SharedPopolo,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyPersonCollection {
    fn __getitem__(&self, py: Python<'_>, id: &str) -> PyResult<Py<PyPerson>> {
        let guard = self.shared.read().unwrap();
        match guard.persons.get(id) {
            Some(PersonType::Person(_)) => {
                let pid = id.to_string();
                drop(guard);
                let p = PyPerson::make_handle(self.shared.clone(), pid);
                Py::new(py, p)
            }
            Some(PersonType::PersonRedirect(r)) => {
                let redir = r.redirect.clone();
                drop(guard);
                self.__getitem__(py, &redir)
            }
            None => Err(PyKeyError::new_err(id.to_string())),
        }
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

    fn from_name(
        &self,
        py: Python<'_>,
        name: &str,
        chamber_id: &str,
        date: &Bound<'_, PyAny>,
    ) -> PyResult<Option<Py<PyPerson>>> {
        let d = py_to_naive_date(date)?;
        let person_id = {
            let guard = self.shared.read().unwrap();
            guard.find_person_id_by_name(name, chamber_id, d)
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
            guard.persons.set_lookup();
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
            guard.persons.set_lookup();
        } else {
            return Err(PyValueError::new_err(
                "append() expects Person or PersonRedirect",
            ));
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// OrganizationCollection
// ---------------------------------------------------------------------------

#[gen_stub_pyclass]
#[pyclass(name = "OrganizationCollection")]
pub struct PyOrganizationCollection {
    shared: SharedPopolo,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyOrganizationCollection {
    fn __getitem__(&self, py: Python<'_>, id: &str) -> PyResult<Py<PyOrganization>> {
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
            });
            guard.organizations.push(data);
            guard.organizations.set_lookup();
        } else {
            return Err(PyValueError::new_err("append() expects an Organization"));
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// PostCollection
// ---------------------------------------------------------------------------

#[gen_stub_pyclass]
#[pyclass(name = "PostCollection")]
pub struct PyPostCollection {
    shared: SharedPopolo,
}

#[gen_stub_pymethods]
#[pymethods]
impl PyPostCollection {
    fn __getitem__(&self, py: Python<'_>, id: &str) -> PyResult<Py<PyPost>> {
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
            });
            guard.posts.push(data);
            guard.posts.set_lookup();
        } else {
            return Err(PyValueError::new_err("append() expects a Post"));
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Popolo — top-level wrapper
// ---------------------------------------------------------------------------

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

    #[getter]
    fn persons(&self) -> PyPersonCollection {
        PyPersonCollection {
            shared: self.shared.clone(),
        }
    }

    #[getter]
    fn memberships(&self) -> PyMembershipCollection {
        PyMembershipCollection {
            shared: self.shared.clone(),
        }
    }

    #[getter]
    fn organizations(&self) -> PyOrganizationCollection {
        PyOrganizationCollection {
            shared: self.shared.clone(),
        }
    }

    #[getter]
    fn posts(&self) -> PyPostCollection {
        PyPostCollection {
            shared: self.shared.clone(),
        }
    }

    fn find_person_id_by_name(
        &self,
        name: &str,
        chamber_id: &str,
        date_str: &str,
    ) -> PyResult<Option<String>> {
        let d = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
            .map_err(|e| PyValueError::new_err(format!("Invalid date: {}", e)))?;
        Ok(self
            .shared
            .read()
            .unwrap()
            .find_person_id_by_name(name, chamber_id, d))
    }

    fn find_person_id_by_identifier(&self, identifier: &str, scheme: &str) -> Option<String> {
        self.shared
            .read()
            .unwrap()
            .find_person_id_by_identifier(identifier, scheme)
    }
}

// ---------------------------------------------------------------------------
// Module
// ---------------------------------------------------------------------------

#[pymodule]
fn _mysoc_validator_fast(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<FuzzyDate>()?;
    m.add_class::<PyPopolo>()?;
    m.add_class::<PyMembership>()?;
    m.add_class::<PyMembershipRedirect>()?;
    m.add_class::<PyPerson>()?;
    m.add_class::<PyPersonRedirect>()?;
    m.add_class::<PyOrganization>()?;
    m.add_class::<PyPost>()?;
    m.add_class::<PyMembershipCollection>()?;
    m.add_class::<PyPersonCollection>()?;
    m.add_class::<PyOrganizationCollection>()?;
    m.add_class::<PyPostCollection>()?;
    m.add_class::<interests::PyRegmemRegister>()?;
    m.add_class::<interests::PyRegmemPerson>()?;
    m.add_class::<interests::PyRegmemCategory>()?;
    m.add_class::<interests::PyRegmemEntry>()?;
    m.add_class::<interests::PyRegmemSummary>()?;
    m.add_class::<interests::PyRegmemDetailGroup>()?;
    m.add_class::<interests::PyRegmemDetail>()?;
    m.add_class::<interests::PyRegmemAnnotation>()?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Stub info gatherer (used by the stub_gen binary)
// ---------------------------------------------------------------------------

define_stub_info_gatherer!(stub_info);
