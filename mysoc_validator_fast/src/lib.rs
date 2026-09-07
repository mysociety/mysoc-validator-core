//! Python extension registration. Domain-specific wrappers live in their modules.
use pyo3::prelude::*;
use pyo3_stub_gen::define_stub_info_gatherer;

mod conversions;
mod interests;
mod popolo;
mod transcripts;

pub use popolo::{
    PyAltName, PyArea, PyBasicPersonName, PyExtra, PyLordName, PyMembership,
    PyMembershipCollection, PyMembershipRedirect, PyOrganization, PyOrganizationCollection,
    PyPerson, PyPersonCollection, PyPersonRedirect, PyPopolo, PyPost, PyPostCollection,
};

#[pymodule]
fn _mysoc_validator_fast(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<fuzzy_date::FuzzyDate>()?;
    popolo::register_popolo_classes(m)?;
    interests::register_interests_classes(m)?;
    transcripts::register_transcript_classes(m)?;
    Ok(())
}

define_stub_info_gatherer!(stub_info);
