//! Python extension registration. Domain-specific wrappers live in their modules.
use pyo3::prelude::*;

mod conversions;
mod interests;
mod popolo;
mod transcripts;

pub use popolo::{
    PyAltName, PyArea, PyBasicPersonName, PyExtra, PyLordName, PyMembership,
    PyMembershipCollection, PyMembershipRedirect, PyOrganization, PyOrganizationCollection,
    PyPerson, PyPersonCollection, PyPersonRedirect, PyPopolo, PyPost, PyPostCollection,
};

#[pymodule(gil_used = true)]
mod _mysoc_validator_fast {
    #[pymodule_export]
    use fuzzy_date::FuzzyDate;

    #[pymodule_export]
    use super::popolo::{
        PyAltName, PyArea, PyBasicPersonName, PyExtra, PyLordName, PyMembership,
        PyMembershipCollection, PyMembershipRedirect, PyOrganization, PyOrganizationCollection,
        PyPerson, PyPersonCollection, PyPersonRedirect, PyPopolo, PyPost, PyPostCollection,
    };

    #[pymodule_export]
    use super::interests::{
        PyRegmemAnnotation, PyRegmemCategory, PyRegmemDetail, PyRegmemDetailGroup, PyRegmemEntry,
        PyRegmemPerson, PyRegmemRegister, PyRegmemSummary,
    };

    #[pymodule_export]
    use super::transcripts::{
        validate_transcript_folder, PyAgreement, PyDivision, PyDivisionCount, PyGidRedirect,
        PyMajorHeading, PyMinorHeading, PyMotion, PyMspName, PyOralHeading, PyQuestion, PyRepList,
        PyRepName, PyReply, PySource, PySpeech, PySpeechItem, PyTranscript,
    };
}
