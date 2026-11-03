"""
mysoc_validator_fast — fast, Rust-backed Popolo data for Python.

All data is stored in a Rust Arc<RwLock<Popolo>>; Python objects are thin
handles that read/write directly to Rust.  Loading is fast because Rust
parses and validates JSON; Python just provides a Pythonic API on top.

Typical usage::

    from mysoc_validator_fast import Popolo
    popolo = Popolo.from_parlparse()

    person = popolo.persons["uk.org.publicwhip/person/10001"]
    print(person.names_on_date(date.today()))
"""

from __future__ import annotations

from .consts import Chamber, CommonKey, IdentifierScheme, MembershipReason
from .dates import ApproxDate, FixedDate
from .interests import (
    EntryIterator,
    RegmemAnnotation,
    RegmemCategory,
    RegmemDetail,
    RegmemDetailContainer,
    RegmemDetailGroup,
    RegmemEntry,
    RegmemPerson,
    RegmemRegister,
    RegmemSummary,
    slugify,
)
from .popolo import (
    AltName,
    Area,
    BasicPersonName,
    LordName,
    Membership,
    MembershipRedirect,
    Organization,
    Person,
    PersonRedirect,
    Popolo,
    Post,
)
from .transcripts import (
    Agreement,
    Division,
    DivisionCount,
    GIDRedirect,
    HeaderSpeechTuple,
    MajorHeading,
    MinorHeading,
    Motion,
    MSPName,
    OralHeading,
    Question,
    RepList,
    Reply,
    RepName,
    Source,
    Speech,
    SpeechItem,
    Transcript,
    TranscriptFileResult,
    TranscriptGroup,
)

__all__ = [
    "Agreement",
    "AltName",
    "ApproxDate",
    "Area",
    "BasicPersonName",
    "Chamber",
    "CommonKey",
    "Division",
    "DivisionCount",
    "EntryIterator",
    "FixedDate",
    "GIDRedirect",
    "HeaderSpeechTuple",
    "IdentifierScheme",
    "LordName",
    "MSPName",
    "MajorHeading",
    "Membership",
    "MembershipReason",
    "MembershipRedirect",
    "MinorHeading",
    "Motion",
    "OralHeading",
    "Organization",
    "Person",
    "PersonRedirect",
    "Popolo",
    "Post",
    "Question",
    "RegmemAnnotation",
    "RegmemCategory",
    "RegmemDetail",
    "RegmemDetailContainer",
    "RegmemDetailGroup",
    "RegmemEntry",
    "RegmemPerson",
    "RegmemRegister",
    "RegmemSummary",
    "RepList",
    "RepName",
    "Reply",
    "Source",
    "Speech",
    "SpeechItem",
    "Transcript",
    "TranscriptFileResult",
    "TranscriptGroup",
    "slugify",
]
