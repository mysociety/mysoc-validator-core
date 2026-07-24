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

from .consts import Chamber, CommonKey, IdentifierScheme, MembershipReason
from .dates import ApproxDate, FixedDate
from .interests import (
    RegmemAnnotation,
    RegmemCategory,
    RegmemDetail,
    RegmemDetailGroup,
    RegmemEntry,
    RegmemPerson,
    RegmemRegister,
    RegmemSummary,
)
from .popolo import (
    Membership,
    MembershipRedirect,
    Organization,
    Person,
    PersonRedirect,
    Popolo,
    Post,
)

__all__ = [
    "ApproxDate",
    "Chamber",
    "CommonKey",
    "FixedDate",
    "IdentifierScheme",
    "Membership",
    "MembershipReason",
    "MembershipRedirect",
    "Organization",
    "Person",
    "PersonRedirect",
    "Popolo",
    "Post",
    "RegmemAnnotation",
    "RegmemCategory",
    "RegmemDetail",
    "RegmemDetailGroup",
    "RegmemEntry",
    "RegmemPerson",
    "RegmemRegister",
    "RegmemSummary",
]
