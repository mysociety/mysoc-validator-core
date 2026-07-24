"""
Type stubs for the _popolo_validator_python Rust extension module.

All types here live in Rust; this file describes their Python-visible interface.
"""

from __future__ import annotations

from datetime import date, timedelta
from decimal import Decimal
from typing import Any, Iterator, List, Optional, Tuple, Union


# ---------------------------------------------------------------------------
# FuzzyDate (re-exported from fuzzy_date crate)
# ---------------------------------------------------------------------------

class FuzzyDate:
    """A date that may have partial precision (year-only, year-month, or full date)."""

    earliest_date: date
    latest_date: date

    def __init__(self, earliest_date: date, latest_date: date) -> None: ...
    @classmethod
    def fromisoformat(cls, iso8601_date_string: str) -> "FuzzyDate": ...
    def isoformat(self) -> str: ...
    def __str__(self) -> str: ...
    def __repr__(self) -> str: ...
    def __eq__(self, other: object) -> bool: ...
    def __lt__(self, other: object) -> bool: ...
    def __gt__(self, other: object) -> bool: ...
    def __le__(self, other: object) -> bool: ...
    def __ge__(self, other: object) -> bool: ...
    def __add__(self, delta: timedelta) -> date:
        """Add a timedelta and return a plain Python date (from earliest_date)."""
        ...


# ---------------------------------------------------------------------------
# Redirects
# ---------------------------------------------------------------------------

class MembershipRedirect:
    id: str
    redirect: str

    def __init__(self, id: str, redirect: str) -> None: ...
    def __repr__(self) -> str: ...


class PersonRedirect:
    id: str
    redirect: str

    def __init__(self, id: str, redirect: str) -> None: ...
    def __repr__(self) -> str: ...


# ---------------------------------------------------------------------------
# Membership
# ---------------------------------------------------------------------------

class Membership:
    """
    A timed connection between a person and a post or organisation.

    When retrieved from a Popolo collection the object is a live handle into
    the Rust data structure — reads and writes go directly to Rust.

    When constructed with keyword arguments it is a standalone object that can
    be validated and added to a collection via :meth:`MembershipCollection.extend`.
    """

    def __init__(
        self,
        id: str,
        person_id: str,
        *,
        start_date: Optional[date | FuzzyDate | str] = None,
        end_date: Optional[date | FuzzyDate | str] = None,
        post_id: Optional[str] = None,
        organization_id: Optional[str] = None,
        on_behalf_of_id: Optional[str] = None,
        role: Optional[str] = None,
        label: Optional[str] = None,
        source: Optional[str] = None,
        start_reason: Optional[str] = None,
        end_reason: Optional[str] = None,
        identifiers: Optional[object] = None,
        name: Optional[object] = None,
        reason: Optional[str] = None,
    ) -> None: ...

    @property
    def id(self) -> str: ...
    @property
    def person_id(self) -> str: ...
    @person_id.setter
    def person_id(self, value: str) -> None: ...
    @property
    def post_id(self) -> Optional[str]: ...
    @property
    def organization_id(self) -> Optional[str]: ...
    @organization_id.setter
    def organization_id(self, value: Optional[str]) -> None: ...
    @property
    def on_behalf_of_id(self) -> Optional[str]: ...
    @on_behalf_of_id.setter
    def on_behalf_of_id(self, value: Optional[str]) -> None: ...
    @property
    def role(self) -> Optional[str]: ...
    @role.setter
    def role(self, value: Optional[str]) -> None: ...
    @property
    def label(self) -> Optional[str]: ...
    @label.setter
    def label(self, value: Optional[str]) -> None: ...
    @property
    def source(self) -> Optional[str]: ...
    @property
    def start_date(self) -> FuzzyDate: ...
    @start_date.setter
    def start_date(self, value: date | FuzzyDate | str) -> None: ...
    @property
    def end_date(self) -> FuzzyDate: ...
    @end_date.setter
    def end_date(self, value: date | FuzzyDate | str) -> None: ...
    @property
    def start_reason(self) -> str:
        """Snake_case string, e.g. 'election', '' for blank."""
        ...
    @start_reason.setter
    def start_reason(self, value: str) -> None: ...
    @property
    def end_reason(self) -> Optional[str]: ...
    @end_reason.setter
    def end_reason(self, value: str) -> None: ...
    def __repr__(self) -> str: ...


# ---------------------------------------------------------------------------
# Person
# ---------------------------------------------------------------------------

class Person:
    """
    A person who has held office.

    Can be constructed standalone for appending to a collection, or retrieved
    as a live handle into the Rust data structure via a PersonCollection.
    """

    def __init__(
        self,
        id: str,
        *,
        biography: Optional[str] = None,
        summary: Optional[str] = None,
        gender: Optional[str] = None,
        national_identity: Optional[str] = None,
        image: Optional[str] = None,
    ) -> None: ...

    @property
    def id(self) -> str: ...
    @property
    def biography(self) -> Optional[str]: ...
    @biography.setter
    def biography(self, value: Optional[str]) -> None: ...
    @property
    def birth_date(self) -> Optional[FuzzyDate]: ...
    @property
    def death_date(self) -> Optional[FuzzyDate]: ...
    @property
    def gender(self) -> Optional[str]: ...
    @gender.setter
    def gender(self, value: Optional[str]) -> None: ...
    @property
    def national_identity(self) -> Optional[str]: ...
    @national_identity.setter
    def national_identity(self, value: Optional[str]) -> None: ...
    @property
    def summary(self) -> Optional[str]: ...
    @summary.setter
    def summary(self, value: Optional[str]) -> None: ...
    @property
    def image(self) -> Optional[str]: ...
    @image.setter
    def image(self, value: Optional[str]) -> None: ...

    def names_on_date(self, date: date | FuzzyDate | str) -> List[str]:
        """Return all name strings active on the given date."""
        ...

    def get_identifier(self, scheme: str) -> Optional[str]:
        """Return the identifier value for the given scheme, or None."""
        ...

    def memberships(self) -> List[Membership]:
        """Return all memberships for this person."""
        ...

    def __repr__(self) -> str: ...


# ---------------------------------------------------------------------------
# Organisation
# ---------------------------------------------------------------------------

class Organization:
    def __init__(
        self,
        id: str,
        name: str = "",
        *,
        classification: Optional[str] = None,
    ) -> None: ...

    @property
    def id(self) -> str: ...
    @property
    def name(self) -> str: ...
    @name.setter
    def name(self, value: str) -> None: ...
    @property
    def classification(self) -> Optional[str]:
        """'party', 'chamber', or 'metro'."""
        ...
    def __repr__(self) -> str: ...


# ---------------------------------------------------------------------------
# Post
# ---------------------------------------------------------------------------

class Post:
    def __init__(
        self,
        id: str,
        label: str = "",
        role: str = "",
        organization_id: str = "",
    ) -> None: ...

    @property
    def id(self) -> str: ...
    @property
    def label(self) -> str: ...
    @label.setter
    def label(self, value: str) -> None: ...
    @property
    def role(self) -> str: ...
    @property
    def organization_id(self) -> str: ...
    @property
    def area_name(self) -> Optional[str]: ...
    def __repr__(self) -> str: ...


# ---------------------------------------------------------------------------
# Collections
# ---------------------------------------------------------------------------

class MembershipCollection:
    """Collection of Membership and MembershipRedirect handles."""

    @property
    def root(self) -> List[Membership | MembershipRedirect]:
        """All items in insertion order."""
        ...

    def __getitem__(self, id: str) -> Membership:
        """Look up by ID; follows redirects automatically."""
        ...

    def __len__(self) -> int: ...
    def __contains__(self, id: str) -> bool: ...
    def __iter__(self) -> Iterator[Membership]: ...

    def redirects(self) -> List[MembershipRedirect]: ...

    def append(self, item: Membership | MembershipRedirect) -> None:
        """Validate and add a single item; raises ValueError on failure."""
        ...

    def extend(self, items: List[Membership | MembershipRedirect]) -> None:
        """Validate and add multiple items; raises ValueError on first failure."""
        ...


class PersonCollection:
    """Collection of Person and PersonRedirect handles."""

    @property
    def root(self) -> List[Person | PersonRedirect]: ...

    def __getitem__(self, id: str) -> Person:
        """Look up by ID; follows redirects automatically."""
        ...

    def __len__(self) -> int: ...
    def __contains__(self, id: str) -> bool: ...

    def redirects(self) -> List[PersonRedirect]: ...

    def from_identifier(self, identifier: str, scheme: str) -> Person:
        """Find a person by external identifier; raises KeyError if not found."""
        ...

    def from_name(
        self,
        name: str,
        chamber_id: str,
        date: date,
    ) -> Optional[Person]:
        """Find a person by slug-normalised name within a chamber on a date."""
        ...

    def append(self, item: Person | PersonRedirect) -> None:
        """Add an item; raises ValueError on duplicate ID."""
        ...


class OrganizationCollection:
    def __getitem__(self, id: str) -> Organization: ...
    def __len__(self) -> int: ...
    def __contains__(self, id: str) -> bool: ...
    def append(self, item: Organization) -> None: ...


class PostCollection:
    @property
    def root(self) -> List[Post]: ...
    def __getitem__(self, id: str) -> Post: ...
    def __len__(self) -> int: ...
    def __contains__(self, id: str) -> bool: ...
    def append(self, item: Post) -> None: ...


# ---------------------------------------------------------------------------
# Popolo
# ---------------------------------------------------------------------------

class Popolo:
    """
    Top-level Popolo container backed by a Rust Arc<RwLock<Popolo>>.

    All data lives in Rust; Python subclasses (see :class:`popolo_validator_python.popolo.Popolo`)
    add I/O helpers on top.
    """

    @classmethod
    def model_validate_json(
        cls,
        json_str: str,
        validate: bool = True,
    ) -> "Popolo":
        """Parse JSON and optionally run full validation."""
        ...

    def model_dump_json(self) -> str:
        """Serialise to compact, unicode-escaped JSON (2-space indent)."""
        ...

    @property
    def persons(self) -> PersonCollection: ...
    @property
    def memberships(self) -> MembershipCollection: ...
    @property
    def organizations(self) -> OrganizationCollection: ...
    @property
    def posts(self) -> PostCollection: ...

    def find_person_id_by_name(
        self,
        name: str,
        chamber_id: str,
        date_str: str,
    ) -> Optional[str]:
        """Rust-powered name lookup; returns person_id or None."""
        ...

    def find_person_id_by_identifier(
        self,
        identifier: str,
        scheme: str,
    ) -> Optional[str]: ...


# ---------------------------------------------------------------------------
# Register of interests
# ---------------------------------------------------------------------------

DetailValue = Union[int, str, float, bool, date, Decimal, List["RegmemDetailGroup"]]


class RegmemAnnotation:
    """A note attached to a register, entry or detail."""

    def __init__(
        self,
        author: str,
        content: str,
        *,
        type: Optional[str] = None,
        date_added: Optional[date] = None,
        content_format: Optional[str] = None,
    ) -> None: ...
    @property
    def author(self) -> str: ...
    @property
    def content(self) -> str: ...
    @property
    def type(self) -> str: ...
    @property
    def date_added(self) -> Optional[date]: ...
    @property
    def content_format(self) -> str: ...


class RegmemDetail:
    """A flexible key/value detail. ``value`` is typed via ``type``."""

    def __init__(
        self,
        *,
        slug: Optional[str] = None,
        display_as: Optional[str] = None,
        value: Optional[DetailValue] = None,
        source: Optional[str] = None,
        description: Optional[str] = None,
        common_key: Optional[str] = None,
    ) -> None: ...
    @property
    def source(self) -> str: ...
    @property
    def slug(self) -> Optional[str]: ...
    @property
    def display_as(self) -> Optional[str]: ...
    @property
    def common_key(self) -> Optional[str]: ...
    @property
    def description(self) -> Optional[str]: ...
    @property
    def type(self) -> Optional[str]: ...
    value: Optional[DetailValue]
    @property
    def annotations(self) -> List[RegmemAnnotation]: ...
    @property
    def sub_detail_groups(self) -> List["RegmemDetailGroup"]: ...


class RegmemDetailGroup:
    """A list of details (the ``details`` of an entry, or a sub-group)."""

    def __init__(self) -> None: ...
    def __len__(self) -> int: ...
    def __getitem__(self, index: int) -> RegmemDetail: ...
    @property
    def root(self) -> List[RegmemDetail]: ...
    def append(self, item: RegmemDetail, *, source: Optional[str] = None) -> None: ...
    def extend(
        self, items: List[RegmemDetail], *, source: Optional[str] = None
    ) -> None: ...
    def detail_dict(self) -> dict: ...


class RegmemSummary:
    """Read-only summary entry."""

    @property
    def info_type(self) -> str: ...
    @property
    def id(self) -> Optional[str]: ...
    @property
    def content(self) -> str: ...
    @property
    def content_format(self) -> str: ...
    @property
    def null_entry(self) -> bool: ...
    @property
    def comparable_id(self) -> str: ...
    @property
    def item_hash(self) -> str: ...
    @property
    def details(self) -> RegmemDetailGroup: ...
    def get_detail(self, name: str) -> Optional[RegmemDetail]: ...


class RegmemEntry:
    """A register entry. Edits persist back into the register."""

    @property
    def info_type(self) -> str: ...
    @property
    def id(self) -> Optional[str]: ...
    content: str
    @property
    def content_format(self) -> str: ...
    @property
    def null_entry(self) -> bool: ...
    @property
    def date_registered(self) -> Optional[date]: ...
    @property
    def date_published(self) -> Optional[date]: ...
    @property
    def date_updated(self) -> Optional[date]: ...
    @property
    def date_received(self) -> Optional[date]: ...
    @property
    def comparable_id(self) -> str: ...
    @property
    def item_hash(self) -> str: ...
    @property
    def details(self) -> RegmemDetailGroup: ...
    @property
    def annotations(self) -> List[RegmemAnnotation]: ...
    @property
    def sub_entries(self) -> List["RegmemEntry"]: ...
    def get_detail(self, name: str) -> Optional[RegmemDetail]: ...
    def get_detail_value(self, name: str) -> Optional[Any]: ...
    def add_details(self, *, source: Optional[str] = None, **values: Any) -> None: ...
    def add_annotation(self, annotation: RegmemAnnotation) -> None: ...


class RegmemCategory:
    """A category of interests for a person."""

    @property
    def category_id(self) -> str: ...
    @property
    def category_name(self) -> str: ...
    @property
    def category_description(self) -> Optional[str]: ...
    @property
    def legislation_or_rule_name(self) -> Optional[str]: ...
    @property
    def legislation_or_rule_url(self) -> Optional[str]: ...
    @property
    def entries(self) -> List[RegmemEntry]: ...
    @property
    def summaries(self) -> List[RegmemSummary]: ...


class RegmemPerson:
    """All registered interests for a person."""

    @property
    def person_id(self) -> str: ...
    @property
    def person_name(self) -> str: ...
    @property
    def published_date(self) -> date: ...
    @property
    def chamber(self) -> str: ...
    @property
    def language(self) -> str: ...
    @property
    def categories(self) -> List[RegmemCategory]: ...
    def get_category_from_id(self, category_id: str) -> RegmemCategory: ...


class RegmemRegister:
    """A release of a register, backed by a Rust Arc<RwLock<RegmemRegister>>."""

    @classmethod
    def model_validate_json(
        cls, json_str: str, validate: bool = True
    ) -> "RegmemRegister": ...
    def model_dump_json(self) -> str: ...
    @property
    def chamber(self) -> Optional[str]: ...
    @property
    def language(self) -> Optional[str]: ...
    @property
    def published_date(self) -> Optional[date]: ...
    @property
    def annotations(self) -> List[RegmemAnnotation]: ...
    @property
    def summaries(self) -> List[RegmemSummary]: ...
    @property
    def persons(self) -> List[RegmemPerson]: ...
    def get_person_from_id(self, person_id: str) -> RegmemPerson: ...
    def iter_entries(
        self,
    ) -> List[
        Tuple[RegmemPerson, RegmemCategory, RegmemEntry, Optional[RegmemEntry]]
    ]: ...
