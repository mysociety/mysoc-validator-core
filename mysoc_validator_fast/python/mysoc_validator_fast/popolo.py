"""
Thin Python wrapper over the Rust-backed mysoc_validator_fast extension.

All data lives in the Rust Popolo struct (shared via Arc<RwLock<>>).  Python
objects are lightweight handles that read/write through to Rust on demand.

This module adds I/O helpers (from_parlparse, from_path, from_url, to_path)
and re-exports the Rust types under the names expected by callers.
"""

from __future__ import annotations

import json
import re
from datetime import date
from pathlib import Path
from typing import ClassVar, Iterator, List, Optional, Type, Union

import requests

from .consts import Chamber, IdentifierScheme, MembershipReason
from .dates import ApproxDate, FixedDate

# ---------------------------------------------------------------------------
# Import from the compiled Rust extension
# ---------------------------------------------------------------------------
from ._mysoc_validator_fast import (  # type: ignore[import]
    Membership as _RustMembership,
    MembershipCollection as _RustMembershipCollection,
    MembershipRedirect as _RustMembershipRedirect,
    Organization as _RustOrganization,
    OrganizationCollection as _RustOrganizationCollection,
    Person as _RustPerson,
    PersonCollection as _RustPersonCollection,
    PersonRedirect as _RustPersonRedirect,
    Popolo as _RustPopolo,
    Post as _RustPost,
    PostCollection as _RustPostCollection,
)

# Re-export Rust types under their expected names.
# Users import Membership, Person, etc. from this module.
Membership = _RustMembership
MembershipRedirect = _RustMembershipRedirect
Organization = _RustOrganization
Person = _RustPerson
PersonRedirect = _RustPersonRedirect
Post = _RustPost

NON_ASCII_RE = re.compile(r"[^\x00-\x7F]")


def _escape_unicode(text: str) -> str:
    """Escape non-ASCII characters to \\uXXXX sequences."""
    def escape(m: re.Match[str]) -> str:
        return f"\\u{ord(m.group(0)):04x}"
    return NON_ASCII_RE.sub(escape, text)


class Popolo(_RustPopolo):
    """
    Drop-in replacement for mysoc_validator.Popolo.

    Extends the Rust PyPopolo base with Python-level I/O helpers.
    All data is stored in the underlying Rust Popolo struct.
    """

    # Make Chamber accessible as Popolo.Chamber.COMMONS etc.
    Chamber: ClassVar[Type[Chamber]] = Chamber
    IdentifierScheme: ClassVar[Type[IdentifierScheme]] = IdentifierScheme

    # ------------------------------------------------------------------
    # Construction
    # ------------------------------------------------------------------

    @classmethod
    def from_json_str(
        cls,
        json_str: str,
        *,
        cross_validate: bool = True,
    ) -> "Popolo":
        """Parse and optionally validate a JSON string."""
        return cls.model_validate_json(json_str, cross_validate)

    @classmethod
    def from_path(
        cls,
        json_path: Union[Path, List[Path]],
        cross_validate: bool = True,
    ) -> "Popolo":
        """Load from a file path (or list of paths — extras merged in)."""
        if isinstance(json_path, (str, Path)):
            paths = [Path(json_path)]
        else:
            paths = [Path(p) for p in json_path]

        base = cls.from_json_str(paths[0].read_text(), cross_validate=cross_validate)
        for extra in paths[1:]:
            extra_popolo = cls.from_json_str(extra.read_text(), cross_validate=False)
            base.update(extra_popolo)
        return base

    @classmethod
    def from_url(
        cls,
        url: Union[str, List[str]],
        cross_validate: bool = True,
    ) -> "Popolo":
        """Load from a URL (or list of URLs — extras merged in)."""
        if isinstance(url, str):
            urls = [url]
        else:
            urls = list(url)

        base = cls.from_json_str(requests.get(urls[0]).text, cross_validate=cross_validate)
        for extra_url in urls[1:]:
            extra_popolo = cls.from_json_str(requests.get(extra_url).text, cross_validate=False)
            base.update(extra_popolo)
        return base

    @classmethod
    def from_parlparse(
        cls,
        *,
        extras: Optional[List[str]] = None,
        branch: str = "master",
    ) -> "Popolo":
        """Load from the parlparse GitHub repository."""
        base_url = f"https://raw.githubusercontent.com/mysociety/parlparse/{branch}/members"
        urls = [f"{base_url}/people.json"]
        for extra in extras or []:
            fname = extra if extra.endswith(".json") else extra + ".json"
            urls.append(f"{base_url}/{fname}")
        return cls.from_url(urls)

    # ------------------------------------------------------------------
    # Serialisation
    # ------------------------------------------------------------------

    def to_json_str(self) -> str:
        """Serialize to JSON string (unicode-escaped, 2-space indent)."""
        return self.model_dump_json()

    def to_path(self, json_path: Union[Path, str]) -> None:
        """Write JSON to a file."""
        Path(json_path).write_text(self.to_json_str())

    # ------------------------------------------------------------------
    # Mutation helpers
    # ------------------------------------------------------------------

    def update(self, other: "Popolo") -> "Popolo":
        """Merge another Popolo into this one (skips duplicates)."""
        # Iterate raw items from the other popolo and add non-duplicates
        for item in other.memberships.root:
            try:
                self.memberships.append(item)
            except (ValueError, KeyError):
                pass
        return self
