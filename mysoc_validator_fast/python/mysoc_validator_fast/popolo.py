"""
Thin Python wrapper over the Rust-backed mysoc_validator_fast extension.

All data lives in the Rust Popolo struct (shared via Arc<RwLock<>>).  Python
objects are lightweight handles that read/write through to Rust on demand.

This module adds I/O helpers (from_parlparse, from_path, from_url, to_path)
and re-exports the Rust types under the names expected by callers.
"""

from __future__ import annotations

import json
from collections.abc import Sequence
from pathlib import Path
from typing import ClassVar

from ._mysoc_validator_fast import (
    AltName,
    Area,
    BasicPersonName,
    LordName,
    Membership,
    MembershipRedirect,
    Organization,
    Person,
    PersonRedirect,
    Post,
)
from ._mysoc_validator_fast import Popolo as _RustPopolo
from .consts import Chamber, IdentifierScheme, MembershipReason
from .dates import ApproxDate, FixedDate

__all__ = [
    "AltName",
    "ApproxDate",
    "Area",
    "BasicPersonName",
    "Chamber",
    "FixedDate",
    "IdentifierScheme",
    "LordName",
    "Membership",
    "MembershipReason",
    "MembershipRedirect",
    "Organization",
    "Person",
    "PersonRedirect",
    "Popolo",
    "Post",
]


class Popolo(_RustPopolo):
    """
    Drop-in replacement for mysoc_validator.Popolo.

    Extends the Rust PyPopolo base with Python-level I/O helpers.
    All data is stored in the underlying Rust Popolo struct.
    """

    # Make Chamber accessible as Popolo.Chamber.COMMONS etc.
    Chamber: ClassVar[type[Chamber]] = Chamber
    IdentifierScheme: ClassVar[type[IdentifierScheme]] = IdentifierScheme

    @classmethod
    def model_validate(cls, value: object) -> Popolo:
        """
        Validate a Python mapping using the Rust Popolo model.
        """
        return cls.model_validate_json(json.dumps(value))

    @classmethod
    def from_json_str(
        cls,
        json_str: str,
        *,
        cross_validate: bool = True,
    ) -> Popolo:
        """
        Parse JSON and optionally run cross-record validation.
        """
        return cls.model_validate_json(json_str, cross_validate)

    @classmethod
    def from_path(
        cls,
        json_path: Path | str | Sequence[Path | str],
        cross_validate: bool = True,
    ) -> Popolo:
        """
        Load from a file path (or list of paths — extras merged in).
        """
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
        url: str | list[str],
        cross_validate: bool = True,
    ) -> Popolo:
        """
        Load from a URL (or list of URLs — extras merged in).
        """
        import requests

        if isinstance(url, str):
            urls = [url]
        else:
            urls = list(url)

        base = cls.from_json_str(
            requests.get(urls[0]).text, cross_validate=cross_validate
        )
        for extra_url in urls[1:]:
            extra_popolo = cls.from_json_str(
                requests.get(extra_url).text, cross_validate=False
            )
            base.update(extra_popolo)
        return base

    @classmethod
    def from_parlparse(
        cls,
        *,
        extras: list[str] | None = None,
        branch: str = "master",
    ) -> Popolo:
        """
        Load from the parlparse GitHub repository.
        """
        base_url = (
            f"https://raw.githubusercontent.com/mysociety/parlparse/{branch}/members"
        )
        urls = [f"{base_url}/people.json"]
        for extra in extras or []:
            fname = extra if extra.endswith(".json") else extra + ".json"
            urls.append(f"{base_url}/{fname}")
        return cls.from_url(urls)

    def to_json_str(self) -> str:
        """
        Serialize to JSON string (unicode-escaped, 2-space indent).
        """
        return self.model_dump_json()

    def to_path(self, json_path: Path | str) -> None:
        """
        Write JSON to a file.
        """
        Path(json_path).write_text(self.to_json_str())
