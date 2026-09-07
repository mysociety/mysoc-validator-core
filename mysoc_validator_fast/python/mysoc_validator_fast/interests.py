"""
Thin Python wrapper over the Rust-backed register-of-interests classes.

Mirrors ``mysoc_validator.models.interests``. All data lives in the Rust
``RegmemRegister`` (shared via ``Arc<RwLock<>>``); the Python objects are
lightweight handles that read/write through to Rust. Register / Person /
Category / Entry edits (adding details or annotations for enrichment) persist
back into the register and survive a ``model_dump_json`` round-trip.

This module adds I/O helpers (``from_path`` / ``to_path``) and re-exports the
Rust types under the names expected by callers.
"""

from __future__ import annotations

from pathlib import Path
from typing import ClassVar, Iterator, NamedTuple, Type, Union

from .consts import Chamber, CommonKey

# ---------------------------------------------------------------------------
# Import from the compiled Rust extension
# ---------------------------------------------------------------------------
from ._mysoc_validator_fast import (  # type: ignore[import]
    RegmemAnnotation as _RustRegmemAnnotation,
    RegmemCategory as _RustRegmemCategory,
    RegmemDetail as _RustRegmemDetail,
    RegmemDetailGroup as _RustRegmemDetailGroup,
    RegmemEntry as _RustRegmemEntry,
    RegmemPerson as _RustRegmemPerson,
    RegmemRegister as _RustRegmemRegister,
    RegmemSummary as _RustRegmemSummary,
)

# Re-export Rust types under their expected names.
RegmemAnnotation = _RustRegmemAnnotation
RegmemCategory = _RustRegmemCategory
RegmemDetail = _RustRegmemDetail
RegmemDetailContainer = _RustRegmemDetail
RegmemDetailGroup = _RustRegmemDetailGroup
RegmemEntry = _RustRegmemEntry
RegmemPerson = _RustRegmemPerson
RegmemSummary = _RustRegmemSummary


def slugify(value: str) -> str:
    """
    Produce the stable machine key used for interest detail names.
    """

    filtered = "".join(
        character for character in value if character.isalnum() or character == " "
    )
    return filtered.strip().replace(" ", "_").lower().replace("__", "_")


class EntryIterator(NamedTuple):
    """
    One entry and its location within a register.
    """

    person: RegmemPerson
    category: RegmemCategory
    entry: RegmemEntry
    parent_entry: RegmemEntry | None


class RegmemRegister(_RustRegmemRegister):
    """
    Drop-in replacement for ``mysoc_validator.models.interests.RegmemRegister``.

    Extends the Rust base with Python-level file I/O helpers.
    """

    Chamber: ClassVar[Type[Chamber]] = Chamber
    CommonKey: ClassVar[Type[CommonKey]] = CommonKey

    @classmethod
    def from_path(
        cls,
        path: Union[Path, str],
        cross_validate: bool = True,
    ) -> "RegmemRegister":
        """Load and validate a register from a JSON file."""
        return cls.model_validate_json(Path(path).read_text(), cross_validate)

    def to_path(self, path: Union[Path, str], full: bool = False) -> None:
        """Write the register to a JSON file."""

        content = self.model_dump_json()
        if not full:
            import json

            content = json.dumps(json.loads(content), separators=(",", ":"))
        Path(path).write_text(content)

    def iter_entries(self) -> Iterator[EntryIterator]:
        """Iterate entries with named parent context."""

        return (EntryIterator(*row) for row in super().iter_entries())
