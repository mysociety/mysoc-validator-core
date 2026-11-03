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

from collections.abc import Iterator
from pathlib import Path
from typing import ClassVar, NamedTuple

from ._mysoc_validator_fast import (
    RegmemAnnotation as _RustRegmemAnnotation,
)
from ._mysoc_validator_fast import (
    RegmemCategory as _RustRegmemCategory,
)
from ._mysoc_validator_fast import (
    RegmemDetail as _RustRegmemDetail,
)
from ._mysoc_validator_fast import (
    RegmemDetailGroup as _RustRegmemDetailGroup,
)
from ._mysoc_validator_fast import (
    RegmemEntry as _RustRegmemEntry,
)
from ._mysoc_validator_fast import (
    RegmemPerson as _RustRegmemPerson,
)
from ._mysoc_validator_fast import (
    RegmemRegister as _RustRegmemRegister,
)
from ._mysoc_validator_fast import (
    RegmemSummary as _RustRegmemSummary,
)
from .consts import Chamber, CommonKey

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

    Chamber: ClassVar[type[Chamber]] = Chamber
    CommonKey: ClassVar[type[CommonKey]] = CommonKey

    @classmethod
    def from_path(
        cls,
        path: Path | str,
        cross_validate: bool = True,
    ) -> RegmemRegister:
        """
        Load and validate a register from a JSON file.
        """
        return cls.model_validate_json(Path(path).read_text(), cross_validate)

    def to_path(self, path: Path | str, full: bool = False) -> None:
        """
        Write the register to a JSON file.
        """

        content = self.model_dump_json()
        if not full:
            import json

            content = json.dumps(json.loads(content), separators=(",", ":"))
        Path(path).write_text(content)

    def iter_entries(self) -> Iterator[EntryIterator]:
        """
        Iterate the named entry contexts constructed by the Rust binding.
        """

        return iter(super().iter_entries())
