from __future__ import annotations

from datetime import date
from typing import Any

# FuzzyDate is the Rust-backed partial-date type that handles year-only,
# year-month, full-date, and range precision.  It is the canonical date type
# for all date fields in this library.
from ._mysoc_validator_fast import FuzzyDate  # type: ignore[import]

# ApproxDate is an alias kept for compatibility with mysoc_validator call sites.
ApproxDate = FuzzyDate


class FixedDateMeta(type):
    def __setattr__(self, _name: str, _value: Any) -> None:
        raise AttributeError("Cannot modify immutable instance")


class FixedDate(metaclass=FixedDateMeta):
    """Sentinel boundary dates used as defaults for open-ended ranges."""

    PAST: date = date(1, 1, 1)
    FUTURE: date = date(9999, 12, 31)
