
from __future__ import annotations

from datetime import date
from dataclasses import dataclass

@dataclass
class FuzzyDate:
    earliest_date: date
    latest_date: date

    def __new__(cls, earliest_date: date, latest_date: date) -> FuzzyDate:
        ...

    @classmethod
    def fromisoformat(cls, iso8601_date_string: str) -> FuzzyDate:
        ...

    def isoformat(self) -> str:
        ...