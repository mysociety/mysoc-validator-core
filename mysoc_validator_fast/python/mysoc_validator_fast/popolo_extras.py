"""
Subclassable Pydantic schemas for project-specific Popolo ``extra`` data.

Rust stores the underlying JSON. These Python models validate custom fields and
provide typed snapshots through ``record.get_extra_as(CustomExtra)``. Mutating a
snapshot does not update the record; use ``record.set_extra(key, value)`` to save
changes. The record's ``extra`` proxy supports live top-level assignments.
"""

from __future__ import annotations

from typing import Any, Generic, Literal, TypeVar

from pydantic import BaseModel, ConfigDict, Field

Language = Literal["en", "cy"]
LocalisedField = TypeVar("LocalisedField", bound=str)


class Extra(BaseModel):
    """
    Forward-compatible container for non-standard Popolo properties.
    """

    model_config = ConfigDict(extra="allow")

    def __getitem__(self, key: str) -> Any:
        return getattr(self, key)

    def __setitem__(self, key: str, value: Any) -> None:
        setattr(self, key, value)


class LocalisedLabelsExtra(Extra, Generic[LocalisedField]):
    """
    Extra values containing field and language keyed localized strings.
    """

    localised_values: dict[LocalisedField, dict[Language, str]] = Field(
        default_factory=dict
    )


MembershipExtra = LocalisedLabelsExtra[Literal["label", "role"]]
OrganizationExtra = LocalisedLabelsExtra[Literal["name", "abstract", "description"]]
PersonExtra = LocalisedLabelsExtra[Literal["biography", "summary"]]
AreaExtra = LocalisedLabelsExtra[Literal["name"]]
PostExtra = LocalisedLabelsExtra[Literal["label", "role"]]
