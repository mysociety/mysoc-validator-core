"""
Static contracts for the Rust extension's generated interface.

Mypy checks this file against the installed wheel. It is not run by pytest.
Unused ignore comments fail the check if a concrete type regresses to Any.
"""

from __future__ import annotations

from collections.abc import Iterator
from typing import Any, assert_type

from mysoc_validator_fast._mysoc_validator_fast import (
    DivisionCount,
    MajorHeading,
    Membership,
    MembershipRedirect,
    MinorHeading,
    MSPName,
    Person,
    PersonRedirect,
    Popolo,
    RegmemAnnotation,
    RegmemCategory,
    RegmemDetail,
    RegmemDetailGroup,
    RegmemEntry,
    RegmemPerson,
    RegmemRegister,
    Speech,
    Transcript,
    validate_transcript_folder,
)
from mysoc_validator_fast.interests import EntryIterator
from mysoc_validator_fast.interests import RegmemRegister as PublicRegister


def check_popolo(popolo: Popolo, person: Person) -> None:
    """
    Preserve live-handle and redirect types in collection results.
    """
    assert_type(person.id, str)
    assert_type(person.memberships(), list[Membership])
    assert_type(popolo.persons.root, list[Person | PersonRedirect])
    assert_type(popolo.persons.redirects(), list[PersonRedirect])
    assert_type(popolo.memberships.root, list[Membership | MembershipRedirect])
    assert_type(popolo.memberships.redirects(), list[MembershipRedirect])
    assert_type(person.model_dump(), dict[str, Any])
    Person(123)  # type: ignore[arg-type]


def check_interests(
    group: RegmemDetailGroup, register: RegmemRegister, public_register: PublicRegister
) -> None:
    """
    Preserve detail iterator elements and the hierarchy of register entries.
    """
    assert_type(iter(group), Iterator[RegmemDetail])
    assert_type(group.detail_dict(), dict[str, Any])
    assert_type(public_register.iter_entries(), Iterator[EntryIterator])
    assert_type(
        register.iter_entries(),
        list[EntryIterator],
    )
    for row in register.iter_entries():
        assert_type(row.person, RegmemPerson)
        assert_type(row.category, RegmemCategory)
        assert_type(row.entry, RegmemEntry)
        assert_type(row.parent_entry, RegmemEntry | None)
        assert_type(row[2], RegmemEntry)
    annotation = RegmemAnnotation("author", "content", type="note")
    assert_type(annotation.type, str)
    RegmemAnnotation("author", "content", type=123)  # type: ignore[arg-type]


def check_transcripts(transcript: Transcript, speech: Speech) -> None:
    """
    Distinguish transcript content shapes and retain typed speech iteration.
    """
    assert_type(speech.content, dict[str, str])
    assert_type(DivisionCount().content, int | None)
    assert_type(MSPName().content, str)
    assert_type(transcript.iter_speeches(), list[Speech])
    assert_type(
        transcript.iter_headed_speeches(),
        list[tuple[MajorHeading | None, MinorHeading | None, Speech, int]],
    )
    assert_type(validate_transcript_folder("transcripts"), list[dict[str, Any]])
    speech.content["text"].nonexistent_method()  # type: ignore[attr-defined]
