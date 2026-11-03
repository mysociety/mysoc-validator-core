"""
Compatibility checks for named entry contexts returned by the Rust binding.
"""

from __future__ import annotations

from collections.abc import Iterator
from datetime import date

import pytest
from mysoc_validator_fast._mysoc_validator_fast import (
    RegmemCategory,
    RegmemEntry,
    RegmemPerson,
)
from mysoc_validator_fast._mysoc_validator_fast import (
    RegmemRegister as NativeRegister,
)
from mysoc_validator_fast.interests import EntryIterator, RegmemRegister


@pytest.fixture
def context_register() -> NativeRegister:
    """
    Build a small register with a parent entry and one sub-entry.
    """
    category = RegmemCategory(
        category_name="Employment",
        entries=[
            RegmemEntry(
                id="parent",
                content="Parent entry",
                sub_entries=[RegmemEntry(id="child", content="Child entry")],
            )
        ],
    )
    person = RegmemPerson(
        person_id="uk.org.publicwhip/person/1",
        person_name="Example Person",
        published_date=date(2026, 1, 1),
        chamber="house-of-commons",
        categories=[category],
    )
    return NativeRegister(persons=[person])


def test_native_contexts_keep_named_tuple_and_live_handle_behaviour(
    context_register: NativeRegister,
) -> None:
    """
    Preserve names, tuple operations, immutability and shared parent identity.
    """
    parent, child = context_register.iter_entries()
    assert isinstance(parent, EntryIterator)
    assert isinstance(child, EntryIterator)
    assert parent._fields == ("person", "category", "entry", "parent_entry")
    person, category, entry, parent_entry = parent
    assert person is parent.person
    assert category is parent.category
    assert entry is parent.entry
    assert parent[2] is parent.entry
    assert parent_entry is None
    assert parent == tuple(parent)
    assert child.parent_entry is parent.entry

    child.parent_entry.content = "Updated parent"
    assert parent.entry.content == "Updated parent"
    assert (
        context_register.persons[0].categories[0].entries[0].content == "Updated parent"
    )

    with pytest.raises(AttributeError):
        parent.entry = child.entry


def test_python_wrapper_keeps_iterator_of_named_contexts(
    context_register: NativeRegister,
) -> None:
    """
    Keep the wrapper's iterator API while Rust constructs the named tuples.
    """
    register = RegmemRegister(persons=context_register.persons)
    contexts = register.iter_entries()
    assert isinstance(contexts, Iterator)
    parent, child = contexts
    assert isinstance(parent, EntryIterator)
    assert isinstance(child, EntryIterator)
    assert child.parent_entry is parent.entry
