"""
Behavioral boundaries that must survive the Rust module refactoring.
"""

from __future__ import annotations

import subprocess
import sys
from datetime import date
from textwrap import dedent

import pytest
from mysoc_validator_fast._mysoc_validator_fast import Popolo as RustPopolo
from mysoc_validator_fast.dates import FuzzyDate

from mysoc_validator_fast import Organization, Person, Popolo


def test_python_fuzzy_comparisons_remain_overlap_based() -> None:
    january = FuzzyDate.fromisoformat("2025-01")
    year = FuzzyDate.fromisoformat("2025")
    february = FuzzyDate.fromisoformat("2025-02")
    assert january == year
    assert year == february
    assert january != february
    assert january < february
    assert not january < year
    assert not year > january
    assert january <= year
    assert january >= year
    assert year == date(2025, 6, 1)
    assert year <= date(2025, 6, 1)
    assert year >= date(2025, 6, 1)
    assert year > date(2024, 12, 31)
    assert year < date(2026, 1, 1)
    assert year == "2025"
    assert year != "2025-06-01"


@pytest.mark.parametrize("remove_method", ["pop", "delete"])
def test_removed_organization_handles_raise_without_poisoning_dataset(
    remove_method: str,
) -> None:
    popolo = Popolo.model_validate(
        {"organizations": [{"id": "party", "name": "Party", "extra": {}}]}
    )
    handle = popolo.organizations["party"]
    extra = handle.extra
    if remove_method == "pop":
        detached = popolo.organizations.pop("party")
        assert detached.name == "Party"
    else:
        handle.delete()
    with pytest.raises(KeyError):
        _ = handle.name
    with pytest.raises(KeyError):
        handle.name = "Missing"
    with pytest.raises(KeyError):
        handle.model_dump()
    with pytest.raises(KeyError):
        extra["example"] = "Missing"
    popolo.organizations.append(Organization("other", "Still usable"))
    assert popolo.organizations["other"].name == "Still usable"


def test_person_handles_follow_repeated_merges_for_reads_and_writes() -> None:
    ids = [f"uk.org.publicwhip/person/{index}" for index in range(1, 4)]
    popolo = Popolo.model_validate(
        {"persons": [{"id": person_id} for person_id in ids]}
    )
    original = popolo.persons[ids[0]]
    popolo.persons.merge_people(ids[1], ids[0])
    popolo.persons.merge_people(ids[2], ids[1])
    original.biography = "Written through the original handle"
    assert original.biography == popolo.persons[ids[2]].biography
    assert popolo.persons[ids[0]].biography == original.biography
    assert original.memberships() == []


def test_appending_a_standalone_person_copies_its_data() -> None:
    popolo = Popolo.model_validate({})
    standalone = Person("uk.org.publicwhip/person/1", biography="Original")
    popolo.persons.append(standalone)
    standalone.biography = "Detached change"
    handle = popolo.persons[standalone.id]
    assert handle.biography == "Original"
    handle.biography = "Live change"
    assert popolo.persons[standalone.id].biography == "Live change"


def test_invalid_localisation_shape_is_a_python_error() -> None:
    popolo = Popolo.model_validate(
        {
            "persons": [
                {"id": "uk.org.publicwhip/person/1", "extra": {"localised_values": []}}
            ]
        }
    )
    person = popolo.persons["uk.org.publicwhip/person/1"]
    with pytest.raises(ValueError, match="localised_values must be an object"):
        person.set_localised_value("biography", "en", "Text")
    person.biography = "Still usable"
    assert person.biography == "Still usable"


def test_native_update_with_self_is_a_no_op() -> None:
    popolo = Popolo.model_validate({})
    assert RustPopolo.update(popolo, popolo) is popolo


@pytest.mark.parametrize("collection", ["persons", "memberships"])
def test_redirect_cycles_raise_key_error(collection: str) -> None:
    popolo = Popolo.model_validate_json(
        '{"'
        + collection
        + '": [{"id": "a", "redirect": "b"}, {"id": "b", "redirect": "a"}]}',
        False,
    )
    with pytest.raises(KeyError):
        _ = getattr(popolo, collection)["a"]


def test_json_conversion_allows_reentrant_model_access() -> None:
    # A subprocess bounds the wait if a lock regression deadlocks the callback.
    subprocess.run(
        [
            sys.executable,
            "-c",
            dedent("""
            import json
            from mysoc_validator_fast import Popolo

            popolo = Popolo.model_validate({"persons": [{
                "id": "uk.org.publicwhip/person/1", "extra": {"key": "value"}
            }]})
            person = popolo.persons["uk.org.publicwhip/person/1"]
            other_handle = popolo.persons["uk.org.publicwhip/person/1"]
            original_loads = json.loads
            def reentrant_loads(value):
                other_handle.biography = "Callback completed"
                return original_loads(value)
            json.loads = reentrant_loads
            assert person.get_extra("key") == "value"
            assert person.model_dump()["biography"] == "Callback completed"
        """),
        ],
        check=True,
        timeout=15,
    )


def test_reversed_bounds_keep_python_comparison_precedence() -> None:
    reversed_range = FuzzyDate(date(2025, 12, 31), date(2025, 1, 1))
    middle = date(2025, 6, 1)
    assert reversed_range < middle
    assert not reversed_range > middle
    assert reversed_range <= middle
    assert not reversed_range >= middle
