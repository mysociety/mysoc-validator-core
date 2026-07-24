"""
Tests for the register-of-interests JSON support.

`test_json_interests` is the upstream test from `mysoc_validator`
(tests/test_interests.py), with the data path adjusted to the bundled fixture.
The remaining tests exercise the Rust-backed query / round-trip / enrichment
API exposed by `mysoc_validator_fast`.
"""

from datetime import date
from decimal import Decimal
from pathlib import Path

import pytest

from mysoc_validator_fast import RegmemAnnotation, RegmemDetail, RegmemRegister

DATA = Path(__file__).parent / "data"
REGISTER_JSON = DATA / "commons-regmem-2025-01-20.json"


@pytest.fixture(scope="session")
def register() -> RegmemRegister:
    return RegmemRegister.from_path(REGISTER_JSON)


# ---------------------------------------------------------------------------
# Upstream test (ported)
# ---------------------------------------------------------------------------


def test_json_interests():
    RegmemRegister.from_path(REGISTER_JSON)


# ---------------------------------------------------------------------------
# Query
# ---------------------------------------------------------------------------


def test_top_level_fields(register: RegmemRegister):
    assert register.chamber == "house-of-commons"
    assert register.published_date == date(2025, 1, 20)
    assert len(register.persons) == 638


def test_iter_entries(register: RegmemRegister):
    entries = register.iter_entries()
    assert len(entries) > 0
    person, category, entry, parent = entries[0]
    assert person.person_id
    assert category.category_name
    assert entry.comparable_id


def test_get_person_and_details(register: RegmemRegister):
    # First person/entry navigation, reading typed detail values.
    person = register.persons[0]
    fetched = register.get_person_from_id(person.person_id)
    assert fetched.person_id == person.person_id

    # Find an entry with details and assert the typed value conversion.
    found_decimal = found_bool = found_string = False
    for _person, _category, entry, _parent in register.iter_entries():
        for detail in entry.details.root:
            value = detail.value
            if isinstance(value, Decimal):
                found_decimal = True
            elif isinstance(value, bool):
                found_bool = True
            elif isinstance(value, str):
                found_string = True
        if found_decimal and found_bool and found_string:
            break
    assert found_decimal and found_bool and found_string


def test_get_person_from_id_missing(register: RegmemRegister):
    with pytest.raises(ValueError):
        register.get_person_from_id("not-a-real-person")


# ---------------------------------------------------------------------------
# Round trip
# ---------------------------------------------------------------------------


def test_round_trip_idempotent(register: RegmemRegister):
    dumped = register.model_dump_json()
    reloaded = RegmemRegister.model_validate_json(dumped)
    # dumping again should be byte-stable
    assert reloaded.model_dump_json() == dumped


# ---------------------------------------------------------------------------
# Enrichment (mutation persists back into the register)
# ---------------------------------------------------------------------------


def test_add_details_persists():
    register = RegmemRegister.from_path(REGISTER_JSON)
    _person, _category, entry, _parent = register.iter_entries()[0]

    entry.add_details(source="mysociety", standardised_name="ACME Ltd", score=5)

    dumped = register.model_dump_json()
    reloaded = RegmemRegister.model_validate_json(dumped)
    _p, _c, reloaded_entry, _pa = reloaded.iter_entries()[0]

    detail = reloaded_entry.get_detail("standardised_name")
    assert detail is not None
    assert detail.value == "ACME Ltd"
    assert detail.source == "mysociety"
    assert reloaded_entry.get_detail_value("score") == 5


def test_append_detail_and_annotation_persists():
    register = RegmemRegister.from_path(REGISTER_JSON)
    _person, _category, entry, _parent = register.iter_entries()[0]

    entry.details.append(
        RegmemDetail(slug="enriched_flag", value=True), source="mysociety"
    )
    entry.add_annotation(
        RegmemAnnotation(author="mysociety", content="checked", type="note")
    )

    reloaded = RegmemRegister.model_validate_json(register.model_dump_json())
    _p, _c, reloaded_entry, _pa = reloaded.iter_entries()[0]
    assert reloaded_entry.get_detail_value("enriched_flag") is True
    assert len(reloaded_entry.annotations) == 1
    assert reloaded_entry.annotations[0].author == "mysociety"


def test_duplicate_detail_slug_raises():
    register = RegmemRegister.from_path(REGISTER_JSON)
    _person, _category, entry, _parent = register.iter_entries()[0]

    entry.details.append(RegmemDetail(slug="dup", value=1))
    with pytest.raises(ValueError):
        entry.details.append(RegmemDetail(slug="dup", value=2))


def test_added_detail_types_round_trip():
    register = RegmemRegister.from_path(REGISTER_JSON)
    _p, _c, entry, _pa = register.iter_entries()[0]

    entry.details.append(RegmemDetail(slug="amount", value=Decimal("1234.50")))
    entry.details.append(RegmemDetail(slug="when", value=date(2025, 1, 2)))
    entry.details.append(RegmemDetail(slug="ok", value=False))

    _p, _c, reloaded_entry, _pa = (
        RegmemRegister.model_validate_json(register.model_dump_json()).iter_entries()[0]
    )

    amount = reloaded_entry.get_detail_value("amount")
    assert isinstance(amount, Decimal) and amount == Decimal("1234.50")
    assert reloaded_entry.get_detail_value("when") == date(2025, 1, 2)
    # bool must survive as bool (not collapse to int)
    assert reloaded_entry.get_detail_value("ok") is False

    assert reloaded_entry.get_detail("amount").type == "decimal"
    assert reloaded_entry.get_detail("when").type == "date"
    assert reloaded_entry.get_detail("ok").type == "boolean"


def test_add_details_default_source_is_official():
    register = RegmemRegister.from_path(REGISTER_JSON)
    _p, _c, entry, _pa = register.iter_entries()[0]
    entry.add_details(note="hello")
    assert entry.get_detail("note").source == "official"


def test_extend_details_persists():
    register = RegmemRegister.from_path(REGISTER_JSON)
    _p, _c, entry, _pa = register.iter_entries()[0]

    entry.details.extend(
        [RegmemDetail(slug="extend_a", value=1), RegmemDetail(slug="extend_b", value=2)],
        source="mysociety",
    )

    _p, _c, reloaded_entry, _pa = (
        RegmemRegister.model_validate_json(register.model_dump_json()).iter_entries()[0]
    )
    assert reloaded_entry.get_detail_value("extend_a") == 1
    assert reloaded_entry.get_detail_value("extend_b") == 2
    assert reloaded_entry.get_detail("extend_a").source == "mysociety"


def test_extend_with_duplicate_raises_and_is_atomic():
    register = RegmemRegister.from_path(REGISTER_JSON)
    _p, _c, entry, _pa = register.iter_entries()[0]
    before = len(entry.details)

    with pytest.raises(ValueError):
        entry.details.extend(
            [RegmemDetail(slug="same", value=1), RegmemDetail(slug="same", value=2)]
        )

    # a failed extend must not partially modify the group
    assert len(entry.details) == before


def test_set_entry_content_persists():
    register = RegmemRegister.from_path(REGISTER_JSON)
    _p, _c, entry, _pa = register.iter_entries()[0]
    entry.content = "Edited content"

    _p, _c, reloaded_entry, _pa = (
        RegmemRegister.model_validate_json(register.model_dump_json()).iter_entries()[0]
    )
    assert reloaded_entry.content == "Edited content"


def test_edits_visible_through_separate_handle():
    # Two handles to the same entry share the underlying Rust register.
    register = RegmemRegister.from_path(REGISTER_JSON)
    entry = register.persons[0].categories[0].entries[0]
    entry.add_details(source="mysociety", nav_added="yes")

    other_handle = register.persons[0].categories[0].entries[0]
    assert other_handle.get_detail_value("nav_added") == "yes"


def test_edit_subentry_persists():
    register = RegmemRegister.from_path(REGISTER_JSON)

    sub_entry = next(
        entry
        for _person, _category, entry, parent in register.iter_entries()
        if parent is not None
    )
    sub_entry.add_details(source="mysociety", sub_added="yes")

    reloaded = RegmemRegister.model_validate_json(register.model_dump_json())
    enriched = [
        entry
        for _p, _c, entry, parent in reloaded.iter_entries()
        if parent is not None and entry.get_detail_value("sub_added") == "yes"
    ]
    assert len(enriched) == 1


# ---------------------------------------------------------------------------
# Builder objects (standalone, before attaching to a register)
# ---------------------------------------------------------------------------


def test_standalone_detail_inference():
    by_display = RegmemDetail(display_as="Donation Source", value="X")
    assert by_display.slug == "donation_source"
    assert by_display.type == "string"

    by_slug = RegmemDetail(slug="donor_name", value=10)
    assert by_slug.display_as == "Donor Name"
    assert by_slug.type == "int"


def test_detail_value_setter_updates_type():
    detail = RegmemDetail(slug="x", value="a")
    assert detail.type == "string"
    detail.value = 7
    assert detail.value == 7
    assert detail.type == "int"


def test_annotation_date_defaults_to_today():
    ann = RegmemAnnotation(author="mysociety", content="checked")
    assert ann.date_added == date.today()
    assert ann.type == "note"
