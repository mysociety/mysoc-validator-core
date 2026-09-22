"""Regression tests imported from the upstream mysoc-validator test suite."""

from __future__ import annotations

from datetime import date
from decimal import Decimal

import pytest
from typer.testing import CliRunner

from mysoc_validator_fast import (
    AltName,
    BasicPersonName,
    LordName,
    Membership,
    Organization,
    Person,
    Popolo,
    RegmemDetail,
    RegmemDetailContainer,
    RegmemDetailGroup,
    RegmemEntry,
    Chamber,
    MembershipReason,
    slugify,
)
from mysoc_validator_fast.__main__ import app


def minimal_popolo() -> Popolo:
    """Create the compact Popolo fixture used by upstream regression tests."""
    return Popolo.model_validate(
        {
            "organizations": [
                {"id": "house-of-commons", "name": "Commons"},
                {"id": "labour", "name": "Labour", "classification": "party"},
            ],
            "persons": [
                {
                    "id": "uk.org.publicwhip/person/1",
                    "identifiers": [{"scheme": "external", "identifier": "old"}],
                    "other_names": [
                        {
                            "given_name": "Ada",
                            "family_name": "Example",
                            "note": "Main",
                            "start_date": "2020-01-01",
                        }
                    ],
                }
            ],
            "memberships": [
                {
                    "id": "uk.org.publicwhip/member/1",
                    "person_id": "uk.org.publicwhip/person/1",
                    "organization_id": "house-of-commons",
                    "start_date": "2020-01-01",
                }
            ],
        }
    )


def test_untyped_boolean_interest_detail_preserves_boolean_type() -> None:
    """Verify untyped boolean interest detail preserves boolean type."""
    group = RegmemDetailGroup.model_validate([{"slug": "flag", "value": True}])
    detail = group.root[0]
    assert detail.value is True
    assert detail.type == "boolean"


def test_duplicate_detail_append_is_atomic() -> None:
    """Verify a failed append leaves the detail group unchanged."""
    group = RegmemDetailGroup.model_validate(
        [{"slug": "name", "value": "first"}]
    )
    with pytest.raises(ValueError, match="Duplicate detail names"):
        group.append(RegmemDetail(slug="name", value="second"))
    assert group.detail_dict() == {"name": "first"}


def test_interest_nested_details_match_upstream_api() -> None:
    """Verify nested detail containers flatten and reduce like upstream."""

    assert slugify("  Company  Name! (UK) ") == "company_name_uk"
    rows = [
        RegmemDetailGroup(
            root=[
                RegmemDetail[str](slug="name", value="A"),
                RegmemDetail[Decimal](slug="amount", value=Decimal("1.50")),
            ]
        ),
        RegmemDetailGroup(
            root=[
                RegmemDetail[str](slug="name", value="B"),
                RegmemDetail[Decimal](slug="amount", value=Decimal("2.50")),
            ]
        ),
    ]
    detail = RegmemDetailContainer(slug="payments", value=rows)
    assert detail.dict_value() == [
        {"name": "A", "amount": Decimal("1.50")},
        {"name": "B", "amount": Decimal("2.50")},
    ]
    assert [item.slug for item in detail.iter_sub_details()] == [
        "name",
        "amount",
        "name",
        "amount",
    ]
    entry = RegmemEntry(
        content="Payments", details=RegmemDetailGroup(root=[detail])
    )
    assert entry.details_dict(reduce={"payments": ["name", "amount"]}) == {
        "id": entry.comparable_id,
        "content": "Payments",
        "name": ["A", "B"],
        "amount": [Decimal("1.50"), Decimal("2.50")],
    }


def test_popolo_relationship_and_party_mutations_match_upstream() -> None:
    """Verify Rust-backed Popolo relationships and membership mutations."""

    popolo = Popolo.model_validate(
        {
            "organizations": [
                {"id": "house-of-commons", "name": "Commons"},
                {"id": "labour", "name": "Labour", "classification": "party"},
                {"id": "independent", "name": "Independent", "classification": "party"},
            ],
            "posts": [
                {
                    "id": "uk.org.publicwhip/cons/1",
                    "label": "Example",
                    "role": "MP",
                    "organization_id": "house-of-commons",
                }
            ],
            "persons": [{"id": "uk.org.publicwhip/person/1"}],
            "memberships": [
                {
                    "id": "uk.org.publicwhip/member/1",
                    "person_id": "uk.org.publicwhip/person/1",
                    "organization_id": "house-of-commons",
                    "on_behalf_of_id": "labour",
                    "post_id": "uk.org.publicwhip/cons/1",
                    "role": "MP",
                    "start_date": "2020-01-01",
                }
            ],
        }
    )
    person = popolo.persons["uk.org.publicwhip/person/1"]
    membership = person.memberships()[0]
    assert person.reduced_id() == "1"
    assert membership.person().id == person.id
    assert membership.post().organization().id == "house-of-commons"
    assert membership.organization().id == "house-of-commons"
    assert membership.on_behalf_of().id == "labour"
    assert person.membership_on_date(date(2022, 1, 1), chamber=Chamber.COMMONS).id == membership.id
    assert person.latest_membership(Chamber.COMMONS).id == membership.id

    person.remove_whip(change_date=date(2024, 1, 1), source_url="example")
    assert person.memberships()[-1].on_behalf_of_id == "independent"
    assert person.memberships()[-1].start_reason == MembershipReason.WHIP_REMOVED
    person.restore_whip(change_date=date(2024, 1, 2))
    assert person.memberships()[-1].on_behalf_of_id == "labour"
    assert person.memberships()[-1].start_reason == MembershipReason.WHIP_RESTORED


def test_interests_validate_cli_handles_directories(tmp_path) -> None:
    """Verify the interests command validates every JSON register in a folder."""

    (tmp_path / "register.json").write_text(
        '{"chamber":"house-of-commons","published_date":"2024-01-01"}'
    )
    (tmp_path / "ignored.txt").write_text("not a register")
    result = CliRunner().invoke(app, ["interests", "validate", str(tmp_path)])
    assert result.exit_code == 0
    assert "1 valid, 0 invalid" in result.stdout


def test_merge_people_moves_names_and_identifiers_and_installs_redirect() -> None:
    """Verify merging people retains data and installs a redirect."""
    popolo = Popolo.model_validate(
        {
            "persons": [
                {
                    "id": "uk.org.publicwhip/person/1",
                    "identifiers": [{"scheme": "one", "identifier": "1"}],
                },
                {
                    "id": "uk.org.publicwhip/person/2",
                    "identifiers": [{"scheme": "two", "identifier": "2"}],
                    "other_names": [
                        {
                            "given_name": "Second",
                            "family_name": "Person",
                            "note": "Main",
                        }
                    ],
                },
            ]
        }
    )
    popolo.persons.merge_people(
        "uk.org.publicwhip/person/1", "uk.org.publicwhip/person/2"
    )
    merged = popolo.persons["uk.org.publicwhip/person/1"]
    assert merged.get_identifier("two") == "2"
    assert "Second Person" in merged.all_name_variants()
    assert popolo.persons["uk.org.publicwhip/person/2"].id == merged.id


def test_alt_name_requires_complete_parts_or_one_name() -> None:
    """Verify incomplete alternative names are rejected."""
    person = Person(id="uk.org.publicwhip/person/1")
    with pytest.raises(ValueError, match="Both given and last"):
        person.add_alt_name(given_name="Ada")
    with pytest.raises(ValueError, match="Either one_name"):
        person.add_alt_name()


def test_redirects_resolve_and_remain_enumerable() -> None:
    """Verify person and membership redirects resolve transparently."""
    popolo = Popolo.model_validate(
        {
            "persons": [
                {"id": "uk.org.publicwhip/person/1"},
                {
                    "id": "uk.org.publicwhip/person/2",
                    "redirect": "uk.org.publicwhip/person/1",
                },
            ],
            "memberships": [
                {
                    "id": "uk.org.publicwhip/member/1",
                    "person_id": "uk.org.publicwhip/person/1",
                    "start_date": "2020-01-01",
                },
                {
                    "id": "uk.org.publicwhip/member/2",
                    "redirect": "uk.org.publicwhip/member/1",
                },
            ],
        }
    )
    assert popolo.persons["uk.org.publicwhip/person/2"].id == (
        "uk.org.publicwhip/person/1"
    )
    assert popolo.memberships["uk.org.publicwhip/member/2"].id == (
        "uk.org.publicwhip/member/1"
    )
    assert len(popolo.persons.redirects()) == 1
    assert len(popolo.memberships.redirects()) == 1


def test_collection_removal_keeps_lookup_indexes_consistent() -> None:
    """Verify collection removal invalidates and rebuilds ID indexes."""
    popolo = minimal_popolo()
    organizations = popolo.organizations
    assert "missing" not in organizations
    assert organizations["labour"].name == "Labour"
    removed = organizations.pop("house-of-commons")
    assert removed.id == "house-of-commons"
    assert organizations["labour"].name == "Labour"
    organizations["labour"].delete()
    assert organizations.get("labour") is None


def test_one_part_alt_name_is_added_once() -> None:
    """Verify a one-part alternative name is not duplicated."""
    person = Person(id="uk.org.publicwhip/person/2")
    person.add_alt_name(one_name="Banksy")
    assert person.all_name_variants() == ["Banksy"]


def test_transcript_directory_mode_only_validates_xml_children(tmp_path) -> None:
    """Verify transcript directory validation ignores non-XML children."""
    (tmp_path / "one.xml").write_text("<publicwhip/>")
    (tmp_path / "two.xml").write_text("<publicwhip/>")
    (tmp_path / "ignored.txt").write_text("not XML")
    result = CliRunner().invoke(app, ["transcript", "validate", str(tmp_path)])
    assert result.exit_code == 0, result.stdout
    assert "2 valid, 0 invalid" in result.stdout


def test_name_change_boundary_matches_upstream() -> None:
    """Verify a replacement name starts after the previous name closes."""
    person = minimal_popolo().persons["uk.org.publicwhip/person/1"]
    person.change_main_name("Augusta", "King", date(2024, 1, 10))
    assert person.get_main_name(date(2024, 1, 9)).nice_name() == "Ada Example"
    assert person.get_main_name(date(2024, 1, 10)).nice_name() == "Augusta King"


def test_name_models_expose_upstream_human_variants() -> None:
    """Verify public name models match upstream display forms."""
    basic = BasicPersonName(family_name="Example", given_name="Ada", note="Main")
    alt = AltName(name="A. Example", note="Alternate")
    bishop = LordName(
        honorific_prefix="Bishop",
        lordofname="Norwich",
        surname="Smith",
        given_name="Ada",
        note="Main",
    )
    assert basic.nice_name() == "Ada Example"
    assert alt.nice_name() == "A. Example"
    assert bishop.nice_name() == "Bishop of Norwich"
    assert bishop.original_full_name() == "Ada Smith"
    assert bishop.name_variants(include_original_name=True) == [
        "Bishop of Norwich",
        "The Bishop of Norwich",
        "The Lord Bishop of Norwich",
        "Ada Smith",
    ]


def test_person_identifier_replace_and_if_missing_semantics() -> None:
    """Verify identifier replacement follows the upstream API."""
    person = minimal_popolo().persons["uk.org.publicwhip/person/1"]
    assert person.get_identifier("external") == "old"
    assert person.add_identifer("external", "old", if_missing=True) is False
    assert person.add_identifer("external", "new") is True
    assert person.get_identifier("external") == "new"
