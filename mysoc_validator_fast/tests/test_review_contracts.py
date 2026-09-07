"""
Compatibility checks for the enum, serde and binding changes from PR review.
"""

from __future__ import annotations

import json
from datetime import date

import pytest

from mysoc_validator_fast import (
    LordName,
    Membership,
    MembershipReason,
    Organization,
    Person,
    Popolo,
    RegmemAnnotation,
    RegmemDetail,
    RegmemEntry,
    RegmemPerson,
    RegmemRegister,
)


@pytest.mark.parametrize(
    "classification",
    [
        "party",
        "chamber",
        "metro",
        "committee",
        "cross_party_group",
        "local_authority",
        "combined_authority",
        "other",
    ],
)
def test_organization_classification_agrees_across_entry_points(
    classification: str,
) -> None:
    """
    Constructed values, serde models and live handles use the same spelling.
    """
    organization = Organization("body", "Body", classification=classification)
    assert organization.classification == classification
    assert organization.model_dump()["classification"] == classification
    assert (
        Organization.model_validate(organization.model_dump()).classification
        == classification
    )
    popolo = Popolo.model_validate({"organizations": [organization.model_dump()]})
    assert popolo.organizations["body"].classification == classification
    dumped = json.loads(popolo.to_json_str())
    assert dumped["organizations"][0]["classification"] == classification


@pytest.mark.parametrize("reason", list(MembershipReason))
def test_membership_reason_setters_share_the_json_vocabulary(
    reason: MembershipReason,
) -> None:
    """
    Converted setters accept every public reason, including the blank value.
    """
    membership = Membership(id="member/1", person_id="uk.org.publicwhip/person/1")
    membership.start_reason = reason
    membership.end_reason = reason
    assert membership.start_reason == reason.value
    assert membership.end_reason == reason.value
    dumped = membership.model_dump()
    assert dumped["start_reason"] == reason.value
    assert Membership.model_validate(dumped).end_reason == reason.value
    with pytest.raises(ValueError, match="Unknown MembershipReason"):
        membership.end_reason = "invalid_value"
    assert membership.end_reason == reason.value


def test_date_accessors_preserve_live_mutations_and_detached_values() -> None:
    """
    Date macros preserve standalone copies, live setters and optional dates.
    """
    popolo = Popolo.model_validate(
        {
            "persons": [
                {
                    "id": "uk.org.publicwhip/person/1",
                    "birth_date": "1980-05",
                    "death_date": "2024-01-01",
                }
            ],
            "memberships": [
                {
                    "id": "member/1",
                    "person_id": "uk.org.publicwhip/person/1",
                    "start_date": "2020-01-01",
                }
            ],
            "organizations": [{"id": "body", "name": "Body", "founding_date": "1990"}],
        }
    )
    membership = popolo.memberships["member/1"]
    original = membership.start_date
    membership.start_date = date(2020, 2, 1)
    membership.end_date = "2024-01-01"
    membership.end_reason = MembershipReason.RESIGNED
    assert original == "2020-01-01"
    assert popolo.memberships["member/1"].start_date == "2020-02-01"
    assert popolo.memberships["member/1"].end_date == "2024-01-01"
    assert popolo.memberships["member/1"].end_reason == "resigned"
    with pytest.raises(ValueError):
        membership.end_date = "invalid-date"
    assert membership.end_date == "2024-01-01"
    person = popolo.persons["uk.org.publicwhip/person/1"]
    assert person.birth_date == "1980-05"
    assert person.birth_date is not person.birth_date
    assert person.death_date == "2024-01-01"
    assert Person("uk.org.publicwhip/person/2").birth_date is None
    organization = popolo.organizations["body"]
    assert organization.founding_date == "1990"
    assert organization.dissolution_date is None
    organization.delete()
    for field in ["founding_date", "dissolution_date"]:
        with pytest.raises(KeyError):
            getattr(organization, field)
    assert len(popolo.organizations) == 0


@pytest.mark.parametrize("surname", [None, "", "Smith"])
def test_lord_name_validation_agrees_between_python_and_json(
    surname: str | None,
) -> None:
    """
    Empty strings use upstream truthiness while nonempty surnames need a style.
    """
    if surname:
        with pytest.raises(ValueError, match="also needs"):
            LordName(note="Main", honorific_prefix="Lord", surname=surname)
        with pytest.raises(ValueError, match="also needs"):
            Popolo.model_validate(
                {
                    "persons": [
                        {
                            "id": "uk.org.publicwhip/person/1",
                            "other_names": [
                                {
                                    "note": "Main",
                                    "honorific_prefix": "Lord",
                                    "surname": surname,
                                }
                            ],
                        }
                    ]
                }
            )
    else:
        name = LordName(note="Main", honorific_prefix="Lord", surname=surname)
        assert name.original_full_name() is None
        assert name.nice_name() == "Unknown"


@pytest.mark.parametrize("language", ["en", "cy", None])
def test_register_language_constructor_and_json_round_trip(
    language: str | None,
) -> None:
    """
    Explicit None permits mixed languages and remains distinct from omission.
    """
    languages = ["en", "cy"] if language is None else [language]
    persons = [
        RegmemPerson(
            person_id=f"p{index}",
            person_name="Example",
            published_date=date(2025, 1, 20),
            chamber="house-of-commons",
            language=person_language,
        )
        for index, person_language in enumerate(languages)
    ]
    register = RegmemRegister(language=language, persons=persons)
    assert register.language == language
    dumped = register.model_dump_json()
    assert json.loads(dumped)["language"] == language
    reloaded = RegmemRegister.model_validate_json(dumped)
    assert reloaded.language == language
    assert [person.language for person in reloaded.persons] == languages
    assert RegmemRegister().language == "en"
    if language is None:
        with pytest.raises(ValueError, match="same language"):
            RegmemRegister(persons=persons)


def test_unknown_enum_inputs_raise_instead_of_silently_defaulting() -> None:
    """
    Python constructors reject spellings that the JSON models reject.
    """
    with pytest.raises(ValueError, match="Unknown language"):
        RegmemRegister(language="fr")
    with pytest.raises(ValueError, match="Unknown language"):
        RegmemPerson(
            person_id="p1",
            person_name="Example",
            published_date="2025-01-20",
            chamber="house-of-commons",
            language="fr",
        )
    with pytest.raises(ValueError, match="Unknown content format"):
        RegmemAnnotation(author="Example", content="Text", content_format="html")
    with pytest.raises(ValueError, match="Unknown content format"):
        RegmemEntry(content="Text", content_format="html")
    with pytest.raises(ValueError, match="Unknown entry type"):
        RegmemEntry(content="Text", info_type="invalid_value")
    with pytest.raises(ValueError, match="Unknown common key"):
        RegmemDetail(slug="example", value="Example", common_key="invalid_value")


def test_typed_extra_is_a_snapshot_and_explicit_writes_update_the_record() -> None:
    """
    Project schemas remain subclassable without replacing Rust's live storage.
    """
    from mysoc_validator_fast import MembershipExtra

    class ProjectExtra(MembershipExtra):
        """
        A project-specific field validated by Pydantic.
        """

        revision: int

    membership = Membership(
        id="member/1",
        person_id="uk.org.publicwhip/person/1",
        extra=ProjectExtra(revision=1),
    )
    snapshot = membership.get_extra_as(ProjectExtra)
    assert isinstance(snapshot, ProjectExtra)
    snapshot.revision = 2
    assert membership.get_extra("revision") == 1
    membership.set_extra("revision", snapshot.revision)
    assert membership.get_extra_as(ProjectExtra).revision == 2
