import json
from datetime import date

import pytest
from mysoc_validator_fast import (
    Area,
    Membership,
    MembershipExtra,
    LordName,
    Organization,
    Person,
    Popolo,
    Post,
)


@pytest.fixture(
    params=[
        lambda: Membership(id="member/1", person_id="uk.org.publicwhip/person/1"),
        lambda: Organization(id="body", name="Body"),
        lambda: Person(id="uk.org.publicwhip/person/1"),
        lambda: Area(name="Area"),
        lambda: Post(id="post", label="Label", organization_id="body", role="role"),
    ]
)
def model_with_extra(request):
    return request.param()


def test_extra_helpers_are_available_on_all_popolo_models(model_with_extra):
    assert model_with_extra.extra is None
    assert model_with_extra.get_extra("anything") is None
    result = model_with_extra.set_extra("a", 1).set_extra("b", {"nested": True})
    assert result is model_with_extra
    assert model_with_extra.get_extra("a") == 1
    assert model_with_extra.extra["b"] == {"nested": True}


def test_extra_proxy_mutations_are_live():
    organization = Organization(id="body", name="Body").set_extra("revision", 1)
    organization.extra["revision"] = 2
    organization.extra["metadata"] = {"source": "example"}
    assert organization.get_extra("revision") == 2
    assert organization.get_extra("metadata") == {"source": "example"}


class TaggedMembershipExtra(MembershipExtra):
    tag: str = ""


def test_typed_extra_input_and_get_extra_as():
    membership = Membership(
        id="member/1",
        person_id="uk.org.publicwhip/person/1",
        extra=TaggedMembershipExtra(tag="whip-removed"),
    )
    assert membership.get_extra("tag") == "whip-removed"
    membership.set_extra("tag", "whip-restored")
    typed = membership.get_extra_as(TaggedMembershipExtra)
    assert isinstance(typed, TaggedMembershipExtra)
    assert typed.tag == "whip-restored"


def test_lord_name_python_api_matches_upstream_variants():
    bishop = LordName(
        note="Main", honorific_prefix="Bishop", lordofname="Norwich"
    )
    assert bishop.nice_name() == "Bishop of Norwich"
    assert bishop.name_variants() == [
        "Bishop of Norwich",
        "The Bishop of Norwich",
        "The Lord Bishop of Norwich",
    ]
    peer = LordName(
        note="Main",
        honorific_prefix="Lord",
        lordname="Stratford",
        given_name="Tony",
        surname="Banks",
    )
    assert peer.original_full_name() == "Tony Banks"
    assert peer.name_variants(include_original_name=True) == [
        "Lord Stratford",
        "Tony Banks",
    ]


def test_lord_name_python_api_rejects_invalid_surname_style():
    with pytest.raises(ValueError, match="also needs a lordname"):
        LordName(note="Main", honorific_prefix="Baron", surname="Smith")


@pytest.mark.parametrize(
    ("model", "field", "canonical"),
    [
        (Organization(id="body", name="Corff / Body"), "name", "Corff / Body"),
        (Person(id="uk.org.publicwhip/person/1", biography="Biography"), "biography", "Biography"),
        (Area(name="Area"), "name", "Area"),
        (Post(id="post", label="Label", role="role", organization_id="body"), "label", "Label"),
        (Membership(id="member/1", person_id="uk.org.publicwhip/person/1", label="Member"), "label", "Member"),
    ],
)
def test_localised_values_fall_back_and_can_be_set(model, field, canonical):
    assert model.get_localised_value(field, "en") == canonical
    model.set_localised_value(field, "cy", "Cymraeg")
    assert model.get_localised_value(field, "cy") == "Cymraeg"
    assert model.extra.localised_values[field] == {"cy": "Cymraeg"}


def test_localised_values_preserve_unknown_extra_and_are_independent():
    organization = Organization(
        id="body",
        name="Body",
        abstract_="Abstract",
        description="Description",
        extra={"custom": {"revision": 1}},
    )
    organization.set_localised_value("name", "cy", "Corff")
    organization.set_localised_value("abstract", "cy", "Crynodeb")
    organization.set_localised_value("description", "cy", "Disgrifiad")
    assert organization.get_extra("custom") == {"revision": 1}
    assert organization.get_localised_value("name", "cy") == "Corff"
    assert organization.get_localised_value("abstract", "cy") == "Crynodeb"
    assert organization.get_localised_value("description", "cy") == "Disgrifiad"


def test_localised_value_rejects_unknown_field():
    organization = Organization(id="body", name="Body")
    with pytest.raises(ValueError, match="Field label not found"):
        organization.get_localised_value("label", "en")
    with pytest.raises(ValueError, match="Field label not found"):
        organization.set_localised_value("label", "cy", "Label")


def test_supplemental_models_round_trip_and_expose_metadata():
    source = {
        "organizations": [
            {"id": "welsh-parliament", "name": "Senedd"},
            {
                "id": "committee",
                "name": "Committee",
                "classification": "committee",
                "abstract": "Abstract",
                "description": "Description",
                "founding_date": "2021-05",
                "dissolution_date": "2026-04-06",
                "links": ["https://example.com/committee"],
                "parent_id": "welsh-parliament",
                "extra": {"localised_values": {"name": {"cy": "Pwyllgor"}}},
            },
        ],
        "posts": [
            {
                "id": "committee-chair",
                "organization_id": "committee",
                "label": "Chair",
                "role": "chair",
                "area": {"name": "Wales", "extra": {"code": "WLS"}},
            }
        ],
    }
    popolo = Popolo.model_validate_json(json.dumps(source))
    organization = popolo.organizations["committee"]
    assert organization.abstract == "Abstract"
    assert organization.description == "Description"
    assert str(organization.founding_date) == "2021-05"
    assert organization.links == ["https://example.com/committee"]
    assert organization.parent_id == "welsh-parliament"
    assert organization.get_localised_value("name", "cy") == "Pwyllgor"
    area = popolo.posts["committee-chair"].area
    assert area.name == "Wales"
    area.extra["code"] = "CYM"
    assert json.loads(popolo.model_dump_json())["posts"][0]["area"]["extra"]["code"] == "CYM"
    expected = {**source, "memberships": [], "persons": []}
    expected["posts"][0]["area"]["extra"]["code"] = "CYM"
    assert json.loads(popolo.model_dump_json()) == expected


def test_individual_models_support_pydantic_style_validation_and_dumping():
    organization_data = {
        "id": "committee",
        "name": "Committee",
        "classification": "committee",
        "abstract": "Abstract",
        "description": "Description",
        "parent_id": "chamber",
        "extra": {"custom": True},
    }
    organization = Organization.model_validate(organization_data)
    assert organization.model_dump() == organization_data

    area_data = {"name": "Wales", "other_names": ["Cymru"], "extra": {"code": "WLS"}}
    assert Area.model_validate(area_data).model_dump() == area_data


def make_overlap_popolo() -> Popolo:
    return Popolo.model_validate_json(
        json.dumps(
            {
                "organizations": [
                    {"id": "lords", "name": "Lords"},
                    {"id": "committee", "name": "Committee"},
                ],
                "persons": [{"id": "uk.org.publicwhip/person/1"}],
                "memberships": [
                    {
                        "id": "member/1",
                        "person_id": "uk.org.publicwhip/person/1",
                        "organization_id": "lords",
                        "start_date": "2020-01-01",
                        "end_date": "2020-12-31",
                    }
                ],
            }
        )
    )


def test_postless_overlap_is_scoped_to_organization():
    popolo = make_overlap_popolo()
    popolo.memberships.append(
        Membership(
            id="member/2",
            person_id="uk.org.publicwhip/person/1",
            organization_id="committee",
            start_date=date(2020, 6, 1),
            end_date=date(2020, 8, 1),
        )
    )
    with pytest.raises(ValueError, match="overlap"):
        popolo.memberships.append(
            Membership(
                id="member/3",
                person_id="uk.org.publicwhip/person/1",
                organization_id="lords",
                start_date=date(2020, 6, 1),
                end_date=date(2020, 8, 1),
            )
        )


def make_historical_name_popolo() -> Popolo:
    return Popolo.model_validate_json(
        json.dumps(
            {
                "organizations": [{"id": "chamber", "name": "Chamber"}],
                "posts": [
                    {
                        "id": "seat",
                        "label": "Seat",
                        "role": "member",
                        "organization_id": "chamber",
                    }
                ],
                "persons": [
                    {
                        "id": "uk.org.publicwhip/person/1",
                        "other_names": [
                            {
                                "given_name": "Jane",
                                "family_name": "Old",
                                "note": "Main",
                                "start_date": "2000-01-01",
                                "end_date": "2005-01-01",
                            },
                            {
                                "honorific_prefix": "Lord",
                                "lordname": "New",
                                "note": "Main",
                                "start_date": "2010-01-01",
                            },
                        ],
                    }
                ],
                "memberships": [
                    {
                        "id": "member/1",
                        "person_id": "uk.org.publicwhip/person/1",
                        "post_id": "seat",
                        "start_date": "2000-01-01",
                        "end_date": "2005-01-01",
                    }
                ],
            }
        )
    )


def test_historical_name_lookup_flag_and_membership_eligibility():
    popolo = make_historical_name_popolo()
    assert (
        popolo.persons.from_name(
            "Lord New", chamber_id="chamber", date=date(2001, 1, 1)
        )
        is None
    )
    person = popolo.persons.from_name(
        "Lord New",
        chamber_id="chamber",
        date=date(2001, 1, 1),
        include_historical_names=True,
    )
    assert person.id == "uk.org.publicwhip/person/1"
    assert (
        popolo.persons.from_name(
            "Lord New",
            chamber_id="chamber",
            date=date(2008, 1, 1),
            include_historical_names=True,
        )
        is None
    )


def test_ambiguous_historical_name_lookup_raises():
    source = json.loads(make_historical_name_popolo().model_dump_json())
    source["persons"].append(
        {
            "id": "uk.org.publicwhip/person/2",
            "other_names": [{"name": "Lord New", "note": "Alternate"}],
        }
    )
    source["memberships"].append(
        {
            "id": "member/2",
            "person_id": "uk.org.publicwhip/person/2",
            "post_id": "seat",
            "start_date": "2000-01-01",
            "end_date": "2005-01-01",
        }
    )
    popolo = Popolo.model_validate_json(json.dumps(source))
    with pytest.raises(ValueError, match="Ambiguous person name"):
        popolo.persons.from_name(
            "Lord New",
            chamber_id="chamber",
            date=date(2001, 1, 1),
            include_historical_names=True,
        )
