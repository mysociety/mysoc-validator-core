import tempfile
from datetime import date, timedelta
from pathlib import Path

import pytest
import requests

from mysoc_validator_fast.dates import FixedDate
from mysoc_validator_fast.popolo import (
    Chamber,
    Membership,
    MembershipRedirect,
    Organization,
    Person,
    PersonRedirect,
    Popolo,
    Post,
)

iso = date.fromisoformat


@pytest.fixture(scope="session")
def popolo_data() -> Popolo:
    return Popolo.from_parlparse()


# ---------------------------------------------------------------------------
# Read / lookup
# ---------------------------------------------------------------------------


def test_lookup_from_id(popolo_data: Popolo) -> None:
    person = popolo_data.persons["uk.org.publicwhip/person/10001"]
    assert "Diane Abbott" in person.names_on_date(date=iso("2024-07-31"))


def test_lookup_from_identifer(popolo_data: Popolo) -> None:
    person = popolo_data.persons.from_identifier("172", scheme="datadotparl_id")
    assert "Diane Abbott" in person.names_on_date(date=iso("2024-07-31"))


def test_lookup_from_name(popolo_data: Popolo) -> None:
    person = popolo_data.persons.from_name(
        "Elizabeth Truss", chamber_id=Chamber.COMMONS, date=iso("2022-07-31")
    )
    assert person is not None
    assert person.id == "uk.org.publicwhip/person/24941"


# ---------------------------------------------------------------------------
# Membership mutation and append
# ---------------------------------------------------------------------------


def test_valid_addition(popolo_data: Popolo) -> None:
    person = popolo_data.persons["uk.org.publicwhip/person/10001"]
    last_membership = person.memberships()[-1]
    if last_membership.end_date == FixedDate.FUTURE:
        last_membership.end_date = last_membership.start_date + timedelta(days=1)
    new_start_date = last_membership.end_date + timedelta(days=1)
    new_end_date = new_start_date + timedelta(days=365)
    new_membership = Membership(
        id="uk.org.publicwhip/member/122323232",
        person_id=person.id,
        start_date=new_start_date,
        end_date=new_end_date,
        organization_id="labour",
        post_id=last_membership.post_id,
    )
    popolo_data.memberships.extend([new_membership])
    assert popolo_data.memberships.root[-1].id == "uk.org.publicwhip/member/122323232"


def test_invalid_overlapping_memberhsip(popolo_data: Popolo) -> None:
    person = popolo_data.persons["uk.org.publicwhip/person/10001"]
    last_membership = person.memberships()[-1]
    new_membership = Membership(
        id="uk.org.publicwhip/member/122323233",
        person_id=person.id,
        start_date=last_membership.start_date,
        end_date=last_membership.end_date,
        organization_id="labour",
        post_id=last_membership.post_id,
    )
    with pytest.raises(ValueError):
        popolo_data.memberships.extend([new_membership])


def test_membership_field_mutation_is_live(popolo_data: Popolo) -> None:
    """Mutating a field on a retrieved Membership should be visible on re-lookup."""
    person = popolo_data.persons["uk.org.publicwhip/person/10001"]
    m = person.memberships()[0]
    original_role = m.role
    m.role = "Test Role"
    # Re-fetch via the collection — must see the updated value
    m2 = popolo_data.memberships[m.id]
    assert m2.role == "Test Role"
    # Restore
    m2.role = original_role


# ---------------------------------------------------------------------------
# Person: create, append, mutate
# ---------------------------------------------------------------------------


def test_create_and_append_person(popolo_data: Popolo) -> None:
    new_person = Person(id="uk.org.publicwhip/person/99991")
    popolo_data.persons.append(new_person)
    assert "uk.org.publicwhip/person/99991" in popolo_data.persons
    retrieved = popolo_data.persons["uk.org.publicwhip/person/99991"]
    assert retrieved.id == "uk.org.publicwhip/person/99991"


def test_person_scalar_mutation_is_live(popolo_data: Popolo) -> None:
    """Setting a scalar field on a Person handle should be visible on re-lookup."""
    person = popolo_data.persons["uk.org.publicwhip/person/10001"]
    original = person.biography
    person.biography = "Updated biography"
    assert person.biography == "Updated biography"
    # Re-fetch via the collection — must see the updated value
    person2 = popolo_data.persons["uk.org.publicwhip/person/10001"]
    assert person2.biography == "Updated biography"
    # Restore
    person2.biography = original


def test_person_gender_mutation(popolo_data: Popolo) -> None:
    person = popolo_data.persons["uk.org.publicwhip/person/10001"]
    original = person.gender
    person.gender = "female"
    person2 = popolo_data.persons["uk.org.publicwhip/person/10001"]
    assert person2.gender == "female"
    person2.gender = original


# ---------------------------------------------------------------------------
# Organization: create, append, mutate
# ---------------------------------------------------------------------------


def test_create_and_append_organization(popolo_data: Popolo) -> None:
    new_org = Organization(id="test-party-xyz", name="Test Party XYZ")
    popolo_data.organizations.append(new_org)
    assert "test-party-xyz" in popolo_data.organizations
    org = popolo_data.organizations["test-party-xyz"]
    assert org.name == "Test Party XYZ"


def test_organization_name_mutation_is_live(popolo_data: Popolo) -> None:
    """Mutating an Organization's name should be visible on re-lookup."""
    org = popolo_data.organizations["labour"]
    original = org.name
    org.name = "Updated Labour"
    org2 = popolo_data.organizations["labour"]
    assert org2.name == "Updated Labour"
    org2.name = original


# ---------------------------------------------------------------------------
# Post: create, append, mutate
# ---------------------------------------------------------------------------


def test_create_and_append_post(popolo_data: Popolo) -> None:
    new_post = Post(
        id="uk.org.publicwhip/cons/99999",
        label="Test Constituency",
        role="Member of Parliament",
        organization_id="house-of-commons",
    )
    popolo_data.posts.append(new_post)
    assert "uk.org.publicwhip/cons/99999" in popolo_data.posts
    post = popolo_data.posts["uk.org.publicwhip/cons/99999"]
    assert post.label == "Test Constituency"
    assert post.organization_id == "house-of-commons"


def test_post_label_mutation_is_live(popolo_data: Popolo) -> None:
    """Mutating a Post's label should be visible on re-lookup."""
    post = popolo_data.posts.root[0]
    original = post.label
    post.label = "Updated Label"
    post2 = popolo_data.posts[post.id]
    assert post2.label == "Updated Label"
    post2.label = original


# ---------------------------------------------------------------------------
# Serialisation
# ---------------------------------------------------------------------------


def test_round_trip() -> None:
    branch = "master"
    parlparse_url = (
        f"https://raw.githubusercontent.com/mysociety/parlparse/{branch}/members/people.json"
    )
    original_text = requests.get(parlparse_url).text.strip()
    popolo = Popolo.model_validate_json(original_text)
    dumped_text = popolo.to_json_str()
    popolo2 = Popolo.model_validate_json(dumped_text)
    dumped_text2 = popolo2.to_json_str()
    assert dumped_text == dumped_text2, "Internal round trip failed"
    assert original_text == dumped_text, "External round trip failed"


def test_write_popolo(popolo_data: Popolo) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
        data_dir = Path(temp_dir, "data")
        data_dir.mkdir()
        dest = Path(data_dir, "people_test_dump.json")
        popolo_data.to_path(dest)
        assert dest.exists()
        Popolo.from_path(dest)
        dest.unlink()
        data_dir.rmdir()


# ---------------------------------------------------------------------------
# Duplicate rejection
# ---------------------------------------------------------------------------


def test_duplicate_person_rejected(popolo_data: Popolo) -> None:
    dup = Person(id="uk.org.publicwhip/person/10001")
    with pytest.raises(ValueError, match="Duplicate Person id"):
        popolo_data.persons.append(dup)


def test_duplicate_person_redirect_rejected(popolo_data: Popolo) -> None:
    dup = PersonRedirect(
        id="uk.org.publicwhip/person/10001",
        redirect="uk.org.publicwhip/person/10002",
    )
    with pytest.raises(ValueError, match="Duplicate PersonRedirect id"):
        popolo_data.persons.append(dup)


def test_duplicate_post_rejected(popolo_data: Popolo) -> None:
    existing: Post = popolo_data.posts.root[0]
    with pytest.raises(ValueError, match="Duplicate Post id"):
        popolo_data.posts.append(existing)


def test_duplicate_organization_rejected(popolo_data: Popolo) -> None:
    dup = Organization(id="house-of-commons", name="Duplicate")
    with pytest.raises(ValueError, match="Duplicate Organization id"):
        popolo_data.organizations.append(dup)


def test_duplicate_membership_redirect_rejected(popolo_data: Popolo) -> None:
    redirects = popolo_data.memberships.redirects()
    if not redirects:
        pytest.skip("No membership redirects in data")
    existing = redirects[0]
    dup = MembershipRedirect(id=existing.id, redirect=existing.redirect)
    with pytest.raises(ValueError, match="Duplicate MembershipRedirect id"):
        popolo_data.memberships.append(dup)
