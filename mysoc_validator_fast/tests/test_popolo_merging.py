"""
Regression coverage for upstream's staged, validated Popolo merges.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest
import requests
from mysoc_validator_fast._mysoc_validator_fast import Popolo as RustPopolo

from mysoc_validator_fast import Popolo

BASE = """{
    "organizations": [{"id": "commons", "name": "Commons"}],
    "posts": [{"id": "seat", "label": "Seat", "role": "member", "organization_id": "commons"}],
    "persons": [{"id": "uk.org.publicwhip/person/1"}],
    "memberships": [{
        "id": "member/1", "person_id": "uk.org.publicwhip/person/1",
        "post_id": "seat", "start_date": "2020-01-01", "end_date": "2020-12-31"
    }]
}"""

EXTRA = """{
    "organizations": [{"id": "committee", "name": "Committee", "parent_id": "commons"}],
    "posts": [{"id": "chair", "label": "Chair", "role": "chair", "organization_id": "committee"}],
    "persons": [
        {"id": "uk.org.publicwhip/person/2"},
        {"id": "uk.org.publicwhip/person/3", "redirect": "uk.org.publicwhip/person/2"}
    ],
    "memberships": [
        {"id": "member/2", "person_id": "uk.org.publicwhip/person/1", "post_id": "chair"},
        {"id": "member/3", "person_id": "uk.org.publicwhip/person/2", "post_id": "seat"},
        {"id": "member/4", "redirect": "member/3"}
    ]
}"""


@pytest.mark.parametrize("native", [False, True])
def test_update_merges_all_collections_and_redirects(native: bool) -> None:
    """
    Extra references resolve against both original and newly added records.
    """

    base = Popolo.from_json_str(BASE)
    with pytest.raises(ValueError):
        Popolo.from_json_str(EXTRA)
    extra = Popolo.from_json_str(EXTRA, cross_validate=False)
    original = base.persons["uk.org.publicwhip/person/1"]
    if native:
        assert RustPopolo.update(base, extra) is base
    else:
        assert base.update(extra) is base
    assert base.organizations["committee"].parent_id == "commons"
    assert base.posts["chair"].organization_id == "committee"
    assert base.persons["uk.org.publicwhip/person/3"].id == "uk.org.publicwhip/person/2"
    assert base.memberships["member/4"].id == "member/3"
    assert len(original.memberships()) == 2
    original.biography = "Live handle"
    assert base.persons[original.id].biography == "Live handle"
    extra.persons["uk.org.publicwhip/person/2"].biography = "Detached source"
    assert base.persons["uk.org.publicwhip/person/2"].biography is None
    assert Popolo.from_json_str(base.to_json_str()).to_json_str() == base.to_json_str()


@pytest.mark.parametrize(
    "collection", ["organizations", "posts", "persons", "memberships"]
)
def test_update_rejects_existing_ids(collection: str) -> None:
    """
    Duplicates raise instead of being skipped or replacing existing records.
    """

    base = Popolo.from_json_str(BASE)
    incoming = Popolo.from_json_str(
        json.dumps({collection: json.loads(BASE)[collection]}), cross_validate=False
    )
    before = base.to_json_str()
    with pytest.raises(ValueError, match="Duplicate"):
        base.update(incoming)
    assert base.to_json_str() == before


@pytest.mark.parametrize(
    "extra, message",
    [
        (
            '{"organizations": [{"id": "child", "name": "Child", "parent_id": "missing"}]}',
            "parent organization",
        ),
        (
            '{"posts": [{"id": "new-seat", "label": "Seat", "role": "member", "organization_id": "missing"}]}',
            "invalid organization",
        ),
        (
            '{"memberships": [{"id": "member/2", "person_id": "uk.org.publicwhip/person/999"}]}',
            "person_id",
        ),
        (
            '{"memberships": [{"id": "member/2", "person_id": "uk.org.publicwhip/person/1", "post_id": "missing"}]}',
            "post_id",
        ),
        (
            '{"memberships": [{"id": "member/2", "person_id": "uk.org.publicwhip/person/1", "organization_id": "missing"}]}',
            "organization_id",
        ),
    ],
)
def test_update_checks_combined_references(extra: str, message: str) -> None:
    """
    Disabling cross-checks for an extra does not let invalid references survive.
    """

    base = Popolo.from_json_str(BASE)
    incoming = Popolo.from_json_str(extra, cross_validate=False)
    with pytest.raises(ValueError, match=message):
        base.update(incoming)


def test_update_checks_each_stage_and_preserves_partial_changes() -> None:
    """
    An invalid post stops the merge before people or memberships are added.
    """

    base = Popolo.from_json_str(BASE)
    extra = Popolo.from_json_str(
        EXTRA.replace('"organization_id": "committee"', '"organization_id": "missing"'),
        cross_validate=False,
    )
    with pytest.raises(ValueError, match="invalid organization"):
        base.update(extra)
    assert base.organizations["committee"].name == "Committee"
    assert base.posts["chair"].organization_id == "missing"
    assert len(base.persons) == 1
    assert len(base.memberships) == 1


@pytest.mark.parametrize("start", ["2020-06-01", "2020-12-31", "2020"])
def test_update_rejects_overlaps_with_existing_memberships(start: str) -> None:
    """
    Insertion checks include shared boundaries and approximate dates.
    """

    base = Popolo.from_json_str(BASE)
    extra = Popolo.from_json_str(
        json.dumps(
            {
                "memberships": [
                    {
                        "id": "member/2",
                        "person_id": "uk.org.publicwhip/person/1",
                        "post_id": "seat",
                        "start_date": start,
                        "end_date": "2021-01-01",
                    }
                ]
            }
        ),
        cross_validate=False,
    )
    with pytest.raises(ValueError, match="overlaps"):
        base.update(extra)
    assert len(base.memberships) == 1


def test_update_with_self_is_a_no_op() -> None:
    """
    Updating through the Python wrapper avoids locking the same dataset twice.
    """

    base = Popolo.from_json_str(BASE)
    before = base.to_json_str()
    assert base.update(base) is base
    assert base.to_json_str() == before


def test_merge_retains_upstream_incoming_batch_overlap_policy() -> None:
    """
    Upstream checks new memberships against the destination, not each other.
    """

    base = Popolo.from_json_str(BASE)
    extra = Popolo.from_json_str(
        json.dumps(
            {
                "memberships": [
                    {
                        "id": "member/2",
                        "person_id": "uk.org.publicwhip/person/1",
                        "post_id": "seat",
                        "start_date": "2021-01-01",
                        "end_date": "2021-12-31",
                    },
                    {
                        "id": "member/3",
                        "person_id": "uk.org.publicwhip/person/1",
                        "post_id": "seat",
                        "start_date": "2021-06-01",
                        "end_date": "2021-12-31",
                    },
                ]
            }
        ),
        cross_validate=False,
    )
    base.update(extra)
    assert len(base.memberships) == 3
    with pytest.raises(ValueError, match="overlaps"):
        Popolo.from_json_str(base.to_json_str())


@pytest.mark.parametrize("scope", [None, "commons"])
def test_merge_checks_historical_and_postless_overlaps(scope: str | None) -> None:
    """
    Historical postless memberships use insertion checks even without a scope.
    """

    base = Popolo.from_json_str(
        json.dumps(
            {
                "organizations": [{"id": "commons", "name": "Commons"}],
                "persons": [{"id": "uk.org.publicwhip/person/1"}],
                "memberships": [
                    {
                        "id": "member/1",
                        "person_id": "uk.org.publicwhip/person/1",
                        "organization_id": scope,
                        "start_date": "1800-01-01",
                        "end_date": "1801-01-01",
                    }
                ],
            }
        )
    )
    extra = Popolo.from_json_str(
        json.dumps(
            {
                "memberships": [
                    {
                        "id": "member/2",
                        "person_id": "uk.org.publicwhip/person/1",
                        "organization_id": scope,
                        "start_date": "1800-06-01",
                        "end_date": "1801-01-01",
                    },
                ]
            }
        ),
        cross_validate=False,
    )
    with pytest.raises(ValueError, match="overlaps"):
        base.update(extra)
    assert len(base.memberships) == 1


@pytest.mark.parametrize(
    "invalid",
    [
        '{"persons": [{"id": "uk.org.publicwhip/person/1"}, {"id": "uk.org.publicwhip/person/1"}]}',
        '{"persons": [{"id": "invalid"}]}',
        '{"memberships": [{"id": "member/2", "person_id": "uk.org.publicwhip/person/1", "start_date": "2021-01-01", "end_date": "2020-01-01"}]}',
        '{"persons": [{"id": "uk.org.publicwhip/person/1", "unknown_field": true}]}',
    ],
)
def test_disabling_cross_validation_still_checks_records(invalid: str) -> None:
    """
    Extras defer cross-checks, retaining uniqueness, field and date validation.
    """

    with pytest.raises(ValueError):
        Popolo.from_json_str(invalid, cross_validate=False)


@pytest.mark.parametrize("loader", ["path", "url", "parlparse"])
def test_multi_source_loads_validate_the_combined_dataset(
    loader: str, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """
    File, URL and parlparse extras share the same validated merge behavior.
    """

    base_path = tmp_path / "people.json"
    extra_path = tmp_path / "extra.json"
    base_path.write_text(BASE)
    extra_path.write_text(EXTRA)

    def get(url: str) -> requests.Response:
        """
        Serve local fixtures for the two expected URLs.
        """

        response = requests.Response()
        response.status_code = 200
        response._content = (BASE if url.endswith("people.json") else EXTRA).encode()
        return response

    monkeypatch.setattr(requests, "get", get)
    if loader == "path":
        result = Popolo.from_path([base_path, extra_path])
    elif loader == "url":
        result = Popolo.from_url(
            ["https://example.com/people.json", "https://example.com/extra.json"]
        )
    else:
        result = Popolo.from_parlparse(extras=["extra"])
    assert len(result.organizations) == 2
    assert len(result.posts) == 2
    assert len(result.persons.root) == 3
    assert len(result.memberships.root) == 4


@pytest.mark.parametrize("loader", ["path", "url", "parlparse"])
def test_multi_source_loads_raise_on_invalid_extras(
    loader: str, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """
    Loader helpers propagate merge errors instead of returning partial results.
    """

    base_path = tmp_path / "people.json"
    extra_path = tmp_path / "extra.json"
    base_path.write_text(BASE)
    invalid = EXTRA.replace('"parent_id": "commons"', '"parent_id": "missing"')
    extra_path.write_text(invalid)

    def get(url: str) -> requests.Response:
        """
        Serve an extra whose parent organization never resolves.
        """

        response = requests.Response()
        response.status_code = 200
        response._content = (BASE if url.endswith("people.json") else invalid).encode()
        return response

    monkeypatch.setattr(requests, "get", get)
    with pytest.raises(ValueError, match="parent organization"):
        if loader == "path":
            Popolo.from_path([base_path, extra_path])
        elif loader == "url":
            Popolo.from_url(
                ["https://example.com/people.json", "https://example.com/extra.json"]
            )
        else:
            Popolo.from_parlparse(extras=["extra"])
