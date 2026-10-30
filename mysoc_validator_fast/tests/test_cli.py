import json
from pathlib import Path

from typer.testing import CliRunner

from mysoc_validator_fast.__main__ import app

runner = CliRunner()

VALID_POPOLO = {
    "organizations": [
        {"id": "chamber", "name": "Chamber"},
        {"id": "party-a", "name": "Party A"},
        {"id": "party-b", "name": "Party B"},
        {"id": "independent", "name": "Independent"},
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
                }
            ],
        }
    ],
    "memberships": [
        {
            "id": "uk.org.publicwhip/member/1",
            "person_id": "uk.org.publicwhip/person/1",
            "organization_id": "chamber",
            "on_behalf_of_id": "party-a",
            "start_date": "2020-01-01",
        }
    ],
}


def write_popolo(path: Path) -> None:
    path.write_text(json.dumps(VALID_POPOLO))


def test_validate_popolo_path(tmp_path: Path):
    path = tmp_path / "people.json"
    write_popolo(path)
    result = runner.invoke(app, ["popolo", "validate", str(path)])
    assert result.exit_code == 0
    assert "Valid Popolo file" in result.stdout
    assert "4 organizations" in result.stdout


def test_validate_popolo_directory_finds_people_json(tmp_path: Path):
    write_popolo(tmp_path / "people.json")
    result = runner.invoke(app, ["popolo", "validate", str(tmp_path)])
    assert result.exit_code == 0
    assert "Valid Popolo file" in result.stdout


def test_validate_popolo_url(monkeypatch):
    class Response:
        text = json.dumps(VALID_POPOLO)

    monkeypatch.setattr(
        "mysoc_validator_fast.popolo.requests.get", lambda _url: Response()
    )
    result = runner.invoke(
        app, ["popolo", "validate", "https://example.com/people.json"]
    )
    assert result.exit_code == 0
    assert "Valid Popolo file" in result.stdout


def test_validate_invalid_popolo_exits_one(tmp_path: Path):
    path = tmp_path / "people.json"
    path.write_text("not json")
    result = runner.invoke(app, ["popolo", "validate", str(path)])
    assert result.exit_code == 1
    assert "Invalid Popolo file" in result.stdout


def test_format_popolo(tmp_path: Path):
    path = tmp_path / "people.json"
    write_popolo(path)
    result = runner.invoke(app, ["popolo", "format", str(path)])
    assert result.exit_code == 0
    assert "Formatted Popolo file saved" in result.stdout
    assert json.loads(path.read_text()) == {
        **VALID_POPOLO,
        "posts": [],
    }
    assert "\n  \"organizations\"" in path.read_text()


def test_download_popolo(monkeypatch, tmp_path: Path):
    class Response:
        text = json.dumps(VALID_POPOLO)

    monkeypatch.setattr(
        "mysoc_validator_fast.popolo.requests.get", lambda _url: Response()
    )
    destination = tmp_path / "downloaded.json"
    result = runner.invoke(
        app,
        ["popolo", "download", str(destination), "--branch", "test-branch"],
    )
    assert result.exit_code == 0
    assert "Popolo file saved" in result.stdout
    assert json.loads(destination.read_text())["organizations"] == VALID_POPOLO["organizations"]


def test_name_add_alt_name(tmp_path: Path):
    path = tmp_path / "people.json"
    write_popolo(path)
    result = runner.invoke(
        app,
        [
            "popolo",
            "name",
            "add-alt-name",
            str(path),
            "--person-id",
            "1",
            "--one-name",
            "Jane Alias",
        ],
    )
    assert result.exit_code == 0, result.stdout
    names = json.loads(path.read_text())["persons"][0]["other_names"]
    assert names[-1]["name"] == "Jane Alias"
    assert names[-1]["note"] == "Alternate"


def test_name_change_name(tmp_path: Path):
    path = tmp_path / "people.json"
    write_popolo(path)
    result = runner.invoke(
        app,
        [
            "popolo",
            "name",
            "change-name",
            str(path),
            "--person-id",
            "1",
            "--given-name",
            "Jane",
            "--family-name",
            "New",
            "--change-date",
            "2024-01-01",
        ],
    )
    assert result.exit_code == 0, result.stdout
    names = json.loads(path.read_text())["persons"][0]["other_names"]
    assert names[0]["end_date"] == "2023-12-31"
    assert names[-1]["family_name"] == "New"
    assert names[-1]["start_date"] == "2024-01-01"


def test_name_ennoble(tmp_path: Path):
    path = tmp_path / "people.json"
    write_popolo(path)
    result = runner.invoke(
        app,
        [
            "popolo",
            "name",
            "ennoble",
            str(path),
            "--person-id",
            "1",
            "--given-name",
            "Jane",
            "--county",
            "County",
            "--honorific-prefix",
            "Baroness",
            "--lordname",
            "New",
            "--lordofname-full",
            "New of Place",
            "--change-date",
            "2024-01-01",
        ],
    )
    assert result.exit_code == 0, result.stdout
    name = json.loads(path.read_text())["persons"][0]["other_names"][-1]
    assert name["honorific_prefix"] == "Baroness"
    assert name["lordname"] == "New"


def test_party_change_remove_and_restore_whip(tmp_path: Path):
    path = tmp_path / "people.json"
    write_popolo(path)
    commands = [
        ("change-party", ["--new-party-id", "party-b"], "2024-01-01"),
        ("remove-whip", [], "2024-02-01"),
        ("restore-whip", [], "2024-03-01"),
    ]
    for command, options, change_date in commands:
        result = runner.invoke(
            app,
            [
                "popolo",
                "party",
                command,
                str(path),
                "--person-id",
                "1",
                "--change-date",
                change_date,
                *options,
            ],
        )
        assert result.exit_code == 0, result.stdout
    memberships = json.loads(path.read_text())["memberships"]
    assert [membership["on_behalf_of_id"] for membership in memberships] == [
        "party-a",
        "party-b",
        "independent",
        "party-b",
    ]
