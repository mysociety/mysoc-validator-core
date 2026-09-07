import json
from pathlib import Path

from typer.testing import CliRunner

from mysoc_validator_fast import (
    Division,
    DivisionCount,
    MajorHeading,
    MinorHeading,
    Question,
    RepList,
    RepName,
    Speech,
    Transcript,
    TranscriptGroup,
)
from mysoc_validator_fast.__main__ import app

runner = CliRunner()

VALID_XML = """<publicwhip scraperversion="1">
<major-heading id="uk.org.publicwhip/debates/2026-01-01.1.mh" nospeaker="True">Heading</major-heading>
<speech id="uk.org.publicwhip/debates/2026-01-01.1.1" person_id="unknown" speakername="Announcer"><p>Hello world</p></speech>
</publicwhip>"""


def test_transcript_load_types_and_round_trip(tmp_path: Path):
    path = tmp_path / "valid.xml"
    path.write_text(VALID_XML)
    transcript = Transcript.from_xml_path(path)
    assert transcript.scraper_version == "1"
    assert isinstance(transcript.items[0], MajorHeading)
    assert str(transcript.items[0]) == "Heading"
    assert isinstance(transcript.items[1], Speech)
    assert transcript.items[1].person_id == "unknown"
    assert str(transcript.items[1]) == "Hello world"
    assert transcript.model_dump_xml() == VALID_XML
    assert Transcript.model_validate_xml(transcript.model_dump_xml()).model_dump_xml() == VALID_XML


def test_transcript_models_support_upstream_style_construction():
    heading = MinorHeading(
        id="uk.org.publicwhip/london-mayors-questions/2026-02-23.10.h",
        content={"text": "Heading", "raw": "Heading"},
    )
    question = Question(
        id="uk.org.publicwhip/london-mayors-questions/2026-02-23.10.q0",
        items=[],
    )
    assert heading.id.endswith(".h")
    assert str(heading) == "Heading"
    assert heading.model_dump()["@tag"] == "minor-heading"
    assert question.items == []


def test_division_uses_typed_nested_models():
    transcript = Transcript.model_validate_xml(
        """<publicwhip><division id="uk.org.publicwhip/debates/2026-01-01.1.1"
        divdate="2026-01-01" divnumber="1"><divisioncount ayes="1" noes="0"/>
        <mplist vote="aye"><mpname id="uk.org.publicwhip/member/1" vote="aye">
        Ada Example</mpname></mplist></division></publicwhip>"""
    )
    division = transcript.items[0]
    assert isinstance(division, Division)
    assert isinstance(division.count, DivisionCount)
    assert division.count.ayes == 1
    assert isinstance(division.representatives[0], RepList)
    representative = division.representatives[0].items[0]
    assert isinstance(representative, RepName)
    assert representative.name.strip() == "Ada Example"


def test_transcript_models_reject_invalid_chamber_segment():
    try:
        Question(
            id="uk.org.publicwhip/london_mayors_questions/2026-02-23.10.q0",
            items=[],
        )
    except ValueError as error:
        assert "invalid id" in str(error)
    else:
        raise AssertionError("invalid transcript GID was accepted")


def test_transcript_iterator_helpers():
    transcript = Transcript.model_validate_xml(VALID_XML)
    assert len(transcript.iter_type(Speech)) == 1
    assert len(transcript.iter_has_text()) == 2
    headed = transcript.iter_headed_speeches()
    assert len(headed) == 1
    assert isinstance(headed[0][0], MajorHeading)
    assert isinstance(headed[0][2], Speech)
    assert headed[0][3] == 0


def test_transcript_group_keeps_validating_after_error(tmp_path: Path):
    (tmp_path / "a-valid.xml").write_text(VALID_XML)
    (tmp_path / "b-invalid.xml").write_text("<not-publicwhip/>")
    (tmp_path / "c-valid.xml").write_text(VALID_XML)
    group = TranscriptGroup.from_glob(tmp_path / "*.xml")
    assert len(group) == 3
    assert len(group.valid) == 2
    assert [result.path.name for result in group.invalid] == ["b-invalid.xml"]
    assert "expected <publicwhip>" in group.invalid[0].error


def test_transcript_cli_single_file(tmp_path: Path):
    path = tmp_path / "valid.xml"
    path.write_text(VALID_XML)
    result = runner.invoke(app, ["transcript", "validate", str(path)])
    assert result.exit_code == 0
    assert "Valid Transcript file" in result.stdout


def test_transcript_cli_glob_lists_invalid_files(tmp_path: Path):
    (tmp_path / "valid.xml").write_text(VALID_XML)
    (tmp_path / "invalid.xml").write_text("<not-publicwhip/>")
    result = runner.invoke(
        app,
        ["transcript", "validate", str(tmp_path / "*.xml"), "--glob"],
    )
    assert result.exit_code == 1
    assert "invalid.xml" in result.stdout
    assert "1 valid, 1 invalid" in result.stdout


def test_transcript_folder_checks_people_when_supplied(tmp_path: Path):
    debates = tmp_path / "debates"
    debates.mkdir()
    (debates / "valid.xml").write_text(VALID_XML.replace("unknown", "uk.org.publicwhip/person/1"))
    (debates / "missing.xml").write_text(VALID_XML.replace("unknown", "uk.org.publicwhip/person/2"))
    people = tmp_path / "people.json"
    people.write_text(
        json.dumps(
            {
                "organizations": [],
                "persons": [{"id": "uk.org.publicwhip/person/1"}],
                "memberships": [],
                "posts": [],
            }
        )
    )
    group = TranscriptGroup.from_folder(debates, people)
    assert len(group.valid) == 1
    assert len(group.invalid) == 1
    assert "unknown person_id: uk.org.publicwhip/person/2" in group.invalid[0].error


def test_transcript_folder_without_people_only_checks_xml(tmp_path: Path):
    (tmp_path / "transcript.xml").write_text(
        VALID_XML.replace("unknown", "uk.org.publicwhip/person/999")
    )
    group = TranscriptGroup.from_folder(tmp_path)
    assert len(group.valid) == 1


def test_transcript_validate_folder_cli(tmp_path: Path):
    (tmp_path / "transcript.xml").write_text(VALID_XML)
    result = runner.invoke(app, ["transcript", "validate-folder", str(tmp_path)])
    assert result.exit_code == 0
    assert "1 valid, 0 invalid" in result.stdout
