from __future__ import annotations

import pytest

from mysoc_validator_fast import (
    HeaderSpeechTuple,
    MajorHeading,
    MinorHeading,
    Speech,
    Transcript,
)

CONTEXT_XML = """<publicwhip>
<speech id="uk.org.publicwhip/debates/2026-01-01.1.1" person_id="unknown"><p>Before headings</p></speech>
<major-heading id="uk.org.publicwhip/debates/2026-01-01.2.mh">First major</major-heading>
<speech id="uk.org.publicwhip/debates/2026-01-01.3.1" person_id="unknown"><p>After major</p></speech>
<minor-heading id="uk.org.publicwhip/debates/2026-01-01.4.h">First minor</minor-heading>
<speech id="uk.org.publicwhip/debates/2026-01-01.5.1" person_id="unknown"><p>First speech</p></speech>
<speech id="uk.org.publicwhip/debates/2026-01-01.6.1" person_id="unknown"><p>Second speech</p></speech>
<minor-heading id="uk.org.publicwhip/debates/2026-01-01.7.h">Second minor</minor-heading>
<speech id="uk.org.publicwhip/debates/2026-01-01.8.1" person_id="unknown"><p>After minor</p></speech>
<major-heading id="uk.org.publicwhip/debates/2026-01-01.9.mh">Second major</major-heading>
<speech id="uk.org.publicwhip/debates/2026-01-01.10.1" person_id="unknown"><p>After second major</p></speech>
</publicwhip>"""


def test_headed_speeches_return_named_tuples() -> None:
    transcript = Transcript.model_validate_xml(CONTEXT_XML)
    row = transcript.iter_headed_speeches()[2]

    assert isinstance(row, HeaderSpeechTuple)
    assert row._fields == ("major_heading", "minor_heading", "speech", "speech_index")
    assert isinstance(row.major_heading, MajorHeading)
    assert isinstance(row.minor_heading, MinorHeading)
    assert isinstance(row.speech, Speech)
    major, minor, speech, index = row
    assert major is row.major_heading
    assert minor is row.minor_heading
    assert speech is row.speech
    assert row[2] is row.speech
    assert index == row.speech_index == 0
    assert row == (major, minor, speech, index)
    with pytest.raises(AttributeError):
        row.speech_index = 99


def test_headed_speeches_preserve_heading_context_and_indices() -> None:
    rows = Transcript.model_validate_xml(CONTEXT_XML).iter_headed_speeches()

    assert len(rows) == 6
    assert rows[0].major_heading is None
    assert rows[0].minor_heading is None
    assert rows[1].minor_heading is None
    assert rows[1].major_heading is rows[2].major_heading is rows[3].major_heading
    assert rows[2].minor_heading is rows[3].minor_heading
    assert rows[4].minor_heading is not rows[3].minor_heading
    assert str(rows[4].minor_heading) == "Second minor"
    assert str(rows[5].major_heading) == "Second major"
    assert rows[5].minor_heading is None
    # Upstream resets the index at minor headings; major headings clear the minor only.
    assert [row.speech_index for row in rows] == [0, 1, 0, 1, 0, 1]
