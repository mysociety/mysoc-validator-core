"""Rust-backed transcript models and multi-file validation helpers."""

from __future__ import annotations

from dataclasses import dataclass
from glob import glob
from pathlib import Path
from typing import Iterator

from ._mysoc_validator_fast import (  # type: ignore[import]
    Agreement,
    Division,
    GIDRedirect,
    MajorHeading,
    MinorHeading,
    OralHeading,
    Question,
    Reply,
    Source,
    Speech,
    SpeechItem,
    Transcript,
    validate_transcript_folder,
)


@dataclass(frozen=True)
class TranscriptFileResult:
    """
    The validation outcome for one file in a transcript group.
    """

    path: Path
    transcript: Transcript | None = None
    error: str | None = None

    @property
    def valid(self) -> bool:
        return self.error is None


class TranscriptGroup:
    """
    Load and validate every transcript matching a glob pattern.

    Invalid files are retained as results rather than stopping validation of
    later matches.
    """

    def __init__(self, results: list[TranscriptFileResult]):
        self.results = results

    @classmethod
    def from_glob(cls, pattern: str | Path) -> "TranscriptGroup":
        paths = sorted(Path(path) for path in glob(str(pattern), recursive=True))
        results = []
        for path in paths:
            try:
                results.append(
                    TranscriptFileResult(
                        path=path, transcript=Transcript.from_xml_path(path)
                    )
                )
            except Exception as error:
                results.append(TranscriptFileResult(path=path, error=str(error)))
        return cls(results)

    @classmethod
    def from_folder(
        cls, folder: str | Path, people_path: str | Path | None = None
    ) -> "TranscriptGroup":
        """
        Validate every XML transcript below a folder in Rust.

        When supplied, the people file additionally checks that every stated
        non-unknown person ID exists.
        """
        native_results = validate_transcript_folder(folder, people_path)
        return cls(
            [
                TranscriptFileResult(
                    path=Path(result["path"]),
                    error="\n".join(result["errors"]) or None,
                )
                for result in native_results
            ]
        )

    def __iter__(self) -> Iterator[TranscriptFileResult]:
        return iter(self.results)

    def __len__(self) -> int:
        return len(self.results)

    @property
    def invalid(self) -> list[TranscriptFileResult]:
        return [result for result in self.results if not result.valid]

    @property
    def valid(self) -> list[TranscriptFileResult]:
        return [result for result in self.results if result.valid]


__all__ = [
    "Agreement",
    "Division",
    "GIDRedirect",
    "MajorHeading",
    "MinorHeading",
    "OralHeading",
    "Question",
    "Reply",
    "Source",
    "Speech",
    "SpeechItem",
    "Transcript",
    "TranscriptFileResult",
    "TranscriptGroup",
]
