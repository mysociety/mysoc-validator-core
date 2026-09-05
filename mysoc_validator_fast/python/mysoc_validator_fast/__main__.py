"""Command-line interface for the Rust-backed validators."""

from __future__ import annotations

from datetime import date, timedelta
from pathlib import Path
from typing import Annotated, Optional
from urllib.parse import urlparse

import rich
import typer

from .consts import MembershipReason
from .popolo import Membership, Popolo
from .transcripts import Transcript, TranscriptGroup

app = typer.Typer()
popolo_app = typer.Typer(help="Commands for Popolo files")
name_app = typer.Typer(help="Commands for name modifications in Popolo files")
party_app = typer.Typer(help="Commands for party modifications in Popolo files")
transcript_app = typer.Typer(help="Commands for Transcript files")
popolo_app.add_typer(name_app, name="name")
popolo_app.add_typer(party_app, name="party")
app.add_typer(popolo_app, name="popolo")
app.add_typer(transcript_app, name="transcript")

OptionalDate = Annotated[Optional[date], typer.Option(parser=date.fromisoformat)]


def find_people_json(root: Path = Path(".")) -> Path | None:
    """
    Find people.json in either a data directory or a parlparse checkout.
    """

    for candidate in (root / "people.json", root / "members" / "people.json"):
        if candidate.exists():
            return candidate
    return None


def process_path(value: str | Path) -> Path:
    """
    Resolve a Popolo file or a directory containing people.json.
    """

    path = Path(value)
    if path.is_dir():
        found = find_people_json(path)
        if found is None:
            raise typer.BadParameter(f"File does not exist or not specified: {path}")
        path = found
    if not path.exists():
        raise typer.BadParameter(f"File does not exist: {path}")
    return path


def is_url(value: str) -> bool:
    """
    Return whether a value is an HTTP(S) URL.
    """

    parsed = urlparse(value)
    return parsed.scheme in {"http", "https"} and bool(parsed.netloc)


def enhance_person_id(person_id: str) -> str:
    """
    Expand a numeric person ID to the canonical publicwhip ID.
    """

    if not person_id:
        raise typer.BadParameter("person_id is required")
    if "uk.org.publicwhip" in person_id:
        return person_id
    return f"uk.org.publicwhip/person/{person_id}"


def next_membership_id(popolo: Popolo) -> str:
    """
    Allocate an unused publicwhip membership ID.
    """

    numeric_ids = []
    for membership in popolo.memberships.root:
        suffix = membership.id.rsplit("/", 1)[-1]
        if suffix.lstrip("-").isdigit():
            numeric_ids.append(int(suffix))
    return f"uk.org.publicwhip/member/{max(numeric_ids, default=0) + 1}"


def change_person_party(
    popolo: Popolo,
    person_id: str,
    new_party_id: str,
    change_date: date,
    change_reason: MembershipReason,
    source_url: str | None,
) -> None:
    """
    Close the latest membership and create its successor for a new party.
    """

    if new_party_id not in popolo.organizations:
        raise typer.BadParameter(f"Unknown organization: {new_party_id}")
    person = popolo.persons[person_id]
    memberships = person.memberships()
    if not memberships:
        raise typer.BadParameter(f"Person has no memberships: {person_id}")
    previous = memberships[-1]
    previous.end_date = change_date
    previous.end_reason = change_reason
    popolo.memberships.append(
        Membership(
            id=next_membership_id(popolo),
            person_id=person_id,
            start_date=change_date + timedelta(days=1),
            organization_id=previous.organization_id,
            on_behalf_of_id=new_party_id,
            post_id=previous.post_id,
            start_reason=change_reason,
            source=source_url,
        )
    )


@name_app.command(name="add-alt-name")
def add_alt_name(
    file: Path = typer.Argument(Path(".")),
    person_id: str = typer.Option("", parser=enhance_person_id),
    one_name: str = "",
    given_name: str = "",
    family_name: str = "",
    start_date: OptionalDate = None,
    end_date: OptionalDate = None,
) -> None:
    """
    Add an alternative name to a person.
    """

    path = process_path(file)
    popolo = Popolo.from_path(path)
    person = popolo.persons[person_id]
    person.add_alt_name(
        given_name=given_name or None,
        family_name=family_name or None,
        one_name=one_name or None,
        start_date=start_date,
        end_date=end_date,
    )
    added_name = one_name or f"{given_name} {family_name}"
    rich.print(f"[green]Added alternative name {added_name} for {person_id}[/green]")
    popolo.to_path(path)


@name_app.command(name="change-name")
def change_name(
    file: Path = typer.Argument(Path(".")),
    person_id: str = typer.Option("", parser=enhance_person_id),
    given_name: str = "",
    family_name: str = "",
    change_date: OptionalDate = None,
) -> None:
    """
    Close a person's current main name and add a replacement.
    """

    if not given_name or not family_name:
        raise typer.BadParameter("Both given_name and family_name are required")
    path = process_path(file)
    popolo = Popolo.from_path(path)
    person = popolo.persons[person_id]
    person.change_main_name(given_name, family_name, change_date or date.today())
    rich.print(f"[green]Changed name for {person_id} to {given_name} {family_name}[/green]")
    popolo.to_path(path)


@name_app.command()
def ennoble(
    file: Path = typer.Argument(Path(".")),
    person_id: str = typer.Option("", parser=enhance_person_id),
    given_name: str = "",
    county: str = "",
    honorific_prefix: str = "",
    lordname: str = "",
    lordofname_full: str = "",
    change_date: OptionalDate = None,
) -> None:
    """
    Replace a person's current main name with a Lords name.
    """

    path = process_path(file)
    popolo = Popolo.from_path(path)
    popolo.persons[person_id].change_main_name_to_lord(
        given_name=given_name,
        county=county,
        honorific_prefix=honorific_prefix,
        lordname=lordname,
        lordofname_full=lordofname_full,
        change_date=change_date or date.today(),
    )
    rich.print(f"[green]Ennobled {person_id}[/green]")
    popolo.to_path(path)


@party_app.command(name="change-party")
def change_party(
    file: Path = typer.Argument(Path(".")),
    person_id: str = typer.Option("", parser=enhance_person_id),
    new_party_id: str = "",
    change_date: OptionalDate = None,
    change_reason: MembershipReason = MembershipReason.CHANGED_PARTY,
    source_url: str | None = None,
) -> None:
    """
    Change the party represented by a person's latest membership.
    """

    if not new_party_id:
        raise typer.BadParameter("new_party_id is required")
    path = process_path(file)
    popolo = Popolo.from_path(path)
    person_id = enhance_person_id(person_id)
    change_person_party(
        popolo,
        person_id,
        new_party_id,
        change_date or date.today(),
        change_reason,
        source_url,
    )
    rich.print(f"[green]Changed party for {person_id} to {new_party_id}[/green]")
    popolo.to_path(path)


@party_app.command(name="remove-whip")
def remove_whip(
    file: Path = typer.Argument(Path(".")),
    person_id: str = typer.Option("", parser=enhance_person_id),
    change_date: OptionalDate = None,
    source_url: str | None = None,
) -> None:
    """
    Change a person's represented party to independent.
    """

    path = process_path(file)
    popolo = Popolo.from_path(path)
    change_person_party(
        popolo,
        person_id,
        "independent",
        change_date or date.today(),
        MembershipReason.WHIP_REMOVED,
        source_url,
    )
    rich.print(f"[green]Removed whip for {person_id}[/green]")
    popolo.to_path(path)


@party_app.command(name="restore-whip")
def restore_whip(
    file: Path = typer.Argument(Path(".")),
    person_id: str = typer.Option("", parser=enhance_person_id),
    change_date: OptionalDate = None,
    source_url: str | None = None,
) -> None:
    """
    Restore the most recently represented non-independent party.
    """

    path = process_path(file)
    popolo = Popolo.from_path(path)
    previous_party = next(
        (
            membership.on_behalf_of_id
            for membership in reversed(popolo.persons[person_id].memberships())
            if membership.on_behalf_of_id
            and membership.on_behalf_of_id != "independent"
        ),
        None,
    )
    if previous_party is None:
        raise typer.BadParameter("No previous party found")
    change_person_party(
        popolo,
        person_id,
        previous_party,
        change_date or date.today(),
        MembershipReason.WHIP_RESTORED,
        source_url,
    )
    rich.print(f"[green]Restored whip for {person_id}[/green]")
    popolo.to_path(path)


def print_counts(people: Popolo) -> None:
    """
    Print the standard Popolo collection summary.
    """

    typer.echo(
        f"Loaded {len(people.organizations)} organizations, "
        f"{len(people.posts)} posts, {len(people.persons)} people, "
        f"and {len(people.memberships)} memberships."
    )


def validate_popolo_file(file: Path, format_file: bool = False) -> None:
    """
    Validate a local Popolo file and optionally format it in place.
    """

    try:
        people = Popolo.from_path(file)
    except Exception as error:
        typer.echo(f"Error: {error}")
        rich.print(f"[red]Invalid Popolo file {file}[/red]")
        raise typer.Exit(code=1) from error

    print_counts(people)
    rich.print(f"[green]Valid Popolo file {file}[/green]")
    if format_file:
        people.to_path(file)
        rich.print(f"[green]Formatted Popolo file saved to {file}[/green]")


def validate_popolo_url_file(url: str) -> None:
    """
    Validate a Popolo document fetched from a URL.
    """

    try:
        people = Popolo.from_url(url)
    except Exception as error:
        typer.echo(f"Error: {error}")
        rich.print("[red]Invalid Popolo file[/red]")
        raise typer.Exit(code=1) from error

    print_counts(people)
    rich.print("[green]Valid Popolo file[/green]")


@popolo_app.command(name="format")
def format_cmd(file: Path = typer.Argument(Path("."))) -> None:
    """
    Validate and format a mySociety-style Popolo file.
    """

    validate_popolo_file(process_path(file), format_file=True)


@popolo_app.command(name="download")
def download(
    file: Path = typer.Argument(Path("people.json")), branch: str = "master"
) -> None:
    """
    Download and validate parlparse people.json.
    """

    try:
        people = Popolo.from_parlparse(branch=branch)
        people.to_path(file)
    except Exception as error:
        typer.echo(f"Error: {error}")
        raise typer.Exit(code=1) from error
    rich.print(f"[green]Popolo file saved to {file}[/green]")


@popolo_app.command(name="validate")
def validate_popolo_cmd(
    loc: str = typer.Argument("."), format_file: bool = False
) -> None:
    """
    Validate a local or remote mySociety-style Popolo file.
    """

    if is_url(loc):
        validate_popolo_url_file(loc)
    else:
        validate_popolo_file(process_path(loc), format_file=format_file)


@transcript_app.command(name="validate")
def validate_transcript_cmd(file: str, glob: bool = False) -> None:
    """
    Validate one transcript or every transcript matching a glob pattern.
    """

    use_group = glob or any(character in file for character in "*?[")
    if use_group:
        group = TranscriptGroup.from_glob(file)
        if not group.results:
            typer.echo(f"No transcript files matched: {file}")
            raise typer.Exit(code=1)
        for result in group.invalid:
            typer.echo(f"Invalid Transcript file: {result.path}")
            typer.echo(f"Error: {result.error}")
        typer.echo(
            f"Validated {len(group)} transcript files: "
            f"{len(group.valid)} valid, {len(group.invalid)} invalid."
        )
        if group.invalid:
            raise typer.Exit(code=1)
        return

    path = Path(file)
    try:
        Transcript.from_xml_path(path)
    except Exception as error:
        typer.echo(f"Error: {error}")
        rich.print(f"[red]Invalid Transcript file: {path}[/red]")
        raise typer.Exit(code=1) from error
    rich.print(f"[green]Valid Transcript file: {path}[/green]")


@transcript_app.command(name="validate-folder")
def validate_transcript_folder_cmd(
    folder: Path,
    people: Path | None = typer.Option(None, "--people"),
) -> None:
    """
    Validate every XML transcript below a folder.

    If a people file is supplied, every stated non-unknown person ID must be
    present in that file.
    """
    try:
        group = TranscriptGroup.from_folder(folder, people)
    except Exception as error:
        typer.echo(f"Error: {error}")
        raise typer.Exit(code=2) from error
    if not group.results:
        typer.echo(f"No transcript files found in: {folder}")
        raise typer.Exit(code=1)
    for result in group.invalid:
        typer.echo(f"Invalid Transcript file: {result.path}")
        typer.echo(f"Error: {result.error}")
    typer.echo(
        f"Validated {len(group)} transcript files: "
        f"{len(group.valid)} valid, {len(group.invalid)} invalid."
    )
    if group.invalid:
        raise typer.Exit(code=1)


if __name__ == "__main__":
    app()
