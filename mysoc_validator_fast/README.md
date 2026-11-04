# mysoc-validator-fast

A Rust-backed shadow of [mysoc-validator](https://github.com/mysociety/mysoc-validator)
for mySociety democracy data: Popolo, publicwhip XML transcripts, and JSON
registers of interests. Rust handles parsing and validation, with a Python API
that currently targets `mysoc-validator` **1.5**. Package versions are independent
of the upstream compatibility target.

Requires Python 3.11 or newer.

```bash
pip install mysoc-validator-fast
```

## Usage

These examples mirror the [upstream README](https://github.com/mysociety/mysoc-validator#readme)
with imports changed to `mysoc_validator_fast`.

Look up a person by identifier or name:

```python
from datetime import date

from mysoc_validator_fast import Chamber, IdentifierScheme, Popolo

popolo = Popolo.from_parlparse()
by_id = popolo.persons.from_identifier("4514", scheme=IdentifierScheme.MNIS)
by_name = popolo.persons.from_name(
    "keir starmer", chamber_id=Chamber.COMMONS, date=date(2022, 7, 31)
)
assert by_id.id == by_name.id
```

Load a transcript or register of interests:

```python
from pathlib import Path

from mysoc_validator_fast import RegmemRegister, Transcript

transcript = Transcript.from_xml_path(Path("data/debates2023-03-28d.xml"))
interests = RegmemRegister.from_path(Path("data/commons-regmem-2025-01-20.json"))
```

Validate files from the command line:

```bash
uvx mysoc-validator-fast popolo validate people.json
uvx mysoc-validator-fast transcript validate debates.xml
uvx mysoc-validator-fast transcript validate transcripts/
uvx mysoc-validator-fast transcript validate 'transcripts/*.xml' --glob
uvx mysoc-validator-fast interests validate interests.json
uvx mysoc-validator-fast popolo format people.json
```

`mysoc-validator-fast` and `msv-fast` are also CLI entry points. Use `--help`
to see available commands.

## Current differences from mysoc-validator

This is not yet a complete drop-in replacement:

- **Imports:** use `mysoc_validator_fast`, with models in `.popolo`, `.interests`
  and `.transcripts`; upstream's `mysoc_validator.models.*` paths are not provided.
  Import Pydantic extra schemas such as `MembershipExtra` from
  `mysoc_validator_fast.popolo_extras`.
- **Coverage:** legacy XML interests (`XMLRegister`), transcript JSON I/O, info
  XML models (`InfoCollection`, `PersonInfo`, `ConsInfo`), and the Popolo TUI are
  not implemented.
- **Pydantic integration:** core models are Rust extension classes, not Pydantic
  models. Selected `model_validate`/`model_dump` helpers are provided; the full
  Pydantic API, its keyword options, and `ValidationError` structure are not.
  Rust validation failures generally raise `ValueError`.
- **Mutation:** Popolo collection lookups return live handles, but appending a
  standalone model copies it. Retrieve it from the collection for later edits.
  Names and dates are copies, and changing a collection's returned `.root` list
  does not edit the collection; use setters and collection methods to write
  changes. Register details, annotations, and summaries are also detached values;
  use entry/detail-group editing methods.
- **Typed extras:** `get_extra_as(CustomExtra)` returns a validated Pydantic
  snapshot. Editing that snapshot does not update the record; write through
  `set_extra` or the live `extra` proxy.
- **Transcript editing:** item wrappers are copies. Editing an item does not
  update the transcript; `model_dump_xml()` returns its retained source XML.

See the [repository README](https://github.com/mysociety/mysoc-validator-core#readme)
for implementation and development details.
