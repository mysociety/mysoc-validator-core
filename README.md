# mysoc-validator-core

Rust workspace providing fast validation logic and PyO3 bindings for mySociety's Python validator libraries.

The repository is a Cargo workspace with three crates: two pure-Rust libraries and one PyO3 bridge that compiles into the `mysoc_validator_fast` Python package.

## Crates

### `fuzzy_date`

A date type that handles the partial-date formats common in political data — where records may only know a year, a year and month, a full date, or an explicit date range.

`FuzzyDate` internally stores an `(earliest_date, latest_date)` pair and infers its own precision from that pair. It serialises back to the most compact representation the data supports:

| Input string | Precision | Example |
|---|---|---|
| `"2010"` | Year only | politician elected sometime in 2010 |
| `"2010-06"` | Year and month | |
| `"2010-06-15"` | Full date | |
| `"2010-06-01/2010-06-30"` | Explicit range | custom date span |

Equality and ordering comparisons are range-aware: two `FuzzyDate` values are equal if either's range overlaps the other's. The crate exposes a `FuzzyDate` PyO3 class (comparable against Python `datetime.date` objects and ISO strings) and implements `serde` serialisation so it can be embedded in JSON structs. It also supports `FuzzyDate + timedelta`, returning a plain Python `date` from the earliest bound.

### `popolo_validator`

Pure Rust domain library — no PyO3 dependency. Parses and validates JSON in the [Popolo](https://www.popoloproject.com/) format — the open standard used across mySociety's parliamentary data projects. The top-level type is `Popolo`, which contains four collections:

- **persons** — individual people, with name variants (`BasicPersonName`, `LordName`, `AltName`), optional birth/death dates, links, and identifiers. Persons may also be redirect entries pointing to a canonical ID.
- **memberships** — a person's membership of a post or organisation over a date range, with optional start/end reasons (e.g. `GeneralElection`, `Resigned`, `Died`).
- **organizations** — parties, chambers, and metro bodies referenced by memberships.
- **posts** — specific elected seats (e.g. a constituency) with their own date ranges and area names.

Validation checks run on load and cover:

- **Unique IDs** — no duplicate person, membership, organisation, or post IDs.
- **Foreign keys** — every `person_id`, `post_id`, `organization_id`, and `on_behalf_of_id` referenced in a membership must exist in the corresponding collection.
- **Date ranges** — `start_date` must not be after `end_date` on any record.
- **Membership overlap** — consecutive memberships for the same person+post/organisation must not overlap or share a boundary date (skipped when either date is approximate).
- **Regex format** — IDs must match expected patterns (e.g. `uk.org.publicwhip/person/\d+` for person IDs, lowercase-alphanumeric for organisation IDs).

The crate also exposes pure-Rust query methods used by the Python bridge:

- `find_person_id_by_name(name, chamber_id, date)` — slug-normalised name lookup within a chamber on a date.
- `find_person_id_by_identifier(identifier, scheme)` — find a person by external identifier scheme.
- `person_membership_ids(person_id)` — all non-redirect membership IDs for a person.

The crate also has a `main.rs` binary entry point, making `popolo_validator` a standalone CLI tool for validating and formatting Popolo files.

### `mysoc_validator_fast`

PyO3 bridge that compiles `fuzzy_date` and `popolo_validator` into the `mysoc_validator_fast` Python package — a drop-in replacement for the `mysoc_validator` Popolo classes, without Pydantic.

**Architecture:** All data lives in a single Rust `Arc<RwLock<Popolo>>`. Python objects are thin handles that read and write through to Rust on demand; there are no Python-side copies of the dataset. This means:

- Loading is fast: Rust parses and validates JSON, then Python gets lightweight handles.
- Mutation goes back to Rust: setting `membership.end_date = new_date` updates the Rust struct.
- Name and identifier lookups run entirely in Rust.

The Python package (in `popolo_validator_python/python/`) layers on top of the extension to provide:

- `Popolo.from_parlparse()`, `from_path()`, `from_url()`, `to_json_str()`, `to_path()` — I/O helpers matching the `mysoc_validator` API.
- `Chamber`, `MembershipReason`, `IdentifierScheme` — Python `StrEnum` constants.
- `FixedDate`, `ApproxDate` — pure-Python date utilities matching `mysoc_validator.models.dates`.
- Full type stubs (`_popolo_validator_python.pyi`) describing every Rust-backed class and method.

**Key Python types** (all backed by Rust data):

| Type | Notable methods / properties |
|---|---|
| `Popolo` | `from_parlparse()`, `model_validate_json()`, `to_json_str()`, `persons`, `memberships`, `organizations`, `posts` |
| `Person` | `names_on_date(date)`, `get_identifier(scheme)`, `memberships()`, read/write `biography`, `gender`, `summary`, etc. |
| `Membership` | Read/write `start_date`, `end_date`, `end_reason`, `role`; `person_id`, `post_id`, `organization_id`, etc. |
| `Organization` | Read/write `name`, `classification` |
| `Post` | Read/write `label`, `role`, `organization_id` |
| `PersonCollection` | `["id"]`, `from_identifier(id, scheme)`, `from_name(name, chamber_id, date)`, `append()`, `redirects()` |
| `MembershipCollection` | `["id"]`, `extend([...])`, `append()`, `redirects()`, `root` |

## Development

### Initial setup

```bash
script/setup   # installs uv, Python 3.9, and syncs the Python environment
```

### Build and install in development mode

```bash
uv run maturin develop -m mysoc_validator_fast/Cargo.toml
```

### Run the tests

```bash
# FuzzyDate unit tests (no network required)
uv run pytest mysoc_validator_fast/tests/test_dates.py -v

# Full Popolo integration tests (requires internet for parlparse fixture)
uv run pytest mysoc_validator_fast/tests/test_popolo.py -v
```

### Build a release wheel

```bash
uv run maturin build -m mysoc_validator_fast/Cargo.toml
```

### Run the CLI via Cargo

```bash
cargo run -p popolo_validator data/people.json
cargo build -p popolo_validator --release
target/release/popolo_validator data/people.json
```

## Workspace structure

```
mysoc-validator-core/
├── fuzzy_date/                  # Pure Rust: partial-date type (PyO3 optional via "python" feature)
├── popolo_validator/            # Pure Rust: Popolo domain logic, no PyO3
└── mysoc_validator_fast/        # PyO3 bridge: full Popolo Python API
    ├── src/lib.rs               # Rust PyO3 wrappers (Arc<RwLock<>> handle pattern)
    └── python/
        └── mysoc_validator_fast/
            ├── __init__.py
            ├── popolo.py        # Thin Python I/O helpers + Popolo subclass
            ├── dates.py         # FixedDate, FuzzyDate/ApproxDate alias
            ├── consts.py        # Chamber, MembershipReason, IdentifierScheme
            └── _mysoc_validator_fast.pyi  # Type stubs for the Rust extension
```
