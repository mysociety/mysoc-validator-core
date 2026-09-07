# mysoc-validator-core

Rust workspace providing fast validation logic and PyO3 bindings for mySociety's Python validator libraries.

The Cargo workspace has five crates: three domain libraries, a shared date library with optional Python support, and a PyO3 bridge that compiles into the `mysoc_validator_fast` Python package. Python 3.11 or newer is required.

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

Python equality is range-aware: two `FuzzyDate` values compare equal when their intervals overlap, and a Python `date` matches an interval containing it. Python ordering treats overlapping intervals as neither less nor greater; strings compare against the compact ISO spelling. In Rust, equality compares identical bounds and ordering uses `(earliest_date, latest_date)`. Domain code uses explicit `overlaps`, `contains`, `is_before` and `is_after` predicates for fuzzy matching. The crate exposes a `FuzzyDate` PyO3 class (comparable against Python `datetime.date` objects and ISO strings) and implements `serde` serialisation so it can be embedded in JSON structs. It also supports `FuzzyDate + timedelta`, returning a plain Python `date` from the earliest bound.

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

### `regmem_validator`

Pure Rust register-of-interests models, custom detail-value serialization and
cross-record validation. `details.rs` owns tagged values and groups; `entries.rs`
owns entry content and annotations; `register.rs` owns the hierarchy and index
locations. The Python bridge supports live entry/detail-group edits, while
individual details, annotations and summaries are detached values.

### `transcript_validator`

Pure Rust transcript XML parsing and validation. `xml.rs` extracts an owned tree
from a parsing copy; `files.rs` discovers and validates folders. The original XML
is retained for round-tripping. Python item wrappers own copies, so editing an
item does not rewrite the transcript's stored XML.

### `mysoc_validator_fast`

PyO3 bridge that wraps all four libraries into the `mysoc_validator_fast` Python package — a drop-in replacement for the `mysoc_validator` Popolo classes, with Rust-backed core models. The Python package still uses Pydantic for typed project-specific extras.

**Popolo architecture:** Dataset data lives in a single Rust `Arc<RwLock<Popolo>>`. Collection lookups return thin handles that read and write through to Rust on demand. Standalone constructors own independent values, and appending copies those values into the dataset. Names and date getters return detached copies. This means:

- Loading is fast: Rust parses and validates JSON, then Python gets lightweight handles.
- Mutation goes back to Rust: setting `membership.end_date = new_date` updates the Rust struct.
- Name and identifier lookups run entirely in Rust.

The Python package (in `mysoc_validator_fast/python/`) layers on top of the extension to provide:

- `Popolo.from_parlparse()`, `from_path()`, `from_url()`, `to_json_str()`, `to_path()` — I/O helpers matching the `mysoc_validator` API.
- `Chamber`, `MembershipReason`, `IdentifierScheme` — Python `StrEnum` constants.
- `FixedDate`, `ApproxDate` — pure-Python date utilities matching `mysoc_validator.models.dates`.
- Full type stubs (`_mysoc_validator_fast.pyi`) describing every Rust-backed class and method.

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
script/setup   # installs uv and syncs the Python environment
```

### Build and install in development mode

```bash
uv run maturin develop -m mysoc_validator_fast/Cargo.toml
```

### Run the tests

```bash
# Rust domain tests and extension compile check
cargo test --workspace --exclude mysoc_validator_fast --locked
cargo check -p mysoc_validator_fast --locked
cargo fmt --all --check

# FuzzyDate Python tests (build the extension first)
uv run pytest mysoc_validator_fast/tests/test_dates.py -v

# Full Python suite (requires internet for parlparse fixtures)
uv run pytest mysoc_validator_fast/tests -q

# Tests using local fixtures
uv run pytest mysoc_validator_fast/tests --ignore=mysoc_validator_fast/tests/test_popolo.py -q
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

```text
mysoc-validator-core/
├── fuzzy_date/src/
│   ├── lib.rs                  # Interval values, serde and exact Rust comparison
│   └── python.rs               # Feature-gated Python fuzzy comparison
├── popolo_validator/src/
│   ├── lib.rs                  # Public exports and load/dump orchestration
│   ├── models.rs, names.rs     # Records, name variants and custom serde
│   ├── collections.rs         # Ordered records and ID index maintenance
│   ├── validation.rs          # Dataset and insertion validation policies
│   ├── queries.rs             # Name, identifier and redirect resolution
│   └── dates.rs               # Popolo sentinel date serialization
├── regmem_validator/src/
│   └── lib.rs, enums.rs, details.rs, entries.rs, register.rs
├── transcript_validator/src/
│   └── lib.rs, xml.rs, files.rs
└── mysoc_validator_fast/
    ├── src/
    │   ├── lib.rs              # Python module registration and Rust re-exports
    │   ├── conversions.rs      # Shared Python/JSON boundary helpers
    │   ├── popolo/             # One module per entity, plus conversions/macros
    │   ├── interests/          # Values, entries and register hierarchy
    │   ├── transcripts/        # Item classes, top-level wrapper and file adapters
    │   └── bin/stub_gen.rs     # Generates annotated API declarations for review
    ├── python/mysoc_validator_fast/
    │   ├── popolo.py, interests.py, transcripts.py
    │   ├── dates.py, consts.py, popolo_extras.py
    │   └── _mysoc_validator_fast.pyi
    └── tests/
```

## Reading the implementation

Start with the Python module for the public API, then follow the corresponding
bridge module into its domain crate. For example, `memberships.append(...)`
reaches `src/popolo/membership.rs`, which calls
`Popolo::validate_membership_insertion` in `popolo_validator/src/validation.rs`.
The model's JSON fields live in `models.rs`, index maintenance in `collections.rs`,
and the policy regressions in `popolo_validator/tests/comparison_and_collections.rs`
and `mysoc_validator_fast/tests/test_refactoring_contracts.py`.

Rust module documentation explains ownership and borrowing at these boundaries.
`Arc::clone` shares a dataset, while cloning an owned domain model copies its data.
Access closures keep record borrows within a lock guard's lifetime. Python
callbacks run after the dataset guard is released. PyO3 separately checks borrows
of each Python wrapper: a callback that needs to mutate a record can use another
handle to it, but mutating the same wrapper during an active read raises a borrow
error. ID-based Popolo handles and index-path
interests handles have different invalidation rules; their modules document those
rules. Removing an organization makes existing handles raise `KeyError` on data
access; person handles follow redirects after merges.

Parsing and validation are separate. Direct serde construction builds records and
indexes but does not run Popolo's cross-record checks. Dataset overlap validation
retains its historical adjacent-pair and approximate-date exclusions; insertion
validation conservatively checks possible overlaps. These policies are explicit
and are not inferred from Rust date equality.

The checked-in type stub includes hand-maintained transcript and compatibility
APIs alongside annotated bindings. Run the generator only with a saved copy and
review its diff; do not replace the complete stub blindly:

```bash
cargo run --manifest-path mysoc_validator_fast/Cargo.toml --bin stub_gen
cargo doc --workspace --no-deps
```
