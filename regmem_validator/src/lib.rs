//! Pure-Rust domain library for the mySociety "register of members' interests"
//! JSON format.
//!
//! This mirrors `mysoc_validator.models.interests` (the `RegmemRegister` family)
//! from the upstream Python library. It is deliberately free of any PyO3
//! dependency — the `mysoc_validator_fast` crate wraps these types for Python.
//!
//! Notable behaviours replicated from upstream:
//!  - `RegmemDetail.value` is a discriminated union keyed by the sibling `type`
//!    field (`decimal`/`date` arrive as strings, `container` as a list of
//!    groups, etc.). When `type` is absent it is inferred from the value.
//!  - `infer_slug` / `infer_type` fill in `slug` <-> `display_as` and `type`.
//!  - `RegmemAnnotation.date_added` defaults to today when absent.
//!  - `RegmemRegister` enforces the global-or-per-person chamber/language/date
//!    rule on load.
//!
//! `comparable_id` / `item_hash` are exposed as best-effort computed values.
//! Upstream computes `item_hash` as a Python `md5(str(model_dump dict))`, which
//! is not cleanly reproducible in Rust; the value here is stable within a build
//! but is intentionally *not* byte-identical to upstream, and is not serialised.

mod enums;
pub use enums::{Chamber, CommonKey, ContentType, Language};
mod details;
pub use details::{slugify, DetailValue, RegmemDetail, RegmemDetailGroup};
mod entries;
pub use entries::{
    EntryType, RegmemAnnotation, RegmemEntry, RegmemInfoBase, RegmemSummary, SummaryType,
};
mod register;
pub use register::{EntryLocation, RegmemCategory, RegmemPerson, RegmemRegister};
