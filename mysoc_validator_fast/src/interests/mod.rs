//! Register-of-interests Python wrappers and shared ownership.
//!
//! Register/person/category/entry wrappers navigate one `Arc<RwLock<Register>>`
//! using index paths; they do not hold borrowed Rust references between Python
//! calls. As in Popolo, access closures keep borrows within a guard's lifetime.
//! Constructors create private trees; embedding a wrapper in another constructor
//! copies its data. Getters from an existing tree produce live handles.
//!
//! Supported edits change content, details and annotations, not the positions of
//! persons, categories, entries or sub-entries. That invariant makes location
//! `expect()` and indexing safe for Python-created handles. Any future deletion
//! or reordering API must first define invalidation or stable identity. Lock
//! `unwrap()` assumes an internal panic has not poisoned the lock.
//!
//! Details, annotations and summaries are detached values. Detail groups can be
//! standalone values or live views of an entry's details; see `values`.

use pyo3::prelude::*;
use std::sync::{Arc, RwLock};
type SharedRegister = Arc<RwLock<regmem_validator::RegmemRegister>>;
mod values;
use values::{PyRegmemAnnotation, PyRegmemDetail, PyRegmemDetailGroup, PyRegmemSummary};
mod entries;
use entries::PyRegmemEntry;
mod register;
use register::{PyRegmemCategory, PyRegmemPerson, PyRegmemRegister};

pub(crate) fn register_interests_classes(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PyRegmemAnnotation>()?;
    m.add_class::<PyRegmemDetail>()?;
    m.add_class::<PyRegmemDetailGroup>()?;
    m.add_class::<PyRegmemSummary>()?;
    m.add_class::<PyRegmemEntry>()?;
    m.add_class::<PyRegmemCategory>()?;
    m.add_class::<PyRegmemPerson>()?;
    m.add_class::<PyRegmemRegister>()?;
    Ok(())
}
