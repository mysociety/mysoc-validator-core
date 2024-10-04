use pyo3::prelude::*;

use fuzzy_date::FuzzyDate;

/// A Python module implemented in Rust. The name of this function must match
/// the `lib.name` setting in the `Cargo.toml`, else Python will not be able to
/// import the module.
#[pymodule]
fn mysoc_validator_core(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<FuzzyDate>()?;
    Ok(())
}
