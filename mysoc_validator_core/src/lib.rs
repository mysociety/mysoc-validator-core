use pyo3::prelude::*;

use fuzzy_date::FuzzyDate;
use popolo_validator::Popolo;

#[pyfunction]
fn cli_convert(popolo_location: &str, verbose: bool, format_file: bool) -> PyResult<()> {
    // Get the command line argument for the postcode

    // read in the file to a string
    let json_str = std::fs::read_to_string(popolo_location).expect("Unable to read file");

    let result = Popolo::model_validate_json(&json_str, &true);

    match result {
        Ok(pop) => {
            let membership_count = pop.memberships.len();
            let organization_count = pop.organizations.len();
            let person_count = pop.persons.len();
            let post_count = pop.posts.len();

            if verbose {
                println!(
                    "Valid Popolo, with {} memberships, {} organizations, {} persons, and {} posts",
                    membership_count, organization_count, person_count, post_count
                );
            }

            if format_file {
                // write to path of file with .formatted.json suffix
                let format_location = format!("{}", popolo_location);
                let formatted = pop.model_dump_json();
                std::fs::write(format_location, formatted).expect("Unable to write file");
            }
            Ok(())
        }
        Err(e) => {
            println!("Popolo file has data issues. Use --verbose for more information");
            // print totals of each type of error
            let mut error_counts = std::collections::HashMap::new();
            for error in e.iter() {
                let count = error_counts.entry(error.validator_type).or_insert(0);
                *count += 1;
            }
            for (validator_type, count) in error_counts.iter() {
                println!("{}: {}", validator_type, count);
            }
            if verbose {
                for error in e.iter() {
                    println!("{}", error.error);
                }
            }
            std::process::exit(1);
        }
    }
}

/// A Python module implemented in Rust. The name of this function must match
/// the `lib.name` setting in the `Cargo.toml`, else Python will not be able to
/// import the module.
#[pymodule]
fn _mysoc_validator_core(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<FuzzyDate>()?;
    m.add_class::<Popolo>()?;
    m.add_function(wrap_pyfunction!(cli_convert, m)?)?;
    Ok(())
}
