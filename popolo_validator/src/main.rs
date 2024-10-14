use std::env;

use popolo_validator::Popolo;

fn main() {
    // Get the command line argument for the postcode
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Please provide a popolo file as an argument");
        std::process::exit(1);
    }
    let popolo_location = &args[1];
    let verbose = args.contains(&String::from("--verbose"));
    let format_file = args.contains(&String::from("--format"));
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
