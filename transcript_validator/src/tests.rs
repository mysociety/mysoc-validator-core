use super::{validate_folder, Transcript};
use std::collections::HashSet;
use std::fs;

#[test]
fn validates_and_round_trips() {
    let xml = r#"<publicwhip scraperversion="1"><major-heading id="uk.org.publicwhip/debates/2026-01-01.1.mh">Heading</major-heading><speech id="uk.org.publicwhip/debates/2026-01-01.1.1" person_id="unknown"><p>Hello</p></speech></publicwhip>"#;
    let transcript = Transcript::model_validate_xml(xml).unwrap();
    assert_eq!(transcript.items.len(), 2);
    assert_eq!(transcript.items[1].string_value(), "Hello");
    assert_eq!(transcript.model_dump_xml(), xml);
}

#[test]
fn rejects_invalid_gid() {
    let xml =
        r#"<publicwhip><speech id="uk.org.publicwhip/Bad_Name/2026-01-01.1.1"/></publicwhip>"#;
    assert!(Transcript::model_validate_xml(xml).is_err());
}

#[test]
fn folder_validation_checks_stated_person_ids() {
    let folder = std::env::temp_dir().join(format!("transcript-validator-{}", std::process::id()));
    fs::create_dir_all(&folder).unwrap();
    fs::write(
            folder.join("valid.xml"),
            r#"<publicwhip><speech id="uk.org.publicwhip/debates/2026-01-01.1.1" person_id="unknown"/></publicwhip>"#,
        )
        .unwrap();
    fs::write(
            folder.join("missing.xml"),
            r#"<publicwhip><speech id="uk.org.publicwhip/debates/2026-01-01.1.2" person_id="uk.org.publicwhip/person/2"/></publicwhip>"#,
        )
        .unwrap();
    let people = HashSet::from(["uk.org.publicwhip/person/1".to_string()]);
    let results = validate_folder(&folder, Some(&people)).unwrap();
    assert_eq!(results.len(), 2);
    assert_eq!(
        results
            .iter()
            .filter(|result| !result.errors.is_empty())
            .count(),
        1
    );
    fs::remove_dir_all(folder).unwrap();
}
