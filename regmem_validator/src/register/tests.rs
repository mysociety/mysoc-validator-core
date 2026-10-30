use super::*;
fn minimal_register_json(global_chamber: bool) -> String {
    let chamber_line = if global_chamber {
        "\"chamber\": \"house-of-commons\","
    } else {
        ""
    };
    format!(
        r#"{{
                {chamber_line}
                "published_date": "2025-01-20",
                "persons": [
                    {{
                        "person_id": "uk.org.publicwhip/person/1",
                        "person_name": "Test Person",
                        "published_date": "2025-01-20",
                        "chamber": "house-of-commons",
                        "categories": [
                            {{
                                "category_id": "1",
                                "category_name": "Cat",
                                "entries": [
                                    {{"id": "1", "content": "An entry"}}
                                ]
                            }}
                        ]
                    }}
                ]
            }}"#
    )
}

#[test]
fn test_register_loads_and_iterates() {
    let register = RegmemRegister::model_validate_json(&minimal_register_json(true), true).unwrap();
    assert_eq!(register.persons.len(), 1);
    let locs = register.iter_entry_locations();
    assert_eq!(locs.len(), 1);
    let entry = register.entry_at(&locs[0]).unwrap();
    assert_eq!(entry.base.content, "An entry");
    assert_eq!(entry.base.comparable_id(), "1");
}

#[test]
fn test_chamber_mismatch_fails() {
    let json = r#"{
            "chamber": "house-of-lords",
            "published_date": "2025-01-20",
            "persons": [
                {
                    "person_id": "p1", "person_name": "x",
                    "published_date": "2025-01-20",
                    "chamber": "house-of-commons", "categories": []
                }
            ]
        }"#;
    let err = RegmemRegister::model_validate_json(json, true).unwrap_err();
    assert!(err.contains("same chamber"));
}

#[test]
fn test_language_mismatch_fails() {
    let json = r#"{
            "chamber": "house-of-commons",
            "language": "en",
            "published_date": "2025-01-20",
            "persons": [
                {
                    "person_id": "p1", "person_name": "x",
                    "published_date": "2025-01-20",
                    "chamber": "house-of-commons", "language": "cy", "categories": []
                }
            ]
        }"#;
    let err = RegmemRegister::model_validate_json(json, true).unwrap_err();
    assert!(err.contains("same language"));
}

#[test]
fn test_unknown_fields_ignored() {
    // comparable_id / item_hash are computed fields in the data; ignored on load.
    let json = r#"{
            "chamber": "house-of-commons",
            "published_date": "2025-01-20",
            "persons": [
                {
                    "person_id": "p1", "person_name": "x",
                    "published_date": "2025-01-20",
                    "chamber": "house-of-commons",
                    "categories": [
                        {"category_id": "1", "category_name": "C", "entries": [
                            {"id": "7041", "content": "c", "comparable_id": "7041", "item_hash": "abc"}
                        ]}
                    ]
                }
            ]
        }"#;
    let register = RegmemRegister::model_validate_json(json, true).unwrap();
    let loc = register.iter_entry_locations()[0];
    assert_eq!(
        register.entry_at(&loc).unwrap().base.id.as_deref(),
        Some("7041")
    );
}

#[test]
fn test_register_round_trip_idempotent() {
    let register = RegmemRegister::model_validate_json(&minimal_register_json(true), true).unwrap();
    let dumped = register.model_dump_json();
    let reloaded = RegmemRegister::model_validate_json(&dumped, true).unwrap();
    assert_eq!(register, reloaded);
}
