use fuzzy_date::FuzzyDate;
use popolo_validator::{
    Membership, MembershipType, Organization, OrganizationCollection, Popolo, ValidatorType,
};
use serde_json::json;

fn membership(id: &str, start: &str, end: &str, organization: &str) -> Membership {
    serde_json::from_value(json!({
        "id": id, "person_id": "uk.org.publicwhip/person/1",
        "organization_id": organization, "start_date": start, "end_date": end
    }))
    .unwrap()
}

fn dataset(memberships: Vec<Membership>) -> Popolo {
    Popolo::model_validate_json(&json!({
        "persons": [{"id": "uk.org.publicwhip/person/1"}],
        "organizations": [{"id": "commons", "name": "Commons"}, {"id": "lords", "name": "Lords"}],
        "memberships": memberships
    }).to_string(), &false).unwrap()
}

#[test]
fn insertion_and_loading_keep_distinct_approximate_and_historical_policies() {
    for (start, end, next_start, next_end) in [
        ("2025", "2026", "2025-06-01", "2025-07-01"),
        ("1800-01-01", "1801-01-01", "1800-06-01", "1800-07-01"),
    ] {
        let existing = membership("member/1", start, end, "commons");
        let new = membership("member/2", next_start, next_end, "commons");
        let popolo = dataset(vec![existing.clone()]);
        assert!(popolo
            .validate_membership_insertion(&new)
            .unwrap_err()
            .contains("overlaps"));
        let combined = dataset(vec![existing, new]);
        assert!(Popolo::model_validate_json(&combined.model_dump_json(), &true).is_ok());
    }
}

#[test]
fn shared_boundaries_are_rejected_and_organization_scopes_are_independent() {
    let first = membership("member/1", "2025-01-01", "2025-02-01", "commons");
    let next = membership("member/2", "2025-02-01", "2025-03-01", "commons");
    let popolo = dataset(vec![first.clone()]);
    assert!(popolo.validate_membership_insertion(&next).is_err());
    let combined = dataset(vec![first, next.clone()]);
    let errors = combined.check_valid_date_ranges().unwrap_err();
    assert_eq!(
        errors[0].validator_type,
        ValidatorType::DateRangeOverlapSameDate
    );
    let other_chamber = Membership {
        organization_id: Some("lords".to_string()),
        ..next
    };
    assert!(popolo.validate_membership_insertion(&other_chamber).is_ok());
}

#[test]
fn membership_sorting_uses_bounds_independently_of_fuzzy_matching() {
    let a = membership("member/1", "2025-01-15", "2025-03-01", "commons");
    let b = membership("member/2", "2025-02-01", "2025-04-01", "commons");
    let approximate = membership("member/3", "2025", "2026", "commons");
    // All permutations must identify the same precise adjacent pair. The year
    // sorts before February deterministically and is itself skipped by policy.
    let expected = "Membership member/1 overlaps with member/2";
    for records in [
        vec![a.clone(), b.clone(), approximate.clone()],
        vec![approximate.clone(), b.clone(), a.clone()],
        vec![b, a, approximate],
    ] {
        let errors = dataset(records).check_valid_date_ranges().unwrap_err();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].error, expected);
    }
}

#[test]
fn date_range_validation_and_sentinels_keep_fuzzy_semantics() {
    let record = membership("member/1", "2025-06", "2025", "commons");
    let popolo = dataset(vec![record]);
    assert!(Popolo::model_validate_json(&popolo.model_dump_json(), &true).is_ok());
    let record = membership("member/1", "0001", "9999", "commons");
    let dumped = serde_json::to_value(record).unwrap();
    assert!(dumped.get("start_date").is_none());
    assert!(dumped.get("end_date").is_none());
    // Exact Rust equality must not drive the compatibility omission policy.
    assert_ne!(
        FuzzyDate::fromisoformat("0001").unwrap(),
        FuzzyDate::fromisoformat("0001-01-01").unwrap()
    );
}

fn organization(id: &str, name: &str) -> Organization {
    serde_json::from_value(json!({"id": id, "name": name})).unwrap()
}

#[test]
fn collection_operations_maintain_indexes_and_duplicate_resolution() {
    let mut collection: OrganizationCollection = serde_json::from_value(json!([
        {"id": "a", "name": "First"}, {"id": "b", "name": "Second"}
    ]))
    .unwrap();
    assert_eq!(collection.get("b").unwrap().name, "Second");
    collection.push(organization("a", "Last duplicate"));
    assert_eq!(collection.get("a").unwrap().name, "Last duplicate");
    assert_eq!(collection.pop().unwrap().name, "Last duplicate");
    assert_eq!(collection.get("a").unwrap().name, "First");
    collection
        .replace(0, organization("c", "Replacement"))
        .unwrap();
    assert!(collection.get("a").is_none());
    assert_eq!(collection.get("c").unwrap().name, "Replacement");
    collection.remove("c").unwrap();
    assert_eq!(collection.id_lookup["b"], 0);
    assert_eq!(collection.get("b").unwrap().name, "Second");
    assert!(collection.remove("missing").is_none());
    collection.pop().unwrap();
    assert!(collection.id_lookup.is_empty());
}

#[test]
fn insertion_reports_first_error_without_mutating() {
    let record = membership("member/1", "2025", "2026", "commons");
    let popolo = dataset(vec![record.clone()]);
    assert_eq!(
        popolo.validate_membership_insertion(&record).unwrap_err(),
        "Duplicate Membership id \"member/1\""
    );
    assert_eq!(popolo.memberships.len(), 1);
    assert!(matches!(
        popolo.memberships.get("member/1"),
        Some(MembershipType::Membership(_))
    ));
}
