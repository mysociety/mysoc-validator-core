use super::*;
#[test]
fn test_slugify() {
    assert_eq!(slugify("Donation Source"), "donation_source");
    assert_eq!(slugify("  Value!! "), "value");
    assert_eq!(slugify("A  B"), "a_b");
}

#[test]
fn test_title_case() {
    assert_eq!(title_case("donation source"), "Donation Source");
}

#[test]
fn test_infer_slug_and_type() {
    let d = RegmemDetail::new(None, Some("Donation Source".to_string()), None);
    assert_eq!(d.slug.as_deref(), Some("donation_source"));

    let d = RegmemDetail::new(Some("donor_name".to_string()), None, None);
    assert_eq!(d.display_as.as_deref(), Some("Donor Name"));

    let d = RegmemDetail::new(
        Some("value".to_string()),
        None,
        Some(DetailValue::Decimal(Decimal::from_str("5000.00").unwrap())),
    );
    assert_eq!(d.detail_type.as_deref(), Some("decimal"));
}

#[test]
fn test_detail_value_roundtrip() {
    let json = r#"[
            {"slug": "v", "type": "decimal", "value": "5000.00"},
            {"slug": "d", "type": "date", "value": "2024-07-23"},
            {"slug": "flag", "type": "boolean", "value": true},
            {"slug": "n", "type": "int", "value": 3}
        ]"#;
    let group: RegmemDetailGroup = serde_json::from_str(json).unwrap();
    assert_eq!(group.len(), 4);
    match group.0[0].value.as_ref().unwrap() {
        DetailValue::Decimal(d) => assert_eq!(d.to_string(), "5000.00"),
        other => panic!("expected decimal, got {other:?}"),
    }
    match group.0[1].value.as_ref().unwrap() {
        DetailValue::Date(d) => assert_eq!(d.format("%Y-%m-%d").to_string(), "2024-07-23"),
        other => panic!("expected date, got {other:?}"),
    }
    // round-trip preserves the decimal string form
    let dumped = serde_json::to_string(&group).unwrap();
    let reparsed: RegmemDetailGroup = serde_json::from_str(&dumped).unwrap();
    assert_eq!(group, reparsed);
}

#[test]
fn test_container_detail() {
    let json = r#"{
            "slug": "legs",
            "type": "container",
            "value": [[{"slug": "from", "type": "string", "value": "A"}]]
        }"#;
    let detail: RegmemDetail = serde_json::from_str(json).unwrap();
    assert_eq!(detail.sub_detail_groups().len(), 1);
    assert_eq!(detail.sub_detail_groups()[0].len(), 1);
}

#[test]
fn test_check_unique_detail_names() {
    let mut group = RegmemDetailGroup::new();
    group
        .append(RegmemDetail::new(Some("a".to_string()), None, None), None)
        .unwrap();
    let err = group
        .append(RegmemDetail::new(Some("a".to_string()), None, None), None)
        .unwrap_err();
    assert!(err.contains("Duplicate detail names"));
    // failed append did not grow the group
    assert_eq!(group.len(), 1);
}
