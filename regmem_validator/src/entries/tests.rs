use super::*;
#[test]
fn test_add_details_infers_type() {
    let mut base = RegmemInfoBase {
        id: None,
        content: String::new(),
        content_format: ContentType::String,
        date_registered: None,
        date_published: None,
        date_updated: None,
        date_received: None,
        null_entry: false,
        annotations: Vec::new(),
        details: RegmemDetailGroup::new(),
        sub_entries: Vec::new(),
    };
    base.add_details(
        Some("mysociety"),
        vec![("score".to_string(), DetailValue::Int(5))],
    )
    .unwrap();
    let detail = base.get_detail("score").unwrap();
    assert_eq!(detail.source, "mysociety");
    assert_eq!(detail.detail_type.as_deref(), Some("int"));
    assert_eq!(detail.display_as.as_deref(), Some("Score"));
}
