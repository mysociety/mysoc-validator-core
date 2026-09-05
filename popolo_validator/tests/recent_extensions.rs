use chrono::NaiveDate;
use popolo_validator::{LordName, NiceName, Popolo};

const SUPPLEMENTAL: &str = r#"{
  "organizations": [
    {"id":"welsh-parliament","name":"Senedd","classification":"chamber"},
    {
      "id":"senedd-committee-210781",
      "name":"Y Pwyllgor Cyllid / Finance Committee",
      "classification":"committee",
      "description":"A finance committee",
      "founding_date":"2021-05",
      "links":["https://example.com/committee"],
      "parent_id":"welsh-parliament",
      "extra":{"localised_values":{"name":{"en":"Finance Committee","cy":"Y Pwyllgor Cyllid"}},"custom":2}
    }
  ],
  "posts": [{
    "id":"senedd-committee-210781-chair",
    "organization_id":"senedd-committee-210781",
    "label":"Chair",
    "role":"chair",
    "extra":{"localised_values":{"role":{"cy":"cadeirydd"}}}
  }],
  "persons": [{"id":"uk.org.publicwhip/person/123","extra":{"source":"test"}}],
  "memberships": [{
    "id":"senedd.wales/Committee/210781/Chair/123",
    "person_id":"uk.org.publicwhip/person/123",
    "post_id":"senedd-committee-210781-chair",
    "extra":{"tag":"committee"}
  }]
}"#;

#[test]
fn supplemental_popolo_round_trips() {
    let popolo = Popolo::model_validate_json(SUPPLEMENTAL, &true).unwrap();
    let dumped = popolo.model_dump_json();
    let source: serde_json::Value = serde_json::from_str(SUPPLEMENTAL).unwrap();
    let output: serde_json::Value = serde_json::from_str(&dumped).unwrap();
    assert_eq!(source, output);
}

#[test]
fn organization_parent_is_cross_checked() {
    let invalid = r#"{"organizations":[{"id":"child","name":"Child","parent_id":"missing"}]}"#;
    let errors = Popolo::model_validate_json(invalid, &true).unwrap_err();
    assert!(errors[0]
        .error
        .contains("Organization child refers to invalid parent organization missing"));
    assert!(Popolo::model_validate_json(invalid, &false).is_ok());
}

#[test]
fn post_organization_is_cross_checked() {
    let invalid = r#"{"posts":[{"id":"committee-chair","label":"Chair","role":"chair","organization_id":"missing"}]}"#;
    let errors = Popolo::model_validate_json(invalid, &true).unwrap_err();
    assert!(errors[0]
        .error
        .contains("Post committee-chair refers to invalid organization missing"));
}

#[test]
fn historical_name_lookup_keeps_membership_eligibility() {
    let json = r#"{
      "organizations":[{"id":"chamber","name":"Chamber"}],
      "posts":[{"id":"supplemental-post","label":"Seat","role":"member","organization_id":"chamber"}],
      "persons":[{"id":"uk.org.publicwhip/person/1","other_names":[
        {"family_name":"Old","given_name":"Jane","note":"Main","start_date":"2000-01-01","end_date":"2005-01-01"},
        {"family_name":"New","given_name":"Jane","note":"Main","start_date":"2010-01-01"}
      ]}],
      "memberships":[{"id":"member/1","person_id":"uk.org.publicwhip/person/1","post_id":"supplemental-post","start_date":"2000-01-01","end_date":"2005-01-01"}]
    }"#;
    let popolo = Popolo::model_validate_json(json, &true).unwrap();
    let date = NaiveDate::from_ymd_opt(2001, 1, 1).unwrap();
    assert_eq!(
        popolo
            .find_person_id_by_name("Jane New", "chamber", date, false)
            .unwrap(),
        None
    );
    assert_eq!(
        popolo
            .find_person_id_by_name("Jane New", "chamber", date, true)
            .unwrap(),
        Some("uk.org.publicwhip/person/1".to_string())
    );
}

#[test]
fn lord_name_generates_current_python_variants() {
    let bishop: LordName = serde_json::from_str(
        r#"{"honorific_prefix":"Bishop","lordofname":"Norwich","note":"Main"}"#,
    )
    .unwrap();
    assert_eq!(bishop.nice_name(), "Bishop of Norwich");
    assert_eq!(
        bishop.name_variants(false),
        vec![
            "Bishop of Norwich",
            "The Bishop of Norwich",
            "The Lord Bishop of Norwich"
        ]
    );

    let peer: LordName = serde_json::from_str(
        r#"{"given_name":"Tony","surname":"Banks","honorific_prefix":"Lord","lordname":"Stratford","note":"Main"}"#,
    )
    .unwrap();
    assert_eq!(peer.original_full_name().as_deref(), Some("Tony Banks"));
    assert_eq!(
        peer.name_variants(true),
        vec!["Lord Stratford", "Tony Banks"]
    );
}

#[test]
fn lord_name_requires_prefix_and_valid_surname_style() {
    assert!(serde_json::from_str::<LordName>(r#"{"lordname":"Smith","note":"Main"}"#).is_err());
    assert!(serde_json::from_str::<LordName>(
        r#"{"surname":"Smith","honorific_prefix":"Baron","note":"Main"}"#
    )
    .is_err());
    assert!(serde_json::from_str::<LordName>(
        r#"{"surname":"Smith","lordofname":"Somewhere","honorific_prefix":"Baron","note":"Main"}"#
    )
    .is_ok());
}

#[test]
fn ambiguous_historical_name_is_rejected() {
    let json = r#"{
      "organizations":[{"id":"chamber","name":"Chamber"}],
      "posts":[{"id":"seat","label":"Seat","role":"member","organization_id":"chamber"}],
      "persons":[
        {"id":"uk.org.publicwhip/person/1","other_names":[{"name":"Shared Name","note":"Alternate"}]},
        {"id":"uk.org.publicwhip/person/2","other_names":[{"name":"Shared Name","note":"Alternate"}]}
      ],
      "memberships":[
        {"id":"member/1","person_id":"uk.org.publicwhip/person/1","post_id":"seat","start_date":"2000-01-01","end_date":"2005-01-01"},
        {"id":"member/2","person_id":"uk.org.publicwhip/person/2","post_id":"seat","start_date":"2000-01-01","end_date":"2005-01-01"}
      ]
    }"#;
    let popolo = Popolo::model_validate_json(json, &true).unwrap();
    let date = NaiveDate::from_ymd_opt(2001, 1, 1).unwrap();
    assert_eq!(
        popolo
            .find_person_id_by_name("Shared Name", "chamber", date, true)
            .unwrap_err(),
        "Ambiguous person name: Shared Name"
    );
}
