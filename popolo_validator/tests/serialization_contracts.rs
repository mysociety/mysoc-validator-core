use popolo_validator::{MembershipReason, NameOptions, OrgType, Popolo};
use serde::de::DeserializeOwned;
use serde::Serialize;
use serde_json::{json, Value};
use std::fmt::{Debug, Display};
use std::str::FromStr;

fn assert_vocabulary<T>(spellings: &[&str])
where
    T: FromStr + Serialize + DeserializeOwned + Display + Debug + PartialEq,
    T::Err: Debug,
{
    for spelling in spellings {
        let parsed: T = spelling.parse().unwrap();
        assert_eq!(parsed.to_string(), *spelling);
        let encoded = serde_json::to_value(&parsed).unwrap();
        assert_eq!(encoded, json!(spelling));
        assert_eq!(serde_json::from_value::<T>(encoded).unwrap(), parsed);
    }
    assert!("invalid_value".parse::<T>().is_err());
    assert!(serde_json::from_value::<T>(json!("invalid_value")).is_err());
}

#[test]
fn enum_strings_match_json_and_standard_parsing() {
    assert_vocabulary::<OrgType>(&[
        "party",
        "chamber",
        "metro",
        "committee",
        "cross_party_group",
        "local_authority",
        "combined_authority",
        "other",
    ]);
    assert_vocabulary::<MembershipReason>(&[
        "",
        "accession",
        "appointed",
        "became_peer",
        "became_presiding_officer",
        "by_election",
        "changed_party",
        "declared_void",
        "devolution",
        "died",
        "disqualified",
        "dissolution",
        "election",
        "general_election",
        "general_election_not_standing",
        "general_election_probably",
        "general_election_standing",
        "recall_petition",
        "regional_election",
        "reinstated",
        "replaced_in_region",
        "resigned",
        "retired",
        "whip_removed",
        "whip_restored",
        "unknown",
    ]);
    assert!(serde_json::from_value::<OrgType>(json!("crosspartygroup")).is_err());
}

#[test]
fn open_dates_normalize_on_every_name_shape() {
    // Upstream removes default bounds from fields_set before exclude_unset dumping.
    let shapes = [
        json!({"note":"Main", "honorific_prefix":"Lord", "lordname":"Example"}),
        json!({"note":"Main", "family_name":"Example"}),
        json!({"note":"Alternate", "name":"An Example"}),
    ];
    for shape in shapes {
        for (field, default) in [("start_date", "0001-01-01"), ("end_date", "9999-12-31")] {
            for value in [
                None,
                Some(json!("")),
                Some(Value::Null),
                Some(json!(default)),
            ] {
                let mut input = shape.clone();
                if let Some(value) = value {
                    input[field] = value;
                }
                let name: NameOptions = serde_json::from_value(input).unwrap();
                let dumped = serde_json::to_value(name).unwrap();
                assert!(dumped.get(field).is_none(), "{dumped}");
            }
        }
        let mut input = shape.clone();
        input["start_date"] = json!("0001-01-02");
        let name: NameOptions = serde_json::from_value(input).unwrap();
        assert_eq!(
            serde_json::to_value(name).unwrap()["start_date"],
            "0001-01-02"
        );
    }
}

#[test]
fn omission_preserves_meaningful_empty_values_and_custom_date_rules() {
    let popolo: Popolo = serde_json::from_value(json!({
        "persons": [{"id":"p/1", "biography":"", "links":[], "extra":{}}],
        "memberships": [{"id":"m/1", "person_id":"p/1", "start_date":"", "end_date":null, "start_reason":""}],
        "posts": [{"id":"post", "label":"", "role":"", "organization_id":"body", "start_date":null, "end_date":""}],
        "organizations": [{"id":"body", "name":"Body", "classification":"local_authority", "links":[]}]
    })).unwrap();
    let dumped = serde_json::to_value(&popolo).unwrap();
    assert_eq!(dumped["persons"][0]["biography"], "");
    assert_eq!(dumped["persons"][0]["links"], json!([]));
    assert_eq!(dumped["persons"][0]["extra"], json!({}));
    assert!(dumped["persons"][0].get("birth_date").is_none());
    assert_eq!(dumped["memberships"][0]["start_reason"], "");
    for collection in ["memberships", "posts"] {
        assert!(dumped[collection][0].get("start_date").is_none());
        assert!(dumped[collection][0].get("end_date").is_none());
    }
    assert!(dumped["organizations"][0].get("links").is_none());
    assert_eq!(
        dumped["organizations"][0]["classification"],
        "local_authority"
    );
    let reparsed: Popolo = serde_json::from_value(dumped.clone()).unwrap();
    assert_eq!(serde_json::to_value(reparsed).unwrap(), dumped);
}

#[test]
fn lord_name_validation_survives_single_definition_serde() {
    for extra in [
        json!({"surname":"Smith"}),
        json!({"surname":"Smith", "lordname":"", "lordofname":""}),
    ] {
        let mut input = json!({"note":"Main", "honorific_prefix":"Lord"});
        input
            .as_object_mut()
            .unwrap()
            .extend(extra.as_object().unwrap().clone());
        assert!(serde_json::from_value::<NameOptions>(input).is_err());
    }
    let empty_surname = json!({"note":"Main", "honorific_prefix":"Lord", "surname":""});
    assert!(serde_json::from_value::<NameOptions>(empty_surname).is_ok());
    let unknown = json!({"note":"Main", "honorific_prefix":"Lord", "unknown":true});
    assert!(serde_json::from_value::<NameOptions>(unknown).is_err());
}
