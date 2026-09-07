use regmem_validator::{Chamber, CommonKey, ContentType, Language, RegmemRegister};
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
        assert_eq!(serde_json::to_value(&parsed).unwrap(), json!(spelling));
        assert_eq!(
            serde_json::from_value::<T>(json!(spelling)).unwrap(),
            parsed
        );
    }
    assert!("invalid_value".parse::<T>().is_err());
    assert!(serde_json::from_value::<T>(json!("invalid_value")).is_err());
}

#[test]
fn register_enum_strings_share_json_and_standard_parsing() {
    assert_vocabulary::<Chamber>(&[
        "house-of-commons",
        "house-of-lords",
        "scottish-parliament",
        "welsh-parliament",
        "london-assembly",
        "northern-ireland-assembly",
    ]);
    assert_vocabulary::<Language>(&["en", "cy"]);
    assert_vocabulary::<ContentType>(&["string", "markdown", "xml"]);
    assert_vocabulary::<CommonKey>(&["companies_house", "url", "standardised_name", "sic_code"]);
}

#[test]
fn null_language_permits_mixed_languages_and_survives_reload() {
    let input = json!({"language":null, "persons":[
        {"person_id":"p1", "person_name":"English", "published_date":"2025-01-20", "chamber":"house-of-commons", "language":"en"},
        {"person_id":"p2", "person_name":"Welsh", "published_date":"2025-01-20", "chamber":"welsh-parliament", "language":"cy"}
    ]});
    let register = RegmemRegister::model_validate_json(&input.to_string(), true).unwrap();
    assert_eq!(register.language, None);
    let dumped = register.model_dump_json();
    assert_eq!(
        serde_json::from_str::<Value>(&dumped).unwrap()["language"],
        Value::Null
    );
    let reloaded = RegmemRegister::model_validate_json(&dumped, true).unwrap();
    assert_eq!(reloaded, register);

    for language in [None, Some(json!("en")), Some(json!("cy"))] {
        let mut constrained = input.clone();
        constrained.as_object_mut().unwrap().remove("language");
        if let Some(language) = language {
            constrained["language"] = language;
        }
        assert!(
            RegmemRegister::model_validate_json(&constrained.to_string(), true)
                .unwrap_err()
                .contains("same language")
        );
    }
    let defaulted = RegmemRegister::model_validate_json("{}", true).unwrap();
    assert_eq!(defaulted.language, Some(Language::En));
}
