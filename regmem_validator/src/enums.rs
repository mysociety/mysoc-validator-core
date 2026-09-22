//! Shared register vocabulary and serialized enum spellings.

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Chamber {
    #[serde(rename = "house-of-commons")]
    Commons,
    #[serde(rename = "house-of-lords")]
    Lords,
    #[serde(rename = "scottish-parliament")]
    Scotland,
    #[serde(rename = "welsh-parliament")]
    Senedd,
    #[serde(rename = "london-assembly")]
    London,
    #[serde(rename = "northern-ireland-assembly")]
    NorthernIreland,
}

impl Chamber {
    pub fn as_str(&self) -> &'static str {
        match self {
            Chamber::Commons => "house-of-commons",
            Chamber::Lords => "house-of-lords",
            Chamber::Scotland => "scottish-parliament",
            Chamber::Senedd => "welsh-parliament",
            Chamber::London => "london-assembly",
            Chamber::NorthernIreland => "northern-ireland-assembly",
        }
    }

    pub fn from_str_opt(s: &str) -> Option<Chamber> {
        match s {
            "house-of-commons" => Some(Chamber::Commons),
            "house-of-lords" => Some(Chamber::Lords),
            "scottish-parliament" => Some(Chamber::Scotland),
            "welsh-parliament" => Some(Chamber::Senedd),
            "london-assembly" => Some(Chamber::London),
            "northern-ireland-assembly" => Some(Chamber::NorthernIreland),
            _ => None,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Default)]
#[serde(rename_all = "lowercase")]
pub enum Language {
    #[default]
    En,
    Cy,
}

impl Language {
    pub fn as_str(&self) -> &'static str {
        match self {
            Language::En => "en",
            Language::Cy => "cy",
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Default)]
#[serde(rename_all = "lowercase")]
pub enum ContentType {
    #[default]
    String,
    Markdown,
    Xml,
}

impl ContentType {
    pub fn as_str(&self) -> &'static str {
        match self {
            ContentType::String => "string",
            ContentType::Markdown => "markdown",
            ContentType::Xml => "xml",
        }
    }
}

pub(crate) fn is_default_content_type(c: &ContentType) -> bool {
    *c == ContentType::String
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum CommonKey {
    CompaniesHouse,
    Url,
    StandardisedName,
    SicCode,
}

impl CommonKey {
    pub fn as_str(&self) -> &'static str {
        match self {
            CommonKey::CompaniesHouse => "companies_house",
            CommonKey::Url => "url",
            CommonKey::StandardisedName => "standardised_name",
            CommonKey::SicCode => "sic_code",
        }
    }

    pub fn from_str_opt(s: &str) -> Option<CommonKey> {
        match s {
            "companies_house" => Some(CommonKey::CompaniesHouse),
            "url" => Some(CommonKey::Url),
            "standardised_name" => Some(CommonKey::StandardisedName),
            "sic_code" => Some(CommonKey::SicCode),
            _ => None,
        }
    }
}
