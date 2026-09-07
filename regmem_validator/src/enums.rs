//! Shared register vocabulary and serialized enum spellings.

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::Display,
    serde_with::SerializeDisplay,
    serde_with::DeserializeFromStr,
)]
pub enum Chamber {
    #[strum(serialize = "house-of-commons")]
    Commons,
    #[strum(serialize = "house-of-lords")]
    Lords,
    #[strum(serialize = "scottish-parliament")]
    Scotland,
    #[strum(serialize = "welsh-parliament")]
    Senedd,
    #[strum(serialize = "london-assembly")]
    London,
    #[strum(serialize = "northern-ireland-assembly")]
    NorthernIreland,
}

impl Chamber {
    pub fn as_str(&self) -> &'static str {
        self.into()
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Default,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::Display,
    serde_with::SerializeDisplay,
    serde_with::DeserializeFromStr,
)]
#[strum(serialize_all = "lowercase")]
pub enum Language {
    #[default]
    En,
    Cy,
}

impl Language {
    pub fn as_str(&self) -> &'static str {
        self.into()
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Default,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::Display,
    serde_with::SerializeDisplay,
    serde_with::DeserializeFromStr,
)]
#[strum(serialize_all = "lowercase")]
pub enum ContentType {
    #[default]
    String,
    Markdown,
    Xml,
}

impl ContentType {
    pub fn as_str(&self) -> &'static str {
        self.into()
    }
}

pub(crate) fn is_default_content_type(c: &ContentType) -> bool {
    *c == ContentType::String
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::Display,
    serde_with::SerializeDisplay,
    serde_with::DeserializeFromStr,
)]
#[strum(serialize_all = "snake_case")]
pub enum CommonKey {
    CompaniesHouse,
    Url,
    StandardisedName,
    SicCode,
}

impl CommonKey {
    pub fn as_str(&self) -> &'static str {
        self.into()
    }
}
