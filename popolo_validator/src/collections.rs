//! Ordered records and ID indexes used by domain queries and Python handles.
//!
//! `root` owns records in serialization order; `id_lookup` maps IDs to positions.
//! Deserialization and push/pop/remove/replace maintain that index. Public fields
//! and mutable references remain for Rust API compatibility, so direct edits to
//! IDs or vector structure require `set_lookup`. Duplicate IDs are retained and
//! lookup selects the last occurrence; validation rejects them separately.
//! The macro generates concrete collection types and their serde/iterator traits,
//! rather than hiding four domain APIs behind a generic Python-facing wrapper.

use crate::models::{
    Membership, MembershipRedirect, MembershipType, Organization, Person, PersonRedirect,
    PersonType, Post,
};
use serde::de::Deserialize;
use serde::Serialize;
use std::collections::HashMap;

macro_rules! create_interface {
    ($name: ident, $type: ident) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            pub root: Vec<$type>,
            pub id_lookup: HashMap<String, usize>,
        }

        impl $name {
            /// Borrow a record until the collection is mutated or dropped.
            pub fn get(&self, id: &str) -> Option<&$type> {
                self.id_lookup.get(id).map(|i| &self.root[*i])
            }

            /// Borrow a record exclusively; call `set_lookup` if its ID changes.
            pub fn get_mut(&mut self, id: &str) -> Option<&mut $type> {
                self.id_lookup.get(id).map(|i| &mut self.root[*i])
            }

            /// Rebuild after direct `root` or ID edits. Duplicate IDs resolve to
            /// the last record, matching deserialization and `push`.
            pub fn set_lookup(&mut self) {
                self.id_lookup = self
                    .root
                    .iter()
                    .enumerate()
                    .map(|(i, x)| (x.get_id().clone(), i))
                    .collect();
            }

            pub fn new() -> Self {
                $name {
                    root: Vec::new(),
                    id_lookup: HashMap::new(),
                }
            }

            pub fn len(&self) -> usize {
                self.root.len()
            }

            /// Append and index a record without applying validation policy.
            pub fn push(&mut self, item: $type) {
                self.id_lookup
                    .insert(item.get_id().clone(), self.root.len());
                self.root.push(item);
            }

            pub fn pop(&mut self) -> Option<$type> {
                let item = self.root.pop()?;
                self.set_lookup();
                Some(item)
            }

            /// Remove by ID while preserving the order of remaining records.
            pub fn remove(&mut self, id: &str) -> Option<$type> {
                let index = *self.id_lookup.get(id)?;
                let item = self.root.remove(index);
                self.set_lookup();
                Some(item)
            }

            /// Replace one position, rebuilding the index even if the ID changes.
            pub fn replace(&mut self, index: usize, item: $type) -> Option<$type> {
                let previous = std::mem::replace(self.root.get_mut(index)?, item);
                self.set_lookup();
                Some(previous)
            }

            pub fn iter(&self) -> std::slice::Iter<'_, $type> {
                self.root.iter()
            }

            /// Mutable iteration does not track ID changes; rebuild afterwards.
            pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, $type> {
                self.root.iter_mut()
            }
        }

        impl<'a> IntoIterator for &'a $name {
            type Item = &'a $type;
            type IntoIter = std::slice::Iter<'a, $type>;

            fn into_iter(self) -> Self::IntoIter {
                self.root.iter()
            }
        }

        impl<'de> Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                let vec = Vec::deserialize(deserializer)?;
                let mut collection = $name {
                    root: vec,
                    id_lookup: HashMap::new(),
                };
                collection.set_lookup();
                Ok(collection)
            }
        }

        impl Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                self.root.serialize(serializer)
            }
        }
    };
}

create_interface!(PersonCollection, PersonType);

create_interface!(MembershipCollection, MembershipType);

create_interface!(OrganizationCollection, Organization);

create_interface!(PostCollection, Post);

pub(crate) trait HasId {
    fn get_id(&self) -> &String;
}

impl HasId for Post {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl HasId for Organization {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl HasId for PersonRedirect {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl HasId for Person {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl HasId for MembershipRedirect {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl HasId for Membership {
    fn get_id(&self) -> &String {
        &self.id
    }
}

impl HasId for PersonType {
    fn get_id(&self) -> &String {
        match self {
            Self::Person(p) => p.get_id(),
            Self::PersonRedirect(p) => p.get_id(),
        }
    }
}

impl HasId for MembershipType {
    fn get_id(&self) -> &String {
        match self {
            Self::Membership(m) => m.get_id(),
            Self::MembershipRedirect(m) => m.get_id(),
        }
    }
}
