//! Name/identifier lookup and membership selection.

use crate::models::{Membership, MembershipType, PersonType};
use crate::Popolo;
use chrono::NaiveDate;
use std::collections::HashSet;

pub fn reduce_to_slug(s: &str) -> String {
    s.chars()
        .filter(|c| c.is_alphabetic())
        .flat_map(|c| c.to_lowercase())
        .collect()
}

impl Popolo {
    /// Resolve a person redirect chain, rejecting missing targets and cycles.
    pub fn resolve_person_id(&self, id: &str) -> Option<String> {
        let mut current = id;
        let mut visited = HashSet::new();
        while visited.insert(current) {
            match self.persons.get(current)? {
                PersonType::Person(person) => return Some(person.id.clone()),
                PersonType::PersonRedirect(redirect) => current = &redirect.redirect,
            }
        }
        None
    }

    /// Resolve a membership redirect chain, rejecting missing targets and cycles.
    pub fn resolve_membership_id(&self, id: &str) -> Option<String> {
        let mut current = id;
        let mut visited = HashSet::new();
        while visited.insert(current) {
            match self.memberships.get(current)? {
                MembershipType::Membership(membership) => return Some(membership.id.clone()),
                MembershipType::MembershipRedirect(redirect) => current = &redirect.redirect,
            }
        }
        None
    }

    pub(crate) fn just_memberships(&self) -> Vec<&Membership> {
        self.memberships
            .iter()
            .filter_map(|m| match m {
                MembershipType::Membership(m) => Some(m),
                MembershipType::MembershipRedirect(_) => None,
            })
            .collect()
    }
    /// Find the person_id for a person with a matching name active in the given chamber on the given date.
    pub fn find_person_id_by_name(
        &self,
        name: &str,
        chamber_id: &str,
        date: NaiveDate,
        include_historical_names: bool,
    ) -> Result<Option<String>, String> {
        let slug = reduce_to_slug(name);

        let post_ids: HashSet<&str> = self
            .posts
            .iter()
            .filter(|p| p.organization_id == chamber_id)
            .map(|p| p.id.as_str())
            .collect();

        let active_person_ids: HashSet<&str> = self
            .just_memberships()
            .iter()
            .filter(|m| {
                let via_post = m
                    .post_id
                    .as_deref()
                    .map(|pid| post_ids.contains(pid))
                    .unwrap_or(false);
                // Lords memberships link directly to the org with no post_id
                let via_org_direct =
                    m.post_id.is_none() && m.organization_id.as_deref() == Some(chamber_id);
                (via_post || via_org_direct)
                    && m.start_date.earliest_date <= date
                    && date <= m.end_date.latest_date
            })
            .map(|m| m.person_id.as_str())
            .collect();

        let mut matches = HashSet::new();
        for pt in self.persons.iter() {
            if let PersonType::Person(person) = pt {
                if !active_person_ids.contains(person.id.as_str()) {
                    continue;
                }
                if let Some(names) = &person.other_names {
                    for n in names {
                        let (start, end) = n.date_range();
                        if include_historical_names
                            || (start.earliest_date <= date && date <= end.latest_date)
                        {
                            for variant in n.name_variants(false) {
                                if reduce_to_slug(&variant) == slug {
                                    matches.insert(person.id.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
        match matches.len() {
            0 => Ok(None),
            1 => Ok(matches.into_iter().next()),
            _ => Err(format!("Ambiguous person name: {}", name)),
        }
    }
    /// Find the person_id for a person with a matching identifier in the given scheme.
    pub fn find_person_id_by_identifier(&self, identifier: &str, scheme: &str) -> Option<String> {
        for pt in self.persons.iter() {
            if let PersonType::Person(person) = pt {
                if let Some(ids) = &person.identifiers {
                    for id in ids {
                        if id.scheme == scheme && id.identifier.as_str() == identifier {
                            return Some(person.id.clone());
                        }
                    }
                }
            }
        }
        None
    }
    /// Return IDs of all non-redirect memberships for a given person_id.
    pub fn person_membership_ids(&self, person_id: &str) -> Vec<String> {
        self.just_memberships()
            .iter()
            .filter(|m| m.person_id == person_id)
            .map(|m| m.id.clone())
            .collect()
    }
}
