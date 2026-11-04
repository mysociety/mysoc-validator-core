//! Upstream-style staged merges, including validation of the growing dataset.

use crate::collections::HasId;
use crate::validation::ValidatorErrorCollection;
use crate::{MembershipType, Popolo, ValidatorError, ValidatorType};
use std::collections::HashSet;

fn check_existing_ids<T: HasId>(
    existing: &[T],
    incoming: &[T],
    item_type: &str,
) -> Result<(), ValidatorErrorCollection> {
    let ids: HashSet<&String> = existing.iter().map(HasId::get_id).collect();
    for item in incoming {
        if ids.contains(item.get_id()) {
            return Err(vec![ValidatorError {
                error: format!("Duplicate {} id {:?}", item_type, item.get_id()),
                validator_type: ValidatorType::NonUniqueId,
            }]);
        }
    }
    Ok(())
}

impl Popolo {
    /// Add organizations, posts, people and memberships, checking each stage.
    ///
    /// Extras may reference existing records. Duplicate IDs and invalid references
    /// raise errors rather than being skipped. As upstream, this is not atomic:
    /// completed stages and a batch failing reference checks remain in the dataset.
    /// Memberships are checked against existing memberships before the batch is
    /// added; this does not run a full dataset overlap check between incoming items.
    pub fn update(&mut self, other: &Self) -> Result<(), ValidatorErrorCollection> {
        check_existing_ids(
            &self.organizations.root,
            &other.organizations.root,
            "Organization",
        )?;
        for organization in &other.organizations {
            self.organizations.push(organization.clone());
        }
        self.check_unique_ids()?;

        check_existing_ids(&self.posts.root, &other.posts.root, "Post")?;
        for post in &other.posts {
            self.posts.push(post.clone());
        }
        self.check_unique_ids()?;

        check_existing_ids(&self.persons.root, &other.persons.root, "Person")?;
        for person in &other.persons {
            self.persons.push(person.clone());
        }
        self.check_unique_ids()?;

        check_existing_ids(
            &self.memberships.root,
            &other.memberships.root,
            "Membership",
        )?;
        for membership in &other.memberships {
            if let MembershipType::Membership(membership) = membership {
                self.check_membership_overlap(membership).map_err(|error| {
                    vec![ValidatorError {
                        error,
                        validator_type: ValidatorType::DateRangeOverlap,
                    }]
                })?;
            }
        }
        for membership in &other.memberships {
            self.memberships.push(membership.clone());
        }
        self.check_unique_ids()
    }
}
