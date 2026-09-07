use super::FuzzyDate;
use std::cmp::Ordering;
use std::collections::BTreeSet;

fn date(value: &str) -> FuzzyDate {
    FuzzyDate::fromisoformat(value).unwrap()
}

#[test]
fn overlap_is_not_rust_equality() {
    let january = date("2025-01");
    let year = date("2025");
    let february = date("2025-02");
    assert!(january.overlaps(&year));
    assert!(year.overlaps(&february));
    assert!(!january.overlaps(&february));
    assert_ne!(january, year);
    assert_ne!(year, february);
    assert_ne!(january, february);
    assert_eq!(january, date("2025-01-01/2025-01-31"));
    let ordered = BTreeSet::from([february.clone(), year.clone(), january.clone()]);
    assert_eq!(
        ordered.into_iter().collect::<Vec<_>>(),
        vec![january, year, february]
    );
}

#[test]
fn trait_ordering_is_transitive_and_matches_equality() {
    let dates = [
        date("2025"),
        date("2025-01"),
        date("2025-01-01"),
        date("2025-02"),
        date("2026"),
    ];
    for a in &dates {
        for b in &dates {
            assert_eq!(a.partial_cmp(b), Some(a.cmp(b)));
            assert_eq!(a == b, a.cmp(b) == Ordering::Equal);
            assert_eq!(a.cmp(b), b.cmp(a).reverse());
            for c in &dates {
                if a <= b && b <= c {
                    assert!(a <= c);
                }
            }
        }
    }
}

#[test]
fn cross_type_equality_requires_identical_bounds() {
    let precise = date("2025-01-01");
    let day = precise.earliest_date;
    let year = date("2025");
    assert_eq!(precise, day);
    assert_ne!(year, day);
    assert!(year.contains(&day));
    assert_eq!(precise.partial_cmp(&day), Some(Ordering::Equal));
    assert_eq!(year.partial_cmp(&day), Some(Ordering::Greater));
    assert!(year == *"2025-01-01/2025-12-31");
    assert!(year != *"2025-01-01");
    assert!(year != *"invalid");
}
