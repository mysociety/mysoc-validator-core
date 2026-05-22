from datetime import date

import pytest

from mysoc_validator_fast.dates import FuzzyDate


def test_create_from_full_iso_8601():
    d = FuzzyDate.fromisoformat("1964-06-26")
    assert isinstance(d, FuzzyDate)
    assert d.earliest_date == date(1964, 6, 26)
    assert d.latest_date == date(1964, 6, 26)
    assert d == "1964-06-26"


def test_create_from_partial_iso_8601_only_year():
    d = FuzzyDate.fromisoformat("1964")
    assert isinstance(d, FuzzyDate)
    assert d.earliest_date == date(1964, 1, 1)
    assert d.latest_date == date(1964, 12, 31)
    assert d == "1964"


def test_create_from_partial_iso_8601_only_year_and_month():
    d = FuzzyDate.fromisoformat("1964-06")
    assert isinstance(d, FuzzyDate)
    assert d.earliest_date == date(1964, 6, 1)
    assert d.latest_date == date(1964, 6, 30)
    assert d == "1964-06"


def test_malformed_iso_8601_date():
    with pytest.raises(ValueError):
        FuzzyDate.fromisoformat("next Tuesday-ish")


def test_arbitrary_date_range():
    d = FuzzyDate(date(1926, 1, 3), date(2016, 3, 8))
    assert d == "1926-01-03/2016-03-08"


def test_equality_to_other_fuzzy_date():
    d1 = FuzzyDate.fromisoformat("1964-06-26")
    d2 = FuzzyDate.fromisoformat("1964-06-26")
    assert d1 == d2


def test_inequality_to_other_fuzzy_date():
    d1 = FuzzyDate.fromisoformat("1964-06-26")
    d2 = FuzzyDate.fromisoformat("1977-12-27")
    assert d1 != d2


def test_equality_across_precision():
    d1 = FuzzyDate.fromisoformat("1964-06-26")
    d2 = FuzzyDate.fromisoformat("1964-06")
    assert d1 == d2


def test_inequality_across_precision():
    d1 = FuzzyDate.fromisoformat("1964-06-26")
    d2 = FuzzyDate.fromisoformat("1964-07")
    assert d1 != d2


def test_equality_to_python_date():
    approx_date = FuzzyDate.fromisoformat("1964-06-26")
    assert approx_date == date(1964, 6, 26)


def test_inequality_to_different_python_date():
    approx_date = FuzzyDate.fromisoformat("1964-06-26")
    assert approx_date != date(1964, 6, 10)


def test_imprecise_equality_to_date_in_range():
    approx_date = FuzzyDate.fromisoformat("1964-06")
    assert approx_date == date(1964, 6, 26)


def test_imprecise_inequality_to_date_out_of_range():
    approx_date = FuzzyDate.fromisoformat("1999")
    assert approx_date != date(1964, 6, 26)


def test_add_timedelta():
    from datetime import timedelta
    d = FuzzyDate.fromisoformat("2020-01-01")
    result = d + timedelta(days=5)
    assert result == date(2020, 1, 6)
