from datetime import date
from mysoc_validator_core import FuzzyDate
import pytest

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


def test_equality_true_to_other_fuzzyDate():
    d1 = FuzzyDate.fromisoformat("1964-06-26")
    d2 = FuzzyDate.fromisoformat("1964-06-26")
    assert d1 == d2


def test_equality_false_to_other_fuzzydate():
    d1 = FuzzyDate.fromisoformat("1964-06-26")
    d2 = FuzzyDate.fromisoformat("1977-12-27")
    assert d1 != d2


def test_inequality_true_to_other_fuzzzydate():
    d1 = FuzzyDate.fromisoformat("1964-06-26")
    d2 = FuzzyDate.fromisoformat("1977-12-27")
    assert d1 != d2


def test_inequality_false_to_other_fuzzydate():
    d1 = FuzzyDate.fromisoformat("1964-06-26")
    d2 = FuzzyDate.fromisoformat("1964-06-26")
    assert d1 == d2


def test_equality_true_to_different_precision():
    d1 = FuzzyDate.fromisoformat("1964-06-26")
    d2 = FuzzyDate.fromisoformat("1964-06")
    assert d1 == d2

def test_equality_false_to_different_precision():
    d1 = FuzzyDate.fromisoformat("1964-06-26")
    d2 = FuzzyDate.fromisoformat("1964-07")
    assert d1 != d2


def test_inequality_true_to_different_precision():
    d1 = FuzzyDate.fromisoformat("1964-06-26")
    d2 = FuzzyDate.fromisoformat("1964-06")
    assert d1 == d2


def test_equality_false_between_precise_and_date():
    approx_date = FuzzyDate.fromisoformat("1964-06-26")
    datetime_date = date(1964, 6, 26)
    assert approx_date == datetime_date


def test_equality_false_between_precise_and_different_date():
    approx_date = FuzzyDate.fromisoformat("1964-06-26")
    datetime_date = date(1964, 6, 10)
    assert approx_date != datetime_date


def test_equality_true_between_imprecise_and_date_in_range():
    approx_date = FuzzyDate.fromisoformat("1964-06")
    datetime_date = date(1964, 6, 26)
    assert approx_date == datetime_date


def test_equality_false_between_imprecise_and_date_out_of_range():
    approx_date = FuzzyDate.fromisoformat("1999")
    datetime_date = date(1964, 6, 26)
    assert approx_date != datetime_date
