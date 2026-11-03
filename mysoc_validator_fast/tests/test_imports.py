"""
Keep unused HTTP, schema and rendering dependencies off the local validation path.
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path


def test_local_validation_defers_optional_imports(tmp_path: Path) -> None:
    """
    Check imports in a fresh process, including after successful validation.
    """
    path = tmp_path / "[local] people.json"
    path.write_text(
        '{"organizations": [], "persons": [], "posts": [], "memberships": []}'
    )
    result = subprocess.run(
        [
            sys.executable,
            "-c",
            """
import sys
from mysoc_validator_fast.__main__ import app

for module in ("requests", "pydantic", "rich.console"):
    assert module not in sys.modules, module
app(args=["popolo", "validate", sys.argv[1]], standalone_mode=False)
for module in ("requests", "pydantic", "rich.console"):
    assert module not in sys.modules, module
""",
            str(path),
        ],
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    )
    assert "0 people" in result.stdout
    assert f"Valid Popolo file {path}" in result.stdout


def test_extra_models_require_explicit_import() -> None:
    """
    Keep wildcard imports lightweight and extra schemas available explicitly.
    """
    subprocess.run(
        [
            sys.executable,
            "-c",
            """
import sys
import mysoc_validator_fast as package

names = {
    "AreaExtra", "Extra", "LocalisedLabelsExtra", "MembershipExtra",
    "OrganizationExtra", "PersonExtra", "PostExtra",
}
assert names.isdisjoint(package.__all__)
assert names.isdisjoint(dir(package))
from mysoc_validator_fast import *
assert "pydantic" not in sys.modules
assert names.isdisjoint(globals())
from mysoc_validator_fast.popolo_extras import Extra, PersonExtra
from mysoc_validator_fast import popolo_extras
assert "pydantic" in sys.modules
assert Extra is popolo_extras.Extra
assert PersonExtra is popolo_extras.PersonExtra
assert "requests" not in sys.modules
""",
        ],
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    )
