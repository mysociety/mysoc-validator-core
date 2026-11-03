"""
Check that the installed package ships complete, parseable extension stubs.
"""

from __future__ import annotations

import ast
import inspect
from pathlib import Path

from mysoc_validator_fast import _mysoc_validator_fast as native


def test_packaged_stubs_cover_runtime_exports() -> None:
    """
    Catch missing wheel data and exported classes or members omitted by generation.
    """
    package = Path(native.__file__).parent
    assert (package / "py.typed").is_file()
    stub = ast.parse((package / "_mysoc_validator_fast.pyi").read_text())
    classes = {node.name: node for node in stub.body if isinstance(node, ast.ClassDef)}
    functions = {node.name for node in stub.body if isinstance(node, ast.FunctionDef)}
    for name, value in vars(native).items():
        if name.startswith("_"):
            continue
        if inspect.isclass(value):
            assert name in classes, f"Missing class: {name}"
            members = {
                node.name
                for node in classes[name].body
                if isinstance(node, ast.FunctionDef)
            }
            for member in vars(value):
                if not member.startswith("_"):
                    assert member in members, f"Missing member: {name}.{member}"
        elif inspect.isbuiltin(value):
            assert name in functions, f"Missing function: {name}"
