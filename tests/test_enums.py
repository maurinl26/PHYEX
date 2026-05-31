"""Tests for the typed scheme selectors (phyex.enums).

Pure-Python (no compiled extension needed). Besides exercising the enum API,
the last test enforces the SYNC CONTRACT: the Python enums must match the
``PHYEX_*`` integer parameters declared in the Fortran bridge, so the two
sources of truth can't silently drift apart.
"""
import re
from pathlib import Path

import pytest

from phyex.enums import (
    MicroScheme, ShallowConvScheme, TurbScheme,
    normalize_micro, normalize_sconv, normalize_turb,
)

BRIDGE_F90 = Path(__file__).resolve().parent.parent / "cmake" / "bridge" / "phyex_bridge.F90"


def test_fortran_name_is_member_name():
    assert MicroScheme.ICE3.fortran_name == "ICE3"
    assert ShallowConvScheme.EDKF.fortran_name == "EDKF"
    assert TurbScheme.TKEL.fortran_name == "TKEL"
    # Every member's legacy code is exactly its name (the invariant enums rely on).
    for enum in (MicroScheme, ShallowConvScheme, TurbScheme):
        for m in enum:
            assert m.fortran_name == m.name


def test_from_value_accepts_enum_int_and_string():
    # passthrough
    assert normalize_micro(MicroScheme.ICE4) is MicroScheme.ICE4
    # integer id
    assert normalize_micro(1) is MicroScheme.ICE3
    # legacy string, case-insensitive + whitespace tolerant
    assert normalize_micro("ice3") is MicroScheme.ICE3
    assert normalize_micro(" ICE3 ") is MicroScheme.ICE3
    assert normalize_sconv("EDKF") is ShallowConvScheme.EDKF
    assert normalize_turb("tkel") is TurbScheme.TKEL


def test_from_value_rejects_unknown_and_bad_types():
    with pytest.raises(ValueError, match="unknown scheme 'ICE9'"):
        normalize_micro("ICE9")
    with pytest.raises(ValueError, match="unknown scheme id 99"):
        normalize_micro(99)
    with pytest.raises(TypeError):
        normalize_micro(3.5)
    # bool must not sneak through as an int id (True == 1).
    with pytest.raises(TypeError):
        normalize_micro(True)


def _parse_fortran_enum_ids():
    """Return {prefix: {SUFFIX: value}} from the bridge's PHYEX_* parameters."""
    text = BRIDGE_F90.read_text()
    pat = re.compile(
        r"PHYEX_(MICRO|SCONV|TURB)_([A-Z0-9]+)\s*=\s*(\d+)")
    out = {}
    for group, suffix, value in pat.findall(text):
        out.setdefault(group, {})[suffix] = int(value)
    return out


def test_python_enums_match_fortran_bridge():
    fortran = _parse_fortran_enum_ids()
    mapping = {
        "MICRO": MicroScheme,
        "SCONV": ShallowConvScheme,
        "TURB": TurbScheme,
    }
    assert set(fortran) == set(mapping), "scheme groups differ between F90 and Python"
    for group, enum in mapping.items():
        f_ids = fortran[group]
        py_ids = {m.name: int(m) for m in enum}
        assert f_ids == py_ids, (
            "{} drift: Fortran={} Python={}".format(enum.__name__, f_ids, py_ids))
