"""Typed scheme selectors for PHYEX.

PHYEX selects its schemes with 4-character string codes (``'ICE3'``, ``'EDKF'``,
``'TKEL'``, ...). Those magic strings are easy to get wrong and a typo silently
mis-dispatches deep in the Fortran. These enums give the schemes typed,
validated names while keeping the legacy 4-char code as the value passed across
the boundary (``.fortran_name``).

Each member is named exactly after its legacy 4-char code, so ``fortran_name``
is just the member name — there is no second mapping to keep in sync.

SYNC CONTRACT: the integer values mirror the ``PHYEX_*`` parameters in
``cmake/bridge/phyex_bridge.F90`` (module ``phyex_bridge_enums``). If you add or
renumber a scheme, change it in both places in the same commit.
"""
from __future__ import annotations

from enum import IntEnum

__all__ = [
    "MicroScheme",
    "ShallowConvScheme",
    "TurbScheme",
    "normalize_micro",
    "normalize_sconv",
    "normalize_turb",
]


class _SchemeEnum(IntEnum):
    """An IntEnum whose member name *is* the legacy 4-character Fortran code."""

    def __str__(self) -> str:
        # IntEnum.__str__ prints the integer on Python 3.11+; show the scheme
        # code instead (these are selectors, readability matters).
        return self.name

    @property
    def fortran_name(self) -> str:
        """The legacy 4-char code passed to INI_PHYEX (e.g. ``'ICE3'``)."""
        return self.name

    @classmethod
    def from_value(cls, value):
        """Coerce an enum / int id / legacy 4-char string to a member.

        Raises ValueError on anything unknown, so a bad scheme is rejected at the
        boundary with a clear message instead of mis-dispatching in Fortran.
        """
        if isinstance(value, cls):
            return value
        if isinstance(value, str):
            key = value.strip().upper()
            try:
                return cls[key]
            except KeyError:
                raise ValueError(
                    "{}: unknown scheme {!r}; valid names: {}".format(
                        cls.__name__, value, ", ".join(m.name for m in cls))
                )
        if isinstance(value, int) and not isinstance(value, bool):
            try:
                return cls(value)
            except ValueError:
                raise ValueError(
                    "{}: unknown scheme id {!r}; valid ids: {}".format(
                        cls.__name__, value,
                        ", ".join("{}={}".format(m.name, int(m)) for m in cls))
                )
        raise TypeError(
            "{}: expected {} / int / str, got {}".format(
                cls.__name__, cls.__name__, type(value).__name__)
        )


class MicroScheme(_SchemeEnum):
    """Microphysics / cloud scheme (INI_PHYEX ``CMICRO``)."""

    NONE = 0
    ICE3 = 1
    ICE4 = 2
    LIMA = 3


class ShallowConvScheme(_SchemeEnum):
    """Shallow convection scheme (INI_PHYEX ``CSCONV``)."""

    NONE = 0
    EDKF = 1


class TurbScheme(_SchemeEnum):
    """Turbulence scheme (INI_PHYEX ``CTURB``)."""

    NONE = 0
    TKEL = 1


def normalize_micro(value) -> MicroScheme:
    """Coerce an enum / id / legacy string to a :class:`MicroScheme`."""
    return MicroScheme.from_value(value)


def normalize_sconv(value) -> ShallowConvScheme:
    """Coerce an enum / id / legacy string to a :class:`ShallowConvScheme`."""
    return ShallowConvScheme.from_value(value)


def normalize_turb(value) -> TurbScheme:
    """Coerce an enum / id / legacy string to a :class:`TurbScheme`."""
    return TurbScheme.from_value(value)
