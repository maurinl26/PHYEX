"""Python bindings for the PHYEX atmospheric physics package (AROME).

The compiled extension ``phyex._phyex_wrapper`` is produced from one of two
Cython sources, selected at build time by the CMake option ``PHYEX_ENABLE_GPU``:

* CPU build  -> host wrapper exposing ``ice_adjust``, ``rain_ice``,
  ``shallow_convection``, ``turb`` (NumPy arrays). Each routine initializes
  the full PHYEX configuration internally via ``INI_PHYEX`` on first use.
* GPU build  -> OpenACC wrapper exposing ``IceAdjustGPU`` / ``RainIceGPU``
  (CuPy device arrays, zero-copy device pointers).

Both flavours install under the same module name, so we expose whatever the
installed build provides.
"""

__version__ = "0.1.0"

# Typed scheme selectors (avoid PHYEX's 4-char magic strings at the boundary).
from .enums import (  # noqa: E402
    MicroScheme,
    ShallowConvScheme,
    TurbScheme,
    normalize_micro,
    normalize_sconv,
    normalize_turb,
)

__all__ = [
    "__version__",
    "MicroScheme",
    "ShallowConvScheme",
    "TurbScheme",
    "normalize_micro",
    "normalize_sconv",
    "normalize_turb",
    "configure",
    "active_micro_scheme",
]

try:
    from . import _phyex_wrapper as _w
except ImportError as exc:  # pragma: no cover - import guard
    import warnings

    warnings.warn(f"PHYEX compiled extension not available: {exc}")
    _w = None

if _w is not None:
    # CPU (host) API
    for _name in ("ice_adjust", "rain_ice",
                  "shallow_convection", "turb"):
        if hasattr(_w, _name):
            globals()[_name] = getattr(_w, _name)
            __all__.append(_name)

    # GPU (OpenACC + CuPy) API
    for _name in ("IceAdjustGPU", "RainIceGPU"):
        if hasattr(_w, _name):
            globals()[_name] = getattr(_w, _name)
            __all__.append(_name)

    # Ergonomic wrappers (hide the *S = R/dt source convention). Available only
    # when the CPU routines are present.
    if "ice_adjust" in __all__:
        from .ergonomics import ice_adjust_step
        __all__.append("ice_adjust_step")


def configure(micro="ICE3", sconv="NONE", turb="TKEL", timestep=1.0):
    """Select the PHYEX schemes for this process.

    PHYEX (via ``INI_PHYEX``) allocates module-global state, so the scheme is
    fixed for the life of the process: the first configuration wins. Call this
    before the first routine call to choose the microphysics/shallow-convection/
    turbulence schemes; otherwise the routines default to ICE3/NONE/TKEL on first
    use.

    Calling it again with the same micro scheme is a no-op; requesting a
    different one raises RuntimeError (start a new process to switch). ``micro``,
    ``sconv`` and ``turb`` accept a scheme enum, its integer id, or the legacy
    4-character code. ``timestep`` only affects init-time precomputation
    (sedimentation splitting); the per-call timestep is what drives the physics.

    Returns the active :class:`MicroScheme`.
    """
    if _w is None or not hasattr(_w, "_configure"):
        raise RuntimeError("PHYEX compiled CPU extension not available")
    m = normalize_micro(micro)
    if m is not MicroScheme.ICE3:
        raise NotImplementedError(
            f"micro scheme {m} is not wired in these bindings yet "
            "(only ICE3 is supported)")
    s = normalize_sconv(sconv)
    t = normalize_turb(turb)
    active = _w.active_micro_scheme()
    if active != -1 and active != int(m):
        raise RuntimeError(
            f"PHYEX is already initialized with micro scheme id {active}; cannot "
            f"switch to {m} (id {int(m)}). The scheme is fixed once per process.")
    _w._configure(float(timestep), int(m), int(s), int(t))
    return m


def active_micro_scheme():
    """Return the active :class:`MicroScheme`, or ``None`` if not yet initialized."""
    if _w is None or not hasattr(_w, "active_micro_scheme"):
        return None
    _id = _w.active_micro_scheme()
    return None if _id == -1 else MicroScheme(_id)


def is_gpu_build() -> bool:
    """True if the installed extension is the OpenACC/CuPy GPU build."""
    return _w is not None and hasattr(_w, "IceAdjustGPU")
