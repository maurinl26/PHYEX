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

__all__ = ["__version__"]

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


def is_gpu_build() -> bool:
    """True if the installed extension is the OpenACC/CuPy GPU build."""
    return _w is not None and hasattr(_w, "IceAdjustGPU")
