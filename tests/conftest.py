"""Shared fixtures: synthetic but physically-plausible columns for ICE_ADJUST."""
import numpy as np
import pytest

NLON, NLEV = 16, 40


def _f32(shape, value):
    # CPU bindings are double precision (parkind_dp); the GPU test casts to
    # float32 itself. Keep the fixture float64 to match the host wrapper.
    return np.full(shape, value, dtype=np.float64, order="F")


@pytest.fixture
def ice_adjust_inputs():
    """A dict of float64 Fortran-ordered arrays for an ICE_ADJUST call.

    A warm, *supersaturated* column (T≈275 K, rv=20 g/kg ≫ rvsat) so the
    saturation adjustment must condense water and form cloud.

    Important PHYEX convention: the tendency/source arrays (rvs, rcs, ris, ths)
    enter as ``R/timestep`` — ICE_ADJUST clamps condensation to the available
    vapour *source* (``MIN(Pcond, PRVS)``), so passing zeros yields no
    adjustment. The Cython wrapper passes these through verbatim; the caller owns
    the convention.
    """
    n2 = (NLON, NLEV)
    dt = 50.0
    rv = _f32(n2, 0.02)
    rc = _f32(n2, 0.0)
    ri = _f32(n2, 0.0)
    th = _f32(n2, 290.0)
    return {
        "timestep": dt,
        "krr": 6,
        "sigqsat": _f32((NLON,), 0.02),
        "pabs": _f32(n2, 85000.0),
        "sigs": _f32(n2, 0.0),
        "th": th,
        "exn": _f32(n2, 0.95),
        "exn_ref": _f32(n2, 0.95),
        "rho_dry_ref": _f32(n2, 1.0),
        "rv": rv,
        "rc": rc,
        "ri": ri,
        "rr": _f32(n2, 0.0),
        "rs": _f32(n2, 0.0),
        "rg": _f32(n2, 0.0),
        "cf_mf": _f32(n2, 0.0),
        "rc_mf": _f32(n2, 0.0),
        "ri_mf": _f32(n2, 0.0),
        # source arrays = R / timestep
        "rvs": rv / dt,
        "rcs": rc / dt,
        "ris": ri / dt,
        "ths": th / dt,
        "cldfr": _f32(n2, 0.0),
        "icldfr": _f32(n2, 0.0),
        "wcldfr": _f32(n2, 0.0),
    }
