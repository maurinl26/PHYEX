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

    Values are uniform and physically plausible (warm, slightly moist column);
    the goal is a deterministic, finite result, not a realistic profile.
    """
    n2 = (NLON, NLEV)
    return {
        "timestep": 50.0,
        "krr": 6,
        "sigqsat": _f32((NLON,), 0.02),
        "pabs": _f32(n2, 85000.0),
        "sigs": _f32(n2, 0.0),
        "th": _f32(n2, 290.0),
        "exn": _f32(n2, 0.95),
        "exn_ref": _f32(n2, 0.95),
        "rho_dry_ref": _f32(n2, 1.0),
        "rv": _f32(n2, 0.01),
        "rc": _f32(n2, 0.0),
        "ri": _f32(n2, 0.0),
        "rr": _f32(n2, 0.0),
        "rs": _f32(n2, 0.0),
        "rg": _f32(n2, 0.0),
        "cf_mf": _f32(n2, 0.0),
        "rc_mf": _f32(n2, 0.0),
        "ri_mf": _f32(n2, 0.0),
        "rvs": _f32(n2, 0.0),
        "rcs": _f32(n2, 0.0),
        "ris": _f32(n2, 0.0),
        "ths": _f32(n2, 0.0),
        "cldfr": _f32(n2, 0.0),
        "icldfr": _f32(n2, 0.0),
        "wcldfr": _f32(n2, 0.0),
    }
