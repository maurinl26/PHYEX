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


@pytest.fixture
def rain_ice_inputs():
    """A dict of float64 Fortran-ordered arrays for a RAIN_ICE call.

    A warm column (T ≈ 282 K, above freezing) carrying both cloud water and
    rain, so the explicit warm-rain microphysics (autoconversion, accretion,
    evaporation, sedimentation) has something to act on. RAIN_ICE *adds* its
    microphysical tendencies to the source arrays (ths, r?s), which enter as
    ``R/timestep`` — same convention as ICE_ADJUST.
    """
    n2 = (NLON, NLEV)
    n1 = (NLON,)
    dt = 50.0
    exn = _f32(n2, 0.957)          # (85000/100000)^(Rd/Cp)
    tht = _f32(n2, 295.0)          # theta -> T = tht*exn ~ 282 K (warm)
    rvt = _f32(n2, 0.012)          # 12 g/kg vapour
    rct = _f32(n2, 1.0e-3)         # 1 g/kg cloud water
    rrt = _f32(n2, 2.0e-4)         # 0.2 g/kg rain
    rit = _f32(n2, 0.0)
    rst = _f32(n2, 0.0)
    rgt = _f32(n2, 0.0)
    return {
        "timestep": dt,
        "krr": 6,
        # atmospheric state
        "exn": exn,
        "dzz": _f32(n2, 100.0),
        "rhodj": _f32(n2, 1.0),
        "rhodref": _f32(n2, 1.0),
        "exnref": _f32(n2, 0.957),
        "pabs": _f32(n2, 85000.0),
        "cldfr": _f32(n2, 1.0),
        "icldfr": _f32(n2, 0.0),
        "ssio": _f32(n2, 0.0),
        "ssiu": _f32(n2, 0.0),
        "ifr": _f32(n2, 0.0),
        # mixing ratios at t
        "tht": tht,
        "rvt": rvt,
        "rct": rct,
        "rrt": rrt,
        "rit": rit,
        "rst": rst,
        "rgt": rgt,
        "sigs": _f32(n2, 0.0),
        # in/out
        "cit": _f32(n2, 0.0),
        "hlc_hrc": _f32(n2, 0.0),
        "hlc_hcf": _f32(n2, 0.0),
        "hli_hri": _f32(n2, 0.0),
        "hli_hcf": _f32(n2, 0.0),
        # source arrays = R / timestep
        "ths": tht / dt,
        "rvs": rvt / dt,
        "rcs": rct / dt,
        "rrs": rrt / dt,
        "ris": rit / dt,
        "rss": rst / dt,
        "rgs": rgt / dt,
        # outputs
        "evap3d": _f32(n2, 0.0),
        "rainfr": _f32(n2, 0.0),
        "inprc": _f32(n1, 0.0),
        "inprr": _f32(n1, 0.0),
        "inprs": _f32(n1, 0.0),
        "inprg": _f32(n1, 0.0),
        "indep": _f32(n1, 0.0),
    }
