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


@pytest.fixture
def turb_inputs():
    """A dict of float64 Fortran-ordered arrays for a TURB call.

    Vertical convention (operational path): level 1 = model top, level nlev =
    surface (NKL=-1). A stably-stratified, sheared column with a positive
    surface heat flux, so the turbulence scheme has gradients to mix. TURB *adds*
    its tendencies to the source arrays (prus, prthls, prtkes, ...).
    """
    nlon, nlev = NLON, NLEV

    def _col(profile):
        a = np.empty((nlon, nlev), dtype=np.float64, order="F")
        a[:] = profile
        return a

    k = np.arange(nlev, dtype=np.float64)
    z = (nlev - 1.0 - k) * 150.0                # height: high at top (k=0), 0 at surface
    pabs = 1.0e5 * np.exp(-z / 8000.0)
    temp = 288.0 - 6.5e-3 * z                   # warmer at surface
    exner = (pabs / 1.0e5) ** 0.2857
    theta = temp / exner                        # increases upward (stable)
    rv = np.maximum(0.012 - 1.0e-6 * z, 1.0e-4)

    prt = np.zeros((nlon, nlev, 6), dtype=np.float64, order="F")
    prt[:, :, 0] = rv

    return {
        "ptstep": 50.0,
        "krr": 6,
        "pdxx": _f32((nlon, nlev), 250.0),
        "pdyy": _f32((nlon, nlev), 250.0),
        "pdzz": _f32((nlon, nlev), 150.0),
        "pzz": _col(z),
        "prhodj": _col(pabs / (287.0 * temp)),
        "pthvref": _col(theta),
        "psfth": _f32((nlon,), 0.20),           # K m/s, unstable surface forcing
        "psfrv": _f32((nlon,), 1.0e-4),
        "ppabst": _col(pabs),
        "put": _col(5.0 + 0.002 * z),           # sheared zonal wind
        "pvt": _f32((nlon, nlev), 0.0),
        "pwt": _f32((nlon, nlev), 0.0),
        "ptket": _f32((nlon, nlev), 0.5),
        "pthlt": _col(theta),
        "prt": prt,
        # source terms (modified in place)
        "prus": _f32((nlon, nlev), 0.0),
        "prvs": _f32((nlon, nlev), 0.0),
        "prws": _f32((nlon, nlev), 0.0),
        "prthls": _f32((nlon, nlev), 0.0),
        "prrs": np.zeros((nlon, nlev, 6), dtype=np.float64, order="F"),
        "prtkes": _f32((nlon, nlev), 0.0),
    }


@pytest.fixture
def shallow_convection_inputs():
    """A dict of arrays for a SHALLOW_CONVECTION (Kain-Fritsch) call.

    A warm, moist, conditionally-unstable column. Whether the scheme actually
    triggers is highly sounding-dependent, so the smoke test only relies on the
    call running and returning finite, in-range results — not on convection
    firing. Note kcltop/kclbas are int32 and pch1/pch1ten are 3D (..., kch1);
    kch1=1 with och1conv=False is the "no chemistry" setup.
    """
    nlon, nlev = NLON, NLEV

    def _col(profile):
        a = np.empty((nlon, nlev), dtype=np.float64, order="F")
        a[:] = profile
        return a

    z = np.arange(nlev, dtype=np.float64) * 150.0
    pabs = 1.0e5 * np.exp(-z / 8000.0)
    temp = 301.0 - 9.0e-3 * z                          # ~9 K/km (unstable)
    rv = np.maximum(0.018 - 1.2e-6 * z, 5.0e-4)        # moist below

    return {
        "kice": 1, "kbdia": 1, "ktdia": 1,
        "osettadj": False, "ptadjs": 3600.0,
        "och1conv": False, "kch1": 1,
        "ptkecls": _f32((nlon,), 0.5),
        "ppabst": _col(pabs),
        "pzz": _col(z),
        "ptt": _col(temp),
        "prvt": _col(rv),
        "prct": _f32((nlon, nlev), 0.0),
        "prit": _f32((nlon, nlev), 0.0),
        "pwt": _f32((nlon, nlev), 0.2),
        # in/out tendencies
        "ptten": _f32((nlon, nlev), 0.0),
        "prvten": _f32((nlon, nlev), 0.0),
        "prcten": _f32((nlon, nlev), 0.0),
        "priten": _f32((nlon, nlev), 0.0),
        "kcltop": np.zeros(nlon, dtype=np.int32, order="F"),
        "kclbas": np.zeros(nlon, dtype=np.int32, order="F"),
        "pumf": _f32((nlon, nlev), 0.0),
        # chemical tracers (unused: och1conv=False, kch1=1)
        "pch1": np.zeros((nlon, nlev, 1), dtype=np.float64, order="F"),
        "pch1ten": np.zeros((nlon, nlev, 1), dtype=np.float64, order="F"),
    }
