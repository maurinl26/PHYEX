"""Tests for the ergonomic wrappers (phyex.ice_adjust_step).

ice_adjust_step takes/returns plain mixing ratios and hides the *S = R/dt
convention. We check (1) the physics is right on a supersaturated column and the
inputs aren't mutated, and (2) it is bit-identical to driving the low-level
ice_adjust with the convention applied by hand.
"""
import numpy as np
import pytest

phyex = pytest.importorskip("phyex")

pytestmark = pytest.mark.skipif(
    not hasattr(phyex, "ice_adjust_step"),
    reason="ergonomic wrapper not present in this build",
)

NLON, NLEV = 16, 40


def _f(value):
    return np.full((NLON, NLEV), value, dtype=np.float64, order="F")


def _env():
    """Environment fields for a warm, supersaturated column (rv >> rvsat)."""
    return dict(
        sigqsat=np.full((NLON,), 0.02, dtype=np.float64, order="F"),
        pabs=_f(85000.0), sigs=_f(0.0), exn=_f(0.95), exn_ref=_f(0.95),
        rho_dry_ref=_f(1.0), rr=_f(0.0), rs=_f(0.0), rg=_f(0.0),
        cf_mf=_f(0.0), rc_mf=_f(0.0), ri_mf=_f(0.0),
    )


def test_ice_adjust_step_condenses_and_preserves_inputs():
    env = _env()
    th, rv, rc, ri = _f(290.0), _f(0.02), _f(0.0), _f(0.0)
    th0, rv0, rc0 = th.copy(), rv.copy(), rc.copy()

    out = phyex.ice_adjust_step(timestep=50.0, krr=6, th=th, rv=rv, rc=rc, ri=ri, **env)

    # Inputs untouched.
    assert np.array_equal(th, th0) and np.array_equal(rv, rv0) and np.array_equal(rc, rc0)
    # Saturation adjustment: cloud forms, vapour drops, latent heat warms theta.
    assert out["rc"].max() > 0.0, "no cloud water formed"
    assert out["rv"].max() < rv0.max(), "vapour did not decrease"
    assert out["th"].max() > th0.max(), "no latent heating"
    assert out["cldfr"].max() > 0.5, "no cloud fraction"
    for v in out.values():
        assert np.all(np.isfinite(v))


def test_ice_adjust_step_matches_manual_convention():
    env = _env()
    dt = 50.0
    th, rv, rc, ri = _f(290.0), _f(0.02), _f(0.0), _f(0.0)

    out = phyex.ice_adjust_step(timestep=dt, krr=6, th=th, rv=rv, rc=rc, ri=ri, **env)

    # Same case driven through the low-level routine with *S = R/dt by hand.
    ths = np.asfortranarray(th / dt); rvs = np.asfortranarray(rv / dt)
    rcs = np.asfortranarray(rc / dt); ris = np.asfortranarray(ri / dt)
    cldfr, icldfr, wcldfr = _f(0.0), _f(0.0), _f(0.0)
    phyex.ice_adjust(
        timestep=dt, krr=6, th=th.copy(order="F"), rv=rv.copy(order="F"),
        rc=rc.copy(order="F"), ri=ri.copy(order="F"),
        rvs=rvs, rcs=rcs, ris=ris, ths=ths,
        cldfr=cldfr, icldfr=icldfr, wcldfr=wcldfr, **env)

    np.testing.assert_array_equal(out["th"], ths * dt)
    np.testing.assert_array_equal(out["rc"], rcs * dt)
    np.testing.assert_array_equal(out["rv"], rvs * dt)
    np.testing.assert_array_equal(out["cldfr"], cldfr)
