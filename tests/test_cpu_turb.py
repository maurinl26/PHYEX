"""CPU binding test for TURB (operational path).

TURB computes turbulent source terms for momentum, heat, moisture and TKE. This
exercises the full path — Cython wrapper -> C bridge -> INI_PHYEX config ->
2-level vertical halo -> TURB — for a stably-stratified, sheared, surface-forced
column.

History: the original hand-rolled wrapper produced all-NaN, because it passed an
uninitialised global CSTURB and used a no-halo vertical grid (TURB does
vertical-stencil mixing). The operational wrapper takes its config from
INI_PHYEX (G_PHYEX, valid CSTURB), builds the nlev+2 halo, and sizes the OUT
arrays to match the schemes' MERGE(...) flag-contracts. We check the result is
finite, that turbulence actually mixed, and that nlev<2 is rejected up front.

Run after building the CPU wheel, from outside the repo root:
    pip install . && (cd /tmp && pytest <repo>/tests/test_cpu_turb.py)
"""
import numpy as np
import pytest

phyex = pytest.importorskip("phyex")

pytestmark = pytest.mark.skipif(
    not hasattr(phyex, "turb"),
    reason="CPU binding (phyex.turb) not present in this build",
)


def _call(inp):
    phyex.turb(
        ptstep=inp["ptstep"], krr=inp["krr"],
        pdxx=inp["pdxx"], pdyy=inp["pdyy"], pdzz=inp["pdzz"], pzz=inp["pzz"],
        prhodj=inp["prhodj"], pthvref=inp["pthvref"],
        psfth=inp["psfth"], psfrv=inp["psfrv"],
        ppabst=inp["ppabst"], put=inp["put"], pvt=inp["pvt"], pwt=inp["pwt"],
        ptket=inp["ptket"], pthlt=inp["pthlt"], prt=inp["prt"],
        prus=inp["prus"], prvs=inp["prvs"], prws=inp["prws"],
        prthls=inp["prthls"], prrs=inp["prrs"], prtkes=inp["prtkes"],
    )


def test_turb_runs_finite_and_mixes(turb_inputs):
    inp = turb_inputs
    prthls_in = inp["prthls"].copy()
    prtkes_in = inp["prtkes"].copy()
    prus_in = inp["prus"].copy()

    _call(inp)

    # 1. Everything stays finite — the whole point vs the old all-NaN wrapper.
    for name in ("prus", "prvs", "prws", "prthls", "prrs", "prtkes"):
        assert np.all(np.isfinite(inp[name])), f"{name} contains non-finite values"

    # 2. Turbulence actually mixed: a sheared, stratified, surface-forced column
    #    must produce non-zero momentum / heat / TKE tendencies.
    assert not np.allclose(inp["prus"], prus_in), "no momentum tendency"
    assert not np.allclose(inp["prthls"], prthls_in), "no heat tendency"
    assert not np.allclose(inp["prtkes"], prtkes_in), "no TKE tendency"

    # 3. Tendency magnitudes are physically sane (not blown up).
    assert np.max(np.abs(inp["prtkes"])) < 1.0, "TKE tendency unreasonably large"
    assert np.max(np.abs(inp["prthls"])) < 1.0, "heat tendency unreasonably large"


def test_turb_requires_two_levels(turb_inputs):
    inp = dict(turb_inputs)
    # A single vertical level can't support the stencil + halo: reject up front.
    one = np.ascontiguousarray(inp["pdxx"][:, :1])
    one = np.asfortranarray(one)
    with pytest.raises(ValueError, match="nlev >= 2"):
        phyex.turb(
            ptstep=inp["ptstep"], krr=inp["krr"],
            pdxx=one, pdyy=one, pdzz=one, pzz=one,
            prhodj=one, pthvref=one, psfth=inp["psfth"], psfrv=inp["psfrv"],
            ppabst=one, put=one, pvt=one, pwt=one, ptket=one, pthlt=one,
            prt=np.asfortranarray(inp["prt"][:, :1, :]),
            prus=one.copy(), prvs=one.copy(), prws=one.copy(), prthls=one.copy(),
            prrs=np.asfortranarray(inp["prrs"][:, :1, :]), prtkes=one.copy(),
        )
