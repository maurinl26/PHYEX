"""CPU binding correctness test for ICE_ADJUST.

A supersaturated column must condense water: the saturation adjustment forms
cloud (cldfr -> 1), moves vapour into cloud water (rcs increases), and releases
latent heat (ths increases). This exercises the full path — Cython wrapper ->
C bridge -> INI_PHYEX config -> ICE_ADJUST — and checks physics, not just that
it runs.

Run after building the CPU wheel, from outside the repo root so the *installed*
package is imported rather than the ./phyex source dir:
    pip install . && (cd /tmp && pytest <repo>/tests/test_cpu_ice_adjust.py)
"""
import numpy as np
import pytest

phyex = pytest.importorskip("phyex")

pytestmark = pytest.mark.skipif(
    not hasattr(phyex, "ice_adjust"),
    reason="CPU binding (phyex.ice_adjust) not present in this build",
)


def test_ice_adjust_condenses_supersaturated_column(ice_adjust_inputs):
    inp = ice_adjust_inputs
    ths_in = inp["ths"].copy()     # = th/dt before the call
    rcs_in = inp["rcs"].copy()     # = 0 (no initial cloud water)

    phyex.ice_adjust(
        timestep=inp["timestep"], krr=inp["krr"],
        sigqsat=inp["sigqsat"], pabs=inp["pabs"], sigs=inp["sigs"], th=inp["th"],
        exn=inp["exn"], exn_ref=inp["exn_ref"], rho_dry_ref=inp["rho_dry_ref"],
        rv=inp["rv"], rc=inp["rc"], ri=inp["ri"], rr=inp["rr"], rs=inp["rs"], rg=inp["rg"],
        cf_mf=inp["cf_mf"], rc_mf=inp["rc_mf"], ri_mf=inp["ri_mf"],
        rvs=inp["rvs"], rcs=inp["rcs"], ris=inp["ris"], ths=inp["ths"],
        cldfr=inp["cldfr"], icldfr=inp["icldfr"], wcldfr=inp["wcldfr"],
    )

    # Finite + cloud fraction stays a fraction.
    for name in ("rvs", "rcs", "ris", "ths", "cldfr"):
        assert np.all(np.isfinite(inp[name])), f"{name} has non-finite values"
    assert inp["cldfr"].min() >= -1e-6
    assert inp["cldfr"].max() <= 1.0 + 1e-6

    # Physics: the supersaturated column must form cloud, condense vapour into
    # cloud water, and warm via latent heat release.
    assert inp["cldfr"].max() > 0.5, "no cloud formed in a supersaturated column"
    assert inp["rcs"].max() > rcs_in.max() + 1e-8, "no condensation (rcs did not rise)"
    assert inp["ths"].max() > ths_in.max() + 1e-4, "no latent heating (ths did not rise)"
