"""CPU binding smoke / ABI test for ICE_ADJUST.

Validates the *build and call path*: the Cython wrapper accepts float64
Fortran-ordered arrays, the C bridge marshals them, ICE_ADJUST runs without
crashing, and outputs come back finite and bounded.

It does NOT yet validate physics: the bridge does not fully initialize PHYEX's
config derived types (NEB / PARAM_ICE / RAIN_ICE_PARAM), so for these inputs the
adjustment tendencies come back as zeros. Making this a real numerical-
correctness test requires completing config initialization in the bridge — see
the "Known limitations" section of docs/PYTHON_BINDINGS_WALKTHROUGH.md.

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


def test_ice_adjust_runs_and_is_finite(ice_adjust_inputs):
    inp = ice_adjust_inputs
    phyex.ice_adjust(
        timestep=inp["timestep"], krr=inp["krr"],
        sigqsat=inp["sigqsat"], pabs=inp["pabs"], sigs=inp["sigs"], th=inp["th"],
        exn=inp["exn"], exn_ref=inp["exn_ref"], rho_dry_ref=inp["rho_dry_ref"],
        rv=inp["rv"], rc=inp["rc"], ri=inp["ri"], rr=inp["rr"], rs=inp["rs"], rg=inp["rg"],
        cf_mf=inp["cf_mf"], rc_mf=inp["rc_mf"], ri_mf=inp["ri_mf"],
        rvs=inp["rvs"], rcs=inp["rcs"], ris=inp["ris"], ths=inp["ths"],
        cldfr=inp["cldfr"], icldfr=inp["icldfr"], wcldfr=inp["wcldfr"],
    )

    # Outputs are updated in place.
    for name in ("rvs", "rcs", "ris", "ths", "cldfr", "icldfr", "wcldfr"):
        arr = inp[name]
        assert np.all(np.isfinite(arr)), f"{name} contains non-finite values"

    # Cloud fraction must stay a fraction.
    assert inp["cldfr"].min() >= -1e-6
    assert inp["cldfr"].max() <= 1.0 + 1e-6
