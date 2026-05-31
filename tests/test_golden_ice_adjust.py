"""Golden-reference test for ICE_ADJUST: binding vs a native PHYEX call.

The reference in tests/data/ice_adjust_golden.npz was produced by a standalone
Fortran oracle (tests/oracle/oracle_ice_adjust.F90) that calls ICE_ADJUST
*directly*, never going through the Cython/C bridge. This test runs
phyex.ice_adjust on the *same* inputs and asserts it reproduces the oracle's
outputs to round-off.

What it proves: the binding's plumbing — pointer mapping, array order /
contiguity, in-place semantics, krr/dimension handling, and the INI_PHYEX config
— does not alter the physics relative to a native call. (It does NOT
independently validate the physics itself; the oracle and the binding share the
same ICE_ADJUST + config by construction.)

Regenerate the reference after building the oracle (see
tests/golden/gen_golden_ice_adjust.py).
"""
import os

import numpy as np
import pytest

phyex = pytest.importorskip("phyex")

pytestmark = pytest.mark.skipif(
    not hasattr(phyex, "ice_adjust"),
    reason="CPU binding (phyex.ice_adjust) not present in this build",
)

GOLDEN = os.path.join(os.path.dirname(__file__), "data", "ice_adjust_golden.npz")

ARRAYS_2D = [
    "pabs", "sigs", "th", "exn", "exn_ref", "rho_dry_ref",
    "rv", "rc", "ri", "rr", "rs", "rg",
    "cf_mf", "rc_mf", "ri_mf",
    "rvs", "rcs", "ris", "ths",
    "cldfr", "icldfr", "wcldfr",
]
OUTPUTS = ["cldfr", "icldfr", "wcldfr", "ths", "rvs", "rcs", "ris"]


@pytest.mark.skipif(not os.path.exists(GOLDEN), reason="golden reference not generated")
def test_ice_adjust_matches_native_oracle():
    g = np.load(GOLDEN)
    a = {name: np.array(g[name], dtype=np.float64, order="F") for name in ARRAYS_2D}
    sigqsat = np.array(g["sigqsat"], dtype=np.float64, order="F")

    phyex.ice_adjust(
        timestep=float(g["timestep"]), krr=int(g["krr"]),
        sigqsat=sigqsat, pabs=a["pabs"], sigs=a["sigs"], th=a["th"],
        exn=a["exn"], exn_ref=a["exn_ref"], rho_dry_ref=a["rho_dry_ref"],
        rv=a["rv"], rc=a["rc"], ri=a["ri"], rr=a["rr"], rs=a["rs"], rg=a["rg"],
        cf_mf=a["cf_mf"], rc_mf=a["rc_mf"], ri_mf=a["ri_mf"],
        rvs=a["rvs"], rcs=a["rcs"], ris=a["ris"], ths=a["ths"],
        cldfr=a["cldfr"], icldfr=a["icldfr"], wcldfr=a["wcldfr"],
    )

    # Sanity: the reference is a real saturation-adjustment case, not all-zeros
    # (so an accidentally-empty golden can't make this pass trivially).
    assert g["out_cldfr"].max() > 0.5, "golden has no cloud — regenerate it"
    assert g["out_rcs"].max() > 0.0, "golden has no condensation — regenerate it"

    # Binding must match the native call. It is bit-identical with matching build
    # flags; the tolerance only absorbs cross-compiler last-bit differences and is
    # far below any real plumbing bug (which would diverge by O(1e-3) or more).
    for name in OUTPUTS:
        np.testing.assert_allclose(
            a[name], g["out_" + name], rtol=1e-10, atol=1e-12,
            err_msg=f"{name} diverges from the native ICE_ADJUST oracle")
