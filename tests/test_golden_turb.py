"""Golden-reference test for TURB: binding vs a native PHYEX call.

The reference in tests/data/turb_golden.npz was produced by a standalone Fortran
oracle (tests/oracle/oracle_turb.F90) that calls TURB *directly* with its own
halo + config, never through the Cython/C bridge. This runs phyex.turb on the
same inputs and asserts it reproduces the oracle's output tendencies to
round-off.

This is the strongest plumbing check we have, because turb carries the most
binding machinery: the nlev+2 vertical halo (copy-in/out + orientation), the
~25 operational config flags, and the flag-gated OUT-array shapes. The binding
is currently bit-identical to the native call (max abs diff 0.0).

Regenerate after building the oracle (see tests/golden/gen_golden_turb.py).
"""
import os

import numpy as np
import pytest

phyex = pytest.importorskip("phyex")

pytestmark = pytest.mark.skipif(
    not hasattr(phyex, "turb"),
    reason="CPU binding (phyex.turb) not present in this build",
)

GOLDEN = os.path.join(os.path.dirname(__file__), "data", "turb_golden.npz")

ARRAYS_2D = ["pdxx", "pdyy", "pdzz", "pzz", "prhodj", "pthvref",
             "ppabst", "put", "pvt", "pwt", "ptket", "pthlt"]
TEND_2D = ["prus", "prvs", "prws", "prthls", "prtkes"]


@pytest.mark.skipif(not os.path.exists(GOLDEN), reason="golden reference not generated")
def test_turb_matches_native_oracle():
    g = np.load(GOLDEN)

    def F(name):
        return np.array(g[name], dtype=np.float64, order="F")

    a = {n: F(n) for n in (ARRAYS_2D + TEND_2D)}
    prt = F("prt")
    prrs = F("prrs")

    phyex.turb(
        ptstep=float(g["timestep"]), krr=int(g["krr"]),
        pdxx=a["pdxx"], pdyy=a["pdyy"], pdzz=a["pdzz"], pzz=a["pzz"],
        prhodj=a["prhodj"], pthvref=a["pthvref"],
        psfth=F("psfth"), psfrv=F("psfrv"),
        ppabst=a["ppabst"], put=a["put"], pvt=a["pvt"], pwt=a["pwt"],
        ptket=a["ptket"], pthlt=a["pthlt"], prt=prt,
        prus=a["prus"], prvs=a["prvs"], prws=a["prws"],
        prthls=a["prthls"], prrs=prrs, prtkes=a["prtkes"],
    )

    # Sanity: the reference column actually mixed (non-trivial golden).
    assert np.abs(g["out_prtkes"]).max() > 0.0, "golden has no TKE tendency — regenerate"

    for name in TEND_2D:
        np.testing.assert_allclose(
            a[name], g["out_" + name], rtol=1e-10, atol=1e-12,
            err_msg=f"{name} diverges from the native TURB oracle")
    np.testing.assert_allclose(
        prrs, g["out_prrs"], rtol=1e-10, atol=1e-12,
        err_msg="prrs diverges from the native TURB oracle")
