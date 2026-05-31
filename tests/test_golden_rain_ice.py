"""Golden-reference test for RAIN_ICE: binding vs a native PHYEX call.

The reference in tests/data/rain_ice_golden.npz was produced by a standalone
Fortran oracle (tests/oracle/oracle_rain_ice.F90) that calls RAIN_ICE *directly*,
never through the Cython/C bridge. This runs phyex.rain_ice on the same inputs
and asserts it reproduces the oracle's outputs to round-off (currently
bit-identical, max abs diff 0.0).

Regenerate after building the oracle (see tests/golden/gen_golden_rain_ice.py).
"""
import os

import numpy as np
import pytest

phyex = pytest.importorskip("phyex")

pytestmark = pytest.mark.skipif(
    not hasattr(phyex, "rain_ice"),
    reason="CPU binding (phyex.rain_ice) not present in this build",
)

GOLDEN = os.path.join(os.path.dirname(__file__), "data", "rain_ice_golden.npz")

ARRAYS_2D = [
    "exn", "dzz", "rhodj", "rhodref", "exnref", "pabs",
    "cldfr", "icldfr", "ssio", "ssiu", "ifr",
    "tht", "rvt", "rct", "rrt", "rit", "rst", "rgt", "sigs",
    "cit", "hlc_hrc", "hlc_hcf", "hli_hri", "hli_hcf",
    "ths", "rvs", "rcs", "rrs", "ris", "rss", "rgs",
    "evap3d", "rainfr",
]
ARRAYS_1D = ["inprc", "inprr", "inprs", "inprg", "indep"]
OUT_2D = ["cit", "hlc_hrc", "hlc_hcf", "hli_hri", "hli_hcf",
          "ths", "rvs", "rcs", "rrs", "ris", "rss", "rgs", "evap3d", "rainfr"]
OUT_1D = ARRAYS_1D


@pytest.mark.skipif(not os.path.exists(GOLDEN), reason="golden reference not generated")
def test_rain_ice_matches_native_oracle():
    g = np.load(GOLDEN)
    a = {n: np.array(g[n], dtype=np.float64, order="F") for n in (ARRAYS_2D + ARRAYS_1D)}

    phyex.rain_ice(
        timestep=float(g["timestep"]), krr=int(g["krr"]),
        exn=a["exn"], dzz=a["dzz"], rhodj=a["rhodj"], rhodref=a["rhodref"],
        exnref=a["exnref"], pabs=a["pabs"], cldfr=a["cldfr"], icldfr=a["icldfr"],
        ssio=a["ssio"], ssiu=a["ssiu"], ifr=a["ifr"],
        tht=a["tht"], rvt=a["rvt"], rct=a["rct"], rrt=a["rrt"],
        rit=a["rit"], rst=a["rst"], rgt=a["rgt"], sigs=a["sigs"],
        cit=a["cit"], hlc_hrc=a["hlc_hrc"], hlc_hcf=a["hlc_hcf"],
        hli_hri=a["hli_hri"], hli_hcf=a["hli_hcf"],
        ths=a["ths"], rvs=a["rvs"], rcs=a["rcs"], rrs=a["rrs"],
        ris=a["ris"], rss=a["rss"], rgs=a["rgs"],
        evap3d=a["evap3d"], rainfr=a["rainfr"],
        inprc=a["inprc"], inprr=a["inprr"], inprs=a["inprs"],
        inprg=a["inprg"], indep=a["indep"],
    )

    # Sanity: the reference column actually ran microphysics (cloud water moved).
    assert not np.allclose(g["out_rcs"], g["rcs"]), "golden shows no microphysics — regenerate"

    for name in OUT_2D + OUT_1D:
        np.testing.assert_allclose(
            a[name], g["out_" + name], rtol=1e-10, atol=1e-12,
            err_msg=f"{name} diverges from the native RAIN_ICE oracle")
