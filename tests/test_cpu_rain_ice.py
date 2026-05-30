"""CPU binding smoke test for RAIN_ICE.

RAIN_ICE computes the explicit mixed-phase microphysical sources. This test
exercises the full path — Cython wrapper -> C bridge -> INI_PHYEX config ->
RAIN_ICE — for a warm column carrying cloud water and rain.

Its first job is to prove the call *runs*: the previous bridge declared two
trailing RAIN_ICE_PARAM/DESCR pointer arguments that the Cython wrapper never
passed, so RAIN_ICE dereferenced uninitialised stack pointers and segfaulted.
The config now comes from INI_PHYEX (G_PHYEX), so no pointers are passed and the
routine runs. Beyond "it runs", we check the result is finite and that the
microphysics actually did something (warm rain processes change the state).

Run after building the CPU wheel, from outside the repo root so the *installed*
package is imported rather than the ./phyex source dir:
    pip install . && (cd /tmp && pytest <repo>/tests/test_cpu_rain_ice.py)
"""
import numpy as np
import pytest

phyex = pytest.importorskip("phyex")

pytestmark = pytest.mark.skipif(
    not hasattr(phyex, "rain_ice"),
    reason="CPU binding (phyex.rain_ice) not present in this build",
)


def test_rain_ice_runs_and_is_finite(rain_ice_inputs):
    inp = rain_ice_inputs
    rcs_in = inp["rcs"].copy()
    rrs_in = inp["rrs"].copy()
    rvs_in = inp["rvs"].copy()

    phyex.rain_ice(
        timestep=inp["timestep"], krr=inp["krr"],
        exn=inp["exn"], dzz=inp["dzz"], rhodj=inp["rhodj"], rhodref=inp["rhodref"],
        exnref=inp["exnref"], pabs=inp["pabs"], cldfr=inp["cldfr"], icldfr=inp["icldfr"],
        ssio=inp["ssio"], ssiu=inp["ssiu"], ifr=inp["ifr"],
        tht=inp["tht"], rvt=inp["rvt"], rct=inp["rct"], rrt=inp["rrt"],
        rit=inp["rit"], rst=inp["rst"], rgt=inp["rgt"], sigs=inp["sigs"],
        cit=inp["cit"], hlc_hrc=inp["hlc_hrc"], hlc_hcf=inp["hlc_hcf"],
        hli_hri=inp["hli_hri"], hli_hcf=inp["hli_hcf"],
        ths=inp["ths"], rvs=inp["rvs"], rcs=inp["rcs"], rrs=inp["rrs"],
        ris=inp["ris"], rss=inp["rss"], rgs=inp["rgs"],
        evap3d=inp["evap3d"], rainfr=inp["rainfr"],
        inprc=inp["inprc"], inprr=inp["inprr"], inprs=inp["inprs"],
        inprg=inp["inprg"], indep=inp["indep"],
    )

    # 1. Everything stays finite (no NaN/Inf escaping the scheme).
    for name in ("ths", "rvs", "rcs", "rrs", "ris", "rss", "rgs",
                 "evap3d", "rainfr", "inprc", "inprr", "inprs", "inprg", "indep"):
        arr = inp[name]
        assert np.all(np.isfinite(arr)), f"{name} contains non-finite values"

    # 2. Surface precipitation rates are physical (non-negative).
    for name in ("inprc", "inprr", "inprs", "inprg", "indep"):
        assert np.all(inp[name] >= 0.0), f"{name} has negative precipitation"

    # 3. The microphysics actually ran: warm cloud+rain must exchange mass, so
    #    at least one of the warm-water source arrays must have changed. A silent
    #    no-op (e.g. an uninitialised config) would leave them all untouched.
    changed = (
        not np.allclose(inp["rcs"], rcs_in)
        or not np.allclose(inp["rrs"], rrs_in)
        or not np.allclose(inp["rvs"], rvs_in)
    )
    assert changed, "RAIN_ICE left cloud/rain/vapour sources unchanged"
