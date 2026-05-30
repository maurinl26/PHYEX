"""Tests for process-level scheme selection (configure() / scheme=).

The PHYEX scheme is fixed once per process, so these tests don't assume a clean
uninitialized state (other tests may have already locked it to ICE3). They check
that ICE3 is accepted everywhere and that unsupported schemes are rejected with a
clear error before any Fortran runs.
"""
import numpy as np
import pytest

phyex = pytest.importorskip("phyex")

pytestmark = pytest.mark.skipif(
    not hasattr(phyex, "ice_adjust") or not hasattr(phyex, "configure"),
    reason="CPU bindings with scheme selection not present in this build",
)


def test_configure_ice3_and_active_scheme():
    assert phyex.configure(micro="ICE3") is phyex.MicroScheme.ICE3
    # Idempotent for the same scheme; enum / id / string all accepted.
    assert phyex.configure(micro=phyex.MicroScheme.ICE3) is phyex.MicroScheme.ICE3
    assert phyex.configure(micro=1) is phyex.MicroScheme.ICE3
    assert phyex.active_micro_scheme() is phyex.MicroScheme.ICE3


def test_configure_rejects_unwired_and_unknown_schemes():
    with pytest.raises(NotImplementedError):
        phyex.configure(micro="ICE4")
    with pytest.raises(NotImplementedError):
        phyex.configure(micro=phyex.MicroScheme.LIMA)
    with pytest.raises(ValueError):
        phyex.configure(micro="NOPE")


def test_ice_adjust_accepts_scheme_keyword(ice_adjust_inputs):
    inp = ice_adjust_inputs
    # Explicit scheme="ICE3" runs the same path as the default (scheme=None).
    phyex.ice_adjust(
        timestep=inp["timestep"], krr=inp["krr"],
        sigqsat=inp["sigqsat"], pabs=inp["pabs"], sigs=inp["sigs"], th=inp["th"],
        exn=inp["exn"], exn_ref=inp["exn_ref"], rho_dry_ref=inp["rho_dry_ref"],
        rv=inp["rv"], rc=inp["rc"], ri=inp["ri"], rr=inp["rr"], rs=inp["rs"], rg=inp["rg"],
        cf_mf=inp["cf_mf"], rc_mf=inp["rc_mf"], ri_mf=inp["ri_mf"],
        rvs=inp["rvs"], rcs=inp["rcs"], ris=inp["ris"], ths=inp["ths"],
        cldfr=inp["cldfr"], icldfr=inp["icldfr"], wcldfr=inp["wcldfr"],
        scheme="ICE3",
    )
    assert phyex.active_micro_scheme() is phyex.MicroScheme.ICE3
    assert np.all(np.isfinite(inp["ths"]))


def test_ice_adjust_rejects_unwired_scheme(ice_adjust_inputs):
    inp = ice_adjust_inputs
    # An unsupported scheme must fail before touching the data (no mutation).
    ths_before = inp["ths"].copy()
    with pytest.raises(NotImplementedError):
        phyex.ice_adjust(
            timestep=inp["timestep"], krr=inp["krr"],
            sigqsat=inp["sigqsat"], pabs=inp["pabs"], sigs=inp["sigs"], th=inp["th"],
            exn=inp["exn"], exn_ref=inp["exn_ref"], rho_dry_ref=inp["rho_dry_ref"],
            rv=inp["rv"], rc=inp["rc"], ri=inp["ri"], rr=inp["rr"], rs=inp["rs"], rg=inp["rg"],
            cf_mf=inp["cf_mf"], rc_mf=inp["rc_mf"], ri_mf=inp["ri_mf"],
            rvs=inp["rvs"], rcs=inp["rcs"], ris=inp["ris"], ths=inp["ths"],
            cldfr=inp["cldfr"], icldfr=inp["icldfr"], wcldfr=inp["wcldfr"],
            scheme="ICE4",
        )
    assert np.array_equal(inp["ths"], ths_before)
