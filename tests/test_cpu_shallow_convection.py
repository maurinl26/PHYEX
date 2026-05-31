"""CPU binding smoke + guardrail test for SHALLOW_CONVECTION.

SHALLOW_CONVECTION is the Kain-Fritsch shallow scheme. Its physical constants
now come from the shared INI_PHYEX config (G_PHYEX%CST) via ensure_phyex_init,
like the other routines; its Kain-Fritsch parameters (CONVPAR_SHAL) stay
hand-rolled in the bridge because they are not part of PHYEX_t.

Whether the scheme triggers is highly sounding-dependent, so the first test only
checks the call runs and returns finite, in-range results. The second test
exercises the input guardrails added to the wrapper — bad scalar arguments must
raise a clear ValueError instead of letting Fortran read out of bounds.

Run after building the CPU wheel, from outside the repo root:
    pip install . && (cd /tmp && pytest <repo>/tests/test_cpu_shallow_convection.py)
"""
import numpy as np
import pytest

phyex = pytest.importorskip("phyex")

pytestmark = pytest.mark.skipif(
    not hasattr(phyex, "shallow_convection"),
    reason="CPU binding (phyex.shallow_convection) not present in this build",
)


def _call(inp):
    phyex.shallow_convection(
        kice=inp["kice"], kbdia=inp["kbdia"], ktdia=inp["ktdia"],
        osettadj=inp["osettadj"], ptadjs=inp["ptadjs"],
        och1conv=inp["och1conv"], kch1=inp["kch1"],
        ptkecls=inp["ptkecls"], ppabst=inp["ppabst"], pzz=inp["pzz"],
        ptt=inp["ptt"], prvt=inp["prvt"], prct=inp["prct"], prit=inp["prit"],
        pwt=inp["pwt"],
        ptten=inp["ptten"], prvten=inp["prvten"], prcten=inp["prcten"],
        priten=inp["priten"], kcltop=inp["kcltop"], kclbas=inp["kclbas"],
        pumf=inp["pumf"], pch1=inp["pch1"], pch1ten=inp["pch1ten"],
    )


def test_shallow_convection_runs_and_is_finite(shallow_convection_inputs):
    inp = shallow_convection_inputs
    nlev = inp["ppabst"].shape[1]

    _call(inp)

    # 1. All tendency / flux outputs stay finite.
    for name in ("ptten", "prvten", "prcten", "priten", "pumf"):
        assert np.all(np.isfinite(inp[name])), f"{name} contains non-finite values"

    # 2. Updraft mass flux is physical (non-negative).
    assert np.all(inp["pumf"] >= 0.0), "pumf has negative mass flux"

    # 3. Cloud top/base indices are valid: 0 (no convection) or a level in
    #    [1, nlev]. A garbage config would typically yield out-of-range indices.
    for name in ("kcltop", "kclbas"):
        idx = inp[name]
        assert np.all(idx >= 0) and np.all(idx <= nlev), f"{name} out of range"


def test_shallow_convection_rejects_bad_scalars(shallow_convection_inputs):
    base = shallow_convection_inputs

    # kch1 < 1 would give the tracer arrays a zero last dim (out-of-bounds &pch1).
    with pytest.raises(ValueError, match="kch1"):
        bad = dict(base); bad["kch1"] = 0
        _call(bad)

    # kbdia must be >= 1.
    with pytest.raises(ValueError, match="kbdia"):
        bad = dict(base); bad["kbdia"] = 0
        _call(bad)

    # ktdia must be within [1, nlev].
    nlev = base["ppabst"].shape[1]
    with pytest.raises(ValueError, match="ktdia"):
        bad = dict(base); bad["ktdia"] = nlev + 5
        _call(bad)
