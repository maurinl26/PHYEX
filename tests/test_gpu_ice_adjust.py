"""GPU (OpenACC + CuPy) correctness test for ICE_ADJUST.

Runs only when:
  * the GPU flavour of the extension is installed (phyex.IceAdjustGPU), and
  * CuPy + an NVIDIA GPU are available.

It allocates device arrays with CuPy and hands the raw device pointers to the
OpenACC bridge (zero-copy). Results are validated for finiteness/bounds and,
when a CPU golden reference produced by the CPU job is present at
tests/data/ice_adjust_ref.npz, for numerical agreement with the host result.
"""
import os
import numpy as np
import pytest

phyex = pytest.importorskip("phyex")
cp = pytest.importorskip("cupy")

pytestmark = pytest.mark.skipif(
    not hasattr(phyex, "IceAdjustGPU"),
    reason="GPU binding (phyex.IceAdjustGPU) not present in this build",
)

REF = os.path.join(os.path.dirname(__file__), "data", "ice_adjust_ref.npz")


def _to_device(inp):
    """Move every array onto the GPU as float32 (the GPU wrapper's dtype)."""
    out = {}
    for k, v in inp.items():
        out[k] = cp.asarray(v, dtype=cp.float32) if isinstance(v, np.ndarray) else v
    return out


def test_gpu_ice_adjust_finite_and_bounded(ice_adjust_inputs):
    d = _to_device(ice_adjust_inputs)

    gpu = phyex.IceAdjustGPU(krr=d["krr"], timestep=d["timestep"])
    gpu(d["sigqsat"],
        d["pabs"], d["sigs"], d["th"], d["exn"], d["exn_ref"], d["rho_dry_ref"],
        d["rv"], d["rc"], d["ri"], d["rr"], d["rs"], d["rg"],
        d["cf_mf"], d["rc_mf"], d["ri_mf"],
        d["rvs"], d["rcs"], d["ris"], d["ths"],
        d["cldfr"], d["icldfr"], d["wcldfr"])

    cldfr = cp.asnumpy(d["cldfr"])
    assert np.all(np.isfinite(cldfr))
    assert cldfr.min() >= -1e-6 and cldfr.max() <= 1.0 + 1e-6


@pytest.mark.skipif(not os.path.exists(REF),
                    reason="no CPU golden reference (tests/data/ice_adjust_ref.npz)")
def test_gpu_matches_cpu_reference(ice_adjust_inputs):
    ref = np.load(REF)
    d = _to_device(ice_adjust_inputs)

    gpu = phyex.IceAdjustGPU(krr=d["krr"], timestep=d["timestep"])
    gpu(d["sigqsat"],
        d["pabs"], d["sigs"], d["th"], d["exn"], d["exn_ref"], d["rho_dry_ref"],
        d["rv"], d["rc"], d["ri"], d["rr"], d["rs"], d["rg"],
        d["cf_mf"], d["rc_mf"], d["ri_mf"],
        d["rvs"], d["rcs"], d["ris"], d["ths"],
        d["cldfr"], d["icldfr"], d["wcldfr"])

    # float32 physics: loose but meaningful tolerance.
    for name in ("cldfr", "ths", "rvs", "rcs", "ris"):
        got = cp.asnumpy(d[name])
        np.testing.assert_allclose(got, ref[name], rtol=1e-3, atol=1e-5,
                                   err_msg=f"GPU {name} disagrees with CPU reference")
