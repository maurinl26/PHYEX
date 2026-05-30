"""Generate the INDEPENDENT golden reference for ICE_ADJUST.

Unlike tests/gen_reference.py (which snapshots the *binding's* own output), this
drives a standalone Fortran oracle that calls ICE_ADJUST *directly* — it never
touches the Cython/C bridge. The committed npz therefore lets the golden test
prove the binding reproduces a native PHYEX call to round-off.

Build the oracle first (not part of the wheel):

    cmake -S . -B build/oracle -G Ninja -DCMAKE_Fortran_COMPILER=gfortran \
        -DPHYEX_USE_TRANSFORMED_SOURCES=ON -DENABLE_DOUBLE_PRECISION=ON \
        -DENABLE_PHYEX_BUILD_ORACLES=ON
    cmake --build build/oracle --target oracle_ice_adjust_dp

Then:

    python tests/golden/gen_golden_ice_adjust.py [path/to/oracle_ice_adjust_dp.exe]

Writes tests/data/ice_adjust_golden.npz (inputs + oracle outputs).
"""
import os
import subprocess
import sys
import tempfile

import numpy as np

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
DEFAULT_ORACLE = os.path.join(REPO, "build", "oracle", "bin", "oracle_ice_adjust_dp.exe")

NLON, NLEV, KRR = 16, 40, 6

# Order MUST match the READ statements in tests/oracle/oracle_ice_adjust.F90.
ARRAYS_2D = [
    "pabs", "sigs", "th", "exn", "exn_ref", "rho_dry_ref",
    "rv", "rc", "ri", "rr", "rs", "rg",
    "cf_mf", "rc_mf", "ri_mf",
    "rvs", "rcs", "ris", "ths",
    "cldfr", "icldfr", "wcldfr",
]
OUTPUTS = ["cldfr", "icldfr", "wcldfr", "ths", "rvs", "rcs", "ris"]


def _f(shape, value):
    return np.full(shape, value, dtype=np.float64, order="F")


def build_inputs():
    """A supersaturated warm column — the same physics case as the smoke test."""
    n2 = (NLON, NLEV)
    dt = 50.0
    rv, rc, ri, th = _f(n2, 0.02), _f(n2, 0.0), _f(n2, 0.0), _f(n2, 290.0)
    return dt, dict(
        sigqsat=_f((NLON,), 0.02),
        pabs=_f(n2, 85000.0), sigs=_f(n2, 0.0), th=th,
        exn=_f(n2, 0.95), exn_ref=_f(n2, 0.95), rho_dry_ref=_f(n2, 1.0),
        rv=rv, rc=rc, ri=ri,
        rr=_f(n2, 0.0), rs=_f(n2, 0.0), rg=_f(n2, 0.0),
        cf_mf=_f(n2, 0.0), rc_mf=_f(n2, 0.0), ri_mf=_f(n2, 0.0),
        rvs=rv / dt, rcs=rc / dt, ris=ri / dt, ths=th / dt,
        cldfr=_f(n2, 0.0), icldfr=_f(n2, 0.0), wcldfr=_f(n2, 0.0),
    )


def main():
    oracle = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_ORACLE
    if not os.path.exists(oracle):
        sys.exit(f"oracle binary not found: {oracle}\nBuild it first (see this file's docstring).")

    dt, arrs = build_inputs()

    with tempfile.TemporaryDirectory() as tmp:
        inp = os.path.join(tmp, "in.bin")
        out = os.path.join(tmp, "out.bin")
        with open(inp, "wb") as fh:
            fh.write(np.array([NLON, NLEV, KRR], dtype="<i4").tobytes())
            fh.write(np.array([dt], dtype="<f8").tobytes())
            fh.write(np.asfortranarray(arrs["sigqsat"]).astype("<f8").tobytes(order="F"))
            for name in ARRAYS_2D:
                fh.write(np.asfortranarray(arrs[name]).astype("<f8").tobytes(order="F"))

        subprocess.run([oracle, inp, out], check=True)

        raw = np.fromfile(out, dtype="<f8")
        n = NLON * NLEV
        assert raw.size == len(OUTPUTS) * n, f"unexpected oracle output size {raw.size}"
        golden = {}
        for i, name in enumerate(OUTPUTS):
            golden["out_" + name] = np.asfortranarray(
                raw[i * n:(i + 1) * n].reshape((NLON, NLEV), order="F"))

    # Store inputs alongside the golden outputs so the test runs on identical data.
    payload = {"timestep": np.float64(dt), "krr": np.int64(KRR), "sigqsat": arrs["sigqsat"]}
    payload.update({k: arrs[k] for k in ARRAYS_2D})
    payload.update(golden)

    dest = os.path.join(REPO, "tests", "data", "ice_adjust_golden.npz")
    np.savez(dest, **payload)
    print(f"wrote {dest} ({len(golden)} golden arrays from {os.path.basename(oracle)})")


if __name__ == "__main__":
    main()
