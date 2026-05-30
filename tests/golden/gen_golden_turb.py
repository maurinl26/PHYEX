"""Generate the INDEPENDENT golden reference for TURB.

Drives a standalone Fortran oracle (tests/oracle/oracle_turb.F90) that calls TURB
*directly* (its own halo + config, not the bridge). The committed npz lets the
golden test prove the binding reproduces a native TURB call to round-off — which
for turb covers the most plumbing (the nlev+2 vertical halo copy-in/out, the
~25 config flags, the flag-consistent OUT-array shapes).

Build the oracle first (not part of the wheel):

    cmake -S . -B build/oracle -G Ninja -DCMAKE_Fortran_COMPILER=gfortran \
        -DPHYEX_USE_TRANSFORMED_SOURCES=ON -DENABLE_DOUBLE_PRECISION=ON \
        -DENABLE_PHYEX_BUILD_ORACLES=ON
    cmake --build build/oracle --target oracle_turb_dp

Then:

    python tests/golden/gen_golden_turb.py [path/to/oracle_turb_dp.exe]

Writes tests/data/turb_golden.npz (inputs + oracle output tendencies).
"""
import os
import subprocess
import sys
import tempfile

import numpy as np

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
DEFAULT_ORACLE = os.path.join(REPO, "build", "oracle", "bin", "oracle_turb_dp.exe")

NLON, NLEV, KRR = 16, 40, 6

# (nlon,nlev) inputs in the oracle's READ order.
ARRAYS_2D = ["pdxx", "pdyy", "pdzz", "pzz", "prhodj", "pthvref",
             "ppabst", "put", "pvt", "pwt", "ptket", "pthlt"]
TEND_2D = ["prus", "prvs", "prws", "prthls", "prtkes"]


def _f(shape, value):
    return np.full(shape, value, dtype=np.float64, order="F")


def build_inputs():
    """Same stratified/sheared column as the turb smoke test: level 1 = top."""
    n2 = (NLON, NLEV)
    k = np.arange(NLEV, dtype=np.float64)
    z = (NLEV - 1.0 - k) * 150.0
    pabs = 1.0e5 * np.exp(-z / 8000.0)
    temp = 288.0 - 6.5e-3 * z
    exner = (pabs / 1.0e5) ** 0.2857
    theta = temp / exner
    rv = np.maximum(0.012 - 1.0e-6 * z, 1.0e-4)

    def col(profile):
        a = np.empty(n2, dtype=np.float64, order="F")
        a[:] = profile
        return a

    prt = np.zeros((NLON, NLEV, KRR), dtype=np.float64, order="F")
    prt[:, :, 0] = rv

    arrs = dict(
        psfth=_f((NLON,), 0.20), psfrv=_f((NLON,), 1.0e-4),
        pdxx=_f(n2, 250.0), pdyy=_f(n2, 250.0), pdzz=_f(n2, 150.0), pzz=col(z),
        prhodj=col(pabs / (287.0 * temp)), pthvref=col(theta), ppabst=col(pabs),
        put=col(5.0 + 0.002 * z), pvt=_f(n2, 0.0), pwt=_f(n2, 0.0),
        ptket=_f(n2, 0.5), pthlt=col(theta), prt=prt,
        prus=_f(n2, 0.0), prvs=_f(n2, 0.0), prws=_f(n2, 0.0),
        prthls=_f(n2, 0.0), prtkes=_f(n2, 0.0),
        prrs=np.zeros((NLON, NLEV, KRR), dtype=np.float64, order="F"),
    )
    return 50.0, arrs


def _w(fh, arr):
    fh.write(np.asfortranarray(arr).astype("<f8").tobytes(order="F"))


def main():
    oracle = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_ORACLE
    if not os.path.exists(oracle):
        sys.exit(f"oracle binary not found: {oracle}\nBuild it first (see this file's docstring).")

    dt, a = build_inputs()
    with tempfile.TemporaryDirectory() as tmp:
        inp = os.path.join(tmp, "in.bin")
        out = os.path.join(tmp, "out.bin")
        with open(inp, "wb") as fh:
            fh.write(np.array([NLON, NLEV, KRR], dtype="<i4").tobytes())
            fh.write(np.array([dt], dtype="<f8").tobytes())
            _w(fh, a["psfth"]); _w(fh, a["psfrv"])
            for n in ARRAYS_2D:
                _w(fh, a[n])
            _w(fh, a["prt"])
            for n in TEND_2D:
                _w(fh, a[n])
            _w(fh, a["prrs"])

        subprocess.run([oracle, inp, out], check=True)

        raw = np.fromfile(out, dtype="<f8")
        n2 = NLON * NLEV
        expected = len(TEND_2D) * n2 + n2 * KRR
        assert raw.size == expected, f"unexpected oracle output size {raw.size} != {expected}"
        golden = {}
        off = 0
        for n in TEND_2D:
            golden["out_" + n] = np.asfortranarray(raw[off:off + n2].reshape((NLON, NLEV), order="F"))
            off += n2
        golden["out_prrs"] = np.asfortranarray(raw[off:off + n2 * KRR].reshape((NLON, NLEV, KRR), order="F"))

    payload = {"timestep": np.float64(dt), "krr": np.int64(KRR)}
    payload.update({k: a[k] for k in (["psfth", "psfrv"] + ARRAYS_2D + ["prt"] + TEND_2D + ["prrs"])})
    payload.update(golden)

    dest = os.path.join(REPO, "tests", "data", "turb_golden.npz")
    np.savez(dest, **payload)
    print(f"wrote {dest} ({len(golden)} golden arrays from {os.path.basename(oracle)})")


if __name__ == "__main__":
    main()
