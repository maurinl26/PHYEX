"""Generate the INDEPENDENT golden reference for SHALLOW_CONVECTION.

Drives a standalone Fortran oracle (tests/oracle/oracle_shallow_convection.F90)
that calls SHALLOW_CONVECTION *directly* (not the bridge). Uses a conditionally-
unstable, moist column that actually *triggers* Kain-Fritsch shallow convection,
so the golden is non-trivial (non-zero tendencies / mass flux / cloud indices)
rather than a vacuous all-zeros reference.

Build the oracle first (not part of the wheel):

    cmake -S . -B build/oracle -G Ninja -DCMAKE_Fortran_COMPILER=gfortran \
        -DPHYEX_USE_TRANSFORMED_SOURCES=ON -DENABLE_DOUBLE_PRECISION=ON \
        -DENABLE_PHYEX_BUILD_ORACLES=ON
    cmake --build build/oracle --target oracle_shallow_convection_dp

Then:

    python tests/golden/gen_golden_shallow_convection.py [path/to/exe]

Writes tests/data/shallow_convection_golden.npz (inputs + oracle outputs).
"""
import os
import subprocess
import sys
import tempfile

import numpy as np

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
DEFAULT_ORACLE = os.path.join(REPO, "build", "oracle", "bin", "oracle_shallow_convection_dp.exe")

NLON, NLEV, KCH1 = 16, 40, 1
KICE, KBDIA, KTDIA = 1, 1, 1
OSETTADJ, OCH1CONV = 0, 0
PTADJS = 3600.0

ARRAYS_2D = ["ppabst", "pzz", "ptt", "prvt", "prct", "prit", "pwt",
             "ptten", "prvten", "prcten", "priten", "pumf"]
OUT_2D = ["ptten", "prvten", "prcten", "priten", "pumf"]


def _f(shape, value):
    return np.full(shape, value, dtype=np.float64, order="F")


def build_inputs():
    """A column tuned to TRIGGER Kain-Fritsch shallow convection (level 1 = ground)."""
    n2 = (NLON, NLEV)
    dz = 100.0
    z = np.arange(NLEV, dtype=np.float64) * dz
    pabs = 1.0e5 * np.exp(-z / 8000.0)
    temp = 303.0 - 9.8e-3 * z                       # steep, conditionally unstable
    rv = np.maximum(0.016 - 1.2e-6 * z, 5.0e-4)     # moist boundary layer

    def col(profile):
        a = np.empty(n2, dtype=np.float64, order="F")
        a[:] = profile
        return a

    a = dict(
        ptkecls=_f((NLON,), 0.5),
        ppabst=col(pabs), pzz=col(z), ptt=col(temp), prvt=col(rv),
        prct=_f(n2, 0.0), prit=_f(n2, 0.0), pwt=_f(n2, 1.0),
        ptten=_f(n2, 0.0), prvten=_f(n2, 0.0), prcten=_f(n2, 0.0),
        priten=_f(n2, 0.0), pumf=_f(n2, 0.0),
        kcltop=np.zeros(NLON, dtype=np.int32, order="F"),
        kclbas=np.zeros(NLON, dtype=np.int32, order="F"),
        pch1=np.zeros((NLON, NLEV, KCH1), dtype=np.float64, order="F"),
        pch1ten=np.zeros((NLON, NLEV, KCH1), dtype=np.float64, order="F"),
    )
    return a


def _w(fh, arr):
    fh.write(np.asfortranarray(arr).astype("<f8").tobytes(order="F"))


def main():
    oracle = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_ORACLE
    if not os.path.exists(oracle):
        sys.exit(f"oracle binary not found: {oracle}\nBuild it first (see this file's docstring).")

    a = build_inputs()
    with tempfile.TemporaryDirectory() as tmp:
        inp, out = os.path.join(tmp, "in.bin"), os.path.join(tmp, "out.bin")
        with open(inp, "wb") as fh:
            fh.write(np.array([NLON, NLEV, KICE, KBDIA, KTDIA, OSETTADJ, OCH1CONV, KCH1],
                              dtype="<i4").tobytes())
            fh.write(np.array([PTADJS], dtype="<f8").tobytes())
            _w(fh, a["ptkecls"])
            for n in ARRAYS_2D:
                _w(fh, a[n])
            fh.write(np.asfortranarray(a["kcltop"]).astype("<i4").tobytes(order="F"))
            fh.write(np.asfortranarray(a["kclbas"]).astype("<i4").tobytes(order="F"))
            _w(fh, a["pch1"])
            _w(fh, a["pch1ten"])

        subprocess.run([oracle, inp, out], check=True)

        with open(out, "rb") as fh:
            buf = fh.read()
        n2 = NLON * NLEV
        golden, off = {}, 0
        for n in OUT_2D:
            golden["out_" + n] = np.frombuffer(buf, dtype="<f8", count=n2, offset=off).reshape(
                (NLON, NLEV), order="F").copy()
            off += n2 * 8
        for n in ("kcltop", "kclbas"):
            golden["out_" + n] = np.frombuffer(buf, dtype="<i4", count=NLON, offset=off).copy()
            off += NLON * 4
        golden["out_pch1ten"] = np.frombuffer(buf, dtype="<f8", count=n2 * KCH1, offset=off).reshape(
            (NLON, NLEV, KCH1), order="F").copy()
        off += n2 * KCH1 * 8
        assert off == len(buf), f"oracle output size mismatch: {off} != {len(buf)}"

    payload = {
        "kice": np.int64(KICE), "kbdia": np.int64(KBDIA), "ktdia": np.int64(KTDIA),
        "osettadj": np.int64(OSETTADJ), "och1conv": np.int64(OCH1CONV),
        "kch1": np.int64(KCH1), "ptadjs": np.float64(PTADJS),
        "ptkecls": a["ptkecls"], "pch1": a["pch1"], "pch1ten": a["pch1ten"],
        "kcltop": a["kcltop"], "kclbas": a["kclbas"],
    }
    payload.update({k: a[k] for k in ARRAYS_2D})
    payload.update(golden)

    dest = os.path.join(REPO, "tests", "data", "shallow_convection_golden.npz")
    np.savez(dest, **payload)
    triggered = golden["out_pumf"].max() > 0.0
    print(f"wrote {dest} ({len(golden)} golden arrays; convection triggered: {triggered})")


if __name__ == "__main__":
    main()
