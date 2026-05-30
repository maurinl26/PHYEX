"""Generate the INDEPENDENT golden reference for RAIN_ICE.

Drives a standalone Fortran oracle (tests/oracle/oracle_rain_ice.F90) that calls
RAIN_ICE *directly* (not the bridge). The committed npz lets the golden test
prove the binding reproduces a native RAIN_ICE call to round-off.

Build the oracle first (not part of the wheel):

    cmake -S . -B build/oracle -G Ninja -DCMAKE_Fortran_COMPILER=gfortran \
        -DPHYEX_USE_TRANSFORMED_SOURCES=ON -DENABLE_DOUBLE_PRECISION=ON \
        -DENABLE_PHYEX_BUILD_ORACLES=ON
    cmake --build build/oracle --target oracle_rain_ice_dp

Then:

    python tests/golden/gen_golden_rain_ice.py [path/to/oracle_rain_ice_dp.exe]

Writes tests/data/rain_ice_golden.npz (inputs + oracle outputs).
"""
import os
import subprocess
import sys
import tempfile

import numpy as np

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
DEFAULT_ORACLE = os.path.join(REPO, "build", "oracle", "bin", "oracle_rain_ice_dp.exe")

NLON, NLEV, KRR = 16, 40, 6

# (nlon,nlev) arrays, in the oracle's READ order.
GROUP_A = ["exn", "dzz", "rhodj", "rhodref", "exnref", "pabs",
           "cldfr", "icldfr", "ssio", "ssiu", "ifr"]
GROUP_B = ["tht", "rvt", "rct", "rrt", "rit", "rst", "rgt", "sigs"]
GROUP_C = ["cit", "hlc_hrc", "hlc_hcf", "hli_hri", "hli_hcf"]
GROUP_D = ["ths", "rvs", "rcs", "rrs", "ris", "rss", "rgs"]
GROUP_E = ["evap3d", "rainfr"]
ARRAYS_2D = GROUP_A + GROUP_B + GROUP_C + GROUP_D + GROUP_E
ARRAYS_1D = ["inprc", "inprr", "inprs", "inprg", "indep"]

OUT_2D = GROUP_C + GROUP_D + GROUP_E  # modified in place by RAIN_ICE
OUT_1D = ARRAYS_1D


def _f(shape, value):
    return np.full(shape, value, dtype=np.float64, order="F")


def build_inputs():
    """Warm cloud+rain column (same physics case as the rain_ice smoke test)."""
    n2 = (NLON, NLEV)
    dt = 50.0
    tht, rvt, rct, rrt = _f(n2, 295.0), _f(n2, 0.012), _f(n2, 1.0e-3), _f(n2, 2.0e-4)
    a = dict(
        exn=_f(n2, 0.957), dzz=_f(n2, 100.0), rhodj=_f(n2, 1.0), rhodref=_f(n2, 1.0),
        exnref=_f(n2, 0.957), pabs=_f(n2, 85000.0),
        cldfr=_f(n2, 1.0), icldfr=_f(n2, 0.0), ssio=_f(n2, 0.0), ssiu=_f(n2, 0.0), ifr=_f(n2, 0.0),
        tht=tht, rvt=rvt, rct=rct, rrt=rrt, rit=_f(n2, 0.0), rst=_f(n2, 0.0), rgt=_f(n2, 0.0),
        sigs=_f(n2, 0.0),
        cit=_f(n2, 0.0), hlc_hrc=_f(n2, 0.0), hlc_hcf=_f(n2, 0.0), hli_hri=_f(n2, 0.0), hli_hcf=_f(n2, 0.0),
        ths=tht / dt, rvs=rvt / dt, rcs=rct / dt, rrs=rrt / dt,
        ris=_f(n2, 0.0), rss=_f(n2, 0.0), rgs=_f(n2, 0.0),
        evap3d=_f(n2, 0.0), rainfr=_f(n2, 0.0),
        inprc=_f((NLON,), 0.0), inprr=_f((NLON,), 0.0), inprs=_f((NLON,), 0.0),
        inprg=_f((NLON,), 0.0), indep=_f((NLON,), 0.0),
    )
    return dt, a


def _w(fh, arr):
    fh.write(np.asfortranarray(arr).astype("<f8").tobytes(order="F"))


def main():
    oracle = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_ORACLE
    if not os.path.exists(oracle):
        sys.exit(f"oracle binary not found: {oracle}\nBuild it first (see this file's docstring).")

    dt, a = build_inputs()
    with tempfile.TemporaryDirectory() as tmp:
        inp, out = os.path.join(tmp, "in.bin"), os.path.join(tmp, "out.bin")
        with open(inp, "wb") as fh:
            fh.write(np.array([NLON, NLEV, KRR], dtype="<i4").tobytes())
            fh.write(np.array([dt], dtype="<f8").tobytes())
            for n in GROUP_A + GROUP_B + GROUP_C + GROUP_D + GROUP_E:
                _w(fh, a[n])
            for n in ARRAYS_1D:
                _w(fh, a[n])

        subprocess.run([oracle, inp, out], check=True)

        raw = np.fromfile(out, dtype="<f8")
        n2 = NLON * NLEV
        expected = len(OUT_2D) * n2 + len(OUT_1D) * NLON
        assert raw.size == expected, f"unexpected oracle output size {raw.size} != {expected}"
        golden, off = {}, 0
        for n in OUT_2D:
            golden["out_" + n] = np.asfortranarray(raw[off:off + n2].reshape((NLON, NLEV), order="F"))
            off += n2
        for n in OUT_1D:
            golden["out_" + n] = np.asfortranarray(raw[off:off + NLON])
            off += NLON

    payload = {"timestep": np.float64(dt), "krr": np.int64(KRR)}
    payload.update({k: a[k] for k in (ARRAYS_2D + ARRAYS_1D)})
    payload.update(golden)

    dest = os.path.join(REPO, "tests", "data", "rain_ice_golden.npz")
    np.savez(dest, **payload)
    print(f"wrote {dest} ({len(golden)} golden arrays from {os.path.basename(oracle)})")


if __name__ == "__main__":
    main()
