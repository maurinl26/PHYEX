"""Generate the independent golden reference for SHALLOW_CONVECTION.

Drives tests/oracle/oracle_shallow_convection.F90 (a direct call, not the bridge)
on a column tuned to actually trigger Kain-Fritsch shallow convection, so the
golden is non-trivial. Freezes tests/data/shallow_convection_golden.npz. Run via
`make goldens`.
"""
import numpy as np

from _oracle import Reader, f8, i4, run_oracle, save_golden

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
    z = np.arange(NLEV, dtype=np.float64) * 100.0
    pabs = 1.0e5 * np.exp(-z / 8000.0)
    temp = 303.0 - 9.8e-3 * z
    rv = np.maximum(0.016 - 1.2e-6 * z, 5.0e-4)

    def col(profile):
        a = np.empty(n2, dtype=np.float64, order="F")
        a[:] = profile
        return a

    return dict(
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


def main():
    a = build_inputs()
    blocks = [
        i4(np.array([NLON, NLEV, KICE, KBDIA, KTDIA, OSETTADJ, OCH1CONV, KCH1])),
        f8(np.array([PTADJS])), f8(a["ptkecls"]),
    ]
    blocks += [f8(a[n]) for n in ARRAYS_2D]
    blocks += [i4(a["kcltop"]), i4(a["kclbas"]), f8(a["pch1"]), f8(a["pch1ten"])]

    out = Reader(run_oracle("shallow_convection", blocks))
    golden = {"out_" + n: out.f8((NLON, NLEV)) for n in OUT_2D}
    golden["out_kcltop"] = out.i4(NLON)
    golden["out_kclbas"] = out.i4(NLON)
    golden["out_pch1ten"] = out.f8((NLON, NLEV, KCH1))
    out.done()

    payload = {
        "kice": np.int64(KICE), "kbdia": np.int64(KBDIA), "ktdia": np.int64(KTDIA),
        "osettadj": np.int64(OSETTADJ), "och1conv": np.int64(OCH1CONV),
        "kch1": np.int64(KCH1), "ptadjs": np.float64(PTADJS),
        "ptkecls": a["ptkecls"], "pch1": a["pch1"], "pch1ten": a["pch1ten"],
        "kcltop": a["kcltop"], "kclbas": a["kclbas"],
    }
    payload.update({k: a[k] for k in ARRAYS_2D})
    payload.update(golden)
    dest = save_golden("shallow_convection", payload)
    print(f"wrote {dest} (convection triggered: {golden['out_pumf'].max() > 0.0})")


if __name__ == "__main__":
    main()
