"""Generate the independent golden reference for TURB.

Drives tests/oracle/oracle_turb.F90 (a direct TURB call with its own halo +
config, not the bridge) and freezes tests/data/turb_golden.npz. Run via
`make goldens`.
"""
import numpy as np

from _oracle import Reader, f8, i4, run_oracle, save_golden

NLON, NLEV, KRR = 16, 40, 6

# (nlon,nlev) inputs in oracle_turb.F90's READ order.
ARRAYS_2D = ["pdxx", "pdyy", "pdzz", "pzz", "prhodj", "pthvref",
             "ppabst", "put", "pvt", "pwt", "ptket", "pthlt"]
TEND_2D = ["prus", "prvs", "prws", "prthls", "prtkes"]


def _f(shape, value):
    return np.full(shape, value, dtype=np.float64, order="F")


def build_inputs():
    """Stratified/sheared boundary-layer column; level 1 = top, nlev = surface."""
    n2 = (NLON, NLEV)
    k = np.arange(NLEV, dtype=np.float64)
    z = (NLEV - 1.0 - k) * 150.0
    pabs = 1.0e5 * np.exp(-z / 8000.0)
    temp = 288.0 - 6.5e-3 * z
    theta = temp / (pabs / 1.0e5) ** 0.2857
    rv = np.maximum(0.012 - 1.0e-6 * z, 1.0e-4)

    def col(profile):
        a = np.empty(n2, dtype=np.float64, order="F")
        a[:] = profile
        return a

    prt = np.zeros((NLON, NLEV, KRR), dtype=np.float64, order="F")
    prt[:, :, 0] = rv
    return 50.0, dict(
        psfth=_f((NLON,), 0.20), psfrv=_f((NLON,), 1.0e-4),
        pdxx=_f(n2, 250.0), pdyy=_f(n2, 250.0), pdzz=_f(n2, 150.0), pzz=col(z),
        prhodj=col(pabs / (287.0 * temp)), pthvref=col(theta), ppabst=col(pabs),
        put=col(5.0 + 0.002 * z), pvt=_f(n2, 0.0), pwt=_f(n2, 0.0),
        ptket=_f(n2, 0.5), pthlt=col(theta), prt=prt,
        prus=_f(n2, 0.0), prvs=_f(n2, 0.0), prws=_f(n2, 0.0),
        prthls=_f(n2, 0.0), prtkes=_f(n2, 0.0),
        prrs=np.zeros((NLON, NLEV, KRR), dtype=np.float64, order="F"),
    )


def main():
    dt, a = build_inputs()
    blocks = [i4(np.array([NLON, NLEV, KRR])), f8(np.array([dt])), f8(a["psfth"]), f8(a["psfrv"])]
    blocks += [f8(a[n]) for n in ARRAYS_2D]
    blocks += [f8(a["prt"])]
    blocks += [f8(a[n]) for n in TEND_2D]
    blocks += [f8(a["prrs"])]

    out = Reader(run_oracle("turb", blocks))
    golden = {"out_" + n: out.f8((NLON, NLEV)) for n in TEND_2D}
    golden["out_prrs"] = out.f8((NLON, NLEV, KRR))
    out.done()

    payload = {"timestep": np.float64(dt), "krr": np.int64(KRR)}
    payload.update({k: a[k] for k in (["psfth", "psfrv"] + ARRAYS_2D + ["prt"] + TEND_2D + ["prrs"])})
    payload.update(golden)
    print(f"wrote {save_golden('turb', payload)}")


if __name__ == "__main__":
    main()
