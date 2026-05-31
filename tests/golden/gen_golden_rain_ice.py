"""Generate the independent golden reference for RAIN_ICE.

Drives tests/oracle/oracle_rain_ice.F90 (a direct RAIN_ICE call, not the bridge)
and freezes tests/data/rain_ice_golden.npz. Run via `make goldens`.
"""
import numpy as np

from _oracle import Reader, f8, i4, run_oracle, save_golden

NLON, NLEV, KRR = 16, 40, 6

# (nlon,nlev) arrays in the order oracle_rain_ice.F90 reads them.
ARRAYS_2D = [
    "exn", "dzz", "rhodj", "rhodref", "exnref", "pabs",
    "cldfr", "icldfr", "ssio", "ssiu", "ifr",
    "tht", "rvt", "rct", "rrt", "rit", "rst", "rgt", "sigs",
    "cit", "hlc_hrc", "hlc_hcf", "hli_hri", "hli_hcf",
    "ths", "rvs", "rcs", "rrs", "ris", "rss", "rgs",
    "evap3d", "rainfr",
]
ARRAYS_1D = ["inprc", "inprr", "inprs", "inprg", "indep"]
OUT_2D = ["cit", "hlc_hrc", "hlc_hcf", "hli_hri", "hli_hcf",
          "ths", "rvs", "rcs", "rrs", "ris", "rss", "rgs", "evap3d", "rainfr"]
OUT_1D = ARRAYS_1D


def _f(shape, value):
    return np.full(shape, value, dtype=np.float64, order="F")


def build_inputs():
    """Warm cloud+rain column — same physics case as the rain_ice smoke test."""
    n2 = (NLON, NLEV)
    dt = 50.0
    tht, rvt, rct, rrt = _f(n2, 295.0), _f(n2, 0.012), _f(n2, 1.0e-3), _f(n2, 2.0e-4)
    return dt, dict(
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


def main():
    dt, a = build_inputs()
    blocks = [i4(np.array([NLON, NLEV, KRR])), f8(np.array([dt]))]
    blocks += [f8(a[n]) for n in ARRAYS_2D]
    blocks += [f8(a[n]) for n in ARRAYS_1D]

    out = Reader(run_oracle("rain_ice", blocks))
    golden = {"out_" + n: out.f8((NLON, NLEV)) for n in OUT_2D}
    golden.update({"out_" + n: out.f8((NLON,)) for n in OUT_1D})
    out.done()

    payload = {"timestep": np.float64(dt), "krr": np.int64(KRR)}
    payload.update({k: a[k] for k in (ARRAYS_2D + ARRAYS_1D)})
    payload.update(golden)
    print(f"wrote {save_golden('rain_ice', payload)}")


if __name__ == "__main__":
    main()
