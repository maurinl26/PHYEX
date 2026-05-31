"""Generate the independent golden reference for ICE_ADJUST.

Drives tests/oracle/oracle_ice_adjust.F90 (a direct ICE_ADJUST call, not the
bridge) and freezes tests/data/ice_adjust_golden.npz. Run via `make goldens`,
or directly once the oracle is built (`make oracles`):

    python tests/golden/gen_golden_ice_adjust.py
"""
import numpy as np

from _oracle import Reader, f8, i4, run_oracle, save_golden

NLON, NLEV, KRR = 16, 40, 6

# (nlon,nlev) arrays in the order oracle_ice_adjust.F90 reads them.
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
    """A supersaturated warm column — same physics case as the smoke test."""
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
    dt, a = build_inputs()
    blocks = [i4(np.array([NLON, NLEV, KRR])), f8(np.array([dt])), f8(a["sigqsat"])]
    blocks += [f8(a[n]) for n in ARRAYS_2D]

    out = Reader(run_oracle("ice_adjust", blocks))
    golden = {"out_" + n: out.f8((NLON, NLEV)) for n in OUTPUTS}
    out.done()

    payload = {"timestep": np.float64(dt), "krr": np.int64(KRR), "sigqsat": a["sigqsat"]}
    payload.update({k: a[k] for k in ARRAYS_2D})
    payload.update(golden)
    print(f"wrote {save_golden('ice_adjust', payload)}")


if __name__ == "__main__":
    main()
