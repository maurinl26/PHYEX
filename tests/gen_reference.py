"""Generate the CPU golden reference consumed by the GPU correctness test.

Run against a CPU build of PHYEX:

    pip install .
    python tests/gen_reference.py

Writes tests/data/ice_adjust_ref.npz with the post-call output arrays.
"""
import os
import sys
import numpy as np

import phyex

if not hasattr(phyex, "ice_adjust"):
    sys.exit("This script needs the CPU build (phyex.ice_adjust).")

NLON, NLEV = 16, 40


def _f32(shape, value):
    # CPU bindings are double precision.
    return np.full(shape, value, dtype=np.float64, order="F")


def main():
    n2 = (NLON, NLEV)
    arrs = dict(
        sigqsat=_f32((NLON,), 0.02),
        pabs=_f32(n2, 85000.0), sigs=_f32(n2, 0.0), th=_f32(n2, 290.0),
        exn=_f32(n2, 0.95), exn_ref=_f32(n2, 0.95), rho_dry_ref=_f32(n2, 1.0),
        rv=_f32(n2, 0.01), rc=_f32(n2, 0.0), ri=_f32(n2, 0.0),
        rr=_f32(n2, 0.0), rs=_f32(n2, 0.0), rg=_f32(n2, 0.0),
        cf_mf=_f32(n2, 0.0), rc_mf=_f32(n2, 0.0), ri_mf=_f32(n2, 0.0),
        rvs=_f32(n2, 0.0), rcs=_f32(n2, 0.0), ris=_f32(n2, 0.0), ths=_f32(n2, 0.0),
        cldfr=_f32(n2, 0.0), icldfr=_f32(n2, 0.0), wcldfr=_f32(n2, 0.0),
    )
    phyex.ice_adjust(timestep=50.0, krr=6, **arrs)

    out = {k: arrs[k] for k in ("cldfr", "icldfr", "wcldfr",
                                "ths", "rvs", "rcs", "ris")}
    dest = os.path.join(os.path.dirname(__file__), "data")
    os.makedirs(dest, exist_ok=True)
    path = os.path.join(dest, "ice_adjust_ref.npz")
    np.savez(path, **out)
    print(f"wrote {path}")


if __name__ == "__main__":
    main()
