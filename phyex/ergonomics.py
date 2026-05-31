"""Ergonomic wrappers that hide PHYEX's ``*S = R / dt`` source convention.

The low-level routines (e.g. ``phyex.ice_adjust``) follow PHYEX's convention:
the tendency/source arrays (``rvs``, ``rcs``, ``ris``, ``ths`` …) enter as
``R / timestep`` and, on return, hold the adjusted ``R_new / timestep``. Callers
must therefore divide by ``dt`` going in and multiply by ``dt`` coming out — easy
to get wrong. These wrappers take and return plain mixing ratios (and theta) and
do that bookkeeping internally.
"""
import numpy as np


def _ff(a):
    """Coerce to a float64, Fortran-ordered array (what the bridge requires)."""
    return np.asarray(a, dtype=np.float64, order="F")


def ice_adjust_step(timestep, krr, *,
                    sigqsat, pabs, sigs, th, exn, exn_ref, rho_dry_ref,
                    rv, rc, ri, rr, rs, rg, cf_mf, rc_mf, ri_mf,
                    cldfr=None, icldfr=None, wcldfr=None):
    """Saturation adjustment (ICE_ADJUST) on mixing ratios — no ``*S`` convention.

    ``th`` (potential temperature) and ``rv``, ``rc``, ``ri`` (vapour, cloud
    water, cloud ice mixing ratios) are the state *before* adjustment; ``rr``,
    ``rs``, ``rg`` (rain, snow, graupel) and the mass-flux / environment fields
    are passed through. Returns a dict with the adjusted ``th``, ``rv``, ``rc``,
    ``ri`` and the cloud fractions ``cldfr``, ``icldfr``, ``wcldfr`` (allocated if
    not supplied). Inputs are not mutated.

    Equivalent to: set ``ths=th/dt`` … ``ris=ri/dt``, call ``phyex.ice_adjust``,
    then read back ``th = ths*dt`` … ``ri = ris*dt``.
    """
    from . import ice_adjust  # compiled low-level routine

    dt = float(timestep)
    # Sources enter as R / dt.
    ths = _ff(th) / dt
    rvs = _ff(rv) / dt
    rcs = _ff(rc) / dt
    ris = _ff(ri) / dt
    ths, rvs, rcs, ris = (np.asfortranarray(a) for a in (ths, rvs, rcs, ris))

    shape = np.asarray(pabs).shape
    cldfr = _ff(cldfr) if cldfr is not None else np.zeros(shape, dtype=np.float64, order="F")
    icldfr = _ff(icldfr) if icldfr is not None else np.zeros(shape, dtype=np.float64, order="F")
    wcldfr = _ff(wcldfr) if wcldfr is not None else np.zeros(shape, dtype=np.float64, order="F")

    ice_adjust(
        timestep=dt, krr=krr,
        sigqsat=_ff(sigqsat), pabs=_ff(pabs), sigs=_ff(sigs), th=_ff(th),
        exn=_ff(exn), exn_ref=_ff(exn_ref), rho_dry_ref=_ff(rho_dry_ref),
        rv=_ff(rv), rc=_ff(rc), ri=_ff(ri), rr=_ff(rr), rs=_ff(rs), rg=_ff(rg),
        cf_mf=_ff(cf_mf), rc_mf=_ff(rc_mf), ri_mf=_ff(ri_mf),
        rvs=rvs, rcs=rcs, ris=ris, ths=ths,
        cldfr=cldfr, icldfr=icldfr, wcldfr=wcldfr,
    )

    # Read the adjusted mixing ratios back (R = source * dt).
    return {
        "th": np.asfortranarray(ths * dt),
        "rv": np.asfortranarray(rvs * dt),
        "rc": np.asfortranarray(rcs * dt),
        "ri": np.asfortranarray(ris * dt),
        "cldfr": cldfr,
        "icldfr": icldfr,
        "wcldfr": wcldfr,
    }
