"""Shared plumbing for the golden-reference generators.

Each tests/golden/gen_golden_<routine>.py builds the input column, calls the
matching standalone oracle (tests/oracle/oracle_<routine>.F90) over a stream-
binary file, and freezes tests/data/<routine>_golden.npz. The binary packing,
the oracle lookup and the npz writing are identical across routines and live
here so the generators only describe the physics case.
"""
import os
import subprocess
import tempfile

import numpy as np

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def oracle_path(name):
    """Locate the built oracle binary (override the dir with PHYEX_ORACLE_DIR)."""
    base = os.environ.get("PHYEX_ORACLE_DIR") or os.path.join(REPO, "build", "oracle", "bin")
    return os.path.join(base, f"oracle_{name}_dp.exe")


def f8(arr):
    """Fortran-ordered little-endian float64 bytes (column-major, any rank)."""
    return np.asfortranarray(arr).astype("<f8").tobytes(order="F")


def i4(arr):
    """Fortran-ordered little-endian int32 bytes."""
    return np.asfortranarray(arr).astype("<i4").tobytes(order="F")


def run_oracle(name, blocks):
    """Write `blocks` (a list of bytes) as the oracle input, run it, return output bytes."""
    exe = oracle_path(name)
    if not os.path.exists(exe):
        raise SystemExit(f"oracle not built: {exe}\nRun `make oracles` first.")
    with tempfile.TemporaryDirectory() as tmp:
        inp, out = os.path.join(tmp, "in.bin"), os.path.join(tmp, "out.bin")
        with open(inp, "wb") as fh:
            for b in blocks:
                fh.write(b)
        subprocess.run([exe, inp, out], check=True)
        with open(out, "rb") as fh:
            return fh.read()


class Reader:
    """Sequential reader for an oracle's flat little-endian output buffer."""

    def __init__(self, buf):
        self._buf = buf
        self._off = 0

    def f8(self, shape):
        n = int(np.prod(shape))
        a = np.frombuffer(self._buf, dtype="<f8", count=n, offset=self._off)
        self._off += n * 8
        return np.asfortranarray(a.reshape(shape, order="F")) if len(shape) > 1 else a.copy()

    def i4(self, n):
        a = np.frombuffer(self._buf, dtype="<i4", count=n, offset=self._off).copy()
        self._off += n * 4
        return a

    def done(self):
        assert self._off == len(self._buf), f"oracle output size mismatch: {self._off} != {len(self._buf)}"


def save_golden(name, payload):
    dest = os.path.join(REPO, "tests", "data", f"{name}_golden.npz")
    np.savez(dest, **payload)
    return dest
