"""Download the CNRM PHYEX testprogs reference dataset.

For each routine, CNRM publishes a tarball of reference columns (input + the
expected output, in the offline testprogs' .dat format) on GitHub. This fetches
and extracts them under tests/refdata/<routine>/, which the reference-validation
harness reads. Idempotent: a routine already present is skipped.

    python tests/refdata/fetch.py            # all routines
    python tests/refdata/fetch.py turb       # one routine
"""
import os
import sys
import tarfile
import tempfile
import urllib.request

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
DEST = os.path.join(REPO, "tests", "refdata")

# Tarballs published by UMR-CNRM/PHYEX (see tools/INSTALL.sh). Each extracts to
# <routine>/<NNNNNNNN>.dat.
SOURCES = {
    "ice_adjust": "https://github.com/UMR-CNRM/PHYEX/files/12783926/ice_adjust.tar.gz",
    "rain_ice":   "https://github.com/UMR-CNRM/PHYEX/files/12783935/rain_ice.tar.gz",
    "shallow":    "https://github.com/UMR-CNRM/PHYEX/files/12783945/shallow.tar.gz",
    "turb":       "https://github.com/UMR-CNRM/PHYEX/files/12783952/turb.tar.gz",
}


def fetch(name, url):
    out_dir = os.path.join(DEST, name)
    if os.path.isdir(out_dir) and any(f.endswith(".dat") for f in os.listdir(out_dir)):
        print(f"{name}: already present ({out_dir})")
        return
    print(f"{name}: downloading {url}")
    with tempfile.NamedTemporaryFile(suffix=".tar.gz", delete=False) as tmp:
        with urllib.request.urlopen(url, timeout=120) as resp:
            tmp.write(resp.read())
        tarball = tmp.name
    try:
        with tarfile.open(tarball) as tar:
            tar.extractall(DEST, filter="data")
    finally:
        os.unlink(tarball)
    n = len([f for f in os.listdir(out_dir) if f.endswith(".dat")])
    print(f"{name}: extracted {n} reference columns to {out_dir}")


def main():
    os.makedirs(DEST, exist_ok=True)
    names = sys.argv[1:] or list(SOURCES)
    for name in names:
        if name not in SOURCES:
            sys.exit(f"unknown routine {name!r}; known: {', '.join(SOURCES)}")
        fetch(name, SOURCES[name])


if __name__ == "__main__":
    main()
