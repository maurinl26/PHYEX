# PHYEX Python bindings -- build & test orchestration.
#
#   make wheel      build & install the CPU binding wheel
#   make oracles    build the standalone golden-reference oracle drivers
#   make goldens    regenerate the golden references from the oracles
#   make test       run the CPU test suite against the installed extension
#   make refdata    download the CNRM testprogs reference dataset
#   make all        wheel + oracles + goldens + test

PY      ?= python
ORACLES := ice_adjust rain_ice turb shallow_convection
ODIR    := build/oracle
TMP     := $(or $(TMPDIR),/tmp)

.PHONY: all wheel oracles goldens test refdata clean help

help:  ## Show this help
	@grep -hE '^[a-z-]+:.*##' $(MAKEFILE_LIST) | sed -E 's/:.*## /\t/' | sort

all: wheel oracles goldens test  ## Full local pipeline

wheel:  ## Build & install the CPU binding wheel
	$(PY) -m pip install . -v

oracles:  ## Build the standalone golden-reference oracle drivers (not in the wheel)
	cmake -S . -B $(ODIR) -G Ninja \
	  -DCMAKE_Fortran_COMPILER=gfortran -DPHYEX_USE_TRANSFORMED_SOURCES=ON \
	  -DENABLE_DOUBLE_PRECISION=ON -DENABLE_PHYEX_BUILD_ORACLES=ON \
	  -DCMAKE_BUILD_TYPE=Release
	@for r in $(ORACLES); do cmake --build $(ODIR) --target oracle_$${r}_dp || exit 1; done

goldens:  ## Regenerate the golden references from the oracles
	@for r in $(ORACLES); do $(PY) tests/golden/gen_golden_$${r}.py || exit 1; done

test:  ## Run the CPU test suite (from a clean cwd, so the installed extension is used)
	cd $(TMP) && $(PY) -m pytest $(CURDIR)/tests -v

refdata:  ## Download the CNRM testprogs reference dataset
	$(PY) tests/refdata/fetch.py

clean:  ## Remove the oracle build tree
	rm -rf $(ODIR)
