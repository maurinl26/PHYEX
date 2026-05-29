#!/usr/bin/env bash
#
# Build and test the PHYEX OpenACC GPU bindings inside an NVHPC container.
# Intended to run on a RunPods GPU pod (RTX 4090/3090, cc89/cc86), driven by
# .github/workflows/gpu-test.yml. Safe to run by hand on any NVHPC + NVIDIA box.
#
#   GPU_ARCH   NVHPC -gpu target (default cc89 = RTX 4090; cc86 = RTX 3090)
#   REPO_DIR   checkout to build (default: current directory)
#
set -euo pipefail

GPU_ARCH="${GPU_ARCH:-cc89}"
REPO_DIR="${REPO_DIR:-$(pwd)}"
cd "$REPO_DIR"

echo "==> Toolchain"
nvfortran --version | head -1
nvidia-smi --query-gpu=name,compute_cap --format=csv,noheader || true
python3 --version

echo "==> Python build/test deps"
python3 -m pip install --upgrade pip
python3 -m pip install "scikit-build-core>=0.10" "cython" "numpy<2.0" pytest
# CuPy supplies the device pointers; match the container's CUDA major version.
python3 -m pip install cupy-cuda12x

echo "==> Build GPU wheel (nvfortran + OpenACC, single precision)"
python3 -m pip install . -v \
  -C cmake.define.CMAKE_Fortran_COMPILER=nvfortran \
  -C cmake.define.PHYEX_ENABLE_GPU=ON \
  -C "cmake.define.PHYEX_GPU_ARCH=${GPU_ARCH}" \
  -C cmake.define.ENABLE_SINGLE_PRECISION=ON \
  -C cmake.define.ENABLE_DOUBLE_PRECISION=OFF

echo "==> Sanity: is this the GPU build?"
python3 -c "import phyex; assert phyex.is_gpu_build(), 'not a GPU build'; print('GPU build OK')"

echo "==> Run GPU correctness tests"
# tests/data/ice_adjust_ref.npz (CPU golden) is optional; if present the
# GPU-vs-CPU agreement test runs, otherwise only finiteness/bounds checks do.
# Run from /tmp so the *installed* extension is imported, not ./phyex source.
TESTS_DIR="$REPO_DIR/tests"
cd /tmp
python3 -m pytest "$TESTS_DIR/test_gpu_ice_adjust.py" -v

echo "==> GPU tests passed"
