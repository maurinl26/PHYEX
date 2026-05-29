# PHYEX Python bindings — integration walkthrough

How the Cython (CPU) and OpenACC (GPU) bindings are wired onto **master**, and
how each piece is tested. This is the rebase-onto-master integration: every
change here is **additive** to master's tree, so the fork stays mergeable with
upstream CNRM/PHYEX.

- **Workstream 1** — CMake cleanup + scikit-build-core packaging
- **Workstream 2** — Cython integration, CPU
- **Workstream 3** — Cython integration, GPU (OpenACC + CuPy)

---

## 0. The one thing that shapes everything: source transformation

master's `src/common/**` is **not directly compilable**. It is written in the
PHYEX authoring dialect — `MZM(PRHODJ)` array operators, `mnh_expand`
directives — which `pyfortool` (`tools/prep_code.sh`, pinned in
`requirements.txt`) transforms into plain Fortran (`MZM_PHY` subroutine calls,
explicit loops) before compilation. **62 files** under `src/common` need this.

`pyfortool` depends on `fxtran` (a C parser). It builds on Linux but **fails to
build on macOS**, so the transform can't run on a typical dev Mac.

We therefore support two source roots, switched by one CMake option:

| `PHYEX_USE_TRANSFORMED_SOURCES` | Source root | Needs pyfortool? | Used by |
|---|---|---|---|
| `OFF` (default) | `src/common/` (canonical) | yes (run in CI) | upstream-faithful builds, Linux CI |
| `ON` | `phyex_transformed/` (vendored) | no | local builds, macOS, quick wheels |

`phyex_transformed/` is a committed snapshot of pyfortool's output. **`src/common`
remains the single source of truth**; regenerate the vendored tree from it with
`tools/prep_code.sh` when syncing upstream physics (see §5).

---

## Workstream 1 — CMake + scikit-build-core

master already carried the scaffolding: an `if(DEFINED SKBUILD)` block pointing
at `cmake/bridge/`. What this branch adds:

1. **`pyproject.toml`** — `scikit-build-core` backend, `phyex` 0.1.0, Python
   ≥3.11, `numpy<2.0`. Build-time CMake defines pin `gfortran`, single
   precision, and `PHYEX_USE_TRANSFORMED_SOURCES=ON`.
2. **Source-root switch** in `CMakeLists.txt` (`PHYEX_USE_TRANSFORMED_SOURCES`),
   driving both the source glob and the `.h` include dirs off `PHYEX_SRC_ROOT`.
3. **Precision correctness** — the extension is compiled with `-DSINGLE_PRECISION`
   and the REAL4 flags so the bridge's working precision (`WP`) matches
   `phyex_sp`. Without this the bridge passes `REAL(8)` arrays to `REAL(4)`
   routines (the first real bug this integration hit).
4. **`PHYEX_ENABLE_GPU`** option (Workstream 3) selecting the GPU wrapper and
   adding NVHPC `-acc -gpu=<arch>` flags.

Build the library only (fast, no Python):

```bash
cmake -S . -B build/cpu -G Ninja \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DENABLE_SINGLE_PRECISION=ON -DENABLE_DOUBLE_PRECISION=OFF \
  -DPHYEX_USE_TRANSFORMED_SOURCES=ON
cmake --build build/cpu --target phyex_sp
```

**Test:** the library links to `libphyex_sp.{so,dylib}` (1008 objects). CI:
`.github/workflows/python-cpu.yml`.

---

## Workstream 2 — Cython, CPU

- `cmake/bridge/phyex_bridge.F90` — `ISO_C_BINDING` `BIND(C)` layer over
  `ICE_ADJUST`, `RAIN_ICE`, `SHALLOW_CONVECTION`, `TURB`.
- `cmake/bridge/_phyex_wrapper.pyx` — host wrapper exposing `ice_adjust`,
  `rain_ice`, `init_rain_ice`, `shallow_convection`, `turb` over NumPy
  (float32, Fortran-ordered) arrays.
- `phyex/__init__.py` — exposes whatever the installed extension provides.

Build + install + test:

```bash
pip install .            # PHYEX_USE_TRANSFORMED_SOURCES=ON via pyproject
python -c "import phyex; print(phyex.__all__)"
pytest tests/test_cpu_ice_adjust.py -v
```

**Tests** (`tests/test_cpu_ice_adjust.py`): `ICE_ADJUST` runs on a synthetic
column, outputs are finite, cloud fraction stays in `[0, 1]`.
`tests/gen_reference.py` saves the result as the golden reference the GPU job
checks against.

---

## Workstream 3 — Cython, GPU (OpenACC + CuPy)

- `cmake/bridge/_phyex_wrapper_acc.pyx` — `IceAdjustGPU` / `RainIceGPU`. Takes
  **CuPy** device arrays and passes the raw device pointers (`arr.data.ptr`)
  into the bridge. **No JAX** — removed; CuPy is the only interop.
- The bridge's `!$acc data ... deviceptr(...)` regions consume those pointers
  (zero-copy; the caller owns device memory). `!$acc` directives are inert
  comments under gfortran, so the same bridge serves CPU and GPU.

Two real bugs were fixed here: a `nogil` Python-coercion error (scalars are now
cast to C types *before* the `nogil` block), and the JAX removal.

Build needs **NVHPC `nvfortran`** + an NVIDIA GPU — neither exists on macOS, so
this is tested on RunPods (see §4):

```bash
pip install . \
  -C cmake.define.CMAKE_Fortran_COMPILER=nvfortran \
  -C cmake.define.PHYEX_ENABLE_GPU=ON \
  -C cmake.define.PHYEX_GPU_ARCH=cc89      # cc89=RTX 4090, cc86=RTX 3090
pytest tests/test_gpu_ice_adjust.py -v
```

> **Status / caveat.** The bridge demonstrates the deviceptr data-region
> pattern, but full on-device execution needs the physics routines themselves
> compiled with `-acc` and carrying their own `!$acc parallel` loops — only
> `mode_bl89` and `mode_turb_hor_dyn_corr` do so far. Treat the GPU path as
> **experimental**: the correctness test (`test_gpu_matches_cpu_reference`) is
> what tells you whether a given routine actually runs correctly on device.

---

## 4. Testing on a real GPU — RunPods

Triggered manually; provisions a **cheap consumer GPU** (RTX 4090/3090),
billed per-second, always torn down.

- `.github/workflows/python-gpu-runpod.yml` — `workflow_dispatch` (inputs:
  GPU type, `-gpu` arch, NVHPC image). Needs repo secret `RUNPOD_API_KEY`.
- `ci/runpod_dispatch.sh` — runs on the GitHub runner: `runpodctl` creates the
  pod, waits for SSH, clones the commit, runs the on-pod script, **always
  removes the pod** (EXIT trap).
- `ci/runpod_gpu_test.sh` — runs on the NVHPC pod: installs `cupy-cuda12x`,
  builds the GPU wheel, asserts `phyex.is_gpu_build()`, runs the GPU tests.

The GPU job downloads the CPU job's golden reference; the GPU result is checked
for finiteness/bounds and (when the reference is present) `allclose` against the
CPU result at float32 tolerance. Run it by hand on any NVHPC + NVIDIA box:

```bash
GPU_ARCH=cc89 bash ci/runpod_gpu_test.sh
```

---

## 4b. Known limitations / open items

What is **done and tested** vs. what still needs work — stated plainly so nobody
over-trusts the bindings:

| Area | Status |
|---|---|
| CMake + scikit-build-core packaging | ✅ done |
| Library build from vendored transformed tree (gfortran) | ✅ `libphyex_dp` links (1008+ objs) |
| CPU wheel builds & imports (`ice_adjust`, `rain_ice`, `turb`, …) | ✅ done |
| CPU `ice_adjust` runs, returns finite, bounded arrays | ✅ smoke test passes |
| CPU `ice_adjust` numerical correctness | ⚠️ **not validated** — see below |
| `init_rain_ice` | ❌ **segfaults** — do not call yet |
| GPU wrapper compiles to C; nogil bug fixed; JAX removed | ✅ done |
| GPU build links / runs on a real GPU | ⏳ **untested** (no local NVHPC/GPU; run the RunPods job) |

**Why correctness isn't proven yet.** The bridge initializes constants via
`INI_CST` but does **not** fully populate the PHYEX configuration derived types
(`NEBN`, `PARAM_ICEN`, `RAIN_ICE_PARAMN`, `CSTURB`, …). As a result
`ice_adjust` runs on under-initialized config and returns **zeros** even for a
supersaturated parcel, and `init_rain_ice` dereferences unset state and
**segfaults**. The smoke test deliberately asserts only finiteness/bounds.

**Next step to "well tested" physics:** complete config initialization in
`phyex_bridge.F90` (fill the `*_t` config structures the way the offline
testprogs `main_ice_adjust.F90` does), then strengthen `test_cpu_ice_adjust.py`
to assert real condensation (`cldfr > 0`, nonzero `ths`) and regenerate a
*meaningful* golden reference for the GPU agreement test (`tests/gen_reference.py`).

**GPU device-layout caveat.** `_phyex_wrapper_acc.pyx` calls
`cp.ascontiguousarray` (C order) before passing pointers, while the bridge
expects Fortran layout — verify this on the first real GPU run.

## 5. Keeping the vendored tree in sync with upstream

`phyex_transformed/` is generated, not authored. After pulling upstream physics
into `src/common`, regenerate it (on Linux, where fxtran builds):

```bash
pip install "pyfortool>=0.2.7"
tools/prep_code.sh -m arome -s turb -s micro -s aux -s conv \
  phyex_transformed -- --mnhExpand --removeACC   # CPU tree
```

Re-run the CPU + GPU workflows to confirm the regenerated tree still builds and
the golden reference is unchanged within tolerance.

---

## Summary of what landed

| File | Purpose |
|---|---|
| `pyproject.toml` | scikit-build-core package definition |
| `CMakeLists.txt` | source-root switch, precision fix, `PHYEX_ENABLE_GPU` |
| `cmake/bridge/phyex_bridge.F90` | C-interop layer (CPU + OpenACC data regions) |
| `cmake/bridge/_phyex_wrapper.pyx` | CPU wrapper |
| `cmake/bridge/_phyex_wrapper_acc.pyx` | GPU wrapper (CuPy; JAX removed; nogil fixed) |
| `phyex/__init__.py` | package entry point |
| `phyex_transformed/**` | vendored pyfortool output (build input) |
| `tests/**` | CPU smoke test, GPU correctness test, reference generator |
| `.github/workflows/python-cpu.yml` | CPU build + test + publish reference |
| `.github/workflows/python-gpu-runpod.yml` | on-demand GPU test on RunPods |
| `ci/runpod_*.sh` | RunPods orchestration + on-pod build/test |
