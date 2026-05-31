# PHYEX Model Releases

PHYEX provides pre-transformed code packages for three atmospheric models. Each package is generated automatically from the master branch with model-specific transformations.

## Overview

| Model | Package | Branch | OpenACC | GPU Support |
|-------|---------|--------|---------|-------------|
| **AROME** | `arome_vX.X.X.tar.gz` | ✅ `arome_vX.X.X` | ❌ Removed | ❌ CPU-only |
| **LMDZ** | `lmdz_vX.X.X.tar.gz` | ❌ None | ❌ Removed | ❌ CPU-only |
| **Meso-NH** | `mesonh_vX.X.X.tar.gz` | ❌ None | ✅ Preserved | ✅ GPU-capable |

---

## AROME

**Special case:** AROME releases include a persistent branch in addition to the archive.

**Transformations applied:**
- Array-syntax → DO loops (`--mnhExpand`)
- OpenACC directives removed (`--removeACC`)
- Shuman operators → subroutine calls (`--shumanFUNCtoCALL`)

### Usage

```bash
# Clone the AROME branch directly
git clone -b arome_v0.8.0 https://github.com/UMR-CNRM/PHYEX.git

# Or download the archive
wget https://github.com/UMR-CNRM/PHYEX/releases/download/v0.8.0/arome_v0.8.0.tar.gz
tar -xzf arome_v0.8.0.tar.gz
```

---

## Meso-NH

**Special case:** Meso-NH preserves OpenACC directives for GPU execution.

**Transformations applied:**
- Array-syntax → DO loops (`--mnhExpand`)
- Files → lowercase `.f90` (`--lowerCase`)
- OpenACC directives **preserved** (no `--removeACC`)

### Usage

```bash
# Download the archive
wget https://github.com/UMR-CNRM/PHYEX/releases/download/v0.8.0/mesonh_v0.8.0.tar.gz
tar -xzf mesonh_v0.8.0.tar.gz
```

### GPU Compilation

```bash
# Compile with NVIDIA Fortran for GPU
nvfortran -acc -Minfo=accel -DMNH_OPENACC source.f90
```

---

## LMDZ

**Transformations applied:**
- Array-syntax → DO loops (`--mnhExpand`)
- OpenACC directives removed (`--removeACC`)
- Shuman operators → subroutine calls (`--shumanFUNCtoCALL`)

### Usage

```bash
# Download the archive
wget https://github.com/UMR-CNRM/PHYEX/releases/download/v0.8.0/lmdz_v0.8.0.tar.gz
tar -xzf lmdz_v0.8.0.tar.gz
```

---

## Package Contents

All packages contain:
```
<model>_release/
├── src/
│   ├── turb/
│   ├── micro/
│   ├── aux/
│   ├── conv/      # AROME and Meso-NH only
│   └── ext/
└── <model>_version.json
```

## See Also

- [Transform Script Documentation](.github/scripts/README.md)
- [Release Workflow](.github/workflows/release.yml)
