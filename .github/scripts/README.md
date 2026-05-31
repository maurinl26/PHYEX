# PHYEX Transformation Scripts

This directory contains scripts for transforming PHYEX code for model-specific releases.

## transform_phyex.py

**Purpose:** Transform PHYEX code from master branch to model-specific branches (AROME, LMDZ, Meso-NH).

### Quick Start

```bash
# Transform for AROME (CPU-only, no OpenACC)
python transform_phyex.py --model arome --version v0.8.0 --output-dir arome_release

# Transform for LMDZ (CPU-only, no OpenACC)
python transform_phyex.py --model lmdz --version v0.8.0 --output-dir lmdz_release

# Transform for Meso-NH (GPU-capable, OpenACC preserved)
python transform_phyex.py --model mesonh --version v0.8.0 --output-dir mesonh_release
```

### What It Does

1. **Merges sources** - Combines `src/common/` with `src/<model>/`
2. **Transforms code** - Runs pyfortool via uvx with model-specific options
3. **Removes suppressed files** - For Meso-NH only
4. **Verifies output** - Checks transformations applied correctly

### Model Configuration

| Model | Subdirectories | pyfortool Options | OpenACC | GPU |
|-------|----------------|-------------------|---------|-----|
| **arome** | turb, micro, aux, conv, ext, gmkpack | `--mnhExpand --removeACC --shumanFUNCtoCALL` | ❌ Removed | ❌ CPU-only |
| **lmdz** | turb, micro, aux, ext | `--mnhExpand --removeACC --shumanFUNCtoCALL` | ❌ Removed | ❌ CPU-only |
| **mesonh** | turb, micro, aux, conv, ext | `--mnhExpand --lowerCase` | ✅ **Preserved** | ✅ **GPU-capable** |

### Requirements

- Python 3.11+
- `uvx` (from uv package manager)
- `pyfortool` (installed via uvx)

### Usage in CI/CD

Used by `.github/workflows/release.yml`:

```yaml
- name: Transform code
  run: |
    python .github/scripts/transform_phyex.py \
      --model ${{ matrix.model }} \
      --version ${{ steps.version.outputs.version }} \
      --output-dir ${{ matrix.model }}_release \
      --verbose
```

### Local Testing

```bash
# Install uv
curl -LsSf https://astral.sh/uv/install.sh | sh

# Transform code
python transform_phyex.py --model arome --version test --output-dir test_output

# Check results
ls -la test_output/src/
grep -r '!\$acc' test_output/src/  # Should be empty for AROME/LMDZ
```

### Output Structure

```
<output-dir>/
├── src/
│   ├── turb/
│   ├── micro/
│   ├── aux/
│   ├── conv/      # Not in LMDZ
│   └── ext/
├── <model>_version.json
└── filesToSuppress.txt  # Meso-NH only
```

### Verification

The script automatically verifies:

**AROME & LMDZ:**
- ✅ OpenACC directives removed (count = 0)
- ✅ mnh_expand directives processed (count = 0)
- ✅ Files have uppercase `.F90` extension

**Meso-NH:**
- ✅ OpenACC directives **preserved** (count > 0)
- ✅ mnh_expand directives processed (count = 0)
- ✅ Files have lowercase `.f90` extension
- ✅ Suppressed files removed

### Adding a New Model

Edit the `MODEL_CONFIG` dictionary:

```python
MODEL_CONFIG = {
    'newmodel': {
        'subdirs': ['turb', 'micro', 'aux'],
        'pyfortool_opts': ['--mnhExpand', '--removeACC'],
        'config_file': 'newmodel_version.json',
        'suppress_files': None,
        'lowercase': False,
        'description': 'NewModel (CPU-only)'
    }
}
```

Then use:
```bash
python transform_phyex.py --model newmodel --version v0.8.0 --output-dir newmodel_release
```

### Error Handling

The script will exit with error code 1 if:
- Unknown model specified
- Source directories don't exist
- pyfortool transformation fails
- File operations fail

Check logs with `--verbose` for detailed error messages.

### Help

```bash
python transform_phyex.py --help
```

## See Also

- [Simplified Workflows](../../docs/Simplified_Workflows.md) - Overview of workflow simplification
- [Release Workflow](../../RELEASE_WORKFLOW.md) - Complete release process
- [Automated Workflows Comparison](../../docs/Automated_Workflows_Comparison.md) - Compare AROME/LMDZ/Meso-NH
