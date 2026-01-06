#!/usr/bin/env python3
"""
PHYEX Code Transformation Script for Model-Specific Releases

This script handles the transformation of PHYEX code from the master branch
to model-specific branches (AROME, LMDZ, Meso-NH).

Usage:
    python transform_phyex.py --model arome --version v0.8.0 --output-dir arome_release
    python transform_phyex.py --model lmdz --version v0.8.0 --output-dir lmdz_release
    python transform_phyex.py --model mesonh --version v0.8.0 --output-dir mesonh_release
"""

import argparse
import logging
import shutil
import subprocess
import sys
from pathlib import Path
from typing import List, Dict

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(levelname)s: %(message)s'
)
logger = logging.getLogger(__name__)


class PHYEXTransformer:
    """Handle PHYEX code transformation for different models"""

    # Model-specific configuration
    MODEL_CONFIG = {
        'arome': {
            'subdirs': ['turb', 'micro', 'aux', 'conv', 'ext', 'gmkpack_ignored_files'],
            'pyfortool_opts': ['--mnhExpand', '--removeACC', '--shumanFUNCtoCALL'],
            'config_file': 'ial_version.json',
            'suppress_files': None,
            'lowercase': False,
            'description': 'AROME (CPU-only, DO loops, no OpenACC)'
        },
        'lmdz': {
            'subdirs': ['turb', 'micro', 'aux', 'ext'],
            'pyfortool_opts': ['--mnhExpand', '--removeACC', '--shumanFUNCtoCALL'],
            'config_file': 'lmdz_version.json',
            'suppress_files': None,
            'lowercase': False,
            'description': 'LMDZ (CPU-only, DO loops, no OpenACC)'
        },
        'mesonh': {
            'subdirs': ['turb', 'micro', 'aux', 'conv', 'ext'],
            'pyfortool_opts': ['--mnhExpand', '--lowerCase'],
            'config_file': 'mesonh_version.json',
            'suppress_files': 'filesToSuppress.txt',
            'lowercase': True,
            'description': 'Meso-NH (GPU-capable, preserves OpenACC)'
        }
    }

    def __init__(self, model: str, version: str, output_dir: Path, src_dir: Path = Path('src')):
        if model not in self.MODEL_CONFIG:
            raise ValueError(f"Unknown model: {model}. Must be one of {list(self.MODEL_CONFIG.keys())}")

        self.model = model
        self.version = version
        self.output_dir = Path(output_dir)
        self.src_dir = Path(src_dir)
        self.config = self.MODEL_CONFIG[model]

        logger.info(f"Initializing PHYEX transformer for {model.upper()}")
        logger.info(f"Description: {self.config['description']}")

    def merge_sources(self) -> None:
        """Merge common and model-specific source files"""
        logger.info("=== Step 1: Merging source files ===")

        src_output = self.output_dir / 'src'
        src_output.mkdir(parents=True, exist_ok=True)

        # Copy common source files
        logger.info("Copying common source files...")
        for subdir in self.config['subdirs']:
            common_path = self.src_dir / 'common' / subdir
            if common_path.exists():
                dest_path = src_output / subdir
                logger.info(f"  - {common_path} → {dest_path}")
                shutil.copytree(common_path, dest_path, dirs_exist_ok=True)

        # Overlay model-specific files
        logger.info(f"Overlaying {self.model}-specific files...")
        model_src = self.src_dir / self.model
        for subdir in self.config['subdirs']:
            model_path = model_src / subdir
            if model_path.exists():
                dest_path = src_output / subdir
                logger.info(f"  - {model_path} → {dest_path}")
                shutil.copytree(model_path, dest_path, dirs_exist_ok=True)

        # Copy configuration file
        config_file = model_src / self.config['config_file']
        if config_file.exists():
            shutil.copy(config_file, self.output_dir / self.config['config_file'])
            logger.info(f"  - Copied {self.config['config_file']}")

        # Copy suppress file for Meso-NH
        if self.config['suppress_files']:
            suppress_file = model_src / self.config['suppress_files']
            if suppress_file.exists():
                shutil.copy(suppress_file, self.output_dir / self.config['suppress_files'])
                logger.info(f"  - Copied {self.config['suppress_files']}")

        # Count files
        fortran_files = list(src_output.rglob('*.F90')) + list(src_output.rglob('*.f90'))
        logger.info(f"Total Fortran files: {len(fortran_files)}")

    def transform_files(self) -> None:
        """Transform Fortran files using pyfortool via uvx"""
        logger.info("=== Step 2: Transforming files with pyfortool ===")
        logger.info(f"Options: {' '.join(self.config['pyfortool_opts'])}")

        src_output = self.output_dir / 'src'
        fortran_files = sorted(src_output.rglob('*.F90')) + sorted(src_output.rglob('*.f90'))

        total = len(fortran_files)
        logger.info(f"Found {total} Fortran files to transform")

        for i, file_path in enumerate(fortran_files, 1):
            logger.info(f"[{i}/{total}] Transforming: {file_path.relative_to(self.output_dir)}")

            # Determine output file
            if self.config['lowercase']:
                output_path = file_path.with_suffix('.f90')
            else:
                output_path = file_path

            # Build pyfortool command
            cmd = ['uvx', '--from', 'pyfortool', 'pyfortool'] + self.config['pyfortool_opts'] + [str(file_path)]

            try:
                # Run transformation
                result = subprocess.run(cmd, capture_output=True, text=True, check=True)

                # Write output
                output_path.write_text(result.stdout)

                # Remove original .F90 if we created .f90
                if self.config['lowercase'] and file_path.suffix == '.F90':
                    file_path.unlink()

            except subprocess.CalledProcessError as e:
                logger.error(f"Failed to transform {file_path}: {e.stderr}")
                raise

        logger.info("Transformation complete!")

    def suppress_files(self) -> None:
        """Remove files listed in suppress file (Meso-NH only)"""
        if not self.config['suppress_files']:
            return

        logger.info("=== Step 3: Removing suppressed files ===")

        suppress_file = self.output_dir / self.config['suppress_files']
        if not suppress_file.exists():
            logger.info("No suppress file found, skipping")
            return

        src_output = self.output_dir / 'src'
        removed_count = 0

        with open(suppress_file) as f:
            for line in f:
                line = line.strip()
                # Skip empty lines and comments
                if not line or line.startswith('#'):
                    continue

                file_to_remove = src_output / line
                if file_to_remove.exists():
                    logger.info(f"  Removing: {line}")
                    file_to_remove.unlink()
                    removed_count += 1
                else:
                    logger.debug(f"  Not found (skipping): {line}")

        logger.info(f"Removed {removed_count} suppressed files")

    def verify_transformations(self) -> Dict[str, int]:
        """Verify the transformations were applied correctly"""
        logger.info("=== Step 4: Verifying transformations ===")

        src_output = self.output_dir / 'src'
        fortran_files = list(src_output.rglob('*.F90')) + list(src_output.rglob('*.f90'))

        # Count OpenACC directives
        acc_count = 0
        for f in fortran_files:
            content = f.read_text()
            acc_count += content.count('!$acc')

        # Count mnh_expand directives
        mnh_count = 0
        for f in fortran_files:
            content = f.read_text()
            mnh_count += content.count('!$mnh_expand')

        # Count file extensions
        F90_count = len(list(src_output.rglob('*.F90')))
        f90_count = len(list(src_output.rglob('*.f90')))

        results = {
            'openacc_directives': acc_count,
            'mnh_expand_directives': mnh_count,
            'uppercase_F90': F90_count,
            'lowercase_f90': f90_count,
            'total_files': F90_count + f90_count
        }

        # Print results
        logger.info(f"OpenACC directives: {results['openacc_directives']}")
        logger.info(f"mnh_expand directives: {results['mnh_expand_directives']}")
        logger.info(f"Uppercase .F90 files: {results['uppercase_F90']}")
        logger.info(f"Lowercase .f90 files: {results['lowercase_f90']}")
        logger.info(f"Total Fortran files: {results['total_files']}")

        # Validate based on model
        if self.model in ['arome', 'lmdz']:
            # CPU-only models should have NO OpenACC
            if results['openacc_directives'] > 0:
                logger.warning(f"⚠️  OpenACC directives found but should be removed for {self.model.upper()}")
            else:
                logger.info("✓ All OpenACC directives successfully removed")

            # Should have uppercase .F90
            if results['uppercase_F90'] == 0:
                logger.warning("⚠️  No .F90 files found")
            else:
                logger.info(f"✓ Files kept as uppercase .F90")

        elif self.model == 'mesonh':
            # Meso-NH should KEEP OpenACC
            if results['openacc_directives'] == 0:
                logger.warning("⚠️  No OpenACC directives found! GPU support may be missing")
            else:
                logger.info(f"✓ OpenACC directives preserved for GPU execution ({results['openacc_directives']} directives)")

            # Should have lowercase .f90
            if results['lowercase_f90'] == 0:
                logger.warning("⚠️  No .f90 files found")
            elif results['uppercase_F90'] > 0:
                logger.warning(f"⚠️  {results['uppercase_F90']} .F90 files remain (should be .f90)")
            else:
                logger.info("✓ All files converted to lowercase .f90 extension")

        # mnh_expand should always be processed
        if results['mnh_expand_directives'] > 0:
            logger.warning(f"⚠️  {results['mnh_expand_directives']} mnh_expand directives remain")
        else:
            logger.info("✓ All mnh_expand directives successfully processed")

        logger.info("=== Verification complete ===")
        return results

    def run(self) -> Dict[str, int]:
        """Run the complete transformation workflow"""
        logger.info(f"Starting PHYEX transformation for {self.model.upper()} version {self.version}")

        # Create output directory
        self.output_dir.mkdir(parents=True, exist_ok=True)

        # Run transformation steps
        self.merge_sources()
        self.transform_files()
        self.suppress_files()
        results = self.verify_transformations()

        logger.info(f"✓ Transformation complete! Output in: {self.output_dir}")
        return results


def main():
    parser = argparse.ArgumentParser(
        description='Transform PHYEX code for model-specific releases',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Transform for AROME
  %(prog)s --model arome --version v0.8.0 --output-dir arome_release

  # Transform for LMDZ
  %(prog)s --model lmdz --version v0.8.0 --output-dir lmdz_release

  # Transform for Meso-NH
  %(prog)s --model mesonh --version v0.8.0 --output-dir mesonh_release
"""
    )

    parser.add_argument(
        '--model',
        required=True,
        choices=['arome', 'lmdz', 'mesonh'],
        help='Target model for transformation'
    )

    parser.add_argument(
        '--version',
        required=True,
        help='Version tag (e.g., v0.8.0)'
    )

    parser.add_argument(
        '--output-dir',
        required=True,
        help='Output directory for transformed code'
    )

    parser.add_argument(
        '--src-dir',
        default='src',
        help='Source directory (default: src)'
    )

    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Enable verbose output'
    )

    args = parser.parse_args()

    if args.verbose:
        logger.setLevel(logging.DEBUG)

    try:
        transformer = PHYEXTransformer(
            model=args.model,
            version=args.version,
            output_dir=Path(args.output_dir),
            src_dir=Path(args.src_dir)
        )

        results = transformer.run()

        # Exit with success
        sys.exit(0)

    except Exception as e:
        logger.error(f"Transformation failed: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        sys.exit(1)


if __name__ == '__main__':
    main()
