# Copyright 2013-2024 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

from spack.package import *


class Phyex(CMakePackage):
    """PHYEX: Physics Externalisation"""

    homepage = "https://github.com/maurinl26/PHYEX"
    url = "https://github.com/maurinl26/PHYEX/archive/refs/tags/v1.0.0.tar.gz"
    git = "https://github.com/maurinl26/PHYEX.git"

    maintainers = ['maurinl26']

    version('master', branch='master')

    depends_on("c", type="build")
    depends_on("fortran", type="build")

    variant('double_precision', default=True, description='Compile with double precision')
    variant('single_precision', default=False, description='Compile with single precision')
    variant('python', default=False, description='Build Python bindings')

    depends_on('ecbuild', type='build')
    depends_on('cmake@3.15:', type='build')
    depends_on('mpi')
    depends_on('fiat')
    
    # Python dependencies
    depends_on('python@3.8:', type=('build', 'run'), when='+python')
    depends_on('py-numpy', type=('build', 'run'), when='+python')
    depends_on('py-cython', type='build', when='+python')
    depends_on('py-scikit-build-core', type='build', when='+python')

    def cmake_args(self):
        args = [
            self.define_from_variant('HAVE_DOUBLE_PRECISION', 'double_precision'),
            self.define_from_variant('HAVE_SINGLE_PRECISION', 'single_precision'),
            self.define('PHYEX_FETCH_FIAT', 'OFF'),
        ]
        
        if '+python' in self.spec:
            args.append(self.define('SKBUILD', 'ON'))
            
        return args
