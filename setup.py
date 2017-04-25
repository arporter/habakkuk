#!/usr/bin/env python

# Copyright (c) 2016, Andrew Porter.
# All rights reserved.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

"""Setup script. Used by easy_install and pip."""

from setuptools import setup, find_packages
import os

PACKAGES = find_packages(where="src")

if __name__ == "__main__":

    setup(
        name='Habakkuk',
        version='0.1.0',
        description='Performance prediction for Fortran kernels.',
        long_description=open('README.md').read(),
        author='Andrew R Porter',
        author_email=('andrew.porter@stfc.ac.uk'),
        url='https://arporter.github.io/habakkuk/',
        license='OSI Approved :: BSD 2-Clause License',
        classifiers=['Development Status :: 3 - Alpha',
                     'Environment :: Console',
                     'Programming Language :: Python',
                     'Programming Language :: Python :: 2.7',
                     'Topic :: Scientific/Engineering',
                     'Topic :: Utilities',
                     'Operating System :: MacOS :: MacOS X',
                     'Operating System :: POSIX',
                     'Operating System :: Unix'],
        packages=PACKAGES,
        package_dir={"": "src"},
        install_requires=['fparser'],
        # We need the following line to ensure we get the fparser/log.config
        # file installed.
        include_package_data=True,
        scripts=['bin/habakkuk'],
    )
