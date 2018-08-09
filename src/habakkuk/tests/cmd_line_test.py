# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
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
# -----------------------------------------------------------------------------
# Author A. R. Porter, STFC Daresbury Lab

''' This module tests aspects of Habakkuk related to handling of
    command-line arguments '''

from __future__ import absolute_import, print_function
import os
import subprocess

# constants - set the location of the Habakkuk script (so that we
# don't just pick-up the one on our PATH)
_DIR = os.path.dirname(os.path.abspath(__file__))
while "src" in _DIR:
    (_DIR, _) = os.path.split(_DIR)
HABAKKUK_SCRIPT = os.path.join(_DIR, 'bin', 'habakkuk')

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_PATH = os.path.join(PWD, "test_files")


def test_usage_message():
    ''' Check that we get a usage message if no command-line arguments
    are supplied '''
    (output, _) = subprocess.Popen([HABAKKUK_SCRIPT],
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT).communicate()
    assert "Usage: habakkuk [options] <Fortran file(s)>" in str(output)


def test_missing_file():
    ''' Check that we get the expected message if the user specifies a
    Fortran source file that cannot be found '''
    # One argument given and it cannot be found...
    (output, _) = subprocess.Popen([HABAKKUK_SCRIPT, 'not_a_file.f90'],
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT).communicate()
    assert "The specified source file ('not_a_file.f90') cannot be found" in \
        str(output)
    # and now when it is the second of two files that does not exist...
    (output, _) = subprocess.Popen([HABAKKUK_SCRIPT,
                                    os.path.join(BASE_PATH, "basic_loop.f90"),
                                    'not_a_file.f90'],
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT).communicate()
    assert "The specified source file ('not_a_file.f90') cannot be found" in \
        str(output)


def test_help_message():
    ''' Check that a help message is produced when the --help flag is
    supplied '''
    (outbytes, _) = subprocess.Popen([HABAKKUK_SCRIPT, '--help'],
                                     stdout=subprocess.PIPE,
                                     stderr=subprocess.STDOUT).communicate()
    output = outbytes.decode("utf-8")
    expected = '''\
Options:
  -h, --help            show this help message and exit
  --no-prune            Do not attempt to prune duplicate operations from the
                        graph
  --no-fma              Do not attempt to generate fused multiply-add
                        operations
  --rm-scalar-tmps      Remove scalar temporaries from the DAG
  --show-weights        Display node weights in the DAG
  --unroll=UNROLL_FACTOR
                        No. of times to unroll a loop. (Applied to every loop
                        that is encountered.)'''
    assert expected in output
