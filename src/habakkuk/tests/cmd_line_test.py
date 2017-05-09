''' This module tests aspects of Habakkuk related to handling of
    command-line arguments '''

import os
import pytest
import subprocess

def test_usage_message():
    ''' Check that we get a usage message if no command-line arguments
    are supplied '''
    (output, _) = subprocess.Popen(['habakkuk'],
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT).communicate()
    assert "Usage: habakkuk [options] <Fortran files>" in output


def test_missing_file():
    ''' Check that we get the expected message if the user specifies a
    Fortran source file that cannot be found '''
    (output, _) = subprocess.Popen(['habakkuk', 'not_a_file.f90'],
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT).communicate()
    assert "The specified source file ('not_a_file.f90') cannot be found" in \
        output
