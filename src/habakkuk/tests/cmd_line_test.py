''' This module tests aspects of Habakkuk related to handling of
    command-line arguments '''

import os
import pytest
import subprocess

def test_usage_message(capsys):
    ''' Check that we get a usage message if no command-line arguments
    are supplied '''
    out = subprocess.check_output(['habakkuk'])
    print out
    assert "Usage: habakkuk [options] <Fortran files>" in out
