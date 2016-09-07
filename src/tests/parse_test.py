
''' This module tests the parse2003 module using pytest '''

import os
import pytest

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")

def test_walk_debug(capsys):
    ''' Test the walk method with debug=True '''
    from fparser.readfortran import FortranFileReader
    from fparser.api import Fortran2003
    from fparser.Fortran2003 import Subroutine_Subprogram
    from parse2003 import walk
    reader = FortranFileReader(os.path.join(BASE_PATH,
                                            "time_step_mod_simple.f90"))
    program = Fortran2003.Program(reader)
    routines = walk(program.content, Subroutine_Subprogram, debug=True)
    result, _ = capsys.readouterr()
    print result
    assert "child type =  <class 'fparser.Fortran2003.Module'>" in result
