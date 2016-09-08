
''' This module tests the parse2003 module using pytest '''

import os
import pytest
from fparser import Fortran2003
from dag import dag_from_strings

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")

def test_walk_debug(capsys):
    ''' Test the walk method with debug=True '''
    from fparser.readfortran import FortranFileReader
    from parse2003 import walk
    reader = FortranFileReader(os.path.join(BASE_PATH,
                                            "time_step_mod_simple.f90"))
    program = Fortran2003.Program(reader)
    names = walk(program.content, Fortran2003.Name, debug=True)
    result, _ = capsys.readouterr()
    assert "child type =  <class 'fparser.Fortran2003.Module'>" in result
    assert isinstance(names[0], Fortran2003.Name)
    assert str(names[0]) == "time_step_mod"


def test_variable_str():
    ''' Test the __str__ method of Variable '''
    from dag import dag_from_strings
    dag = dag_from_strings(["a(i) = 2.0 * b(i) * c"])
    anode = dag._nodes["a(i)"]
    print anode
    assert hasattr(anode, "variable")
    assert str(anode.variable) == "a(i)"


def test_variable_scalar_index_expr():
    ''' Test that the Variable.index_expr() behaves as expected when
    the variable is a scalar rather than an array reference '''
    dag = dag_from_strings(["a(i) = 2.0 * b(i) * c"])
    cnode = dag._nodes["c"]
    assert hasattr(cnode, "variable")
    assert cnode.variable.index_expr == ""


def test_variable_index_exprn_minus1():
    ''' Test the code that attempts to simply array-index expressions
    when the net increment to an index is negative '''
    dag = dag_from_strings(["a(i) = 2.0 * b(i)"])
    anode = dag._nodes["a(i)"]
    var = anode.variable
    var._index_exprns[0] += "-1-1+1"
    assert var.indices[0] == "i-1"
    
