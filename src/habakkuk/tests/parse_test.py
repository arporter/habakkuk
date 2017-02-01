
''' This module tests the parse2003 module using pytest '''

import os
import pytest
from fparser import Fortran2003
from test_utilities import dag_from_strings
from habakkuk.parse2003 import ParseError

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


def test_walk_debug(capsys):
    ''' Test the walk method with debug=True '''
    from fparser.readfortran import FortranFileReader
    from habakkuk.parse2003 import walk_ast
    reader = FortranFileReader(os.path.join(BASE_PATH,
                                            "time_step_mod_simple.f90"))
    program = Fortran2003.Program(reader)
    names = walk_ast(program.content, [Fortran2003.Name], debug=True)
    result, _ = capsys.readouterr()
    assert (
        "child type =  <class 'fparser.Fortran2003.Module'>"
        in result)
    assert isinstance(names[0], Fortran2003.Name)
    assert str(names[0]) == "time_step_mod"


def test_variable_str():
    ''' Test the __str__ method of Variable '''
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
    ''' Test the code that attempts to simplify array-index expressions
    when the net increment to an index is negative '''
    dag = dag_from_strings(["a(i) = 2.0 * b(i)"])
    anode = dag._nodes["a(i)"]
    var = anode.variable
    var._index_exprns[0] += "-1-1+1"
    assert var.indices[0] == "i-1"


def test_variable_load_lhs_mapping():
    ''' Test that we generate the correct name for a scalar variable that
    appears on the LHS of an assignment when its name is already
    in the naming map '''
    dag = dag_from_strings(["a = 2.0 * b(i)", "a = a * b(i)"])
    node_names = [node.name for node in dag._nodes.itervalues()]
    assert "a'" in node_names


def test_array_variable_load_lhs_mapping():
    ''' Test that we generate the correct name for an array ref that
    appears on the LHS of an assignment when its name is already
    in the naming map '''
    dag = dag_from_strings(["a(i) = 2.0 * b(i)", "a(i) = a(i) * b(i)"])
    node_names = [node.name for node in dag._nodes.itervalues()]
    assert "a'(i)" in node_names


def test_indirect_array_access1():
    ''' Test the creation of a Variable object representing an indirect
    array reference (i.e. when an array-index expression is itself an
    array access) '''
    dag = dag_from_strings(["a(i) = 2.0 * b(map(i))"])
    node_names = [node.name for node in dag._nodes.itervalues()]
    print node_names
    assert "b(map(i))" in node_names
    assert "map(i)" in node_names


def test_indirect_array_access2():
    ''' Test the creation of a Variable object representing an indirect
    array reference (i.e. when an array-index expression involves an
    array access within a larger expression) '''
    dag = dag_from_strings(["a(i) = 2.0 * b(map(i)+j)"])
    node_names = [node.name for node in dag._nodes.itervalues()]
    print node_names
    assert "b(map(i)+j)" in node_names
    assert "map(i)" in node_names


def test_load_unrecognised_array_access():
    ''' Check that we raise the expected exception when we don't recognise
    the form of an array access. '''
    from habakkuk.parse2003 import Variable
    assign = Fortran2003.Assignment_Stmt("a(i**2) = 2.0*b(i)")
    mapping = {}
    lhs_var = Variable()
    with pytest.raises(ParseError) as err:
        lhs_var.load(assign.items[0], mapping=mapping, lhs=True)
    assert "Unrecognised array-index expression" in str(err)


def test_load_array_section():
    ''' Check that we successfully create a Variable for an array
    section '''
    from habakkuk.parse2003 import Variable
    assign = Fortran2003.Assignment_Stmt("a(:) = 2.0*b(:)")
    mapping = {}
    lhs_var = Variable()
    lhs_var.load(assign.items[0], mapping=mapping, lhs=True)
    assert "a(:)" in lhs_var.full_name


def test_load_unrecognised_type():
    ''' Check that Variable.load() raises the expected exception when we
    don't recognise the type of the node that is supplied '''
    from habakkuk.parse2003 import Variable
    mapping = {}
    lhs_var = Variable()
    with pytest.raises(ParseError) as err:
        lhs_var.load("Not a node", mapping=mapping, lhs=True)
    assert "Unrecognised type for variable " in str(err)
