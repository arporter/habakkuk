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

''' This module tests the parse2003 module using pytest '''

# Since this is a file containing tests which often have to get in and
# change the internal state of objects we disable pylint's warning
# about such accesses
# pylint: disable=protected-access
from __future__ import absolute_import, print_function
import os
from six import itervalues
import pytest
from fparser.two import Fortran2003
from test_utilities import dag_from_strings
from habakkuk.parse2003 import ParseError

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


def test_walk_debug(capsys):
    ''' Test the walk method with debug=True '''
    from fparser.common.readfortran import FortranFileReader
    from habakkuk.parse2003 import walk_ast
    reader = FortranFileReader(os.path.join(BASE_PATH,
                                            "time_step_mod_simple.f90"))
    program = Fortran2003.Program(reader)
    names = walk_ast(program.content, [Fortran2003.Name], debug=True)
    result, _ = capsys.readouterr()
    assert (
        "child type =  <class 'fparser.two.Fortran2003.Module'>"
        in result)
    assert isinstance(names[0], Fortran2003.Name)
    assert str(names[0]) == "time_step_mod"


def test_variable_str():
    ''' Test the __str__ method of Variable '''
    dag = dag_from_strings(["a(i) = 2.0 * b(i) * c"])
    anode = dag._nodes["a(i)"]
    print(anode)
    assert hasattr(anode, "variable")
    assert str(anode.variable) == "a(i)"


def test_var_scalar_index_expr():
    ''' Test that the Variable.index_expr() behaves as expected when
    the variable is a scalar rather than an array reference '''
    dag = dag_from_strings(["a(i) = 2.0 * b(i) * c"])
    cnode = dag._nodes["c"]
    assert hasattr(cnode, "variable")
    assert cnode.variable.index_expr == ""


def test_var_index_exprn_minus1():
    ''' Test the code that attempts to simplify array-index expressions
    when the net increment to an index is negative '''
    dag = dag_from_strings(["a(i) = 2.0 * b(i)"])
    anode = dag._nodes["a(i)"]
    var = anode.variable
    var._index_exprns[0] += "-1-1+1"
    assert var.indices[0] == "i-1"


def test_var_load_lhs_mapping():
    ''' Test that we generate the correct name for a scalar variable that
    appears on the LHS of an assignment when its name is already
    in the naming map '''
    dag = dag_from_strings(["a = 2.0 * b(i)", "a = a * b(i)"])
    node_names = [node.name for node in itervalues(dag._nodes)]
    assert "a'" in node_names


def test_array_var_load_lhs_mapping():
    ''' Test that we generate the correct name for an array ref that
    appears on the LHS of an assignment when its name is already
    in the naming map '''
    dag = dag_from_strings(["a(i) = 2.0 * b(i)", "a(i) = a(i) * b(i)"])
    node_names = [node.name for node in itervalues(dag._nodes)]
    assert "a'(i)" in node_names


def test_indirect_array_access1():
    ''' Test the creation of a Variable object representing an indirect
    array reference (i.e. when an array-index expression is itself an
    array access) '''
    dag = dag_from_strings(["a(i) = 2.0 * b(map(i))"])
    node_names = [node.name for node in itervalues(dag._nodes)]
    print(node_names)
    assert "b(map(i))" in node_names
    assert "map(i)" in node_names


def test_indirect_array_access2():
    ''' Test the creation of a Variable object representing an indirect
    array reference (i.e. when an array-index expression involves an
    array access within a larger expression) '''
    dag = dag_from_strings(["a(i) = 2.0 * b(map(i)+j)"])
    node_names = [node.name for node in itervalues(dag._nodes)]
    print(node_names)
    assert "b(map(i)+j)" in node_names
    assert "map(i)" in node_names


def test_load_unrecognised_array_access():  # pylint: disable=invalid-name
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


def test_load_array_parenthesis():
    ''' Check that we create the correct type of Variable if an
    array access has an index expression within parentheses '''
    from habakkuk.parse2003 import Variable
    assign = Fortran2003.Assignment_Stmt("a((i+j)) = 2.0*b(i)")
    mapping = {}
    lhs_var = Variable()
    lhs_var.load(assign.items[0], mapping=mapping, lhs=True)
    assert lhs_var.is_array_ref is True
    assert "a(i+j)" in lhs_var.full_name


def test_load_dtype_compt_array():
    ''' Check that we recognise an array access when the
    array is a component of a derived type '''
    from habakkuk.parse2003 import Variable
    assign = Fortran2003.Assignment_Stmt("ssha%data(ji,jj) = a_value")
    mapping = {}
    lhs_var = Variable()
    lhs_var.load(assign.items[0], mapping=mapping, lhs=True)
    assert lhs_var.full_name == "ssha%data(ji,jj)"
    assert lhs_var.name == "ssha%data"
    assert lhs_var.is_array_ref is True


def test_load_dtype_array_cmpt_array():  # pylint: disable=invalid-name
    ''' Check that we recognise an array access when the
    array is a component of a derived type which itself is held in
    an array '''
    from habakkuk.parse2003 import Variable
    assign = Fortran2003.Assignment_Stmt("ssha(jt)%data(ji,jj) = a_value")
    mapping = {}
    lhs_var = Variable()
    lhs_var.load(assign.items[0], mapping=mapping, lhs=True)
    assert lhs_var.full_name == "ssha(jt)%data(ji,jj)"
    assert lhs_var.name == "ssha(jt)%data"
    assert lhs_var.is_array_ref is True


def test_load_dtype_dtype_array():
    ''' Check that we recognise an array access when the array is a
    component of a component of a derived type '''
    from habakkuk.parse2003 import Variable
    assign = Fortran2003.Assignment_Stmt(
        "sshn_t(jt)%grid%area_t(ji,jj) = a_value(jt)")
    mapping = {}
    lhs_var = Variable()
    lhs_var.load(assign.items[0], mapping=mapping, lhs=True)
    assert lhs_var.full_name == "sshn_t(jt)%grid%area_t(ji,jj)"
    assert lhs_var.name == "sshn_t(jt)%grid%area_t"
    assert lhs_var.is_array_ref is True

    assign = Fortran2003.Assignment_Stmt(
        "sshn_t(jt)%grid(jg)%area_t(ji,jj) = a_value(jt)")
    lhs_var = Variable()
    lhs_var.load(assign.items[0], mapping=mapping, lhs=True)
    assert lhs_var.full_name == "sshn_t(jt)%grid(jg)%area_t(ji,jj)"
    assert lhs_var.name == "sshn_t(jt)%grid(jg)%area_t"
    assert lhs_var.is_array_ref is True


def test_load_dtype_with_map():
    ''' Check that we get a Variable with the correct name when a
    mapping is provided alongside a derived-type access '''
    from habakkuk.parse2003 import Variable
    assign = Fortran2003.Assignment_Stmt(
        "sshn_t(jt)%grid%area_t = a_value(jt)")
    # The key in the name mapping is the fully-indexed expression while
    # the associated entry is the base-name (everything except the index
    # expression).
    mapping = {"sshn_t(jt)%grid%area_t":
               "sshn_t(jt)%grid%area_t'"}
    lhs_var = Variable()
    lhs_var.load(assign.items[0], mapping=mapping, lhs=True)
    assert lhs_var.full_name == "sshn_t(jt)%grid%area_t'"


def test_arr_slice_arg_fn_call():
    ''' Check that the use of an array-slice in what might otherwise be an
    array reference results instead in the identification of a function
    call '''
    from habakkuk.parse2003 import Variable
    assign = Fortran2003.Assignment_Stmt(
        "sshn_t(jt)%grid%area_t = my_fn(a_value(:))")
    rhs_var = Variable()
    mapping = {}
    rhs_var.load(assign.items[-1], mapping=mapping)
    assert rhs_var.is_array_ref is False


def test_load_unrecognised_type():
    ''' Check that Variable.load() raises the expected exception when we
    don't recognise the type of the node that is supplied '''
    from habakkuk.parse2003 import Variable
    mapping = {}
    lhs_var = Variable()
    with pytest.raises(ParseError) as err:
        lhs_var.load("Not a node", mapping=mapping, lhs=True)
    assert "Unrecognised type for variable " in str(err)
