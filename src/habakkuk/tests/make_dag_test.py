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

''' Contains pytest tests for make_dag.py '''

# Since this is a file containing tests which often have to get in and
# change the internal state of objects we disable pylint's warning
# about such accesses
# pylint: disable=protected-access

from __future__ import print_function, absolute_import
import os
from six import itervalues
import pytest
from test_utilities import Options
from habakkuk import make_dag
# constants
PWD = os.path.dirname(os.path.abspath(__file__))
BASE_PATH = os.path.join(PWD, "test_files")


def test_dag_of_code_block_items():
    ''' Test the dag_of_code_block() function when the supplied
    parent node has only items and not content '''
    from fparser.two import Fortran2003
    from habakkuk.make_dag import dag_of_code_block
    assign = Fortran2003.Assignment_Stmt("a(:) = 2.0*b(:)")
    dag = dag_of_code_block(assign, "Test dag")
    node_names = [node.name for node in itervalues(dag._nodes)]
    print(node_names)
    assert len(dag._nodes) == 4
    assert "a(:)" in node_names
    assert "b(:)" in node_names


def test_empty_routine(capsys):
    ''' Test that we do not generate a DAG if we find no assignment
    statements '''
    make_dag.dag_of_files(
        Options(), [os.path.join(BASE_PATH, "empty_routine.f90")])
    result, _ = capsys.readouterr()
    assert ("Code empty_routine contains no assignment statements - "
            "skipping" in result)


def test_basic_loop(tmpdir):
    ''' Check that we correctly generate a DAG for a subroutine containing
    a simple loop '''
    # Run test in a temporary directory
    os.chdir(str(tmpdir.mkdir("tmp")))
    make_dag.dag_of_files(
        Options(), [os.path.join(BASE_PATH, "basic_loop.f90")])
    graph_file = os.path.join(os.getcwd(), "basic_loop_routine_loop1.gv")
    assert os.path.isfile(graph_file)
    with open(graph_file, "r") as fout:
        loop_graph = fout.read()
    assert "label=\"2.0\", color=\"green\"" in loop_graph
    assert "label=\"i\", color=\"black\"" in loop_graph
    assert "label=\"*\", color=\"red\", shape=\"box\"" in loop_graph
    assert "label=\"aprod(i)\", color=\"blue\"" in loop_graph


def test_basic_loop_unroll(tmpdir):
    ''' Check that we correctly generate a DAG for a subroutine containing
    a simple loop that we unroll once '''
    options = Options()
    options.unroll_factor = 2
    # Run test in a temporary directory
    os.chdir(str(tmpdir.mkdir("tmp")))
    make_dag.dag_of_files(
        options, [os.path.join(BASE_PATH, "basic_loop.f90")])
    graph_file = os.path.join(os.getcwd(),
                              "basic_loop_routine_loop1_unroll2.gv")
    assert os.path.isfile(graph_file)
    with open(graph_file, "r") as fout:
        loop_graph = fout.read()
    # TODO need to find some way of testing the connectivity of the nodes,
    # not just their existence
    print(loop_graph)
    assert "label=\"2.0\", color=\"green\"" in loop_graph
    assert "label=\"i\", color=\"black\"" in loop_graph
    assert "label=\"i'\", color=\"black\"" in loop_graph
    assert "label=\"*\", color=\"red\", shape=\"box\"" in loop_graph
    assert "label=\"aprod(i)\", color=\"blue\"" in loop_graph
    assert "label=\"aprod(i')\", color=\"blue\"" in loop_graph


def test_unroll_no_loop_var(tmpdir, capsys):
    ''' Check that we generate the expected DAG when we encounter a
    loop for which we have no loop variable '''
    options = Options()
    options.unroll_factor = 2
    # Run test in a temporary directory
    os.chdir(str(tmpdir.mkdir("tmp")))
    make_dag.dag_of_files(
        options, [os.path.join(BASE_PATH, "uncontrolled_loop.f90")])
    result, _ = capsys.readouterr()
    assert "1 FLOPs in total" in result
    assert "1 distinct cache-line ref" in result


def test_main_routine_no_file_err():
    ''' Test that we raise expected error if the user doesn't supply the name
    of a Fortran file to process '''
    from habakkuk.make_dag import runner
    args = []
    with pytest.raises(IOError) as err:
        runner(args)
    assert "The name of a Fortran source file must be provided" in str(err)


def test_main_routine_file_not_present_err():
    ''' Test that we raise expected error if the file specified by the user
    doesn't exist '''
    from habakkuk.make_dag import runner
    args = ["not_a_file"]
    with pytest.raises(IOError) as err:
        runner(args)
    assert (
        "The specified source file ('not_a_file') cannot be found" in str(err))


def test_main_routine_valid_file(tmpdir):
    ''' Test that we can run the main routine of Habakkuk and that we get
    the expected dag written to file '''
    from habakkuk.make_dag import runner
    f90_file = os.path.join(BASE_PATH, "basic_loop.f90")
    args = [f90_file]
    os.chdir(str(tmpdir.mkdir("tmp")))
    runner(args)
    graph_file = os.path.join(os.getcwd(), "basic_loop_routine_loop1.gv")
    assert os.path.isfile(graph_file)
    with open(graph_file, "r") as fout:
        graph = fout.read()
    assert "label=\"2.0\", color=\"green\"" in graph
    assert "label=\"i\", color=\"black\"" in graph
    assert "label=\"*\", color=\"red\", shape=\"box\"" in graph
    assert "label=\"aprod(i)\", color=\"blue\"" in graph


def test_main_routine_prune_scalar_temporaries(tmpdir):
    ''' Test that we can run the main routine of Habakkuk and request
    that any nodes representing scalar temporaries be pruned from the dag
    that is constructed '''
    from habakkuk.make_dag import runner
    os.chdir(str(tmpdir.mkdir("tmp")))
    args = ["--rm-scalar-tmps",
            os.path.join(PWD, "test_files/two_different_duplicate_op.f90")]
    runner(args)
    graph_file = os.path.join(os.getcwd(), "test_duplicate_prod_div.gv")
    assert os.path.isfile(graph_file)


def test_main_routine_invalid_fortran(capsys):
    ''' Check that we print the expected error if given a source file that
    is not valid Fortran '''
    from habakkuk.make_dag import runner
    args = [os.path.join(PWD, "make_dag_test.py")]
    runner(args)
    result, _ = capsys.readouterr()
    print(result)
    assert "failed at line #1" in result
    assert "Is the file valid Fortran?" in result


def test_array_deref_count(tmpdir, capsys):
    ''' Check that we cope with indirectly-addressed array
    references '''
    from habakkuk.make_dag import runner
    os.chdir(str(tmpdir.mkdir("tmp")))
    args = [os.path.join(PWD, "test_files/gather.f90")]
    runner(args)
    result, _ = capsys.readouterr()
    print(result)
    assert "4 array references" in result
    assert "4 distinct cache-line references" in result


def test_multiple_array_accesses(tmpdir, capsys):
    ''' Check that we cope with the array accesses in a real-world
    NEMO example '''
    from habakkuk.make_dag import runner
    os.chdir(str(tmpdir.mkdir("tmp")))
    args = [os.path.join(PWD, "test_files/zpshde_loop6.f90")]
    runner(args)
    result, _ = capsys.readouterr()
    print(result)
    assert "14 addition operators" in result
    assert "38 FLOPs in total"
    assert "33 distinct cache-line references" in result
