
''' Contains pytest tests for make_dag.py '''

import os
import pytest
from test_utilities import dag_from_strings, Options
import make_dag

# constants
PWD = os.path.dirname(os.path.abspath(__file__))
BASE_PATH = os.path.join(PWD, "test_files")


def test_dag_of_code_block_items():
    ''' Test the dag_of_code_block() function when the supplied
    parent node has only items and not content '''
    from fparser import Fortran2003
    from make_dag import dag_of_code_block
    assign = Fortran2003.Assignment_Stmt("a(:) = 2.0*b(:)")
    dag = dag_of_code_block(assign, "Test dag")
    node_names = [node.name for node in dag._nodes.itervalues()]
    print node_names
    assert len(dag._nodes) == 4
    assert "a(:)" in node_names
    assert "b(:)" in node_names


def test_empty_routine(capsys):
    ''' Test that we do not generate a DAG if we find no assignment
    statements '''
    make_dag.runner(Options(),
                    [os.path.join(BASE_PATH, "empty_routine.f90")])
    result, _ = capsys.readouterr()
    assert ("Code empty_routine contains no assignment statements - "
            "skipping" in result)


def test_basic_loop():
    ''' Check that we correctly generate a DAG for a subroutine containing
    a simple loop '''
    make_dag.runner(Options(),
                    [os.path.join(BASE_PATH, "basic_loop.f90")])
    graph_file = os.path.join(PWD, "basic_loop_routine_loop1.gv")
    assert os.path.isfile(graph_file)
    with open(graph_file, "r") as fout:
        loop_graph = fout.read()
    assert "label=\"2.0\", color=\"green\"" in loop_graph
    assert "label=\"i\", color=\"black\"" in loop_graph
    assert "label=\"*\", color=\"red\", shape=\"box\"" in loop_graph
    assert "label=\"aprod(i)\", color=\"blue\"" in loop_graph


def test_basic_loop_unroll():
    ''' Check that we correctly generate a DAG for a subroutine containing
    a simple loop that we unroll once '''
    options = Options()
    options.unroll_factor = 2
    make_dag.runner(options,
                    [os.path.join(BASE_PATH, "basic_loop.f90")])
    graph_file = os.path.join(PWD, "basic_loop_routine_loop1_unroll2.gv")
    assert os.path.isfile(graph_file)
    with open(graph_file, "r") as fout:
        loop_graph = fout.read()
    # TODO need to find some way of testing the connectivity of the nodes,
    # not just their existence
    assert "label=\"2.0\", color=\"green\"" in loop_graph
    assert "label=\"i\", color=\"black\"" in loop_graph
    assert "label=\"i+1\", color=\"black\"" in loop_graph
    assert "label=\"*\", color=\"red\", shape=\"box\"" in loop_graph
    assert "label=\"aprod(i)\", color=\"blue\"" in loop_graph
    assert "label=\"aprod(i+1)\", color=\"blue\"" in loop_graph


def test_main_routine_no_file_err():
    ''' Test that we raise expected error if the user doesn't supply the name
    of a Fortran file to process '''
    args = []
    with pytest.raises(IOError) as err:
        make_dag.main(args)
    assert "The name of a Fortran source file must be provided" in str(err)


def test_main_routine_file_not_present_err():
    ''' Test that we raise expected error if the file specified by the user
    doesn't exist '''
    args = ["not_a_file"]
    with pytest.raises(IOError) as err:
        make_dag.main(args)
    assert (
        "The specified source file ('not_a_file') does not exist" in str(err))


def test_main_routine_valid_file():
    ''' Test that we can run the main routine of Habakkuk and that we get
    the expected dag written to file '''
    args = ["test_files/basic_loop.f90"]
    make_dag.main(args)
    graph_file = os.path.join(PWD, "basic_loop_routine_loop1.gv")
    assert os.path.isfile(graph_file)
    with open(graph_file, "r") as fout:
        graph = fout.read()
    assert "label=\"2.0\", color=\"green\"" in graph
    assert "label=\"i\", color=\"black\"" in graph
    assert "label=\"*\", color=\"red\", shape=\"box\"" in graph
    assert "label=\"aprod(i)\", color=\"blue\"" in graph


def test_main_routine_prune_scalar_temporaries():
    ''' Test that we can run the main routine of Habakkuk and request
    that any nodes representing scalar temporaries be pruned from the dag
    that is constructed '''
    args = ["--rm-scalar-tmps", "test_files/two_different_duplicate_op.f90"]
    make_dag.main(args)
    graph_file = os.path.join(PWD, "test_duplicate_prod_div.gv")
    assert os.path.isfile(graph_file)


def test_main_routine_invalid_fortran():
    ''' Check that we raise the expected error if given a source file that
    is not valid Fortran '''
    from parse2003 import ParseError
    args = ["make_dag_test.py"]
    with pytest.raises(ParseError) as err:
        make_dag.main(args)
    print str(err)
    assert "Parse Error: Parsing " in str(err)
    assert "Is the file valid Fortran?" in str(err)
