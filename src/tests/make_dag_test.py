
''' Contains pytest tests for make_dag.py '''

import os
from test_utilities import dag_from_strings, Options
import make_dag

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


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
