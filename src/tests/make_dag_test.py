
''' Contains pytest tests for make_dag.py '''

from dag import dag_from_strings


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

