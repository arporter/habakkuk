
''' This module contains tests of the schedule-generation capabilities
    of Habakkuk. '''

import pytest
from habakkuk.fparser import Fortran2003
from habakkuk.dag import DirectedAcyclicGraph


def test_addition_schedule():
    ''' Check the construction of a schedule of a very simple DAG '''
    assign = Fortran2003.Assignment_Stmt("a = b + c")
    dag = DirectedAcyclicGraph("Test dag")
    mapping = {}
    dag.add_assignments([assign], mapping)
    node_names = []
    plus_node = None
    for node in dag._nodes.itervalues():
        node_names.append(node.name)
        if node.name == "+":
            plus_node = node
    
    assert "a" in node_names
    assert "b" in node_names
    assert "+" in node_names
    input_nodes = dag.input_nodes()
    for node in input_nodes:
        assert not node.ready
        node.mark_ready()
    # The addition node should now be ready to go...
    assert plus_node.dependencies_satisfied
    # ...but not actually marked as executed...
    assert not plus_node.ready
    # Now generate the schedule and check that it only has a single
    # step and that the total cost is just the cost of the
    # addition operation
    nsteps, schedule = dag.generate_schedule()
    assert nsteps == 1
    from habakkuk.config_ivy_bridge import OPERATORS
    from habakkuk.dag import schedule_cost
    cost = schedule_cost(nsteps, schedule)
    assert cost == OPERATORS["+"]["cost"]


def test_exp_schedule():
    ''' Check that we correctly schedule (the instructions for) the
    ** intrinsic operation '''
    from habakkuk.dag import schedule_cost
    assign = Fortran2003.Assignment_Stmt("a = b**c")
    dag = DirectedAcyclicGraph("Test dag")
    mapping = {}
    dag.add_assignments([assign], mapping)
    node_names = [node.name for node in dag._nodes.itervalues()]
    assert "**" in node_names
    nsteps, schedule = dag.generate_schedule()
    cost = schedule_cost(nsteps, schedule)
    print cost
    assert nsteps > 0
    assert cost > 0
    
