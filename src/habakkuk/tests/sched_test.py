
''' This module contains tests of the schedule-generation capabilities
    of Habakkuk. '''

import pytest
from habakkuk.fparser import Fortran2003

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
    
