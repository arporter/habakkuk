
''' This module contains tests of the schedule-generation capabilities
    of Habakkuk. '''

import pytest
import os
from fparser import Fortran2003
from habakkuk.dag import DirectedAcyclicGraph, DAGError
from test_utilities import Options


def test_schedule_too_long():
    ''' Check that we raise the expected error if the computed schedule
    is too long '''
    from habakkuk import dag, make_dag
    old_max_length = dag.MAX_SCHEDULE_LENGTH
    dag.MAX_SCHEDULE_LENGTH = 5
    fortran_text = "".join(
        ["  prod{0} = b{0} + a{0}\n".format(i) for i in range(6)])
    # For some reason the parser fails without the initial b0 = 1.0
    # assignment.
    fortran_text = ("program long_sched_test\n"
                    "  b0 = 1.0\n" + fortran_text +
                    "end program long_sched_test\n")
    tmp_file = os.path.join(os.getcwd(), "test_long_sched.f90")
    with open(tmp_file, 'w') as fout:
        fout.write(fortran_text)
    with pytest.raises(DAGError) as err:
        make_dag.dag_of_files(Options(), [tmp_file])
    # Restore the original value
    dag.MAX_SCHEDULE_LENGTH = old_max_length
    # Delete the files generated during this test
    os.remove(tmp_file)
    os.remove(os.path.join(os.getcwd(), "long_sched_test.gv"))
    for i in range(6):
        step_file = os.path.join(os.getcwd(),
                                 "long_sched_test_step{0}.gv".format(i))
        if os.path.isfile(step_file):
            os.remove(step_file)
    assert "Unexpectedly long schedule" in str(err)


def test_addition_schedule():
    ''' Check the construction of a schedule of a very simple DAG '''
    assign = Fortran2003.Assignment_Stmt("a = b + c")
    dag = DirectedAcyclicGraph("Test_dag")
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
    '**' intrinsic operation '''
    from habakkuk.dag import schedule_cost
    from habakkuk.config_ivy_bridge import OPERATORS
    assign = Fortran2003.Assignment_Stmt("a = b**c")
    dag = DirectedAcyclicGraph("Test_dag")
    mapping = {}
    dag.add_assignments([assign], mapping)
    node_names = []
    pow_node = None
    for node in dag._nodes.itervalues():
        node_names.append(node.name)
        if node.name == "**":
            pow_node = node
    assert "**" in node_names
    input_nodes = dag.input_nodes()
    for node in input_nodes:
        assert not node.ready
        node.mark_ready()
    # The pow node should now be ready to go...
    assert pow_node.dependencies_satisfied
    # ...but not actually marked as executed...
    assert not pow_node.ready
    nsteps, schedule = dag.generate_schedule()
    cost = schedule_cost(nsteps, schedule)
    print cost
    assert nsteps == 1
    assert cost == OPERATORS["**"]["cost"]


def test_sin_schedule():
    ''' Check that we correctly schedule (the instructions for) the
    'sin' intrinsic operation '''
    from habakkuk.dag import schedule_cost
    from habakkuk.config_ivy_bridge import OPERATORS
    assign = Fortran2003.Assignment_Stmt("a = sin(b)")
    dag = DirectedAcyclicGraph("Test_dag")
    mapping = {}
    dag.add_assignments([assign], mapping)
    node_names = []
    sin_node = None
    for node in dag._nodes.itervalues():
        node_names.append(node.name)
        if node.name == "SIN":
            sin_node = node
    assert "SIN" in node_names
    input_nodes = dag.input_nodes()
    for node in input_nodes:
        assert not node.ready
        node.mark_ready()
    # The sin node should now be ready to go...
    assert sin_node.dependencies_satisfied
    # ...but not actually marked as executed...
    assert not sin_node.ready
    nsteps, schedule = dag.generate_schedule()
    cost = schedule_cost(nsteps, schedule)
    print cost
    assert nsteps == 1
    assert cost == OPERATORS["SIN"]["cost"]


def test_sin_plus_schedule(capsys):
    ''' Check that we correctly schedule (the instructions for) a DAG
    containing the 'sin' intrinsic operation as well as an addition '''
    assign = Fortran2003.Assignment_Stmt("a = sin(b) + c")
    dag = DirectedAcyclicGraph("Test_dag")
    mapping = {}
    dag.add_assignments([assign], mapping)
    dag.calc_critical_path()
    dag.report()
    result, _ = capsys.readouterr()
    print result
    assert "Schedule contains 2 steps" in result
    assert "Cost of schedule as a whole = 50 cycles" in result
    assert (
        "Critical path contains 4 nodes, 41 FLOPs and is 50 cycles long" in
        result)
    assert (
        "Cost if all ops on different execution ports are perfectly "
        "overlapped = 49 cycles" in result)


def test_cos_product_schedule(capsys):
    ''' Check that we correctly schedule (the instructions for) a DAG
    containing the 'cos' intrinsic operation as well as a multiplication '''
    assign = Fortran2003.Assignment_Stmt("a = sin(b) * c")
    dag = DirectedAcyclicGraph("Test_dag")
    mapping = {}
    dag.add_assignments([assign], mapping)
    dag.calc_critical_path()
    dag.report()
    result, _ = capsys.readouterr()
    print result
    assert "Schedule contains 2 steps" in result
    assert "Cost of schedule as a whole = 50 cycles" in result
    assert (
        "Critical path contains 4 nodes, 41 FLOPs and is 50 cycles long" in
        result)
    assert (
        "Cost if all ops on different execution ports are perfectly "
        "overlapped = 50 cycles" in result)


def test_max_min_addition_schedule(capsys):
    ''' Check that we generate correct schedule when DAG contains a MAX,
    a MIN and an addition '''
    assign = Fortran2003.Assignment_Stmt("a = MIN(b,c) * MAX(c,d) + e")
    dag = DirectedAcyclicGraph("Test_dag")
    mapping = {}
    dag.add_assignments([assign], mapping)
    dag.calc_critical_path()
    dag.report()
    result, _ = capsys.readouterr()
    print result
    assert "Schedule contains 4 steps" in result
    assert "Cost of schedule as a whole = 4 cycles" in result
    assert ("Critical path contains 5 nodes, 2 FLOPs and is 3 cycles long"
            in result)
    assert ("Cost if all ops on different execution ports are perfectly "
            "overlapped = 2 cycles" in result)
