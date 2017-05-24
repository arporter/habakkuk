
''' This module contains tests of the schedule-generation capabilities
    of Habakkuk. '''

import os
import pytest
from fparser import Fortran2003
from habakkuk.dag import DirectedAcyclicGraph, DAGError
from test_utilities import Options, dag_from_strings

# Since this is a file containing tests which often have to get in and
# change the internal state of objects we disable pylint's warning
# about such accesses
# pylint: disable=protected-access


def test_schedule_too_long(monkeypatch):
    ''' Check that we raise the expected error if the computed schedule
    is too long '''
    from habakkuk import make_dag, schedule
    # Monkeypatch the dag object to override the maximum permitted
    # schedule length with something much less
    monkeypatch.setattr(schedule, "MAX_SCHEDULE_LENGTH", value=5)
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
    from habakkuk.config_ivy_bridge import OPERATORS
    schedule = dag.schedule()
    assert schedule.nsteps == 1
    assert schedule.cost == OPERATORS["+"]["cost"]


def test_exp_schedule():
    ''' Check that we correctly schedule (the instructions for) the
    '**' intrinsic operation '''
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
    assert dag.schedule().nsteps == 1
    assert dag.schedule().cost == OPERATORS["**"]["cost"]


def test_sin_schedule():
    ''' Check that we correctly schedule (the instructions for) the
    'sin' intrinsic operation '''
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
    assert dag.schedule().nsteps == 1
    assert dag.schedule().cost == OPERATORS["SIN"]["cost"]


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


def test_div_mul_overlap():
    ''' Check that we correctly overlap independent division and
    multiplication operations '''
    from habakkuk.config_ivy_bridge import OPERATORS, div_overlap_mul_cost
    dag = dag_from_strings(["a = b * c", "d = b/c", "e = c * b"])
    cost = dag.schedule().cost
    assert cost == OPERATORS["/"]["cost"]
    # Make the division depend on the result of the first multiplication
    dag = dag_from_strings(["a = b * c", "d = b/a", "e = c * b"])
    cost = dag.schedule().cost
    assert cost == (OPERATORS["/"]["cost"] + OPERATORS["*"]["cost"])
    # Make the second product depend on the result of the division
    dag = dag_from_strings(["a = b * c", "d = b/c", "e = d * b"])
    cost = dag.schedule().cost
    assert cost == (OPERATORS["/"]["cost"] + OPERATORS["*"]["cost"])
    # 6 independent multiplications
    string_list = ["a{0} = b * c".format(idx) for idx in range(6)]
    string_list += ["d = b/c", "e = d * b"]
    dag = dag_from_strings(string_list)
    cost = dag.schedule().cost
    assert cost == (OPERATORS["/"]["cost"] + OPERATORS["*"]["cost"])
    # One more independent multiplication takes us to 7
    string_list += ["e2 = b * b"]
    dag = dag_from_strings(string_list)
    cost = dag.schedule().cost
    assert cost == div_overlap_mul_cost(7) + OPERATORS["*"]["cost"]
    # A (very unlikely) 12 independent multiplications...
    string_list = ["a{0} = b * c".format(idx) for idx in range(12)]
    string_list += ["d = b/c", "e = d * b"]
    dag = dag_from_strings(string_list)
    cost = dag.schedule().cost
    assert cost == div_overlap_mul_cost(12) + OPERATORS["*"]["cost"]


def test_sched_to_dot(tmpdir):
    ''' Check that we correctly generate dot files for each stage
    of a schedule '''
    tmpdir.chdir()
    dag = dag_from_strings(["a = b * c", "d = b/c", "e = d * b"])
    sched = dag.schedule(to_dot=True)
    assert sched.nsteps == 3
    for idx in range(0, sched.nsteps+1):
        dot_file = os.path.join(str(tmpdir),
                                "Test dag_step{0}.gv".format(idx))
        assert os.path.isfile(dot_file)
    # Check the colouring of some of the nodes in the initial dag
    dot_file = os.path.join(str(tmpdir),
                            "Test dag_step0.gv")
    with open(str(dot_file)) as dfile:
        dot = dfile.read()
        assert ('[label="b (w=0)", color="black", shape="ellipse", '
                'style="filled", fillcolor="grey"]') in dot
        assert ('[label="/ (w=8)", color="red", shape="box", '
                'height="0.58", style="filled", fillcolor="green"]') in dot
        assert ('[label="* (w=1)", color="red", shape="box", '
                'height="0.51", style="filled", fillcolor="green"]') in dot
    # The first step in the schedule is a division so that should
    # now have been done and therefore be filled with grey
    dot_file = os.path.join(str(tmpdir),
                            "Test dag_step1.gv")
    with open(str(dot_file)) as dfile:
        dot = dfile.read()
        assert ('[label="/ (w=8)", color="red", shape="box", height="0.58", '
                'style="filled", fillcolor="grey"]') in dot
    # After the final step in the schedule all nodes should have been
    # processed and therefore none should be filled with green
    dot_file = os.path.join(str(tmpdir),
                            "Test dag_step3.gv")
    with open(str(dot_file)) as dfile:
        dot = dfile.read()
        assert 'fillcolor="green"' not in dot
