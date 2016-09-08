
''' This module tests the DAG generator using pytest '''

import os
import pytest
from parse2003 import ParseError
import make_dag
from dag_node import DAGError
from fparser import Fortran2003
from dag import dag_from_strings, DirectedAcyclicGraph

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")

CODE = '''
module test_dag
contains
  subroutine testkern_qr_code(a,b,c,d)
    integer, intent(inout) :: a, b, c, d, e

    a = b + c
    d = a + e

  end subroutine testkern_qr_code
end module test_dag
'''

class Options(object):
    ''' Class used to store what would be command-line options when
    running the tool outwith the test suite '''

    def __init__(self):
        self.no_prune = False
        self.no_fma = True
        self.rm_scalar_tmps = False
        self.show_weights = False
        self.unroll_factor = 1
        self.mode = 'auto'

# The tests themselves...


def test_is_intrinsic_err():
    ''' Check that the expected exception is raised if we pass an
    incorrect object to the is_intrinsic_fn() function '''
    from dag import is_intrinsic_fn
    fake_parse_obj = Fortran2003.Part_Ref("a(i,j)")
    # items should be a list with the first element a Name. Make
    # it a str instead by over-writing the items component.
    fake_parse_obj.items = ["a_string"]
    with pytest.raises(DAGError) as excinfo:
        is_intrinsic_fn(fake_parse_obj)
    assert "expects first item to be Name" in str(excinfo)


def test_is_intrinsic_false():
    ''' Check that is_intrinsic_fn() returns False if passed something
    that is not a Fortran intrinsic '''
    from dag import is_intrinsic_fn
    fake_parse_obj = Fortran2003.Part_Ref("a(i,j)")
    val = is_intrinsic_fn(fake_parse_obj)
    assert not val


def test_is_intrinsic_true():
    ''' Check that is_intrinsic_fn() returns False if passed something
    that is not a Fortran intrinsic '''
    from dag import is_intrinsic_fn
    fake_parse_obj = Fortran2003.Part_Ref("sin(r)")
    val = is_intrinsic_fn(fake_parse_obj)
    assert val


def test_critical_path_no_input():
    ''' Check that we raise expected error when querying a critical path
    for the input node when it has none '''
    from dag import Path
    path = Path()
    with pytest.raises(DAGError) as err:
        path.input_node()
    assert "Failed to find input node for critical path" in str(err)


def test_dag_get_node_err():
    ''' Check that we raise expected error when calling get_node() without
    a name or a variable '''
    dag = DirectedAcyclicGraph("Test dag")
    with pytest.raises(DAGError) as err:
        dag.get_node()


def test_dag_get_node_with_name():
    ''' Check that we get a valid node when calling get_node() with a
    name specified '''
    dag = DirectedAcyclicGraph("Test dag")
    dnode = dag.get_node(name="my_node")
    assert dnode.name == "my_node"


def test_dag_get_node_with_name_and_mapping():
    ''' Check that we get a valid node when calling get_node() with a
    name and a mapping specified '''
    dag = DirectedAcyclicGraph("Test dag")
    map = {"my_node":"my_node'"}
    dnode = dag.get_node(name="my_node", mapping=map)
    assert dnode.name == "my_node'"


def test_dag_get_node_unique():
    ''' Check that we get a valid node when requesting a new, unique
    node in the DAG '''
    dag = DirectedAcyclicGraph("Test dag")
    dnode = dag.get_node(parent=None, name="my_node", unique=True)
    assert not dnode.consumers
    assert not dnode.producers
    assert dnode.name == "my_node"


def test_dag_del_node():
    ''' Check that we can delete a node from the DAG when it was
    created by supplying a name. '''
    dag = DirectedAcyclicGraph("Test dag")
    map = {"my_node":"my_node'"}
    dnode = dag.get_node(name="my_node", mapping=map)
    dag.delete_node(dnode)
    assert dag.total_cost() == 0
    # Dictionary of nodes should now be empty
    assert not dag._nodes


def test_dag_del_wrong_node():
    ''' Check that we raise an appropriate error if we attempt to
    delete a node that is not part of the DAG. '''
    dag1 = DirectedAcyclicGraph("Test dag")
    dag2 = DirectedAcyclicGraph("Another dag")
    map = {"my_node": "my_node'"}
    # Create a node in the second dag
    dnode = dag2.get_node(name="my_node", mapping=map)
    # Attempt to delete it from the first dag
    with pytest.raises(DAGError) as err:
        dag1.delete_node(dnode)
    assert "Object 'my_node'' (" in str(err)
    assert "not in list of nodes in graph!" in str(err)


def test_del_sub_graph():
    ''' Check that we can delete a sub-graph from a DAG '''
    dag = DirectedAcyclicGraph("Test dag")
    dnode1 = dag.get_node(name="node_a")
    dnode2 = dag.get_node(name="node_b", parent=dnode1)
    dnode3 = dag.get_node(name="node_c", parent=dnode1)
    dnode4 = dag.get_node(name="node_d", parent=dnode3)
    dnode5 = dag.get_node(name="node_e", parent=dnode3)
    assert len(dag._nodes) == 5
    # Disconnect the sub-graph from the rest of the DAG
    dnode1.rm_producer(dnode3)
    dnode3.rm_consumer(dnode1)
    # Delete the sub graph. This should remove 3 nodes.
    dag.delete_sub_graph(dnode3)
    assert len(dag._nodes) == 2
    assert "node_e" not in dag._nodes
    assert "node_d" not in dag._nodes
    assert "node_c" not in dag._nodes


def test_multi_op_err():
    ''' Check that we raise the expected error if we encounter a node
    that has >1 operator as a child '''
    assign = Fortran2003.Assignment_Stmt("a = b + c + d")
    dag = DirectedAcyclicGraph("Test dag")
    mapping = {}
    tmp_node = dag.get_node(parent=None,
                            name="tmp_node",
                            unique=True)
    # items is a tuple and tuples are immutable. We therefore have
    # to create a new tuple with the last element replaced by a second
    # addition operator
    item2 = assign.items[2]
    item2.items = (item2.items[0], item2.items[1], "+")
    with pytest.raises(DAGError) as err:
        dag.make_dag(tmp_node, assign.items[2:], mapping)
    assert "Found more than one operator amongst list of siblings" in str(err)


def test_intrinsic_call():
    ''' Test that the correct DAG is created from an assignment involving
    a call to an intrinsic. '''
    assign = Fortran2003.Assignment_Stmt("a = sin(b)")
    dag = DirectedAcyclicGraph("Sin dag")
    mapping = {}
    tmp_node = dag.get_node(parent=None,
                            name="tmp_node",
                            unique=True)
    dag.make_dag(tmp_node, assign.items[2:], mapping)
    node_names = []
    for key, node in dag._nodes.iteritems():
        node_names.append(node.name)
        if node.name == "SIN":
            assert node.node_type == "intrinsic"
    assert "SIN" in node_names
    assert "b" in node_names


def test_rm_scalar_tmps():
    ''' Test the code that removes nodes that represent scalar tempories
    from the DAG '''
    from parse2003 import Variable
    dag = dag_from_strings(["a = 2.0 * b", "c = 2.0 * a"])
    node_names = [node.name for node in dag._nodes.itervalues()]
    # Check that the resulting dag has the right nodes
    assert node_names.count("a") == 1
    assert node_names.count("b") == 1
    assert node_names.count("c") == 1
    assert node_names.count("*") == 2
    # Now delete any scalar temporaries - this should remove node 'a'
    dag.rm_scalar_temporaries()
    node_names = [node.name for node in dag._nodes.itervalues()]
    assert "a" not in node_names
    assert node_names.count("b") == 1
    assert node_names.count("c") == 1
    assert node_names.count("*") == 2


def test_basic_scalar_dag(capsys):
    ''' Test basic operation with some simple Fortran containing the
    product of three scalar variables '''
    make_dag.runner(Options(),
                    [os.path.join(BASE_PATH, "triple_product.f90")])
    result, _ = capsys.readouterr()
    print result
    assert "Wrote DAG to test_triple_product.gv" in result
    assert "2 multiplication operators." in result
    assert "2 FLOPs in total." in result
    assert "Did not find any array/memory references" in result
    assert "Schedule contains 2 steps:" in result


@pytest.mark.xfail(reason="Currently only Intel Ivybridge microarchitecture "
                   "is supported and that doesn't have an FMA")
def test_basic_fma(capsys):
    ''' Test basic operation of tool's ability to spot opportunities for
    fused multiply-add instructions '''
    options = Options()
    options.no_fma = False
    make_dag.runner(options,
                    [os.path.join(BASE_PATH, "fma_test.f90")])
    result, _ = capsys.readouterr()
    print result


def test_array_readwrite_no_fma(capsys):
    ''' Test the analysis of code of the form x(i) = a + x(i) without
    attempting to spot opportunities for Fused Multiply Adds '''
    options = Options()
    options.no_fma = True

    make_dag.runner(options,
                    [os.path.join(BASE_PATH, "shallow_loop11.f90")])
    result, _ = capsys.readouterr()
    print result
    assert "6 addition operators." in result
    assert "3 subtraction operators." in result
    assert "6 multiplication operators." in result
    assert "15 FLOPs in total." in result
    assert "9 array references." in result
    assert "Sum of cost of all nodes = 15" in result


@pytest.mark.xfail(reason="Currently only Intel Ivybridge microarchitecture "
                   "is supported and that doesn't have an FMA")
def test_array_readwrite_with_fma(capsys):
    ''' Test the analysis of code of the form x(i) = a + x(i) '''
    options = Options()
    options.no_fma = False

    make_dag.runner(options,
                    [os.path.join(BASE_PATH, "shallow_loop11.f90")])
    result, _ = capsys.readouterr()
    print result
    assert "6 addition operators." in result
    assert "3 subtraction operators." in result
    assert "6 multiplication operators." in result
    assert "15 FLOPs in total." in result
    assert "9 array references." in result
    assert "Sum of cost of all nodes = 15" in result


@pytest.mark.xfail(reason="parser fails to generate a "
                   "Fortran2003.Execution_Part object when first executable "
                   "statement is an assignment to an array element")
def test_array_assign():
    ''' Test that the parser copes if the first executable statement is an
    array assignment '''
    make_dag.runner(Options(),
                    [os.path.join(BASE_PATH, "first_line_array_assign.f90")])
    result, _ = capsys.readouterr()


def test_repeated_assign_array(capsys):
    ''' Test that we get correctly-named nodes when it is an array reference
    that is repeatedly assigned to. '''
    make_dag.runner(Options(),
                    [os.path.join(BASE_PATH, "repeated_array_assign.f90")])
    result, _ = capsys.readouterr()
    fout = open('test_repeated_assign1.gv', 'r')
    graph = fout.read()
    fout.close()
    print graph
    print result
    node1 = "label=\"aprod(i,j)\", color=\"blue\""
    node2 = "label=\"aprod'(i,j)\", color=\"blue\""
    assert node1 in graph
    assert node2 in graph


def test_repeated_assing_1darray_slice():
    ''' Test that we get correctly-named nodes when it is an array slice
    that is repeatedly assigned to. '''
    make_dag.runner(Options(),
                    [os.path.join(BASE_PATH, "repeated_array_assign.f90")])
    with open('test_repeated_assign4.gv', 'r') as fout:
        graph = fout.read()
    print graph
    assert "label=\"aprod(:)\", color=\"blue\"" in graph
    assert "label=\"aprod'(:)\", color=\"blue\"" in graph


def test_repeated_assign_2darray_slice():
    ''' Test that we get correctly-named nodes when it is an array slice
    that is repeatedly assigned to. '''
    #dag = dag_from_strings(["a(:) = 2.0 * a(:)"])
    #node_names = [node.name for node in dag._nodes.itervalues()]
    #assert "a'(:)" in node_names
    #assert "a(:)" in node_names
    make_dag.runner(Options(),
                    [os.path.join(BASE_PATH, "repeated_array_assign.f90")])
    with open('test_repeated_assign3.gv', 'r') as fout:
        graph = fout.read()
    print graph
    assert "label=\"aprod(:,j)\", color=\"blue\"" in graph
    assert "label=\"aprod'(:,j)\", color=\"blue\"" in graph


def test_write_back_array(capsys):
    ''' Test that we get correctly-named nodes when it is an array reference
    that is read from and written to in the first statement we encounter it. '''
    make_dag.runner(Options(),
                    [os.path.join(BASE_PATH, "repeated_array_assign.f90")])
    result, _ = capsys.readouterr()
    fout = open('test_repeated_assign2.gv', 'r')
    graph = fout.read()
    fout.close()
    print graph
    print result
    node1 = "label=\"aprod(i,j)\", color=\"blue\""
    node2 = "label=\"aprod'(i,j)\", color=\"blue\""
    node3 = "label=\"aprod''(i,j)\", color=\"blue\""
    assert node1 in graph
    assert node2 in graph
    assert node3 in graph
    assert graph.count("aprod") == 3


def test_repeated_assign_diff_elements(capsys):
    ''' Test that we get correctly-named nodes when different elements of
    the same array are accessed in a code fragment '''
    make_dag.runner(Options(),
                    [os.path.join(BASE_PATH,
                                  "repeated_array_assign_diff_elements.f90")])
    result, _ = capsys.readouterr()
    fout = open('test_repeated_assign_diff_elems.gv', 'r')
    graph = fout.read()
    fout.close()
    print graph
    assert "label=\"aprod(i,j)\", color=\"blue\"" in graph
    assert "label=\"aprod(i+1,j)\", color=\"blue\"" in graph
    assert "label=\"aprod'(i,j)\", color=\"blue\"" in graph
    assert graph.count("aprod") == 3


def test_node_display(capsys):
    ''' Test the display method of DAGNode '''
    dag = dag_from_strings(["aprod = var1 * var2 * var3",
                            "bprod = var1 * var2 / var3",
                            "cprod = var1 * var2 + var3"])
    node = dag._nodes["aprod"]
    assert node.name == "aprod"
    node.display()
    result, _ = capsys.readouterr()
    print result
    expected = (
        " aprod\n"
        "      *\n"
        "           *\n"
        "                var1\n"
        "                var2\n"
        "           var3\n")
    assert expected in result


def test_node_name():
    ''' Test the setter method for node name '''
    dag = dag_from_strings(["aprod = var1 * var2 * var3",
                            "bprod = var1 * var2 / var3",
                            "cprod = var1 * var2 + var3"])
    node = dag._nodes["bprod"]
    assert node.name == "bprod"
    node.name = "bprod_renamed"
    assert node.name == "bprod_renamed"


def test_node_rm_producer():
    ''' Test the rm_producer method of DAGNode '''
    dag = dag_from_strings(["aprod = var1 * var2 * var3",
                            "bprod = var1 * var2",
                            "cprod = var1 * var2 + var3"])
    # bprod does not depend on var3 so we should get an
    # error when we attempt to remove it.
    bnode = dag._nodes["bprod"]
    var3node = dag._nodes["var3"]
    with pytest.raises(DAGError) as err:
        bnode.rm_producer(var3node)
    assert "is not a producer (dependency) for this node" in str(err)


def test_node_rm_consumer():
    ''' Test the rm_consumer method of DAGNode '''
    dag = dag_from_strings(["aprod = var1 * var2",
                            "bprod = var1 * var2 / var3",
                            "cprod = var1 * var2 + var3"])
    # aprod does not depend on var3 so we should get an error when
    # we attempt to remove it.
    anode = dag._nodes["aprod"]
    var3node = dag._nodes["var3"]
    with pytest.raises(DAGError) as err:
        var3node.rm_consumer(anode)
    assert " as a consumer!" in str(err)


def test_node_type_setter():
    dag = dag_from_strings(["aprod = var1 * var2",
                            "bprod = var1 * var2 / var3",
                            "cprod = var1 * var2 + var3"])
    anode = dag._nodes["aprod"]
    with pytest.raises(DAGError) as err:
        anode.node_type = "not-a-type"
    assert ("node_type must be one of ['+', '*', '-', '/', 'intrinsic', "
            "'constant', 'array_ref'] but got 'not-a-type'" in str(err))


def test_node_is_op():
    ''' Check that the is_operator method works as expected '''
    dag = dag_from_strings(["aprod = var1 * var2",
                            "bprod = var1 * var2 / var3",
                            "cprod = var1 * var2 + var3"])
    anode = dag._nodes["aprod"]
    assert not anode.is_operator
    for node in dag._nodes.itervalues():
        if node.name == "*":
            break
    assert node.is_operator


def test_node_walk_too_deep():
    ''' Check that the walk() method aborts correctly if the recursion
    depth is too great '''
    import dag_node
    dag = dag_from_strings(["xprod = 1.0",
                            "var1 = 2.0",
                            "aprod = var1 * var2 * xprod",
                            "bprod = var1 * var2 / aprod",
                            "cprod = var1 * var2 + bprod"])
    cnode = dag._nodes["cprod"]
    old_recursion_depth = dag_node.MAX_RECURSION_DEPTH
    dag_node.MAX_RECURSION_DEPTH = 2
    with pytest.raises(DAGError) as err:
        cnode.walk()
    dag_node.MAX_RECURSION_DEPTH = old_recursion_depth
    assert "Max recursion depth (2) exceeded when walking tree" in str(err)


def test_node_weight_intrinsic():
    ''' Check that node.weight() works for an intrinsic '''
    dag = dag_from_strings(["var1 = sin(2.0)",
                            "aprod = var1 * var2",
                            "bprod = var1 * var2 / aprod",
                            "cprod = var1 * var2 + bprod"])
    for node in dag._nodes.itervalues():
        if node.name.lower() == "sin":
            break
    assert node.node_type == "intrinsic"
    # Currently we only support the Ivy Bridge architecture
    from config_ivy_bridge import FORTRAN_INTRINSICS
    assert node.weight == FORTRAN_INTRINSICS["SIN"]


def test_node_dot_colours():
    ''' Check that the dot output has nodes coloured correctly '''
    dag = dag_from_strings(["var1 = sin(2.0)",
                            "aprod = var1 * var2",
                            "bprod = var1 * var2 / aprod",
                            "cprod = var1 * var2 + bprod"],
                           name="dot_test")
    outnodes = dag.output_nodes()
    dag.to_dot()
    dot_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "dot_test.gv")
    with open(dot_file, 'r') as graph_file:
        graph = graph_file.read()
    print graph
    assert "label=\"SIN (w=0)\", color=\"gold\"" in graph
    assert "label=\"var1 (w=0)\", color=\"black\"" in graph
    assert "label=\"* (w=0)\", color=\"red\", shape=\"box\"" in graph
    os.remove(dot_file)


def test_prune_duplicates():
    ''' Test that we are able to identify and remove nodes representing
    duplicate computation '''
    dag = dag_from_strings(["aprod = var1 * var2 * var3",
                            "bprod = var1 * var2 / var3",
                            "cprod = var1 * var2 + var3"])
    node_names = [node.name for node in dag._nodes.itervalues()]
    assert node_names.count("*") == 4
    assert node_names.count("/") == 1
    assert node_names.count("+") == 1
    dag.prune_duplicate_nodes()
    node_names = [node.name for node in dag._nodes.itervalues()]
    assert node_names.count("*") == 2
    assert node_names.count("/") == 1
    assert node_names.count("+") == 1


def test_no_flops(capsys):
    ''' Check that we handle a DAG that contains 0 FLOPs '''
    dag = dag_from_strings(["aprod = var1",
                            "bprod = var2"])
    dag.report()
    result, _ = capsys.readouterr()
    print result
    assert "DAG contains no FLOPs so skipping performance estimate" in result


def test_schedule_too_long():
    ''' Check that we raise the expected error if the computed schedule
    is too long '''
    import dag
    old_max_length = dag.MAX_SCHEDULE_LENGTH
    dag.MAX_SCHEDULE_LENGTH = 5
    fortran_text = "".join(
        ["  prod{0} = b{0} + a{0}\n".format(i,i,i) for i in range(6)])
    # For some reason the parser fails without the initial b0 = 1.0
    # assignment.
    fortran_text = ("program long_sched_test\n"
                    "  b0 = 1.0\n" + fortran_text +
                    "end program long_sched_test\n")
    tmp_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "test_long_sched.f90")
    with open(tmp_file, 'w') as fout:
        fout.write(fortran_text)
    with pytest.raises(DAGError) as err:
        make_dag.runner(Options(), [tmp_file])
    # Restore the original value
    dag.MAX_SCHEDULE_LENGTH = old_max_length
    # Delete the files generated during this test
    os.remove(tmp_file)
    os.remove(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                           "long_sched_test.gv"))
    for i in range(6):
        os.remove(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "long_sched_test_step{0}.gv".format(i)))
    assert "Unexpectedly long schedule" in str(err)
