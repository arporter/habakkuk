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

''' This module tests the DAG generator using pytest '''

# Since this is a file containing tests which often have to get in and
# change the internal state of objects we disable pylint's warning
# about such accesses
# pylint: disable=protected-access
from __future__ import absolute_import, print_function

import os
from six import itervalues
import pytest
from fparser.two import Fortran2003
from habakkuk import make_dag
from habakkuk.dag_node import DAGError
from habakkuk.dag import DirectedAcyclicGraph
from test_utilities import dag_from_strings, Options

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


def test_is_intrinsic_err():
    ''' Check that the expected exception is raised if we pass an
    incorrect object to the is_intrinsic_fn() function '''
    from habakkuk.dag import is_intrinsic_fn
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
    from habakkuk.dag import is_intrinsic_fn
    fake_parse_obj = Fortran2003.Part_Ref("a(i,j)")
    val = is_intrinsic_fn(fake_parse_obj)
    assert not val


def test_is_intrinsic_true():
    ''' Check that is_intrinsic_fn() returns True if passed something
    that *is* a Fortran intrinsic '''
    from habakkuk.dag import is_intrinsic_fn
    fake_parse_obj = Fortran2003.Part_Ref("sin(r)")
    val = is_intrinsic_fn(fake_parse_obj)
    assert val


def test_max_is_intrinsic():
    ''' Check that we recognise max as a Fortran intrinsic '''
    from habakkuk.dag import is_intrinsic_fn
    fake_parse_obj = Fortran2003.Part_Ref("max(r,p)")
    val = is_intrinsic_fn(fake_parse_obj)
    assert val
    fake_parse_obj = Fortran2003.Part_Ref("MAX(r,p)")
    val = is_intrinsic_fn(fake_parse_obj)
    assert val


def test_min_is_intrinsic():
    ''' Check that we recognise min as a Fortran intrinsic '''
    from habakkuk.dag import is_intrinsic_fn
    fake_parse_obj = Fortran2003.Part_Ref("min(r,p)")
    val = is_intrinsic_fn(fake_parse_obj)
    assert val
    fake_parse_obj = Fortran2003.Part_Ref("MIN(r,p)")
    val = is_intrinsic_fn(fake_parse_obj)
    assert val


def test_critical_path_no_input():
    ''' Check that we raise expected error when querying a critical path
    for the input node when it has none '''
    from habakkuk.dag import Path
    path = Path()
    with pytest.raises(DAGError) as err:
        path.input_node()
    assert "Failed to find input node for critical path" in str(err)


def test_critical_path_length():
    ''' Test that computed critical path is correct '''
    dag = dag_from_strings(["var1 = 3.0 + 2.0", "aprod(i+1) = 2.0 * var1"])
    dag.calc_critical_path()
    path = dag.critical_path
    assert path.cycles() == 2
    node_names = [node.name for node in path.nodes]
    assert "var1" in node_names
    assert "aprod(i+1)" in node_names


def test_differ_by_const():
    ''' Check that differ_by_constant() works correctly for expressions
    involving array accesses '''
    from habakkuk.dag import differ_by_constant
    dag = dag_from_strings(["var(i) = 2.0", "var(i+1) = 3.0"])
    node1 = dag._nodes["var(i)"].producers[0]
    node2 = dag._nodes["var(i+1)"].producers[0]
    assert differ_by_constant(node1, node2)

    dag = dag_from_strings(["var(i) = 2.0", "var(j) = 3.0"])
    node1 = dag._nodes["var(i)"].producers[0]
    node2 = dag._nodes["var(j)"].producers[0]
    assert not differ_by_constant(node1, node2)

    dag = dag_from_strings(["var(i) = 2.0", "var(i*2) = 3.0"])
    node1 = dag._nodes["var(i)"].producers[0]
    node2 = dag._nodes["var(i*2)"].producers[0]
    assert not differ_by_constant(node1, node2)

    dag = dag_from_strings(["var(map(i)) = 2.0", "var(map(i)+1) = 3.0"])
    node1 = dag._nodes["var(map(i))"].producers[0]
    node2 = dag._nodes["var(map(i)+1)"].producers[0]
    assert differ_by_constant(node1, node2)

    dag = dag_from_strings(["var(map(i+1)) = 2.0", "var(map(i)+1) = 3.0"])
    node1 = dag._nodes["var(map(i+1))"].producers[0]
    node2 = dag._nodes["var(map(i)+1)"].producers[0]
    assert not differ_by_constant(node1, node2)


def test_prune_constants():
    ''' Check that dag.prune_constants() works correctly '''
    import copy
    from habakkuk.dag import prune_array_index_constants
    dag = dag_from_strings(["b((1+map(i+1))+j, i) = 1.0",
                            "b((1+map(i))+j, i) = 1.0",
                            "b(1+2*(ji+2), i) = 1.0",
                            "b(1+(map(i)+j)+1, i) = 1.0"])
    node1 = dag._nodes["b(1+(map(i)+j)+1,i)"].producers[0]
    node2 = dag._nodes["b((1+map(i))+j,i)"].producers[0]
    # prune_constants() modifies the original tree so we take
    # copies to play with (just as differ_by_constant() does)
    new1 = copy.deepcopy(node1)
    new2 = copy.deepcopy(node2)
    # Sanity check that the initial tree is what we expect
    constant_nodes = new1.walk(node_type="constant")
    assert len(constant_nodes) == 2
    root1 = prune_array_index_constants(new1)
    root2 = prune_array_index_constants(new2)
    assert root1.node_type == "+"
    constant_nodes = root1.walk(node_type="constant")
    assert not constant_nodes
    assert root2.node_type == "+"
    constant_nodes = root2.walk(node_type="constant")
    assert not constant_nodes

    # Check that we do not prune a constant when it is itself inside
    # an array reference (i.e. we have an indirect memory access)
    node3 = dag._nodes["b((1+map(i+1))+j,i)"].producers[0]
    new3 = copy.deepcopy(node3)
    root3 = prune_array_index_constants(new3)
    constant_nodes = root3.walk(node_type="constant")
    assert len(constant_nodes) == 1
    map_ref = root3.walk(node_type="array_ref")
    assert map_ref[0].name == "map(i+1)"

    # Check that we don't prune a constant if it is not +/-
    node4 = dag._nodes["b(1+2*(ji+2),i)"].producers[0]
    new4 = copy.deepcopy(node4)
    root4 = prune_array_index_constants(new4)
    constant_nodes = root4.walk(node_type="constant")
    assert len(constant_nodes) == 2
    assert root4.node_type == "*"


def test_dag_get_node_err():
    ''' Check that we raise expected error when calling get_node() without
    a name or a variable '''
    dag = DirectedAcyclicGraph("Test dag")
    with pytest.raises(DAGError) as err:
        dag.get_node()
    assert "get_node: one of 'name' or 'variable' must be supplied" in str(err)


def test_dag_get_node_with_name():
    ''' Check that we get a valid node when calling get_node() with a
    name specified '''
    dag = DirectedAcyclicGraph("Test dag")
    dnode = dag.get_node(name="my_node")
    assert dnode.name == "my_node"


def test_dag_get_node_with_name_and_mapping():  # pylint: disable=invalid-name
    ''' Check that we get a valid node when calling get_node() with a
    name and a mapping specified '''
    dag = DirectedAcyclicGraph("Test dag")
    name_map = {"my_node": "my_node'"}
    dnode = dag.get_node(name="my_node", mapping=name_map)
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
    name_map = {"my_node": "my_node'"}
    dnode = dag.get_node(name="my_node", mapping=name_map)
    dag.delete_node(dnode)
    assert dag.total_cost() == 0
    # Dictionary of nodes should now be empty
    assert not dag._nodes


def test_dag_del_wrong_node():
    ''' Check that we raise an appropriate error if we attempt to
    delete a node that is not part of the DAG. '''
    dag1 = DirectedAcyclicGraph("Test dag")
    dag2 = DirectedAcyclicGraph("Another dag")
    name_map = {"my_node": "my_node'"}
    # Create a node in the second dag
    dnode = dag2.get_node(name="my_node", mapping=name_map)
    # Attempt to delete it from the first dag
    with pytest.raises(DAGError) as err:
        dag1.delete_node(dnode)
    assert "Object 'my_node'' (" in str(err)
    assert "not in list of nodes in graph!" in str(err)


def test_del_sub_graph():
    ''' Check that we can delete a sub-graph from a DAG '''
    dag = DirectedAcyclicGraph("Test dag")
    dnode1 = dag.get_node(name="node_a")
    _ = dag.get_node(name="node_b", parent=dnode1)
    dnode3 = dag.get_node(name="node_c", parent=dnode1)
    _ = dag.get_node(name="node_d", parent=dnode3)
    _ = dag.get_node(name="node_e", parent=dnode3)
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
    for node in itervalues(dag._nodes):
        node_names.append(node.name)
        if node.name == "SIN":
            assert node.node_type == "SIN"
    assert "SIN" in node_names
    assert "b" in node_names


def test_max_intrinsic_call():
    ''' Test that the correct DAG is created from an assignment involving
    a call to the max intrinsic. '''
    assign = Fortran2003.Assignment_Stmt("a = max(b,c)")
    dag = DirectedAcyclicGraph("Max dag")
    mapping = {}
    tmp_node = dag.get_node(parent=None,
                            name="tmp_node",
                            unique=True)
    dag.make_dag(tmp_node, assign.items[2:], mapping)
    node_names = []
    for node in itervalues(dag._nodes):
        node_names.append(node.name)
        if node.name == "MAX":
            assert node.node_type == "MAX"
            max_node = node
    prod_names = [pnode.name for pnode in max_node.producers]
    assert "MAX" in node_names
    assert "b" in prod_names
    assert "c" in prod_names


def test_min_intrinsic_call():
    ''' Test that the correct DAG is created from an assignment involving
    a call to the min intrinsic. '''
    assign = Fortran2003.Assignment_Stmt("a = min(b,c,d)")
    dag = DirectedAcyclicGraph("Max_dag")
    mapping = {}
    tmp_node = dag.get_node(parent=None,
                            name="tmp_node",
                            unique=True)
    dag.make_dag(tmp_node, assign.items[2:], mapping)
    node_names = []
    for node in itervalues(dag._nodes):
        node_names.append(node.name)
        if node.name == "MIN":
            assert node.node_type == "MIN"
            min_node = node
    prod_names = [pnode.name for pnode in min_node.producers]
    assert "MIN" in node_names
    assert "b" in prod_names
    assert "c" in prod_names
    assert "d" in prod_names


def test_rm_scalar_tmps():
    ''' Test the code that removes nodes that represent scalar tempories
    from the DAG '''
    dag = dag_from_strings(["a = 2.0 * b", "c = 2.0 * a"])
    node_names = [node.name for node in itervalues(dag._nodes)]
    # Check that the resulting dag has the right nodes
    assert node_names.count("a") == 1
    assert node_names.count("b") == 1
    assert node_names.count("c") == 1
    assert node_names.count("*") == 2
    # Now delete any scalar temporaries - this should remove node 'a'
    dag.rm_scalar_temporaries()
    node_names = [node.name for node in itervalues(dag._nodes)]
    assert "a" not in node_names
    assert node_names.count("b") == 1
    assert node_names.count("c") == 1
    assert node_names.count("*") == 2


def test_basic_scalar_dag(tmpdir, capsys):
    ''' Test basic operation with some simple Fortran containing the
    product of three scalar variables '''
    os.chdir(str(tmpdir.mkdir("tmp")))
    make_dag.dag_of_files(Options(),
                          [os.path.join(BASE_PATH, "triple_product.f90")])
    result, _ = capsys.readouterr()
    print(result)
    assert "Wrote DAG to test_triple_product.gv" in result
    assert "2 multiplication operators." in result
    assert "2 FLOPs in total." in result
    assert "Did not find any array/memory references" in result
    assert "Schedule contains 2 steps:" in result


@pytest.mark.xfail(reason="Currently only Intel Ivybridge microarchitecture "
                   "is supported and that doesn't have an FMA")
def test_basic_fma(tmpdir, capsys):
    ''' Test basic operation of tool's ability to spot opportunities for
    fused multiply-add instructions '''
    os.chdir(str(tmpdir.mkdir("tmp")))
    options = Options()
    options.no_fma = False
    make_dag.dag_of_files(options,
                          [os.path.join(BASE_PATH, "fma_test.f90")])
    result, _ = capsys.readouterr()
    print(result)
    assert "Ivybridge architecture does not have FMA" in result


def test_array_readwrite_no_fma(tmpdir, capsys):
    ''' Test the analysis of code of the form x(i) = a + x(i) without
    attempting to spot opportunities for Fused Multiply Adds '''
    options = Options()
    options.no_fma = True

    os.chdir(str(tmpdir.mkdir("tmp")))

    make_dag.dag_of_files(options,
                          [os.path.join(BASE_PATH, "shallow_loop11.f90")])
    result, _ = capsys.readouterr()
    print(result)
    assert "6 addition operators." in result
    assert "3 subtraction operators." in result
    assert "6 multiplication operators." in result
    assert "15 FLOPs in total." in result
    assert "9 array references." in result
    assert "Sum of cost of all nodes = 15" in result


@pytest.mark.xfail(reason="Currently only Intel Ivybridge microarchitecture "
                   "is supported and that doesn't have an FMA")
def test_array_readwrite_with_fma(tmpdir, capsys):
    ''' Test the analysis of code of the form x(i) = a + x(i) '''
    options = Options()
    options.no_fma = False

    os.chdir(str(tmpdir.mkdir("tmp")))

    make_dag.dag_of_files(options,
                          [os.path.join(BASE_PATH, "shallow_loop11.f90")])
    result, _ = capsys.readouterr()
    print(result)
    assert "6 addition operators." in result
    assert "3 subtraction operators." in result
    assert "6 multiplication operators." in result
    assert "15 FLOPs in total." in result
    assert "9 array references." in result
    assert "Sum of cost of all nodes = 15" in result


@pytest.mark.xfail(reason="parser fails to generate a "
                   "Fortran2003.Execution_Part object when first executable "
                   "statement is an assignment to an array element")
def test_array_assign(tmpdir, capsys):
    ''' Test that the parser copes if the first executable statement is an
    array assignment '''
    os.chdir(str(tmpdir.mkdir("tmp")))
    make_dag.dag_of_files(Options(),
                          [os.path.join(BASE_PATH,
                                        "first_line_array_assign.f90")])
    result, _ = capsys.readouterr()
    print(result)
    assert "Stats for DAG" in result


def test_repeated_assign_array(tmpdir, capsys):
    ''' Test that we get correctly-named nodes when it is an array reference
    that is repeatedly assigned to. '''
    os.chdir(str(tmpdir.mkdir("tmp")))
    make_dag.dag_of_files(Options(),
                          [os.path.join(BASE_PATH,
                                        "repeated_array_assign.f90")])
    result, _ = capsys.readouterr()
    fout = open('test_repeated_assign1.gv', 'r')
    graph = fout.read()
    fout.close()
    print(graph)
    print(result)
    node1 = "label=\"aprod(i,j)\", color=\"blue\""
    node2 = "label=\"aprod'(i,j)\", color=\"blue\""
    assert node1 in graph
    assert node2 in graph


def test_repeated_assign_1darray_slice(tmpdir):  # pylint: disable=invalid-name
    ''' Test that we get correctly-named nodes when it is an array slice
    that is repeatedly assigned to. '''
    os.chdir(str(tmpdir.mkdir("tmp")))
    make_dag.dag_of_files(Options(),
                          [os.path.join(BASE_PATH,
                                        "repeated_array_assign.f90")])
    with open('test_repeated_assign4.gv', 'r') as fout:
        graph = fout.read()
    print(graph)
    assert "label=\"aprod(:)\", color=\"blue\"" in graph
    assert "label=\"aprod'(:)\", color=\"blue\"" in graph


def test_repeated_assign_1darr_slice_string():  # pylint: disable=invalid-name
    ''' Test that we get correctly-named nodes when it is an array slice
    that is repeatedly assigned to and we generate the dag from strings. '''
    dag = dag_from_strings(["a(:) = 2.0 * a(:)"])
    node_names = [node.name for node in itervalues(dag._nodes)]
    assert "a'(:)" in node_names
    assert "a(:)" in node_names


def test_repeated_assign_2darr_slice(tmpdir):  # pylint: disable=invalid-name
    ''' Test that we get correctly-named nodes when it is an array slice
    that is repeatedly assigned to. '''
    os.chdir(str(tmpdir.mkdir("tmp")))
    make_dag.dag_of_files(Options(),
                          [os.path.join(BASE_PATH,
                                        "repeated_array_assign.f90")])
    with open('test_repeated_assign3.gv', 'r') as fout:
        graph = fout.read()
    print(graph)
    assert "label=\"aprod(:,j)\", color=\"blue\"" in graph
    assert "label=\"aprod'(:,j)\", color=\"blue\"" in graph


def test_write_back_array(tmpdir, capsys):
    ''' Test that we get correctly-named nodes when it is an array reference
    that is read from and written to in the first statement we encounter
    it. '''
    os.chdir(str(tmpdir.mkdir("tmp")))
    make_dag.dag_of_files(Options(),
                          [os.path.join(BASE_PATH,
                                        "repeated_array_assign.f90")])
    result, _ = capsys.readouterr()
    fout = open('test_repeated_assign2.gv', 'r')
    graph = fout.read()
    fout.close()
    print(graph)
    print(result)
    node1 = "label=\"aprod(i,j)\", color=\"blue\""
    node2 = "label=\"aprod'(i,j)\", color=\"blue\""
    node3 = "label=\"aprod''(i,j)\", color=\"blue\""
    assert node1 in graph
    assert node2 in graph
    assert node3 in graph
    assert graph.count("aprod") == 3


def test_repeated_assign_diff_elements(tmpdir):  # pylint: disable=invalid-name
    ''' Test that we get correctly-named nodes when different elements of
    the same array are accessed in a code fragment '''
    os.chdir(str(tmpdir.mkdir("tmp")))
    make_dag.dag_of_files(
        Options(),
        [os.path.join(BASE_PATH,
                      "repeated_array_assign_diff_elements.f90")])
    with open('test_repeated_assign_diff_elems.gv', 'r') as fout:
        graph = fout.read()
    print(graph)
    assert "label=\"aprod(i,j)\", color=\"blue\"" in graph
    assert "label=\"aprod(i+1,j)\", color=\"blue\"" in graph
    assert "label=\"aprod'(i,j)\", color=\"blue\"" in graph
    assert graph.count("aprod") == 3


def test_repeated_assign_index():
    ''' Test naming of nodes when an array is repeated assigned to and one of
    its indices has been assigned to more than once too. '''
    dag = dag_from_strings(
        ["ik = mbkt(ji,jj)",
         "ik = mikt(ji,jj)",
         "ptsd(ji,jj,ik,jp_tem) = (1.-zl) * ptsd(ji,jj,ik,jp_tem) + "
         "zl * ptsd(ji,jj,ik+1,jp_tem)"])
    node_names = [node.name for node in itervalues(dag._nodes)]
    print(node_names)
    assert "ptsd'(ji,jj,ik',jp_tem)" in node_names
    assert "ptsd(ji,jj,ik',jp_tem)" in node_names


def test_node_display(capsys):
    ''' Test the display method of DAGNode '''
    dag = dag_from_strings(["aprod = var1 * var2 * var3",
                            "bprod = var1 * var2 / var3",
                            "cprod = var1 * var2 + var3"])
    node = dag._nodes["aprod"]
    assert node.name == "aprod"
    node.display()
    result, _ = capsys.readouterr()
    print(result)
    # For clarity, we escape the '\' character in the string below...
    expected = (
        "\\- aprod\n"
        "  \\- *\n"
        "    \\- *\n"
        "      \\- var1\n"
        "      \\- var2\n"
        "    \\- var3\n")
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


def test_node_int_consumers():
    ''' Test the DAGNode.has_producer/consumer methods '''
    dag = dag_from_strings(["i = 2", "aprod(i) = 2.0"])
    anode = dag._nodes["aprod(i)"]
    assert anode.has_producer
    assert not anode.has_consumer
    for node in itervalues(dag._nodes):
        if node.name == "2.0":
            twonode = node
            break
    assert not twonode.has_producer
    assert twonode.has_consumer


def test_node_type_setter():
    ''' Test the node-type setter method of DAGNode '''
    dag = dag_from_strings(["aprod = var1 * var2",
                            "bprod = var1 * var2 / var3",
                            "cprod = var1 * var2 + var3"])
    anode = dag._nodes["aprod"]
    with pytest.raises(DAGError) as err:
        anode.node_type = "not-a-type"
    print(str(err))
    assert ("node_type must be one of ['*', '**', '+', '-', '/', 'ABS', "
            "'ACOS', 'ATAN', 'COS', 'COUNT', 'DBLE', 'EXP', 'IACHAR', 'INT', "
            "'LOG', 'MAX', 'MIN', 'MOD', 'NINT', 'PRESENT', 'REAL', 'SIGN', "
            "'SIN', 'SQRT', 'SUM', 'TAN', 'TANH', 'TRIM', 'constant', "
            "'array_ref', 'call'] but got 'not-a-type'" in str(err))


def test_node_is_op():
    ''' Check that the is_operator method works as expected '''
    dag = dag_from_strings(["aprod = var1 * var2",
                            "bprod = var1 * var2 / var3",
                            "cprod = var1 * var2 + var3"])
    anode = dag._nodes["aprod"]
    assert not anode.is_operator
    op_node = None
    for node in itervalues(dag._nodes):
        if node.name == "*":
            op_node = node
            break
    assert op_node
    assert op_node.is_operator


def test_node_walk_too_deep(monkeypatch):
    ''' Check that the walk() method aborts correctly if the recursion
    depth is too great '''
    dag = dag_from_strings(["xprod = 1.0",
                            "var1 = 2.0",
                            "aprod = var1 * var2 * xprod",
                            "bprod = var1 * var2 / aprod",
                            "cprod = var1 * var2 + bprod"])
    cnode = dag._nodes["cprod"]
    monkeypatch.setattr("habakkuk.dag_node.MAX_RECURSION_DEPTH", 2)
    with pytest.raises(DAGError) as err:
        cnode.walk()
    assert "Max recursion depth (2) exceeded when walking tree" in str(err)


def test_walk_cyclic_dep():
    ''' Check that the walk() method aborts correctly if it detects a
    cyclic dependency. '''
    dag = dag_from_strings(["xprod = 1.0",
                            "var1 = 2.0",
                            "aprod = var1 * var2 * xprod",
                            "bprod = var1 * var2 / aprod",
                            "cprod = var1 * var2 + bprod"])
    anode = dag._nodes["aprod"]
    cnode = dag._nodes["cprod"]
    with pytest.raises(DAGError) as err:
        # Erroneously claim that anode is an ancestor of cnode
        cnode.walk(ancestor_list=[anode])
    assert ("Cyclic dependency: node '/' has node 'aprod' as both a "
            "producer and an ancestor" in str(err))


def test_node_weight_intrinsic():
    ''' Check that node.weight() works for an intrinsic '''
    dag = dag_from_strings(["var1 = sin(2.0)",
                            "aprod = var1 * var2",
                            "bprod = var1 * var2 / aprod",
                            "cprod = var1 * var2 + bprod"])
    sin_node = None
    for node in itervalues(dag._nodes):
        if node.name.lower() == "sin":
            sin_node = node
            break
    assert sin_node
    assert sin_node.node_type == "SIN"
    # Currently we only support the Ivy Bridge architecture
    from habakkuk.config_ivy_bridge import OPERATORS
    assert sin_node.weight == OPERATORS["SIN"]["cost"]


def test_node_dot_colours(tmpdir):
    ''' Check that the dot output has nodes coloured correctly '''
    os.chdir(str(tmpdir.mkdir("tmp")))
    dag = dag_from_strings(["var1 = sin(2.0)",
                            "aprod = var1 * var2",
                            "bprod = var1 * var2 / aprod",
                            "cprod = var1 * var2 + bprod"],
                           name="dot_test")
    dag.to_dot()
    dot_file = os.path.join(os.getcwd(), "dot_test.gv")
    with open(dot_file, 'r') as graph_file:
        graph = graph_file.read()
    print(graph)
    assert "label=\"SIN (w=0)\", color=\"gold\"" in graph
    assert "label=\"var1 (w=0)\", color=\"black\"" in graph
    assert "label=\"* (w=0)\", color=\"red\", shape=\"box\"" in graph


@pytest.mark.xfail(reason="Test not yet implemented")
def test_exclude_int_nodes_from_dot():
    ''' Check that we can turn-off output of integer nodes in dot '''
    assert False


def test_prune_duplicates():
    ''' Test that we are able to identify and remove nodes representing
    duplicate computation '''
    dag = dag_from_strings(["aprod = var1 * var2 * var3",
                            "bprod = var1 * var2 / var3",
                            "cprod = var1 * var2 + var3"])
    node_names = [node.name for node in itervalues(dag._nodes)]
    assert node_names.count("*") == 4
    assert node_names.count("/") == 1
    assert node_names.count("+") == 1
    dag.prune_duplicate_nodes()
    node_names = [node.name for node in itervalues(dag._nodes)]
    assert node_names.count("*") == 2
    assert node_names.count("/") == 1
    assert node_names.count("+") == 1


def test_prune_duplicate_array_refs():
    ''' Test that we are able to identify and remove nodes representing
    duplicate computation when array references are involved '''
    dag = dag_from_strings(
        ["zu = 8._wp * ( un(ji-1,jj  ,jk) * un(ji-1,jj  ,jk) "
         "+ un(ji  ,jj  ,jk) * un(ji  ,jj  ,jk) ) "
         "+ ( un(ji-1,jj-1,jk) + un(ji-1,jj+1,jk) ) * "
         "  ( un(ji-1,jj-1,jk) + un(ji-1,jj+1,jk) ) "
         "+ ( un(ji  ,jj-1,jk) + un(ji  ,jj+1,jk) ) * "
         "  ( un(ji  ,jj-1,jk) + un(ji  ,jj+1,jk) )"])
    node_names = [node.name for node in itervalues(dag._nodes)]
    assert "un(ji-1,jj-1,jk)" in node_names
    dag.prune_duplicate_nodes()
    node_names = [node.name for node in itervalues(dag._nodes)]
    # The pruning should have resulted in the introduction of just
    # two intermediate nodes
    assert "sub_exp0" in node_names
    assert "sub_exp1" in node_names
    assert "sub_exp2" not in node_names


def test_rm_scalar_tmps_array_accesses():  # pylint: disable=invalid-name
    ''' Check that we can successfully remove scalar temporaries when
    we have array accesses '''
    dag = dag_from_strings(
        ["iku = miku(ji,jj)", "ikup1 = miku(ji,jj) + 1",
         "ikv = mikv(ji,jj)", "ikvp1 = mikv(ji,jj) + 1",
         "ze3wu  = (gdepw_0(ji+1,jj,ikup1) - gdept_0(ji+1,jj,iku)) - "
         "(gdepw_0(ji,jj,iku+1) - gdept_0(ji,jj,iku))",
         "ze3wv  = (gdepw_0(ji,jj+1,ikvp1) - gdept_0(ji,jj+1,ikv)) - "
         "(gdepw_0(ji,jj,ikv+1) - gdept_0(ji,jj,ikv))",
         "pgzui  (ji,jj) = (gdep3w_0(ji+1,jj,iku) + ze3wu) - "
         "gdep3w_0(ji,jj,iku)"])
    dag.rm_scalar_temporaries()
    node_names = [node.name for node in itervalues(dag._nodes)]
    assert "index" not in node_names


def test_no_flops(capsys):
    ''' Check that we handle a DAG that contains 0 FLOPs '''
    dag = dag_from_strings(["aprod = var1",
                            "bprod = var2"])
    dag.report()
    result, _ = capsys.readouterr()
    print(result)
    assert "DAG contains no FLOPs so skipping performance estimate" in result


def test_mult_operand(tmpdir):
    ''' Test that we handle the case where the Fortran parser generates
    a Mult_Operand object '''
    os.chdir(str(tmpdir.mkdir("tmp")))
    make_dag.dag_of_files(
        Options(), [os.path.join(BASE_PATH,
                                 "pert_pressure_gradient_kernel_mod.F90")])
    out_file = os.path.join(os.getcwd(),
                            "pert_pressure_gradient_code_loop6.gv")
    assert os.path.isfile(out_file)
    with open(out_file, 'r') as fout:
        graph = fout.read()
    print(graph)
    # Check that we have power operation in the graph as an intrinsic
    assert "label=\"**\", color=\"gold\", shape=\"ellipse\"" in graph


def test_unrecognised_child():
    ''' Check that we raise the expected exception if we encounter an
    unrecognised object in the tree produced by the parser. '''
    dag = dag_from_strings(["aprod = var1 * var2 * var3",
                            "bprod = var1 * var2 / var3",
                            "cprod = var1 * var2 + var3"])
    anode = dag._nodes["aprod"]
    children = [anode]
    with pytest.raises(DAGError) as err:
        dag.make_dag(anode, children, {})
    assert ("Unrecognised child; type = <class 'habakkuk.dag_node.DAGNode'>"
            in str(err))


def test_flop_count_err():
    ''' Check that we raise the expected exception if we pass something
    that is not a list or a dictionary to the flop_count() function '''
    from habakkuk.dag import flop_count
    not_a_list = "hello"
    with pytest.raises(DAGError) as err:
        _ = flop_count(not_a_list)
    assert ("flop_count requires a list or a dictionary of nodes "
            "but got " in str(err))


def test_flop_count_basic():
    ''' Check that flop_count() returns the correct value for a DAG
    containing only basic arithmetic operations '''
    from habakkuk.dag import flop_count
    dag = dag_from_strings(["aprod = var1 * var2 * var3"])
    nflops = flop_count(dag._nodes)
    assert nflops == 2


def test_flop_count_power():
    ''' Check that flop_count() returns the correct value for a DAG
    containing a ** operation '''
    from habakkuk.dag import flop_count
    from habakkuk.config_ivy_bridge import OPERATORS
    dag = dag_from_strings(["aprod = var1 ** var2"])
    nflops = flop_count(dag._nodes)
    assert nflops == OPERATORS["**"]["flops"]


def test_flop_count_sin():
    ''' Check that flop_count() returns the correct value for a DAG
    containing a sin() operation '''
    from habakkuk.dag import flop_count
    from habakkuk.config_ivy_bridge import OPERATORS
    dag = dag_from_strings(["aprod = sin(var1)"])
    nflops = flop_count(dag._nodes)
    assert nflops == OPERATORS["SIN"]["flops"]


def test_flop_count_ignore_ints():
    ''' Check that flop_count() correctly ignores integer operations '''
    from habakkuk.dag import flop_count
    dag = dag_from_strings(["a(i) = b(i) + c(i+1) + d(2*i)"])
    nflops = flop_count(dag._nodes)
    assert nflops == 2


def test_adj_ref_same_cache_line():
    ''' Check that two accesses to adjacent locations in memory are counted
    as a single cache-line '''
    dag = dag_from_strings(
        ["pgzui  (ji,jj) = (gdep3w_0(ji+1,jj,iku) + ze3wu) - "
         "gdep3w_0(ji,jj,iku)"])
    dag.to_dot()
    assert dag.cache_lines() == 2


def test_write_read_same_cache_line():
    ''' Check that a write followed by a read of the same array element is
    only counted as a single reference '''
    dag = dag_from_strings(
        ["ze3wu = 2.0*pgzui(ji,jj)",
         "pgzui(ji,jj) = (gdep3w_0(ji+1,jj,iku) + ze3wu)",
         "pgzui(ji,jj) = pgzui(ji,jj) * pgzui(ji,jj)",
         "pgx(ji,jj) = pgzui(ji,jj) + 1.0"])
    dag.to_dot()
    assert dag.cache_lines() == 3


def test_index_product_same_cache_line():  # pylint: disable=invalid-name
    ''' Check that two accesses to adjacent locations in memory are counted
    as a single cache-line, even when a product is involved '''
    dag = dag_from_strings(
        ["pgzui  (ji,jj) = (gdep3w_0(2*ji+1,jj,iku) + ze3wu) - "
         "gdep3w_0(2*ji,jj,iku)"])
    assert dag.cache_lines() == 2


def test_indirect_1darr_acc_diff_cachelines():  # pylint: disable=invalid-name
    ''' Check that we correctly identify two indirect array accesses as
    (probably) belonging to two different cache lines '''
    dag = dag_from_strings(["a(i) = 2.0 * b(map(i)+j) * b(map(i+1)+j)"])
    assert dag.cache_lines() == 3


def test_indirect_1darr_write_read():  # pylint: disable=invalid-name
    ''' Check that we correctly identify two indirect array accesses as
    (probably) belonging to two different cache lines and that we don't
    double-count accesses due to writing/reading from the same location '''
    from habakkuk.dag import differ_by_constant
    dag = dag_from_strings(["b(map(i)+j) = 2.0 * b(map(i)+j) * b(map(i+1)+j)"])
    node1 = dag._nodes["b(map(i)+j)"].producers[0]
    node2 = dag._nodes["b(map(i+1)+j)"].producers[0]
    assert not differ_by_constant(node1, node2)
    assert dag.cache_lines() == 2


def test_indirect_2darr_acc_difft_cachelines():  # pylint: disable=invalid-name
    ''' Check that we correctly identify 3 indirect array accesses as
    (probably) belonging to different cache lines '''
    dag = dag_from_strings(["a(i) = b(map(i)+j,k) * b(map(i)+j, i) * "
                            "b(map(i+1)+j, i)"])
    assert dag.cache_lines() == 4


def test_indirect_2darr_acc_same_cachelines():  # pylint: disable=invalid-name
    ''' Check that we identify two indirect array accesses that differ only
    by a constant (in the first index) as (probably) belonging to the same
    cache line '''
    from habakkuk.dag import differ_by_constant
    dag = dag_from_strings(["a(i) = b(map(i)+j,k) * b(map(i)+j, i) * "
                            "b(map(i+1)+j, i) + b(map(i)+j+1, i)"])
    node1 = dag._nodes["b(map(i)+j+1,i)"].producers[0]
    node2 = dag._nodes["b(map(i)+j,i)"].producers[0]
    assert differ_by_constant(node1, node2)
    assert dag.cache_lines() == 4

    dag = dag_from_strings(["a(i) = b(map(i)+j,k) * b(map(i)+j, i) * "
                            "b(map(i+1)+j, i) + b(1+map(i)+j, i)"])
    node1 = dag._nodes["b(1+map(i)+j,i)"].producers[0]
    node2 = dag._nodes["b(map(i)+j,i)"].producers[0]
    assert differ_by_constant(node1, node2)
    assert dag.cache_lines() == 4

    dag = dag_from_strings(["a(i) = b(map(i)+j,k) * b(map(i)+j, i) * "
                            "b(map(i+1)+j, i) + b(1+map(i)+j+1, i)"])
    node1 = dag._nodes["b(1+map(i)+j+1,i)"].producers[0]
    node2 = dag._nodes["b(map(i)+j,i)"].producers[0]
    assert differ_by_constant(node1, node2)
    assert dag.cache_lines() == 4

    dag = dag_from_strings(["a(i) = b(map(i)+j,k) * b(map(i)+j, i) * "
                            "b((1+map(i))+j, i) + b(1+(map(i)+j)+1, i)"])
    node1 = dag._nodes["b(1+(map(i)+j)+1,i)"].producers[0]
    node2 = dag._nodes["b((1+map(i))+j,i)"].producers[0]
    assert differ_by_constant(node1, node2)
    assert dag.cache_lines() == 3


def test_array_ref_contains_array_ref():  # pylint: disable=invalid-name
    ''' Check that an array reference that uses
    another array ref as index is identified as an array reference '''
    dag = dag_from_strings(["aprod = my_array(x(1,2))"])
    for node in itervalues(dag._nodes):
        if "my_array" in node.name:
            assert node.node_type == "array_ref"
    dag = dag_from_strings(["aprod = my_array(x(i,j))"])
    node = dag._nodes["my_array(x(i,j))"]
    assert node.node_type == "array_ref"
    dag = dag_from_strings(["aprod = my_array(x(i,j)+1)"])
    node = dag._nodes["my_array(x(i,j)+1)"]
    assert node.node_type == "array_ref"


def test_fn_call_contains_array_slice():  # pylint: disable=invalid-name
    ''' Check that we correctly identify a Part_Ref that itself
    contains an array slice as being a function call rather than
    an array reference '''
    dag = dag_from_strings(["aprod = my_fn(x(:))", "bprod = an_array(:)"])
    for node in itervalues(dag._nodes):
        if "my_fn" in node.name:
            assert node.node_type == "call"
        if "an_array" in node.name:
            assert node.node_type == "array_ref"


def test_part_ref_is_call():
    ''' Check that we identify a Part_Ref containing one or more array
    sections as a function call rather than an array reference '''
    dag = dag_from_strings(["area = glob_sum( e1e2t(:,:) * tmask(:,:,1))",
                            "bob = x(:) + y(1:3)"])
    for node in itervalues(dag._nodes):
        if "glob_sum" in node.name:
            gsum_node = node
            break
    assert gsum_node
    assert gsum_node.node_type == "call"


def test_string_ref_is_call():
    ''' Check that a Part_Ref that contains a string is identified as a
    function call '''
    dag = dag_from_strings(["area = my_file('name')"])
    for node in itervalues(dag._nodes):
        if "my_file" in node.name:
            file_node = node
            break
    assert file_node
    assert file_node.node_type == "call"


def test_assign_dtype_components():
    ''' Test that we can generate a dag for an assignment involving references
    to components of derived types '''
    dag = dag_from_strings(["zphi = sladatqc%rphi(jobs)",
                            "sladatqc%rmod(jobs,1) = sladatqc%rext(jobs,1) "
                            "- sladatqc%rext(jobs,2)"])
    dag.verify_acyclic()
    count = 0
    for node in itervalues(dag._nodes):
        if "sladatqc" in node.name:
            count += 1
    assert count == 4


def test_parentheses_in_function(tmpdir, capsys):
    ''' Test Habakkuk against Fortran containing a function with a
    fairly complex parenthesised expression. '''
    options = Options()
    options.no_fma = True

    os.chdir(str(tmpdir.mkdir("tmp")))

    make_dag.dag_of_files(options,
                          [os.path.join(BASE_PATH, "fn_parentheses.f90")])
    result, _ = capsys.readouterr()
    print(result)
    assert "Stats for DAG fspott:" in result
    assert "9 FLOPs in total." in result
    assert "4 multiplication operators." in result


def test_repeat_assign_derived_type_array():  # pylint: disable=invalid-name
    ''' Test for assignment to an array element in a derived type '''
    dag = dag_from_strings(
        ["itmp = sd(jf)%nrec_a(1)",
         "sd(jf)%nrec_a(1) = sd(jf)%nreclast",
         "sd(jf)%nrec_b(1) = sd(jf)%nrec_a(1)",
         "sd(jf)%nrec_a(1) = itmp"])
    node_names = [node.name for node in itervalues(dag._nodes)]
    dag.verify_acyclic()
    assert "sd(jf)%nrec_a(1)" in node_names
    assert "sd(jf)%nrec_a'(1)" in node_names
    assert "sd(jf)%nrec_a''(1)" in node_names


def test_propagate_ints():
    ''' Test that operations are correctly identified as being integer
    if their arguments are integer '''
    dag = dag_from_strings(
        ["nbidta(ib2, igrd, ib_bdy2) =-ib_bdy2",
         "nbjdta(ib2, igrd, ib_bdy2) =-ib_bdy2",
         "nbidta(ib1, igrd, ib_bdy1) =-ib_bdy1",
         "nbjdta(ib1, igrd, ib_bdy1) =-ib_bdy1"])
    sub_list = []
    for node in itervalues(dag._nodes):
        if node.node_type == "-":
            sub_list.append(node)
            assert not node.is_integer
    dag.update_integer_nodes()
    for node in sub_list:
        assert node.is_integer
