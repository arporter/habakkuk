
''' This module tests the DAG generator using pytest '''

import os
import pytest
from parse2003 import ParseError
import make_dag

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


def test_is_intrinsic_err():
    ''' Check that the expected exception is raised if we pass an
    incorrect object to the is_intrinsic_fn() function '''
    from fparser import Fortran2003
    from dag_node import DAGError
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
    from fparser import Fortran2003
    from dag import is_intrinsic_fn
    fake_parse_obj = Fortran2003.Part_Ref("a(i,j)")
    val = is_intrinsic_fn(fake_parse_obj)
    assert not val


def test_is_intrinsic_true():
    ''' Check that is_intrinsic_fn() returns False if passed something
    that is not a Fortran intrinsic '''
    from fparser import Fortran2003
    from dag import is_intrinsic_fn
    fake_parse_obj = Fortran2003.Part_Ref("sin(r)")
    val = is_intrinsic_fn(fake_parse_obj)
    assert val


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

