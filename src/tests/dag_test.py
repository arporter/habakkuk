
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
        self.no_fma = False
        self.rm_scalar_tmps = False
        self.show_weights = False
        self.unroll_factor = 1
        self.mode = 'auto'


def test_basic(capsys):
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


def test_array_readwrite_with_fma(capsys):
    ''' Test the analysis of code of the form x(i) = a + x(i) '''
    make_dag.runner(Options(),
                    [os.path.join(BASE_PATH, "shallow_loop11.f90")])
    result, _ = capsys.readouterr()
    print result
    assert "6 addition operators." in result
    assert "3 subtraction operators." in result
    assert "6 multiplication operators." in result
    assert "15 FLOPs in total." in result
    assert "9 array references." in result
    assert "Sum of cost of all nodes = 15" in result

