''' Module containing utilities used by the test suite '''


def dag_from_strings(lines, name=None):
    ''' Function that takes a list of strings (containing Fortran
    assignment statements) and generates a DAG. '''
    from fparser import Fortran2003
    from habakkuk.dag import DirectedAcyclicGraph

    assigns = []
    for line in lines:
        assigns.append(Fortran2003.Assignment_Stmt(line))
    mapping = {}
    if name:
        dag_name = name
    else:
        dag_name = "Test dag"

    # Create the DAG object
    dag = DirectedAcyclicGraph(dag_name)

    # Populate it from the Assignment statements
    dag.add_assignments(assigns, mapping)

    return dag


class Options(object):
    ''' Class used to store what would be command-line options when
    running the tool from the test suite '''

    def __init__(self):
        self.no_prune = False
        self.no_fma = True
        self.rm_scalar_tmps = False
        self.show_weights = False
        self.unroll_factor = 1
        self.mode = 'auto'
