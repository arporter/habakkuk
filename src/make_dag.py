#!/usr/bin/env python

''' A python script to parse a Fortran source file and produce a DAG
    for each subroutine it contains. '''

from dag import DirectedAcyclicGraph
from parse2003 import walk

try:
    from iocbio.optparse_gui import OptionParser
except ImportError:
    from optparse import OptionParser
from fparser.script_options import set_f2003_options

def dag_of_assignments(digraph, assignments, mapping):
    ''' Add to the existing DAG using the supplied list of assignments. Each
    assignment is an instance of a fparser.Fortran2003.Assignment_Stmt '''
    from parse2003 import Variable

    for assign in assignments:

        # Create a Variable to represent the LHS of the assignment
        lhs_var = Variable()
        lhs_var.load(assign.items[0], mapping=mapping, lhs=True)

        # Create a *temporary* node to store the result of the RHS of
        # this assignment (in case it references the variable on the
        # LHS)
        tmp_node = digraph.get_node(parent=None,
                                    name="tmp_node",
                                    unique=True)

        # First two items of an Assignment_Stmt are the name of
        # the var being assigned to and '=' so skip them
        rhs_node_list = digraph.make_dag(tmp_node, assign.items[2:], mapping)

        # Sanity check - the temporary node representing the LHS of
        # the assignment should not (yet) be consumed by anything
        # because it is a unique node and only receives the output of
        # the RHS.
        if tmp_node.consumers:
            raise Exception("Temporary node should not have any consumers!")

        # Only update the map once we've created a DAG of the
        # assignment statement. This is because any references
        # to this variable in that assignment are to the previous
        # version of it, not the one being assigned to.
        if lhs_var.full_orig_name in mapping:
            mapping[lhs_var.full_orig_name] += "'"
        else:
            # The LHS variable wasn't already in the map - we use the full
            # variable expression (including any array indices) as the
            # dictionary key. We only store the base of the variable name
            # as the dictionary entry (so that when we assign to array
            # elements, the resulting node is named eg. array'(i,j)).
            mapping[lhs_var.full_orig_name] = lhs_var.orig_name

            for node in rhs_node_list:
                if node.variable:
                    if node.variable.full_orig_name == lhs_var.full_orig_name:
                        # If the LHS variable appeared on the RHS of this
                        # assignment then we must append a ' character to its
                        # name. This then means we get a new node representing
                        # the variable being assigned to.
                        mapping[lhs_var.full_orig_name] += "'"
                        break

        # Update the base name of the LHS variable to match that in the map
        lhs_var.name = mapping[lhs_var.full_orig_name]

        # Create the LHS node proper now that we've updated the
        # naming map
        lhs_node = digraph.get_node(parent=None,
                                    mapping=mapping,
                                    variable=lhs_var)

        # Copy over the dependencies from the temporary node
        for node in tmp_node.producers:
            lhs_node.add_producer(node)
            node.add_consumer(lhs_node)

        # Delete the temporary node (this also removes it from any
        # nodes that have it listed as a producer/consumer)
        digraph.delete_node(tmp_node)


def dag_of_code_block(parent_node, name, loop=None, unroll_factor=1):
    ''' Creates and returns a DAG for the code that is a child of the
    supplied node '''
    from fparser.Fortran2003 import Assignment_Stmt, Name
    from parse2003 import Variable

    # Create a new DAG object
    digraph = DirectedAcyclicGraph(name)

    # Keep a list of variables that are assigned to. This
    # enables us to update the name by which they are known
    # in the graph.
    # e.g.:
    #  a = b + c
    #  a = a + 1 =>  a' = a + 1
    #  a = a*a   => a'' = a' * a'
    mapping = {}

    # Find all of the assignment statements in the code block
    if hasattr(parent_node, "items"):
        assignments = walk(parent_node.items, Assignment_Stmt)
    else:
        assignments = walk(parent_node.content, Assignment_Stmt)

    if not assignments:
        # If this subroutine has no assignment statements
        # then we skip it
        print "Code {0} contains no assignment statements - skipping".\
            format(name)
        return None
 
    if loop:
        # Put the loop variable in our mapping
        mapping[loop.var_name] = loop.var_name

    dag_of_assignments(digraph, assignments, mapping)

    if loop:
        for repeat in range(1, unroll_factor):
            # Increment the loop counter and then add to the DAG again
            mapping[loop.var_name] += "+1"
            dag_of_assignments(digraph, assignments, mapping)

    # Correctness check - if we've ended up with e.g. my_var' as a key
    # in our name-mapping dictionary then something has gone wrong.
    for name in mapping:
        if "'" in name:
            raise ParseError(
                "Found {0} in name map but names with ' characters "
                "appended should only appear in the value part of "
                "the dictionary")

    return digraph

    
def runner(options, args):
    ''' Parses the files listed in args and generates a DAG for all of the
    subroutines it finds '''
    from fparser.api import Fortran2003
    from fparser.readfortran import FortranFileReader
    from fparser.Fortran2003 import Program, Main_Program, Program_Stmt, \
        Subroutine_Subprogram, Assignment_Stmt, \
        Subroutine_Stmt, Name, Block_Nonlabel_Do_Construct, Execution_Part
    from parse2003 import Loop, get_child, ParseError

    apply_fma_transformation = not options.no_fma
    prune_duplicate_nodes = not options.no_prune
    unroll_factor = int(options.unroll_factor)
    rm_scalar_temporaries = options.rm_scalar_tmps
    show_weights = options.show_weights

    for filename in args:
        reader = FortranFileReader(filename)
        if options.mode != 'auto':
            reader.set_mode_from_str(options.mode)
        try:
            program = Fortran2003.Program(reader)
            # Find all the subroutines contained in the file
            routines = walk(program.content, Subroutine_Subprogram)
            # Add the main program as a routine to analyse - take care
            # here as the Fortran source file might not contain a
            # main program (might just be a subroutine in a module)
            try:
                main = get_child(program, Main_Program)
            except ParseError:
                main = None
            if main:
                routines.append(main)
            
            # Create a DAG for each (sub)routine
            for subroutine in routines:
                # Get the name of this (sub)routine
                if isinstance(subroutine, Subroutine_Subprogram):
                    substmt = walk(subroutine.content, Subroutine_Stmt)
                elif isinstance(subroutine, Main_Program):
                    substmt = walk(subroutine.content, Program_Stmt)
                sub_name = str(substmt[0].get_name())

                # Find the section of the tree containing the execution part
                # of the code
                exe_part = get_child(subroutine, Execution_Part)

                # Make a list of all Do loops in the routine
                loops = walk(exe_part.content, Block_Nonlabel_Do_Construct)
                digraphs = []
                
                if not loops:
                    # There are no Do loops in this subroutine so just
                    # generate a DAG for the body of the routine...
                    digraph = dag_of_code_block(exe_part, sub_name)
                    if digraph:
                        digraphs.append(digraph)
                else:
                    # Create a DAG for the body of each innermost loop
                    loop_count = 0
                    for loop in loops:

                        # Check that we are an innermost loop
                        inner_loops = walk(loop.content,
                                           Block_Nonlabel_Do_Construct)
                        if inner_loops:
                            # We're not so skip
                            continue

                        # Create a Loop object for this loop
                        myloop = Loop()
                        myloop.load(loop)

                        # Generate a suitable name for this DAG. Since we're
                        # processing Fortran code we count from 1 rather than 0.
                        name = sub_name + "_loop" + str(loop_count+1)
                        if unroll_factor > 1:
                            name += "_unroll" + str(unroll_factor)

                        # Create the DAG
                        digraph = dag_of_code_block(
                            loop, name,
                            loop=myloop,
                            unroll_factor=unroll_factor)
                        if digraph:
                            digraphs.append(digraph)

                        # Increment count of (inner) loops found
                        loop_count += 1


                for digraph in digraphs:

                    if prune_duplicate_nodes:
                        digraph.prune_duplicate_nodes()

                    if rm_scalar_temporaries:
                        digraph.rm_scalar_temporaries()

                    # Work out the critical path through this graph
                    digraph.calc_critical_path()

                    # Write the digraph to file
                    digraph.to_dot(show_weights=show_weights)
                    digraph.report()

                    # Fuse multiply-adds where possible
                    if apply_fma_transformation:
                        num_fused = digraph.fuse_multiply_adds()
                        if num_fused:
                            digraph.name = digraph.name + "_fused"
                            # Re-compute the critical path through this graph
                            digraph.calc_critical_path()
                            digraph.to_dot()
                            digraph.report()
                        else:
                            print "No opportunities to fuse multiply-adds"

        except Fortran2003.NoMatchError:
            print 'parsing %r failed at %s' % (filename, reader.fifo_item[-1])
            print 'started at %s' % (reader.fifo_item[0])
            print 'Quitting'
            return


def main():
    parser = OptionParser()
    set_f2003_options(parser)
    parser.add_option("--no-prune",
                      help="Do not attempt to prune duplicate operations "
                      "from the graph",
                      action="store_true",
                      dest="no_prune",
                      default=False)
    parser.add_option("--no-fma",
                      help="Do not attempt to generate fused multiply-add "
                      "operations",
                      action="store_true",
                      dest="no_fma",
                      default=True)
    parser.add_option("--rm-scalar-tmps",
                      help="Remove scalar temporaries from the DAG",
                      action="store_true",
                      dest="rm_scalar_tmps",
                      default=False)
    parser.add_option("--show-weights",
                      help="Display node weights in the DAG",
                      action="store_true",
                      dest="show_weights",
                      default=False)
    parser.add_option("--unroll",
                      help="No. of times to unroll a loop. (Applied to every "
                      "loop that is encountered.)",
                      metavar="UNROLL_FACTOR",
                      action="store",
                      type="int",
                      dest="unroll_factor",
                      default=1)
    if hasattr(parser, 'runner'):
        parser.runner = runner
    options, args = parser.parse_args()

    runner(options, args)
    return


if __name__=="__main__":
    main()
