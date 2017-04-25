#!/usr/bin/env python

''' A python script to parse a Fortran source file and produce a DAG
    for each subroutine it contains. '''

from habakkuk.dag import DirectedAcyclicGraph
from parse2003 import walk_ast

# TODO swap to using argparse since optparse is deprecated
from optparse import OptionParser
from fparser.script_options import set_f2003_options


def dag_of_code_block(parent_node, name, loop=None, unroll_factor=1):
    ''' Creates and returns a DAG for the code that is a child of the
    supplied node '''
    from fparser.Fortran2003 import Assignment_Stmt

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
        assignments = walk_ast(parent_node.items, [Assignment_Stmt])
        if isinstance(parent_node, Assignment_Stmt):
            assignments.append(parent_node)
    elif hasattr(parent_node, "content"):
        assignments = walk_ast(parent_node.content, [Assignment_Stmt])

    if not assignments:
        # If this subroutine has no assignment statements
        # then we skip it
        print "Code {0} contains no assignment statements - skipping".\
            format(name)
        return None

    if loop:
        # Put the loop variable in our mapping
        mapping[loop.var_name] = loop.var_name

    digraph.add_assignments(assignments, mapping)

    if loop and loop.var_name:
        for _ in range(1, unroll_factor):
            # Increment the loop counter and then add to the DAG again
            digraph.add_assignments(
                [Assignment_Stmt("{0} = {0} + 1".format(loop.var_name))],
                mapping)
            digraph.add_assignments(assignments, mapping)

    return digraph


def dag_of_files(options, args):
    ''' Parses the files listed in args and generates a DAG for all of
    the subroutines/inner loops that it finds '''
    from fparser.api import Fortran2003
    from fparser.readfortran import FortranFileReader
    from fparser.Fortran2003 import Main_Program, Program_Stmt, \
        Subroutine_Subprogram, Function_Subprogram, Function_Stmt, \
        Subroutine_Stmt, Block_Nonlabel_Do_Construct, Execution_Part, \
        Name
    from parse2003 import Loop, get_child, ParseError

    apply_fma_transformation = not options.no_fma
    prune_duplicate_nodes = not options.no_prune
    unroll_factor = int(options.unroll_factor)
    rm_scalar_temporaries = options.rm_scalar_tmps
    show_weights = options.show_weights

    for filename in args:
        print "Habakkuk processing file '{0}'".format(filename)
        reader = FortranFileReader(filename)
        if options.mode != 'auto':
            reader.set_mode_from_str(options.mode)
        try:
            program = Fortran2003.Program(reader)
            # Find all the subroutines contained in the file
            routines = walk_ast(program.content, [Subroutine_Subprogram,
                                                  Function_Subprogram])
            # Add the main program as a routine to analyse - take care
            # here as the Fortran source file might not contain a
            # main program (might just be a subroutine in a module)
            try:
                main_prog = get_child(program, Main_Program)
                routines.append(main_prog)
            except ParseError:
                pass

            # Create a DAG for each (sub)routine
            for subroutine in routines:
                # Get the name of this (sub)routine
                substmt = walk_ast(subroutine.content,
                                   [Subroutine_Stmt, Function_Stmt,
                                    Program_Stmt])
                if isinstance(substmt[0], Function_Stmt):
                    for item in substmt[0].items:
                        if isinstance(item, Name):
                            sub_name = str(item)
                else:
                    sub_name = str(substmt[0].get_name())

                # Find the section of the tree containing the execution part
                # of the code
                try:
                    exe_part = get_child(subroutine, Execution_Part)
                except ParseError:
                    # This subroutine has no execution part so we skip it
                    # TODO log this event
                    continue

                # Make a list of all Do loops in the routine
                loops = walk_ast(exe_part.content,
                                 [Block_Nonlabel_Do_Construct])
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
                        inner_loops = walk_ast(loop.content,
                                               [Block_Nonlabel_Do_Construct])
                        if inner_loops:
                            # We're not so skip
                            continue

                        # Create a Loop object for this loop
                        myloop = Loop()
                        myloop.load(loop)

                        # Generate a suitable name for this DAG. Since
                        # we're processing Fortran code we count from
                        # 1 rather than 0.
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

                    digraph.update_integer_nodes()

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
            # TODO log this error
            print "Parsing '{0}' (starting at {1}) failed at {2}. "\
                "Is the file valid Fortran?".\
                format(filename, reader.fifo_item[0], reader.fifo_item[-1])
            # Carry on to next file
            continue


def runner(argv):
    ''' The top-level routine that runs Habakkuk. Parses the command-line
    arguments passed in to this routine. '''
    import os
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

    # Use the parser object to parse the command-line arguments
    options, args = parser.parse_args(argv)

    # Check that we've been passed the name of an existing file
    if not args:
        raise IOError("The name of a Fortran source file must be provided.")
    if not os.path.isfile(args[0]):
        raise IOError("The specified source file ('{0}') does not exist"
                      .format(args[0]))

    dag_of_files(options, args)
