
''' This module provides support for the construction of a Directed
    Acyclic Graph. '''

from habakkuk.fparser import Fortran2003
from habakkuk.dag_node import DAGNode, DAGError
# TODO manange the import of these CPU-specific values in a way that permits
# the type of CPU to be changed
from habakkuk.config_ivy_bridge import OPERATORS, EXAMPLE_CLOCK_GHZ, \
    FORTRAN_INTRINSICS, NUM_EXECUTION_PORTS, CPU_EXECUTION_PORTS

# Maximum length of schedule we expect to handle.
MAX_SCHEDULE_LENGTH = 500


def is_subexpression(expr):
    ''' Returns True if the supplied node is itself a sub-expression. '''
    return (isinstance(expr, Fortran2003.Add_Operand) or
            isinstance(expr, Fortran2003.Level_2_Expr) or
            isinstance(expr, Fortran2003.Level_2_Unary_Expr) or
            isinstance(expr, Fortran2003.Parenthesis))


def is_intrinsic_fn(obj):
    ''' Checks whether the supplied object is a call to a Fortran
        intrinsic '''
    if not isinstance(obj.items[0], Fortran2003.Name):
        raise DAGError("is_intrinsic_fn: expects first item to be Name")
    return str(obj.items[0]).upper() in FORTRAN_INTRINSICS


def subgraph_matches(node1, node2):
    ''' Returns True if the two nodes (and any children they may
    have) represent the same quantity. '''
    matches = True
    if node1.name != node2.name:
        matches = False
    if len(node1.producers) != len(node2.producers):
        matches = False
    if node1.node_type != node2.node_type:
        matches = False
    # TODO correct the code that stores the denominator of any division
    # operations
    # if node1.node_type == "/":
    #    if node1.operands[0] != node2.operands[0]:
    #        matches = False
    elif node1.node_type == "FMA":
        # Check that the two nodes being multiplied are the same
        if node1.operands[0] not in node2.operands or \
           node1.operands[1] not in node2.operands:
            matches = False
    for child1 in node1.producers:
        found = False
        # We can't assume that the two lists of children have the same
        # ordering...
        for child2 in node2.producers:
            # Recurse down
            if subgraph_matches(child1, child2):
                found = True
                break
        if not found:
            matches = False
    return matches


def ready_ops_from_list(nodes):
    ''' Look through supplied list of nodes and identify those that
    are operations/intrinsics which are ready to execute '''
    op_list = []
    for node in nodes:
        if not node.ready and node.node_type in OPERATORS and \
           node.dependencies_satisfied:
            op_list.append(node)
    return op_list


def schedule_cost(nsteps, schedule):
    ''' Calculate the cost (in cycles) of the supplied schedule '''

    print "Schedule contains {0} steps:".format(nsteps)
    cost = 0
    for step in range(0, nsteps):

        sched_str = str(step)
        max_cost = 0

        # Find the most expensive operation on any port at this step of
        # the schedule
        for port in range(NUM_EXECUTION_PORTS):
            sched_str += " {0}".format(schedule[port][step])

            port_cost = 0

            # If there is an operation on this port at this step of
            # the schedule then calculate its cost...
            if schedule[port][step]:
                operator = str(schedule[port][step])
                port_cost = OPERATORS[operator]["cost"]

                if False:
                    # Account for operation latency - assume we have to
                    # pay it and then subsequently set it to zero if we don't
                    latency = OPERATORS[operator]["latency"]

                    # If this isn't the first step in the schedule *and* the
                    # port executed an operation in the previous step then
                    # check what type it was
                    if step > 0 and schedule[port][step-1]:
                        previous_op = str(schedule[port][step-1])
                        if OPERATORS[previous_op] == \
                           OPERATORS[operator]:
                            # This operation is the same as the previous
                            # one on this port so assume pipelined
                            latency = 0
                        elif (OPERATORS[operator] in ["+", "-"] and
                              OPERATORS[previous_op] in ["+", "-"]):
                            # Assume '+' and '-' are treated as the same
                            # and thus we pay no latency
                            latency = 0
                    port_cost += latency

            if port_cost > max_cost:
                max_cost = port_cost
        sched_str += " (cost = {0})".format(max_cost)
        cost += max_cost
        print sched_str
    return cost


def flop_count(nodes):
    '''The number of floating point operations in the supplied list of
    nodes. This is NOT the same as the number of cycles. '''
    count = 0
    if isinstance(nodes, dict):
        node_list = nodes.itervalues()
    elif isinstance(nodes, list):
        node_list = nodes
    else:
        raise DAGError(
            "flop_count requires a list or a dictionary of nodes "
            "but got {0}.".format(type(nodes)))

    for node in node_list:
        if node.node_type in OPERATORS:
            count += OPERATORS[node.node_type]["flops"]
    return count


# TODO: would it be better to inherit from the built-in list object?
class Path(object):
    ''' Class to encapsulate functionality related to a specifc path
    through a DAG '''

    def __init__(self):
        self._nodes = []

    @property
    def input_node(self):
        ''' Searches through the nodes in this Path to find the one
        that is input (has no producers/dependencies). Raises an
        exception if none is found. '''
        for node in self._nodes:
            if not node.has_producer:
                return node
        raise DAGError("Failed to find input node for critical path")

    @property
    def nodes(self):
        ''' Returns the list of nodes in this Path '''
        return self._nodes

    def load(self, obj_list):
        ''' Populate this object using the supplied list of nodes '''
        self._nodes = obj_list

    def cycles(self):
        ''' The length of the path in cycles '''
        cost = 0
        for node in self._nodes:
            cost += node.weight
        return cost

    def __len__(self):
        ''' Over-load the built-in len operation so that it behaves as
        expected '''
        return len(self._nodes)

    def to_dot(self, fileobj):
        ''' Write this path to the supplied DOT file '''
        # We output the nodes in reverse order so that flow is from
        # input node to output node.
        pathstr = self._nodes[-1].node_id
        for node in reversed(self._nodes[:-1]):
            pathstr += " -> {0}".format(node.node_id)
        pathstr += "[color=red,penwidth=3.0];"
        fileobj.write(pathstr)


class DirectedAcyclicGraph(object):
    ''' Class that encapsulates a Directed Acyclic Graph representing a
    piece of Fortran code '''

    def __init__(self, name):
        # Dictionary of all nodes in the graph. Keys are the node names,
        # values are the corresponding DAGNode objects themselves.
        self._nodes = {}
        # Name of this DAG
        self._name = name
        # The critical path through the graph
        self._critical_path = Path()
        # Counter for duplicate sub-expressions (for naming the node
        # used to store the result)
        self._sub_exp_count = 0

    @property
    def name(self):
        ''' Returns the name of this DAG. This is (normally) derived from
        the subroutine containing the Fortran code from which it is
        generated. '''
        return self._name

    @name.setter
    def name(self, new_name):
        ''' Set the name of this DAG '''
        self._name = new_name

    def add_assignments(self, assignments, mapping):
        ''' Add to the existing DAG using the supplied list of
        assignments. Each assignment is an instance of a
        fparser.Fortran2003.Assignment_Stmt '''
        from habakkuk.parse2003 import Variable

        for assign in assignments:

            # Create a Variable to represent the LHS of the assignment
            lhs_var = Variable()
            lhs_var.load(assign.items[0], mapping=mapping, lhs=True)

            # Create a *temporary* node to store the result of the RHS of
            # this assignment (in case it references the variable on the
            # LHS)
            tmp_node = self.get_node(parent=None,
                                     name="tmp_node",
                                     unique=True)

            # First two items of an Assignment_Stmt are the name of
            # the var being assigned to and '=' so skip them
            rhs_node_list = self.make_dag(tmp_node, assign.items[2:], mapping)

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
                        if node.variable.full_orig_name == \
                           lhs_var.full_orig_name:
                            # If the LHS variable appeared on the RHS
                            # of this assignment then we must append a
                            # ' character to its name. This then means
                            # we get a new node representing the
                            # variable being assigned to.
                            mapping[lhs_var.full_orig_name] += "'"
                            break

            # Update the base name of the LHS variable to match that in the map
            lhs_var.name = mapping[lhs_var.full_orig_name]

            # Create the LHS node proper now that we've updated the
            # naming map
            lhs_node = self.get_node(parent=None,
                                     mapping=mapping,
                                     variable=lhs_var)

            # Copy over the dependencies from the temporary node
            for node in tmp_node.producers:
                lhs_node.add_producer(node)
                node.add_consumer(lhs_node)

            # Delete the temporary node (this also removes it from any
            # nodes that have it listed as a producer/consumer)
            self.delete_node(tmp_node)

    def get_node(self, parent=None, mapping=None, name=None, unique=False,
                 node_type=None, variable=None, is_integer=False):
        ''' Looks-up or creates a node in the graph. If unique is False and
        we do not already have a node with the supplied name then we create a
        new one. If unique is True then we always create a new node. If a
        mapping is supplied then it is used to name the node. '''

        if not name and not variable:
            raise DAGError("get_node: one of 'name' or 'variable' must "
                           "be supplied")

        if unique:
            # Node is unique so we make a new one, no questions asked.
            node = DAGNode(parent=parent, name=name, digraph=self,
                           variable=variable, is_integer=is_integer)
            # Store this node in our list using its unique ID in place of a
            # name (since a unique node has been requested). This then
            # ensures we have a list of all nodes in the graph.
            self._nodes[node.node_id] = node
        else:
            if name:
                if mapping and name in mapping:
                    node_name = mapping[name]
                else:
                    node_name = name
            else:
                # Use the supplied variable object to generate the name
                # of this node
                node_name = variable.full_name
            # Node is not necessarily unique so check whether we
            # already have one with the supplied name
            if node_name in self._nodes:
                node = self._nodes[node_name]
                # Record the fact that the parent now has a dependence
                # on this node and that this node is consumed by the parent
                if parent:
                    parent.add_producer(node)
                    node.add_consumer(parent)
            else:
                # Create a new node and store it in our list so we
                # can refer back to it in future if needed
                node = DAGNode(parent=parent, name=node_name,
                               variable=variable, is_integer=is_integer)
                self._nodes[node_name] = node

        if node_type:
            node.node_type = node_type

        return node

    def delete_node(self, node):
        ''' Removes the supplied node from the list of nodes in
        this graph and then deletes it altogether '''
        # We don't know the key with which this node was stored in the
        # dictionary - it might have been the name or, for a 'unique' node,
        # its node_id.
        if node.name in self._nodes and self._nodes[node.name] == node:
            self._nodes.pop(node.name)
        elif node.node_id in self._nodes and self._nodes[node.node_id] == node:
            self._nodes.pop(node.node_id)
        else:
            raise DAGError("Object '{0}' (id={1}) not in list of nodes in "
                           "graph!".format(str(node), node.node_id))
        # Remove this node from any node that has it as a producer (dependency)
        for pnode in node.consumers[:]:
            pnode.rm_producer(node)
        # Remove this node from any node that has it listed as a consumer
        for pnode in node.producers[:]:
            pnode.rm_consumer(node)
        # Finally, delete it altogether
        del node

    def delete_sub_graph(self, node):
        ''' Recursively deletes the supplied node *and all of its
        dependencies/children* '''
        node_list = node.walk(top_down=True, depth=0)
        if not node.has_consumer:
            self.delete_node(node)
        for child in node_list:
            # We only delete the node if no other node has it as a
            # dependency (child)
            if not child.has_consumer:
                self.delete_node(child)

    def output_nodes(self):
        ''' Returns a list of all nodes that do not have a node
        that is dependent upon them - i.e. a consumer.
        These are outputs of the DAG. '''
        node_list = []
        for node in self._nodes.itervalues():
            if not node.has_consumer:
                node_list.append(node)
        return node_list

    def input_nodes(self):
        ''' Returns a list of all nodes that do not have any producers
        (dependencies). These are inputs to the DAG. '''
        node_list = []
        for node in self._nodes.itervalues():
            if not node.has_producer:
                node_list.append(node)
        return node_list

    def count_nodes(self, node_type):
        ''' Count the number of nodes in the graph that are of the
        specified type '''
        ancestors = self.output_nodes()
        node_list = []
        for node in ancestors:
            nodes = node.walk(node_type)
            for new_node in nodes:
                if new_node not in node_list:
                    node_list.append(new_node)
        return len(node_list)

    def cache_lines(self):
        ''' Count the number of cache lines accessed by the graph. This
        is the number of distinct memory references. We assume that
        any array reference of the form u(i+1,j) will have been fetched
        when u(i,j) was accessed. '''
        # Set of unique array references
        array_refs = set()
        # Loop over all nodes in the tree, looking for array references
        ancestors = self.output_nodes()
        for ancestor in ancestors:
            nodes = ancestor.walk("array_ref")
            for node in nodes:
                if node.is_integer:
                    # Ignore integer array references
                    # TODO include integer array refs in cache-line count
                    continue
                # We assume that two references to the same array are
                # from the same cache line if they differ only in
                # their first array index and then only in the 'linear'
                # part of it. i.e. my_array(my_map(df) + i) will be
                # fetched with my_array(my_map(df)+i+1) but e.g.
                # my_array(my_map(df+1)+i) will not.
                # 1. Check whether the expression for the first array index
                #    itself contains any array references
                map_refs = node.array_index_nodes[0].walk("array_ref")
                if map_refs:
                    print "1st map lookup: ", map_refs[0]
                # Look at the immediate dependencies of the first array-
                # index expression
                for prod_node in node.array_index_nodes[0].producers:
                    print prod_node
                    if prod_node.node_type == "+" or prod_node.node_type == "-":
                        pass  # TODO
                # We care about the name of the array and the value of
                # anything other than the first index (assuming that any
                # accesses that differ only in the first index are all
                # fetched in the same cache line).
                key = node.variable.name
                for index in node.variable.indices[1:]:
                    key += "_" + index
                array_refs.add(key)
        return len(array_refs)

    def calc_costs(self):
        ''' Analyse the DAG and calculate a weight for each node. '''
        ancestors = self.output_nodes()
        for node in ancestors:
            node.calc_weight()

    def total_cost(self):
        ''' Calculate the total cost of the graph by summing up the cost of
        each node '''
        cost = 0
        for node in self._nodes.itervalues():
            cost += node.weight
        return cost

    def fuse_multiply_adds(self):
        ''' Processes the existing graph and creates FusedMultiplyAdds
        where possible. Returns the number of FMAs created. '''
        num_fma = 0
        ancestors = self.output_nodes()
        for node in ancestors:
            num_fma += node.fuse_multiply_adds()
        return num_fma

    def make_dag(self, parent, children, mapping, array_index=False):
        ''' Makes a DAG from the RHS of a Fortran assignment statement and
        returns a list of the nodes that represent the variables involved '''
        from habakkuk.parse2003 import Variable

        node_list = []
        opcount = 0
        is_division = False
        for child in children:
            if isinstance(child, str):
                if child in OPERATORS:
                    # This is the operator which is then the parent
                    # of the DAG of this subexpression. All operators
                    # are unique nodes in the DAG.
                    my_type = child
                    opnode = self.get_node(parent, mapping, name=child,
                                           unique=True, node_type=my_type,
                                           is_integer=array_index)
                    # Make this operation the parent of the rest of the nodes
                    # making up the expression
                    parent = opnode
                    is_division = (child == "/")
                    opcount += 1

        if opcount > 1:
            raise DAGError("Found more than one operator amongst list of "
                           "siblings: this is not supported!")

        for idx, child in enumerate(children):

            if isinstance(child, Fortran2003.Name):
                var = Variable()
                var.load(child, mapping)
                tmpnode = self.get_node(parent, variable=var,
                                        is_integer=array_index)
                node_list.append(tmpnode)
                if is_division and idx == 2:
                    parent.operands.append(tmpnode)
            elif (isinstance(child, Fortran2003.Real_Literal_Constant) or
                  isinstance(child, Fortran2003.Int_Literal_Constant)):
                # This is a constant and thus a leaf in the tree
                const_var = Variable()
                const_var.load(child, mapping)
                tmpnode = self.get_node(parent, variable=const_var,
                                        unique=True,
                                        node_type="constant",
                                        is_integer=array_index)
                if is_division and idx == 2:
                    parent.operands.append(tmpnode)
            elif isinstance(child, Fortran2003.Part_Ref):
                print child
                # This may be either a function call or an array reference
                if is_intrinsic_fn(child):
                    # Create a unique node to represent the intrinsic call.
                    # Names of intrinics are stored in upper case.
                    intr_name = str(child.items[0]).upper()
                    tmpnode = self.get_node(parent, mapping,
                                            name=intr_name,
                                            unique=True,
                                            node_type=intr_name,
                                            is_integer=array_index)
                    if is_division and idx == 2:
                        parent.operands.append(tmpnode)
                    # Add its dependencies
                    node_list += self.make_dag(tmpnode,
                                               child.items[1:], mapping,
                                               array_index)
                else:
                    # Assume it's an array reference
                    arrayvar = Variable()
                    arrayvar.load(child, mapping)
                    array_node = self.get_node(parent, variable=arrayvar,
                                               node_type="array_ref",
                                               is_integer=array_index)
                    node_list.append(array_node)
                    if is_division and idx == 2:
                        parent.operands.append(array_node)
                    # Include the array index expression in the DAG. Set
                    # flag to indicate that this is an array index so that
                    # we know we're dealing with integers.
                    # The first item in the list child.items is the name
                    # of the array variable itself so we skip that.
                    for idx, item in enumerate(child.items[1:]):
                        if array_index:
                            # We are down within an array-index expression
                            # so we don't create a node to represent each
                            # array-index expression
                            tmpnode = array_node
                        else:
                            # Array is not within an array-index expression so
                            # we create a node to represent each index
                            # expression
                            tmpnode = self.get_node(array_node,
                                                    name="index{0}".format(idx+1),
                                                    is_integer=True,
                                                    unique=True)
                            array_node.array_index_nodes.append(tmpnode)
                        node_list += self.make_dag(tmpnode, [item],
                                                   mapping,
                                                   array_index=True)
            elif isinstance(child, Fortran2003.Array_Section):
                arrayvar = Variable()
                arrayvar.load(child, mapping)
                tmpnode = self.get_node(parent, variable=arrayvar,
                                        node_type="array_ref",
                                        is_integer=array_index)
                node_list.append(tmpnode)
            elif is_subexpression(child):
                # We don't make nodes to represent sub-expresssions - just
                # carry-on down to the children
                node_list += self.make_dag(parent, child.items, mapping,
                                           array_index)
            elif isinstance(child, Fortran2003.Section_Subscript_List):
                # We have a list of arguments
                node_list += self.make_dag(parent, child.items, mapping,
                                           array_index)
            elif isinstance(child, Fortran2003.Mult_Operand):
                # We have an expression that is something like (a * b) ** c
                # and can just carry-on down to the children
                node_list += self.make_dag(parent, child.items, mapping,
                                           array_index)
            elif isinstance(child, str):
                # This is the operator node which we've already dealt with
                pass
            else:
                raise DAGError("Unrecognised child type: {0}, {1}".
                               format(type(child), str(child)))
        return node_list

    def calc_critical_path(self):
        ''' Calculate the critical path through the graph '''
        paths = []

        # Compute inclusive weights for each node
        self.calc_costs()

        # Each of the ancestor (output) nodes represents a starting
        # point for a critical path. The longest of the resulting set
        # of paths is then the critical path of the DAG as a whole.
        for node in self.output_nodes():
            path = Path()
            node_list = []
            node.critical_path(node_list)
            if node_list:
                path.load(node_list)
                paths.append(path)

        # Find the longest of these paths
        max_cycles = 0
        crit_path = None
        for path in paths:
            if path.cycles() > max_cycles:
                max_cycles = path.cycles()
                crit_path = path

        self._critical_path = crit_path

    def rm_scalar_temporaries(self):
        ''' Remove any nodes that represent scalar temporaries. These are
        identified as any node that is not an operator and has just
        one consumer and one producer. '''
        dead_nodes = []
        # _nodes is a dictionary - we want the values, not the keys
        for node in self._nodes.itervalues():
            if node.node_type not in OPERATORS:
                if len(node.producers) == 1 and \
                   len(node.consumers) == 1:
                    cnode = node.consumers[0]
                    pnode = node.producers[0]
                    # Remove the refs to this node in the consumer and producer
                    cnode.rm_producer(node)
                    pnode.rm_consumer(node)
                    # Make the consumer depend on the producer
                    cnode.add_producer(pnode)
                    pnode.add_consumer(cnode)
                    # Remove the dependencies from this node
                    node.rm_producer(pnode)
                    node.rm_consumer(cnode)
                    # Add this node to our list to remove - saves
                    # attempting to modify the contents of the dict
                    # while iterating over it.
                    dead_nodes.append(node)

        # Finally, remove all of the nodes marked for deletion.
        for node in dead_nodes:
            self.delete_node(node)

    def nodes_with_multiple_consumers(self):
        ''' Returns a list of the nodes that have > 1 consumer '''
        multiple_consumers = []
        for node in self._nodes.itervalues():
            if len(node.consumers) > 1:
                multiple_consumers.append(node)
        return multiple_consumers

    def prune_duplicate_nodes(self):
        ''' Walk through the graph and remove all but one of any
        duplicated sub-graphs that represent FLOPs'''

        multiple_consumers = self.nodes_with_multiple_consumers()

        found_duplicate = (len(multiple_consumers) > 0)

        while found_duplicate:

            # Each node with > 1 consumer represents a possible duplication
            for multi_node in multiple_consumers[:]:

                matching_nodes = []
                node1 = multi_node.consumers[0]
                for node2 in multi_node.consumers[1:]:
                    if subgraph_matches(node1, node2):
                        matching_nodes.append(node2)

                if not matching_nodes:
                    found_duplicate = False
                    continue

                # We've found one or more nodes that match node1
                print "Node {0} matches:".format(str(node1))
                for node in matching_nodes:
                    print str(node)

                found_duplicate = True

                # Create a new node to store the result of this
                # duplicated operation
                new_node = self.get_node(
                    name="sub_exp"+str(self._sub_exp_count),
                    unique=True)

                # Increment the count of duplicate sub-expressions
                self._sub_exp_count += 1

                # Each node that had node1 as a dependency must now
                # have that replaced by new_node...
                for pnode in node1.consumers[:]:
                    pnode.add_producer(new_node)
                    new_node.add_consumer(pnode)
                    pnode.rm_producer(node1)
                    node1.rm_consumer(pnode)

                # Make this new node depend on node1
                new_node.add_producer(node1)
                node1.add_consumer(new_node)

                for node2 in matching_nodes:
                    # Add the new node as a dependency for those nodes
                    # that previously had node2 as a producer
                    for pnode in node2.consumers[:]:
                        pnode.add_producer(new_node)
                        new_node.add_consumer(pnode)
                        pnode.rm_producer(node2)
                        node2.rm_consumer(pnode)

                    # Delete node2 and all of its dependencies unless
                    # they have consumers besides node2.
                    self.delete_sub_graph(node2)

                # Update list of nodes with > 1 consumer
                multiple_consumers = self.nodes_with_multiple_consumers()
                break

    @property
    def critical_path(self):
        ''' Returns the Path object holding the critical path through this
        DAG. calc_critical_path() must have previously been called to
        calculate this path. '''
        return self._critical_path

    def to_dot(self, name=None, show_weights=True):
        ''' Write the DAG to file in DOT format. If a critical path has
        been computed then it is also written to the file. '''

        if name:
            filename = name
        else:
            filename = self._name + ".gv"

        # Create a file for the graph of this subroutine
        outfile = open(filename, "w")
        outfile.write("strict digraph {\n")

        for node in self.output_nodes():
            node.to_dot(outfile, show_weights)

        # Write the critical path
        if self.critical_path:
            self._critical_path.to_dot(outfile)

        outfile.write("}\n")
        print "Wrote DAG to {0}".format(outfile.name)
        outfile.close()

    def report(self):
        ''' Report the properties of this DAG to stdout '''
        # Compute some properties of the graph
        op_count = {}
        for operation in OPERATORS:
            op_count[operation] = self.count_nodes(operation)
        num_ref = self.count_nodes("array_ref")
        num_cache_ref = self.cache_lines()
        total_cycles = self.total_cost()
        total_flops = flop_count(self._nodes)
        print "Stats for DAG {0}:".format(self._name)
        print "  {0} addition operators.".format(op_count["+"])
        print "  {0} subtraction operators.".format(op_count["-"])
        print "  {0} multiplication operators.".format(op_count["*"])
        print "  {0} division operators.".format(op_count["/"])
        if "FMA" in op_count:
            print "  {0} fused multiply-adds.".format(op_count["FMA"])
        print "  {0} FLOPs in total.".format(total_flops)
        print "  {0} array references.".format(num_ref)
        print "  {0} distinct cache-line references.".\
            format(num_cache_ref)

        if num_cache_ref > 0:
            flop_per_byte = total_flops / (num_cache_ref*8.0)
            # This is naive because all FLOPs are not equal - a division
            # costs ~10-40x as much as an addition.
            print "  Naive FLOPs/byte = {:.3f}".format(flop_per_byte)
        else:
            print "  Did not find any array/memory references"

        # Execution of the DAG requires that num_cache_ref cache lines
        # be fetched from (somewhere in) the memory
        # hierarchy. However, we assume that we only have to do this
        # fetch once every nwords iterations where nwords is the
        # number of (double-precision/8-byte) words in one cache line.
        mem_traffic_bytes = num_cache_ref * 8

        # Performance estimate using whole graph. This is a lower bound
        # since it ignores all Instruction-Level Parallelism apart from
        # FMAs (if the DAG contains any)...
        if not total_cycles > 0:
            print "  DAG contains no FLOPs so skipping performance estimate."
            return

        min_flops_per_hz = float(total_flops)/float(total_cycles)
        print "  Whole DAG in serial:"
        print "    Sum of cost of all nodes = {0} (cycles)".\
            format(total_cycles)
        print "    {0} FLOPs in {1} cycles => {2:.4f}*CLOCK_SPEED FLOPS".\
            format(total_flops, total_cycles, min_flops_per_hz)
        if num_cache_ref:
            min_mem_bw = float(mem_traffic_bytes) / float(total_cycles)
            print ("    Associated mem bandwidth = {0:.2f}*CLOCK_SPEED "
                   "bytes/s".format(min_mem_bw))

        # Performance estimate using critical path - this is an upper
        # bound (assumes all other parts of the graph can somehow be
        # computed in parallel to the critical path).
        print "  Everything in parallel to Critical path:"
        ncycles = self._critical_path.cycles()
        print ("    Critical path contains {0} nodes, {1} FLOPs and "
               "is {2} cycles long".format(
                   len(self._critical_path),
                   flop_count(self._critical_path.nodes),
                   ncycles))
        # Graph contains total_flops and will execute in at
        # least path.cycles() CPU cycles. A cycle has duration
        # 1/CLOCK_SPEED (s) so kernel will take at least
        # path.cycles()*1/CLOCK_SPEED (s).
        # Theoretical max FLOPS = total_flops*CLOCK_SPEED/path.cycles()
        max_flops_per_hz = float(total_flops)/float(ncycles)
        print ("    FLOPS (ignoring memory accesses) = "
               "{:.4f}*CLOCK_SPEED".format(max_flops_per_hz))

        if num_cache_ref:
            # Kernel/DAG will take at least ncycles/CLOCK_SPEED (s)
            max_mem_bw = float(mem_traffic_bytes) / float(ncycles)
            print ("    Associated mem bandwidth = {0:.2f}*CLOCK_SPEED "
                   "bytes/s".format(max_mem_bw))

        # Construct a schedule for the execution of the nodes in the DAG,
        # allowing for the microarchitecture of the chosen CPU
        # TODO currently this is picked up from config_ivy_bridge.py
        nsteps, schedule = self.generate_schedule()

        cost = schedule_cost(nsteps, schedule)
        print "  Estimate using computed schedule:"
        print "    Cost of schedule as a whole = {0} cycles".format(cost)
        sched_flops_per_hz = float(total_flops)/float(cost)
        print ("    FLOPS from schedule (ignoring memory accesses) = "
               "{:.4f}*CLOCK_SPEED".format(sched_flops_per_hz))
        if num_cache_ref:
            # Kernel/DAG will take at least ncycles/CLOCK_SPEED (s)
            sched_mem_bw = float(mem_traffic_bytes) / float(cost)
            print ("    Associated mem bandwidth = {0:.2f}*CLOCK_SPEED "
                   "bytes/s".format(sched_mem_bw))

        # Given that each execution port can run in parallel with the
        # others, the time taken to do the graph will be the time
        # taken by the port that takes longest (i.e. has the most work
        # to do). Use a dictionary to hold the cost for each port in
        # case the port numbers aren't contiguous.
        port_cost = {}
        for port in CPU_EXECUTION_PORTS.itervalues():
            # Zero the cost for each port
            port_cost[str(port)] = 0
        # We've previously counted the number of each type of operation.
        # Use that information to compute the number of cycles for which
        # each operation will occupy the port to which it is despatched.
        for operator in op_count:
            port_cost[str(CPU_EXECUTION_PORTS[operator])] += (
                op_count[operator] * OPERATORS[operator]["cost"])

        net_cost = 0
        for port in port_cost:
            if port_cost[port] > net_cost:
                net_cost = port_cost[port]
        perfect_sched_flops_per_hz = float(total_flops)/float(net_cost)
        if num_cache_ref:
            perfect_sched_mem_bw = float(mem_traffic_bytes) / float(net_cost)

        print "  Estimate using perfect schedule:"
        print ("    Cost if all ops on different execution ports are "
               "perfectly overlapped = {0} cycles".format(net_cost))

        # Print out example performance figures using the clock speed
        # in EXAMPLE_CLOCK_GHZ
        print ("  e.g. at {0} GHz, these different estimates give (GFLOPS): ".
               format(EXAMPLE_CLOCK_GHZ))
        print (
            "  No ILP  |  Computed Schedule  |  Perfect Schedule | "
            "Critical path")
        print ("  {0:5.2f}   |         {1:5.2f}       |       {2:5.2f}       "
               "|   {3:5.2f}".
               format(min_flops_per_hz*EXAMPLE_CLOCK_GHZ,
                      sched_flops_per_hz*EXAMPLE_CLOCK_GHZ,
                      perfect_sched_flops_per_hz*EXAMPLE_CLOCK_GHZ,
                      max_flops_per_hz*EXAMPLE_CLOCK_GHZ))
        if num_cache_ref:
            print (" with associated BW of {0:.2f},{1:.2f},{2:.2f},{3:.2f} "
                   "GB/s".format(
                       min_mem_bw*EXAMPLE_CLOCK_GHZ,
                       sched_mem_bw*EXAMPLE_CLOCK_GHZ,
                       perfect_sched_mem_bw*EXAMPLE_CLOCK_GHZ,
                       max_mem_bw*EXAMPLE_CLOCK_GHZ))

    def generate_schedule(self, sched_to_dot=True):
        '''Create a schedule mapping operations to hardware

        Creates a schedule describing how the nodes/operations in the DAG
        map onto the available hardware (execution ports on an Intel CPU)

        Keyword arguments:
        sched_to_dot - Whether or not to output each step in the schedule to
                       a separate dot file to enable visualisation.
        '''

        # Flag all input nodes as being ready
        input_nodes = self.input_nodes()
        for node in input_nodes:
            node.mark_ready()

        # Output this initial graph
        if sched_to_dot:
            self.to_dot(name=self._name+"_step0.gv")

        # Construct a schedule
        step = 0

        # We have one slot per execution port at each step in the schedule.
        # Each port then has its own schedule (list) with each entry being the
        # DAGNode representing the operation to be performed or None
        # if a slot is empty (nop).
        slot = []
        for port in range(NUM_EXECUTION_PORTS):
            slot.append([None])

        # Generate a list of all operations that have their dependencies
        # satisfied and are thus ready to go
        available_ops = self.operations_ready()

        while available_ops:

            # Attempt to schedule each operation
            for operation in available_ops:
                port = CPU_EXECUTION_PORTS[operation.node_type]
                if not slot[port][step]:
                    # Put this operation into next slot on appropriate port
                    slot[port][step] = operation
                    # Mark the operation as done (executed) and update
                    # any consumers
                    operation.mark_ready()

            for port in range(NUM_EXECUTION_PORTS):
                # Prepare the next slot in the schedule on this port
                slot[port].append(None)

            if sched_to_dot:
                self.to_dot(name=self._name+"_step{0}.gv".format(step+1))

            # Update our list of operations that are now ready to be
            # executed
            available_ops = self.operations_ready()

            # Move on to the next step in the schedule that we are
            # constructing
            step += 1

            if step > MAX_SCHEDULE_LENGTH:
                raise DAGError(
                    "Unexpectedly long schedule ({0} steps) - this is "
                    "probably a bug.".format(step))
        return step, slot

    def operations_ready(self):
        ''' Create a list of all operations in the DAG that are ready to
        be executed (all producers are 'ready') '''

        available_ops = []
        # Check nodes on critical path first so as to prioritise them
        # when generating schedule
        if self._critical_path:
            available_ops.extend(
                ready_ops_from_list(self._critical_path.nodes))

            # Next we check the dependencies of the next un-computed node
            # on the critical path
            input_node = self._critical_path.input_node
            node_list = self._critical_path.nodes
            node = input_node
            while True:
                if not node.ready:
                    # This node is the next one on the critical path - look
                    # at its dependencies
                    nodes = node.walk()
                    available_ops.extend(ready_ops_from_list(nodes))
                    break

                if not node.has_consumer:
                    # Have reached the output of the critical path
                    break

                # Find the next node on the critical path
                for consumer in node.consumers:
                    if consumer in node_list:
                        node = consumer
                        break

        # Finally, broaden the search out to the whole tree...
        available_ops.extend(ready_ops_from_list(self._nodes.itervalues()))

        # Remove duplicates from the list while preserving their order
        unique_available_ops = []
        for opnode in available_ops:
            if opnode not in unique_available_ops:
                unique_available_ops.append(opnode)

        return unique_available_ops
