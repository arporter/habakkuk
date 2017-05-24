
''' This module provides support for the construction of a Directed
    Acyclic Graph. '''

from fparser import Fortran2003
from habakkuk.dag_node import DAGNode, DAGError
# TODO manange the import of these CPU-specific values in a way that permits
# the type of CPU to be changed
from habakkuk.config_ivy_bridge import OPERATORS, EXAMPLE_CLOCK_GHZ, \
    FORTRAN_INTRINSICS, CPU_EXECUTION_PORTS


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
    if node1.node_type == "FMA":
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
            return False
    return matches


def differ_by_constant(node1, node2):
    ''' Returns True if the two expressions represented by node1 and node2
    differ only by a numerical constant. '''

    # Are the two expressions identical?
    if subgraph_matches(node1, node2):
        return True

    # Check that one or both nodes have producers otherwise we have
    # two entirely different nodes
    if not (node1.producers or node2.producers):
        return False

    # If the top-level node is not a variable then it must be either "+" or
    # "-" if the two expressions are to differ by just a constant
    node1_is_pm = False
    node2_is_pm = False

    if node1.node_type:
        node1_is_pm = node1.node_type in ["+", "-"]
    if node2.node_type:
        node2_is_pm = node2.node_type in ["+", "-"]

    if not (node1_is_pm or node2_is_pm):
        return False

    if node1_is_pm and node2_is_pm:
        #  node1=="+" and node2=="-" (e.g. "ji + 1" and "ji - 1")
        #    Both sets of producers must contain a Constant and the producer
        #    that is not a Constant must be the same for both nodes
        found = False
        for node in node1.producers:
            if node.node_type == "constant":
                found = True
            else:
                non_constant1 = node
        if not found:
            return False
        found = False
        for node in node2.producers:
            if node.node_type == "constant":
                found = True
            else:
                non_constant2 = node
        if not found:
            return False

    else:
        #  node{1,2}=="+-" and node{2,1}==var (e.g. "ji + 1" and "ji")
        #    The node that is an operator must have one producer that is a
        #    Constant and the other producer must match the other node.
        if node1_is_pm:
            producers = node1.producers
            non_constant1 = node2
        else:
            producers = node2.producers
            non_constant1 = node1
        found = False
        for node in producers:
            if node.node_type == "constant":
                found = True
            else:
                non_constant2 = node
        if not found:
            return False
    return subgraph_matches(non_constant1, non_constant2)


def ready_ops_from_list(nodes):
    ''' Look through supplied list of nodes and identify those that
    are operations/intrinsics which are ready to execute '''
    op_list = []
    for node in nodes:
        if not node.ready and node.node_type in OPERATORS and \
           node.dependencies_satisfied:
            op_list.append(node)
    return op_list


def flop_count(nodes):
    '''The number of *floating point* operations in the supplied list of
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
        if node.node_type in OPERATORS and not node.is_integer:
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
        # Holds the Schedule for this DAG
        self._schedule = None

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

    def add_assignments(self, assignments, mapping, all_integer=False):
        ''' Add to the existing DAG using the supplied list of
        assignments. Each assignment is an instance of a
        fparser.Fortran2003.Assignment_Stmt.

        assignments - list of Fortran2003.Assignment_Stmt objects
        mapping - variable-name map to allow for repeated assignment,
                  e.g. i' = i + 1
        all_integer - specifies whether the set of assignments is for
                      all-integer quantities. '''
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
            rhs_node_list = self.make_dag(tmp_node, assign.items[2:], mapping,
                                          array_index=all_integer)

            # Only update the map once we've created a DAG of the
            # assignment statement. This is because any references
            # to this variable in that assignment are to the previous
            # version of it, not the one being assigned to.
            if lhs_var.indexed_name in mapping:
                mapping[lhs_var.indexed_name] += "'"
            else:
                # The LHS variable wasn't already in the map - we use the full
                # variable expression (including any array indices) as the
                # dictionary key. We only store the base of the variable name
                # as the dictionary entry (so that when we assign to array
                # elements, the resulting node is named eg. array'(i,j)).
                mapping[lhs_var.indexed_name] = lhs_var.orig_name

                for node in rhs_node_list:
                    if node.variable:
                        if node.variable.indexed_name == \
                           lhs_var.indexed_name:
                            # If the LHS variable appeared on the RHS
                            # of this assignment then we must append a
                            # ' character to its name. This then means
                            # we get a new node representing the
                            # variable being assigned to.
                            mapping[lhs_var.indexed_name] += "'"
                            break

            # Create the LHS node proper now that we've updated the
            # naming map. We use make_dag() to do this so that we
            # capture any dependencies on variables within array-index
            # expressions
            new_nodes = self.make_dag(None,
                                      [assign.items[0]],
                                      mapping, array_index=all_integer)
            lhs_node = new_nodes[0]
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
                # of this node. The name of this variable has already
                # been re-mapped when it was created.
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
                # If this node is now flagged as being integer we update
                # it with this information. This enables us to handle the
                # case where array indices are set within the body of a loop
                if is_integer:
                    node.is_integer = True
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

    def count_nodes(self, node_type, include_integers=False):
        ''' Count the number of nodes in the graph that are of the
        specified type '''
        ancestors = self.output_nodes()
        node_list = set()
        for node in ancestors:
            if node.node_type == node_type:
                if include_integers:
                    node_list.add(node)
                else:
                    if not node.is_integer:
                        node_list.add(node)
            nodes = node.walk(node_type)
            for new_node in nodes:
                if include_integers:
                    node_list.add(new_node)
                else:
                    if not new_node.is_integer:
                        node_list.add(new_node)
        if node_type not in OPERATORS:
            # If we're dealing with, e.g. array references we don't
            # want to count reads and write separately. We must
            # therefore examine the full original name of the variable
            # rather than the name given to the node in the DAG (since
            # that may have a ' appended if the variable is readwrite)
            unique_names = set()
            for node in node_list:
                if node.variable:
                    unique_names.add(node.variable.full_orig_name)
            return len(unique_names)
        else:
            return len(node_list)

    def cache_lines(self):
        ''' Count the number of cache lines accessed by the graph. This
        is the number of distinct memory references. We assume that
        any array reference of the form u(i+1,j) will have been fetched
        when u(i,j) was accessed. '''
        # Dictionary of unique array references
        array_refs = {}
        # List of nodes representing array accesses
        nodes = []
        # Loop over all nodes in the tree, looking for array references
        ancestors = self.output_nodes()
        for ancestor in ancestors:
            nodes += ancestor.walk("array_ref")
            # Include this output node in the list if it is of the correct type
            if ancestor.node_type == "array_ref":
                nodes.append(ancestor)
        # Ensure we have a list of unique nodes - use a set
        unique_nodes = set(nodes)

        # Examine all of the array refs that we've found
        for node in unique_nodes:
            if node.is_integer:
                # Ignore integer array references
                # TODO include integer array refs in cache-line count
                continue

            if node.variable.name not in array_refs:
                array_refs[node.variable.name] = []
            # For each access to a given array we add a list of the
            # index-expressions...
            # array_index_nodes can contain None, e.g. if the corresponding
            # array index expression is ':'
            array_refs[node.variable.name].append(node.array_index_nodes)

        # Loop over each array that has been accessed and examine the ways in
        # which it is accessed
        cline_count = 0
        for array in array_refs:
            if len(array_refs[array]) == 1:
                # There's only one access to an array with this name
                cline_count += 1
                continue
            # We need to find the number of unique array accesses
            # We can construct a string representation of each index expression
            # for all indices > 1.
            # Find out how many dimensions the first access to this array has
            ndims = len(array_refs[array][0])

            if ndims > 1:
                index_exprns = set()
                # Loop over all accesses to this array and construct a string
                # representation of their non-rank 1 index expressions.
                # Each unique expression then represents a different cache-line
                # access
                access_hash = []
                for access in array_refs[array]:
                    index_str = "_".join([str(obj) for obj in access[1:]])
                    access_hash.append(index_str)
                    index_exprns.add(index_str)

                # Now check the array accesses that we've found to match in
                # all bar the first dimension
                for index_str in index_exprns:
                    # Construct a list of the accesses whose non-rank-1
                    # indexing matches the hash in index_str
                    match_list = []
                    for idx, access in enumerate(array_refs[array]):
                        if access_hash[idx] == index_str:
                            match_list.append(access)
                    # We potentially have as many non-contiguous accesses
                    # as we have items in the list
                    count = len(match_list)
                    # Loop over all pairs of such accesses. For every match
                    # we reduce the number of non-contiguous accesses
                    # by one...
                    for idx, match1 in enumerate(match_list[:-1]):
                        for match2 in match_list[idx+1:]:
                            if differ_by_constant(match1[0], match2[0]):
                                count -= 1
                    cline_count += count
            else:
                # This is an array of rank 1 (1D). We potentially need
                # as many cache lines as there are array accesses
                count = len(array_refs[array])
                # Compare each access to this array. For every one that
                # is the same +/- a constant, we reduce the number of
                # cache lines required by 1.
                for idx, access1 in enumerate(array_refs[array][:-1]):
                    for access2 in array_refs[array][idx+1:]:
                        # If a full slice of an array is accessed (e.g.
                        # my_array(:) then we don't have a node
                        # describing the index expression.
                        # TODO I think we should have such a node
                        if (access1 and access2) and \
                           differ_by_constant(access1[0], access2[0]):
                            count -= 1
                cline_count += count
        return cline_count

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
                    node_list.append(opnode)
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
                  isinstance(child, Fortran2003.Int_Literal_Constant) or
                  isinstance(child, Fortran2003.Char_Literal_Constant) or
                  isinstance(child, Fortran2003.Logical_Literal_Constant)):
                # This is a constant and thus a leaf in the tree
                const_var = Variable()
                const_var.load(child, mapping)
                tmpnode = self.get_node(parent, variable=const_var,
                                        node_type="constant",
                                        is_integer=array_index)
                if is_division and idx == 2:
                    parent.operands.append(tmpnode)
            elif isinstance(child, Fortran2003.Part_Ref):
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
                    from habakkuk.parse2003 import walk_ast
                    # First check to see whether this Part_Ref itself contains
                    # an Array_Section or a character string. If it does then
                    # we can immediately assume that it is a function call.
                    # item[0] is the Name of this Part_Ref so we can skip that
                    section_list = walk_ast(
                        child.items[1:],
                        [Fortran2003.Array_Section,
                         Fortran2003.Char_Literal_Constant])
                    if not section_list:
                        # It didn't - does it contain any Part_Refs that then
                        # have Subscript_Triplets?
                        part_ref_list = walk_ast(child.items[1:],
                                                 [Fortran2003.Part_Ref])
                        for part_ref in part_ref_list:
                            section_list += walk_ast(
                                part_ref.items[1:],
                                [Fortran2003.Subscript_Triplet])
                            if section_list:
                                # We've found one so that means it must be
                                # a function call
                                break

                    if section_list:
                        # An array reference won't include an array
                        # section in the index expression so this must
                        # be a call to a routine. We make each such
                        # call a unique node.
                        # TODO remove this restriction by checking the args
                        # passed to the call.
                        tmp_node = self.get_node(parent,
                                                 name=str(child.items[0]),
                                                 unique=True,
                                                 node_type="call")
                        node_list.append(tmp_node)
                        node_list += self.make_dag(tmp_node,
                                                   child.items[1:],
                                                   mapping, array_index)
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
                        if isinstance(child.items[1],
                                      Fortran2003.Section_Subscript_List):
                            arg_list = child.items[1].items
                        else:
                            arg_list = child.items[1:]
                        for idx, item in enumerate(arg_list):

                            child_nodes = self.make_dag(array_node, [item],
                                                        mapping,
                                                        array_index=True)
                            node_list += child_nodes

                            # We don't know whether array_node is new
                            # or a pre-existing node. If the
                            # latter then we don't want to add to
                            # the existing array_index_nodes list
                            if len(array_node.array_index_nodes) < \
                               len(arg_list):
                                if idx > 0:
                                    # Just store a string representation
                                    # for any index expression other than
                                    # the first
                                    # TODO make this more robust by storing
                                    # node reference and comparing
                                    # sub-graphs
                                    array_node.array_index_nodes.append(
                                        str(item))
                                else:
                                    # For the first array index we store the
                                    # parent node of the whole index
                                    # expression. This permits us to
                                    # subsequently reason about array
                                    # accesses that differ only in the
                                    # first index and therefore might share
                                    # a cache line.
                                    if child_nodes:
                                        tmpnode = child_nodes[0]
                                    else:
                                        tmpnode = self.get_node(
                                            array_node, name=str(item),
                                            is_integer=True, unique=True)
                                    array_node.array_index_nodes.append(
                                        tmpnode)

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
            elif array_index and isinstance(child,
                                            Fortran2003.Subscript_Triplet):
                # We've got a ':' as part of an array index expression -
                # don't generate a node for this.
                pass
            elif (isinstance(child, Fortran2003.And_Operand) or
                  isinstance(child, Fortran2003.Or_Operand)):
                # We have an expression that is something like
                # .NOT. sdjf % ln_clim
                # and can just carry-on down to the children
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
            elif isinstance(child, Fortran2003.Array_Constructor):
                # This is an array constructor. Make a node for it.
                node_list.append(self.get_node(parent, name=str(child)))
            elif isinstance(child, Fortran2003.Structure_Constructor):
                # This is a structure constructor. Make a node for it.
                node_list.append(self.get_node(parent, name=str(child)))
            elif isinstance(child, Fortran2003.Structure_Constructor_2):
                # This is a structure constructor, e.g.
                # mask = tmask_i(:, :). Make a node for it.
                tmp_node = self.get_node(parent, name=str(child.items[0]))
                node_list.append(tmp_node)
                node_list += self.make_dag(tmp_node, child.items[2:], mapping)
            elif (isinstance(child, Fortran2003.Level_3_Expr) or
                  isinstance(child, Fortran2003.Level_4_Expr)):
                # Have an expression that is something like
                # TRIM(ssnd(ji) % clname) // '_cat' // cli2. Carry on
                # down to the children
                node_list += self.make_dag(parent, child.items, mapping)
            elif isinstance(child, Fortran2003.Data_Ref):
                # Have an expression that is something like
                # ssnd(ji) % clname. Make a node to represent it.
                dvar = Variable()
                dvar.load(child, mapping)
                tmp_node = self.get_node(parent, variable=dvar)
                node_list.append(tmp_node)
                # TODO handle case where the component of the derived type
                # is itself an array, e.g. ssnd % clname(ji,jj)
                # node_list += self.make_dag(tmp_node,
                #                            [child.items[1]], mapping)
            elif isinstance(child, Fortran2003.Equiv_Operand):
                # A logical expression c.f.
                #  kinfo == OASIS_Recvd .OR. kinfo == OASIS_FromRest
                pass
            else:
                raise DAGError("Unrecognised child; type = {0}, str = '{1}'".
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
            if len(node.consumers) > 1 and not node.is_integer:
                multiple_consumers.append(node)
        return multiple_consumers

    def update_integer_nodes(self):
        ''' Walk through the graph and ensure that information on which
        nodes are integer is propagated as far as possible '''
        while True:
            dag_updated = False
            for node in self._nodes.itervalues():
                if node.is_integer:
                    # This node is integer. Look at the node(s) that consume
                    # it. If they are not already marked as being integer
                    # but have all-integer producers then they must also be
                    # integer.
                    for cnode in node.consumers:
                        # TODO only propagate changes if cnode
                        # represents an operator.
                        if cnode.is_integer or \
                           cnode.node_type not in OPERATORS:
                            # The consumer of this integer node is already
                            # marked as being an integer so skip onto the next
                            # consumer.
                            continue
                        all_integer = True
                        # This node is not marked as being integer. Are all of
                        # its producers integer?
                        for pnode in cnode.producers:
                            if not pnode.is_integer:
                                all_integer = False
                                break
                        if all_integer:
                            # All producers are integer so mark this node
                            # as being integer
                            cnode.is_integer = True
                            # Must now re-start our search
                            dag_updated = True
                            break
                    if dag_updated:
                        break
            if not dag_updated:
                break
        return

    def prune_duplicate_nodes(self):
        ''' Walk through the graph and remove all but one of any
        duplicated sub-graphs that represent FLOPs'''

        _found_duplicate = True

        while _found_duplicate:

            # Update list of nodes with > 1 consumer
            multiple_consumers = self.nodes_with_multiple_consumers()
            if len(multiple_consumers) == 0:
                break

            # Each node with > 1 consumer represents a possible duplication
            for multi_node in multiple_consumers[:]:

                matching_nodes = []
                node1 = multi_node.consumers[0]
                for node2 in multi_node.consumers[1:]:
                    if subgraph_matches(node1, node2):
                        matching_nodes.append(node2)

                if not matching_nodes:
                    # None of the multiple consumers of the current node
                    # represents duplicate computation so skip on to the
                    # next node.
                    _found_duplicate = False
                    continue

                # We've found one or more nodes that match node1
                _found_duplicate = True

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

                for node2 in matching_nodes[:]:
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

                # Break out of this loop so that we restart our loop over
                # nodes with multiple consumers
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
        # Count all real array references
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
        # If a loop body contains, e.g. just an assignment then it's possible
        # we won't have a critical path
        if self._critical_path:
            ncycles = self._critical_path.cycles()
        else:
            print "No FLOPS found so have no critical path"
            ncycles = 0
            max_mem_bw = 0
            max_flops_per_hz = 0

        if ncycles > 0:
            print "  Everything in parallel to Critical path:"
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

        if num_cache_ref and ncycles:
            # Kernel/DAG will take at least ncycles/CLOCK_SPEED (s)
            max_mem_bw = float(mem_traffic_bytes) / float(ncycles)
            print ("    Associated mem bandwidth = {0:.2f}*CLOCK_SPEED "
                   "bytes/s".format(max_mem_bw))

        # Construct a schedule for the execution of the nodes in the DAG,
        # allowing for the microarchitecture of the chosen CPU
        # TODO currently this is picked up from config_ivy_bridge.py
        cost = self.schedule().cost
        print "  Estimate using computed schedule:"
        print "    Cost of schedule as a whole = {0} cycles".format(cost)
        if self._schedule.nsteps:
            sched_flops_per_hz = float(total_flops)/float(cost)
            print ("    FLOPS from schedule (ignoring memory accesses) = "
                   "{:.4f}*CLOCK_SPEED".format(sched_flops_per_hz))
            if num_cache_ref:
                # Kernel/DAG will take at least ncycles/CLOCK_SPEED (s)
                sched_mem_bw = float(mem_traffic_bytes) / float(cost)
                print ("    Associated mem bandwidth = {0:.2f}*CLOCK_SPEED "
                       "bytes/s".format(sched_mem_bw))
        else:
            sched_flops_per_hz = 0

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
        if net_cost:
            perfect_sched_flops_per_hz = float(total_flops)/float(net_cost)
        else:
            perfect_sched_flops_per_hz = 0

        if num_cache_ref:
            perfect_sched_mem_bw = float(mem_traffic_bytes) / float(net_cost)
        else:
            perfect_sched_mem_bw = 0

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

    def verify_acyclic(self):
        ''' Check that the graph is acyclic. If it isn't then something
        has gone wrong. '''
        for onode in self.output_nodes():
            node_list = [onode]
            for node in onode.producers:
                node.walk(ancestor_list=node_list)


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

    def schedule(self, to_dot=False):
        ''' Compute, store and return the execution schedule for this DAG '''
        if not self._schedule:
            # For reproducible results we first calculate the critical
            # path.
            if not self._critical_path:
                self.calc_critical_path()
            from habakkuk.schedule import Schedule
            self._schedule = Schedule(self, to_dot)
        return self._schedule
