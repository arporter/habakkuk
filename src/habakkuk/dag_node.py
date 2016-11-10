
''' Module containing class holding information on a single node in
    a Directed Acyclic Graph '''

from habakkuk.config_ivy_bridge import OPERATORS, FORTRAN_INTRINSICS

# Valid types for a node in the DAG
VALID_NODE_TYPES = OPERATORS.keys() + ["constant", "array_ref", "call"]

INDENT_STR = "  "
# At what depth to abort attempting to recursively walk down a graph
# (hitting this probably indicates a bug!)
MAX_RECURSION_DEPTH = 120


class DAGError(Exception):
    ''' Class for exceptions related to DAG manipulations '''

    def __init__(self, value):
        self.value = "DAG Error: " + value

    def __str__(self):
        return repr(self.value)


class DAGNode(object):
    ''' Base class for a node in a Directed Acyclic Graph '''

    def __init__(self, parent=None, name=None, digraph=None, variable=None,
                 is_integer=False):
        # Keep a reference back to the digraph object containing
        # this node
        self._digraph = digraph
        # The list of nodes upon which this node has a dependence
        self._producers = []
        # The list of nodes that have a dependence upon this node
        self._consumers = []
        if parent:
            # If a consumer of this node has been supplied then update
            # both its state and that of this node to record the
            # relationship
            self.add_consumer(parent)
            parent.add_producer(self)
        # The type of this node
        self._node_type = None
        # Whether or not this node represents an integer quantity (as
        # opposed to the default of floating point)
        self._integer = is_integer
        # The name of this node - used to label the node in DOT. This
        # name is not necessarily the same as the name of the variable
        # in the Fortran code: if it has been assigned to then it becomes
        # a new node and we will have appended a "'" to its name.
        if name:
            self._name = name
        elif variable:
            self._name = variable.full_name
        if variable and variable.is_array_ref:
            self._node_type = "array_ref"
        # The variable (if any) that this node represents
        self._variable = variable
        # The inclusive weight (cost) of this node. This is the cost of
        # this node plus that of all of its dependencies (producers). This
        # then enables us to find the critical path through the graph.
        self._incl_weight = 0
        # List of key operands required to uniquely identify the
        # operation associated with this node (if it is an
        # operator). For an FMA this is the two nodes that are
        # multiplied. For a division it is the denominator.
        self._operands = []
        # Whether the quantity represented by this node is ready to
        # be consumed (if an operator then that means it has been
        # executed). Used when generating a schedule for the DAG.
        # Since we're not interested in integer operations we mark
        # this node as ready if it represents an integer quantity.
        if self._integer:
            self._ready = True
        else:
            self._ready = False
        # If this node represents an array access then this list contains
        # a list of parent nodes, one for each array index expression
        self.array_index_nodes = []

    def __str__(self):
        return self.name

    @property
    def is_integer(self):
        ''' Returns True if the quantity represented by this node is
        an integer (or operates on integers) '''
        return self._integer

    @is_integer.setter
    def is_integer(self, flag):
        ''' Setter for the is_integer property of the node. Required because
        we may only determine that a node represents an integer quantity
        after we have first encountered it. '''
        self._integer = flag

    @property
    def dependencies_satisfied(self):
        ''' Returns true if all dependencies of this node are satisfied '''
        for node in self._producers:
            if not node.ready:
                return False
        return True

    @property
    def ready(self):
        ''' Getter method for self._ready. Used during schedule generation.
        Is set to true once (quantity represented by this) node has been
        computed/updated '''
        return self._ready

    def mark_ready(self):
        ''' Mark this node as ready (done). Propagate this up to any
        consumers of this node unless they are operators/intrinsics (which
        must be scheduled in order to be executed) '''
        self._ready = True
        for node in self._consumers:
            if node.node_type not in OPERATORS:
                if node.dependencies_satisfied:
                    node.mark_ready()

    @property
    def node_id(self):
        ''' Returns a unique string identifying this node in the graph '''
        return "node"+str(id(self))

    @property
    def name(self):
        ''' Returns the name (label) of this node '''
        return self._name

    @name.setter
    def name(self, new_name):
        ''' Set (or change) the name/label of this node. Note that if there
        is a Variable associated with this node then the name of that
        object overrides this. '''
        self._name = new_name

    def display(self, indent=0):
        ''' Prints a textual representation of this node to stdout '''
        prefix = indent*INDENT_STR
        print prefix+"\- "+self.name
        for child in self._producers:
            child.display(indent=indent+1)

    def add_producer(self, child):
        ''' Add a producer (dependency) to this node '''
        if child not in self._producers:
            self._producers.append(child)

    def rm_producer(self, child):
        ''' Remove a producer/child (dependency) from this node '''
        if child not in self._producers:
            raise DAGError("Node {0} is not a producer (dependency) for "
                           "this node ({1})".
                           format(str(child), str(self)))
        # Remove it from the list of producers/dependencies for this node
        self._producers.remove(child)

    def add_consumer(self, node):
        ''' Add the supplied node to the list of nodes that have this one as
        a dependency (child) '''
        if node not in self._consumers:
            self._consumers.append(node)

    def rm_consumer(self, node):
        ''' Remove the supplied node from the list of nodes that consume
        this node (have it as a dependency) '''
        if node not in self._consumers:
            raise DAGError("Node {0} does not have {1} as a consumer!"
                           .format(str(self), str(node)))
        self._consumers.remove(node)

    @property
    def has_consumer(self):
        ''' Returns true if one or more nodes have this node as a
        dependency '''
        if self._consumers:
            return True
        return False

    @property
    def has_producer(self):
        ''' Returns true if this node has one or more
        dependencies/producers. By default we ignore nodes representing
        integer quantities since they are only part of array-index
        expressions. '''
        for prod in self._producers:
            if not prod.is_integer:
                return True
        return False

    @property
    def consumers(self):
        ''' Returns the list of nodes that have this node as a dependency
        (i.e. they consume it) '''
        return self._consumers

    @property
    def producers(self):
        ''' Returns the list of dependencies/producers for this node '''
        return self._producers

    @property
    def node_type(self):
        ''' Returns the type of this node (one of VALID_NODE_TYPES) '''
        return self._node_type

    @node_type.setter
    def node_type(self, mytype):
        ''' Set the type of this node '''
        if mytype not in VALID_NODE_TYPES:
            raise DAGError("node_type must be one of {0} but "
                           "got '{1}'".format(VALID_NODE_TYPES, mytype))
        self._node_type = mytype

    @property
    def is_operator(self):
        ''' Returns true if this node represents a floating point operation '''
        return self._node_type in OPERATORS

    @property
    def variable(self):
        ''' Return the Variable object associated with this node or None
        if there isn't one '''
        return self._variable

    def walk(self, node_type=None, top_down=False, depth=0,
             ancestor_list=None):
        ''' Walk down the tree from this node and generate a list of all
        nodes of type node_type (excluding this node). If no node type is
        supplied then return all descendents '''
        if depth > MAX_RECURSION_DEPTH:
            print "Current node = ", str(self)
            print "Producers:"
            for idx, node in enumerate(self._producers):
                print idx, str(node), type(node)
            raise DAGError(
                "Max recursion depth ({0}) exceeded when walking tree".
                format(MAX_RECURSION_DEPTH))
        # If a list of ancestors has been provided then check that none
        # of this node's producers are in it
        if ancestor_list:
            for child in self._producers:
                if child in ancestor_list:
                    print "->".join(
                        ["{0}({1})".format(node.name, node.node_type)
                         for node in ancestor_list])
                    raise DAGError("Cyclic dependency: node '{0}' has node "
                                   "'{1}'  as both a producer and an ancestor"
                                   .format(str(self), str(child)))
                else:
                    ancestor_list.append(child)
        local_list = []
        if top_down:
            # Add the children of this node before recursing down
            for child in self._producers:
                if not node_type or child.node_type == node_type:
                    local_list.append(child)
            for child in self._producers:
                local_list += child.walk(node_type, top_down, depth+1,
                                         ancestor_list)
        else:
            for child in self._producers:
                local_list += child.walk(node_type, top_down, depth+1,
                                         ancestor_list)
                if not node_type or child.node_type == node_type:
                    local_list.append(child)
        return local_list

    @property
    def weight(self):
        ''' Returns the (exclusive) weight/cost of this node '''
        if not self._node_type or self._integer:
            return 0
        else:
            if self._node_type in OPERATORS:
                return OPERATORS[self._node_type]["cost"]
            else:
                return 0

    @property
    def incl_weight(self):
        ''' Getter for the inclusive weight of this node. This must have
        previously been calculated by a call to ``calc_weight()`` '''
        return self._incl_weight

    def calc_weight(self):
        ''' Calculate the inclusive weight of this node by recursing
        down the tree and summing the weight of all descendants '''
        self._incl_weight = self.weight
        for child in self._producers:
            self._incl_weight += child.calc_weight()
        return self._incl_weight

    def fuse_multiply_adds(self):
        ''' Recursively take any opportunities to fuse multiplication and
        addition operations. Returns the no. of FMAs created. '''
        fma_count = 0

        for child in self._producers:
            fma_count += child.fuse_multiply_adds()

        # If this node is a floating-point addition
        if self._node_type == "+" and not self._integer:
            # Loop over a copy of the list of producers as this loop
            # modifies the original
            for child in self._producers[:]:
                if child.node_type == "*":
                    # We can create an FMA. This replaces the addition
                    # operation and inherits the children of the
                    # multiplication operation.
                    for grandchild in child.producers:
                        self.add_producer(grandchild)
                        grandchild.add_consumer(self)
                        self._operands.append(grandchild)

                    # Delete the multiplication node - this automatically
                    # updates any nodes that have dependencies on it.
                    self._digraph.delete_node(child)

                    # Change the type of this node
                    self._name = "FMA"
                    self._node_type = "FMA"
                    if len(self._producers) != 3:
                        raise Exception("An FMA node must have three nodes "
                                        "as input but found {0}".
                                        format(len(self._producers)))
                    fma_count += 1
                    break
        return fma_count

    @property
    def operands(self):
        ''' Return the list of operands for this node. For a division this
        is the denominator. For a Fused Multiply Add this is the two nodes
        that are multiplied. '''
        return self._operands

    def critical_path(self, path):
        ''' Compute the critical (most expensive) path from this node '''
        # Add ourself to the path unless we represent an integer
        # quantity in which case we terminate the path.
        if self.is_integer:
            return
        path.append(self)
        # Find the child with the greatest inclusive weight that doesn't
        # represent an integer quantity
        max_weight = -0.01
        node = None
        for child in self._producers:
            if not child.is_integer and child.incl_weight > max_weight:
                max_weight = child.incl_weight
                node = child
        # Move down to that child
        if node:
            node.critical_path(path)

    def to_dot(self, fileobj, show_weight, include_integer_nodes=True):
        ''' Generate representation in the DOT language '''

        if not include_integer_nodes and self.is_integer:
            # Don't output nodes representing integer quantities
            return

        for child in self._producers:
            child.to_dot(fileobj, show_weight, include_integer_nodes)

        nodestr = "{0} [label=\"{1}".format(self.node_id,
                                            self.name)
        if show_weight:
            nodestr += " (w={0})".format(str(self._incl_weight))
        nodestr += "\""

        # Default node style is a black elipse
        node_colour = "black"
        node_shape = "ellipse"
        node_size = None

        if self._node_type:
            # Check whether it is an intrinsic first as intrinsics are
            # a special case of an operator
            if self._node_type in FORTRAN_INTRINSICS:
                node_colour = "gold"
                node_shape = "ellipse"
            elif self._node_type in OPERATORS:
                node_colour = "red"
                node_shape = "box"
                node_size = str(0.5 + 0.01*self.weight)
            elif self._node_type == "constant":
                node_colour = "green"
                node_shape = "ellipse"
            elif self._node_type == "array_ref":
                node_colour = "blue"
                node_shape = "ellipse"

        nodestr += ", color=\"{0}\", shape=\"{1}\"".format(node_colour,
                                                           node_shape)
        if node_size:
            nodestr += ", height=\"{0}\"".format(node_size)

        # Set node fill colours in order to animate the execution
        # schedule
        if self._integer:
            # Node represents an integer quantity and does not take
            # part in schedule. Use X11 colour 'Green Yellow' for such
            # nodes
            nodestr += ", style=\"filled\", fillcolor=\"#ADFF2F\""
        else:
            if self._ready:
                # Node has been executed/updated
                nodestr += ", style=\"filled\", fillcolor=\"grey\""
            elif self.dependencies_satisfied:
                # Node is ready to be executed/updated
                nodestr += ", style=\"filled\", fillcolor=\"green\""

        nodestr += "]\n"

        fileobj.write(nodestr)
        if self._consumers:
            fileobj.write(self.node_id+" -> {\n")
            for child in self._consumers:
                if not include_integer_nodes and child.is_integer:
                    continue
                fileobj.write(" "+child.node_id)
            fileobj.write("}\n")
