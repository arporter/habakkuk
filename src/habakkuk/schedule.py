
''' Module containing class related to schedule of operations to
be performed on a core '''

# Maximum length of schedule we expect to handle.
MAX_SCHEDULE_LENGTH = 500


class Schedule(object):
    ''' Description of the sequence of operations to be executed
    on a core '''

    def __init__(self, dag, to_dot=False):
        '''Create a schedule mapping operations to hardware

        Creates a schedule describing how the nodes/operations in the DAG
        map onto the available hardware (execution ports on an Intel CPU)

        Keyword arguments:
        to_dot - Whether or not to output each step in the schedule to
                 a separate dot file to enable visualisation.
        '''
        from habakkuk.config_ivy_bridge import NUM_EXECUTION_PORTS, \
            CPU_EXECUTION_PORTS
        # Flag all input nodes as being ready
        input_nodes = dag.input_nodes()
        for node in input_nodes:
            node.mark_ready()

        # Output this initial graph
        if to_dot:
            dag.to_dot(name=dag._name+"_step0.gv")

        # Construct a schedule
        step = 0

        # We have one slot per execution port at each step in the schedule.
        # Each port then has its own schedule (list) with each entry being the
        # DAGNode representing the operation to be performed or None
        # if a slot is empty (nop).
        self._slots = []
        for port in range(NUM_EXECUTION_PORTS):
            self._slots.append([None])

        # Generate a list of all operations that have their dependencies
        # satisfied and are thus ready to go
        available_ops = dag.operations_ready()

        while available_ops:

            # Attempt to schedule each operation
            for operation in available_ops:
                port = CPU_EXECUTION_PORTS[operation.node_type]
                if not self._slots[port][step]:
                    # Put this operation into next slot on appropriate port
                    self._slots[port][step] = operation
                    # Mark the operation as done (executed) and update
                    # any consumers
                    operation.mark_ready()

            for port in range(NUM_EXECUTION_PORTS):
                # Prepare the next slot in the schedule on this port
                self._slots[port].append(None)

            if to_dot:
                dag.to_dot(name=dag._name+"_step{0}.gv".format(step+1))

            # Update our list of operations that are now ready to be
            # executed
            available_ops = dag.operations_ready()

            # Move on to the next step in the schedule that we are
            # constructing
            step += 1

            if step > MAX_SCHEDULE_LENGTH:
                from habakkuk.dag import DAGError
                raise DAGError(
                    "Unexpectedly long schedule ({0} steps) - this is "
                    "probably a bug.".format(step))
        # Save the no. of steps in the schedule
        self._nsteps = step

    @property
    def nsteps(self):
        ''' Returns the number of steps in the Schedule '''
        return self._nsteps

    @property
    def cost(self):
        ''' Calculate the cost (in cycles) of this schedule.

        :return: The estimated cost (in cycles) of the schedule '''

        # Use a dictionary to store how many multiplications can be
        # overlapped with each division. The division FLOP is the
        # key and the number of multiplications the associated entry.
        # This dictionary is left empty if the microarchitecture does not
        # support overlapping of multiplications with divisions.
        overlaps = {}
        from config_ivy_bridge import CPU_EXECUTION_PORTS, OPERATORS, \
            SUPPORTS_DIV_MUL_OVERLAP, div_overlap_mul_cost, NUM_EXECUTION_PORTS
        if SUPPORTS_DIV_MUL_OVERLAP:
            flop_port = CPU_EXECUTION_PORTS["/"]
            for idx, flop in enumerate(self._slots[flop_port]):
                if str(flop) == '/':
                    # Get the nodes that are inputs to this division
                    # operation
                    div_producers = flop.walk()
                    # Now work backwards from the division and delete any
                    # independent mult operations as they can be overlapped
                    # with the division
                    for step in range(idx-1, -1, -1):
                        if str(self._slots[flop_port][step]) == "*":
                            if self._slots[flop_port][step] not in div_producers:
                                # This product is independent of the division
                                # and may therefore be overlapped with it
                                if flop in overlaps:
                                    overlaps[flop] += 1
                                else:
                                    overlaps[flop] = 1
                                self._slots[flop_port][step] = None
                    # Same again but this time work forwards from the division
                    for step in range(idx+1, self.nsteps):
                        if str(self._slots[flop_port][step]) == "*":
                            # Get the nodes that this multiplication is
                            # dependent upon
                            ancestors = self._slots[flop_port][step].walk()
                            if flop not in ancestors:
                                # The division isn't one of them so we can
                                # overlap this operation with it
                                if flop in overlaps:
                                    overlaps[flop] += 1
                                else:
                                    overlaps[flop] = 1
                                self._slots[flop_port][step] = None

        print "Schedule contains {0} steps:".format(self._nsteps)

        # Create header string for print-out of schedule
        print "    {0:^30s}".format("Execution Port")
        header_str = "    "
        for port in range(NUM_EXECUTION_PORTS):
            header_str += " {0:^4n}".format(port)
        print header_str

        cost = 0
        for step in range(0, self._nsteps):

            sched_str = "{0:3s}".format(str(step))
            max_cost = 0

            # Find the most expensive operation on any port at this step of
            # the schedule
            for port in range(NUM_EXECUTION_PORTS):

                sched_str += " {0:4s}".format(self._slots[port][step])

                port_cost = 0

                # If there is an operation on this port at this step of
                # the schedule then calculate its cost...
                if self._slots[port][step]:
                    flop = self._slots[port][step]
                    operator = str(flop)
                    if operator == "/" and flop in overlaps:
                        # If the operation is a division *and* we've overlapped
                        # some multiplications with it then we must look-up
                        # the cost
                        port_cost = div_overlap_mul_cost(overlaps[flop])
                    else:
                        port_cost = OPERATORS[operator]["cost"]

                    if False:
                        # Account for operation latency - assume we have to
                        # pay it and then subsequently set it to zero if we don't
                        latency = OPERATORS[operator]["latency"]

                        # If this isn't the first step in the schedule *and* the
                        # port executed an operation in the previous step then
                        # check what type it was
                        if step > 0 and self._slots[port][step-1]:
                            previous_op = str(self._slots[port][step-1])
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

        
