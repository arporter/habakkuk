
''' Module containing class related to schedule of operations to
be performed on a core '''

# Maximum length of schedule we expect to handle.
MAX_SCHEDULE_LENGTH = 500

# Whether or not to try and include instruction latency costs when
# computing the number of cycles a schedule will require
ALLOW_FOR_LATENCY = False


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

        # Use a dictionary to store how many multiplications can be
        # overlapped with each division. The division FLOP is the key
        # and the associated entry is itself a dictionary with keys
        # "*" and "+". The values associated with each of those
        # entries is the number of those operations that have been
        # overlapped with the division. If no entry exists for a given
        # division OP then no operations have been overlapped with it.
        self._overlaps = {}

        # Output this initial graph
        if to_dot:
            dag.to_dot(name=dag.name+"_step0.gv")

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
                dag.to_dot(name=dag.name+"_step{0}.gv".format(step+1))

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

        from habakkuk.config_ivy_bridge import OPERATORS, \
            SUPPORTS_DIV_MUL_OVERLAP, div_overlap_mul_cost, \
            NUM_EXECUTION_PORTS

        # Allow for out-of-order execution and overlapping of
        # operations. e.g. on Ivy Bridge, operations on different
        # execution ports can be overlapped *and* multiplications may
        # be overlapped with a division, even though they execute on
        # the same port.
        if SUPPORTS_DIV_MUL_OVERLAP:
            self._compute_overlaps()

        # Now we compute the cost of the schedule by looking for the most
        # expensive operation at each step. We also construct a textual
        # representation of the schedule for output.

        cost = 0  # Total cost of this schedule (cycles)
        step_count = 0  # Counter for non-empty steps in schedule
        sched_str = ""
        for step in range(0, self._nsteps):

            max_cost = 0
            sched_line = ""

            # Find the most expensive operation on any port at this step of
            # the schedule
            for port in range(NUM_EXECUTION_PORTS):

                sched_line += " {0:4s}".format(str(self._slots[port][step]))
                port_cost = 0

                # If there is an operation on this port at this step of
                # the schedule then calculate its cost...
                if self._slots[port][step]:
                    flop = self._slots[port][step]
                    operator = str(flop)
                    if operator == "/" and flop in self._overlaps:
                        # If the operation is a division *and* we've overlapped
                        # some multiplications with it then we must look-up
                        # the cost
                        port_cost = div_overlap_mul_cost(self._overlaps[flop])
                    else:
                        port_cost = OPERATORS[operator]["cost"]

                    if ALLOW_FOR_LATENCY:
                        # Account for operation latency - assume we have to
                        # pay it and then subsequently set it to zero if
                        # we don't
                        latency = OPERATORS[operator]["latency"]

                        # If this isn't the first step in the schedule *and*
                        # the port executed an operation in the previous step
                        # then check what type it was
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
            if max_cost:
                # Only output this stage in the schedule if it contains an
                # operation (some stages may not because of the overlapping
                # of ops)
                step_count += 1
                sched_str += "{0:3s}".format(str(step_count)) + sched_line + \
                             " (cost = {0})\n".format(max_cost)
                cost += max_cost

        # Create header string for print-out of schedule
        print("Schedule contains {0} steps:".format(step_count))
        print("    {0:^30s}".format("Execution Port"))
        sched_line = "    "
        for port in range(NUM_EXECUTION_PORTS):
            sched_line += " {0:^4n}".format(port)
        print(sched_line)
        # Print the schedule itself
        print(sched_str)

        return cost

    def _compute_overlaps(self):
        ''' Work through the schedule and, for each division operation,
        overlap as many multiplications/additions/subtractions with it
        as possible '''
        from habakkuk.config_ivy_bridge import CPU_EXECUTION_PORTS
        div_port = CPU_EXECUTION_PORTS["/"]
        add_port = CPU_EXECUTION_PORTS["+"]
        for idx, flop in enumerate(self._slots[div_port]):
            if str(flop) != '/':
                continue
            # Get the nodes that are inputs to this division
            # operation
            div_producers = flop.walk()
            # Now work backwards from the division and delete any
            # independent mult operations as they can be overlapped
            # with the division
            for step in range(idx-1, -1, -1):
                for port in [div_port, add_port]:
                    opn = self._slots[port][step]
                    if str(opn) in ["*", "+", "-"]:
                        if opn not in div_producers:
                            # This op is independent of the division
                            # and may therefore be overlapped with it
                            self._overlap_ops(flop, idx, step, port)

            # Same again but this time work forwards from the division
            for step in range(idx+1, self.nsteps):
                for port in [div_port, add_port]:
                    opn = self._slots[port][step]
                    if str(opn) in ["*", "+", "-"]:
                        # Get the nodes that this op is
                        # dependent upon
                        ancestors = opn.walk()
                        if flop not in ancestors:
                            # The division isn't one of them so we can
                            # overlap this operation with it
                            self._overlap_ops(flop, idx, step, port)

    def _overlap_ops(self, flop, step_div, step, port):
        '''Overlap the operation on port `port` at step `step` with the
        division operation `flop` which occurs at `step_div` in the
        schedule

        :param flop: The division operation with which to overlap
        :param step_div: The step in the Schedule at which this divsion occurs
        :param step: the step in the Schedule at which the operation to be
                     overlapped occurs
        :param port: the port on the CPU on which the operation to be
                     overlapped is executed
        :type flop: DAGNode
        :type step_div: integer
        :type step: integer
        :type port: integer

        '''
        from habakkuk.config_ivy_bridge import CPU_EXECUTION_PORTS, \
            MAX_DIV_OVERLAP
        opn = self._slots[port][step]
        op_str = str(opn)
        if op_str == "-":
            # Treat subtraction as addition when considering overlap
            op_str = "+"
        if flop not in self._overlaps:
            # We've not overlapped anything with flop before so set-up
            # the dictionary entries
            self._overlaps[flop] = {"*": 0, "+": 0}
        if self._overlaps[flop][op_str] < MAX_DIV_OVERLAP[op_str]:
            self._overlaps[flop][op_str] += 1
            # If this is an addition/subtraction then move it to the
            # same step as the division with which it is overlapped
            # (purely for display purposes)
            if op_str == "+":
                self._slots[CPU_EXECUTION_PORTS["+"]][step_div] = \
                    self._slots[port][step]
            # Remove it from this slot in the
            # schedule
            self._slots[port][step] = None
