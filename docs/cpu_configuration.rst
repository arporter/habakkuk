CPU Configuration
=================

In order to predict performance on any given CPU, Habakkuk must be
configured with certain parameters that describe that CPU. Currently,
only parameters for the Intel Ivy Bridge micro-architecture are
supplied. In this section we describe the various parameters that
Habakkuk uses.

Instruction Cost
----------------

The number of cycles that a given arithmetic operation requires is
fundamental to constructing a performance estimate of a kernel.  For
floating-point operations on Intel architectures, Agner Fog provides
comprehensive data. However, since Habakkuk processes high-level
Fortran code, it must also account for Fortran intrinsic operations
such as SQRT and COS. The cost of these operations has been estimated
by running simple micro-benchmarks (dl_microbench) on the target
CPU. All of this data is provided to Habakkuk in a dictionary,
`OPERATORS`. The keys in this dictionary are the Fortran symbols for
the various arithmetic operations (e.g. "*" for multiplication) and
the names of Fortran intrinsics in uppercase (e.g. "SIN"). Each entry
in the dictionary is itself a dictionary with keys "cost"
and "flops". The entries for these keys are integers giving the number
of cycles and number of floating-point operations, respectively,
associated with the operation.

Mapping of Instructions to Execution Ports
------------------------------------------

In the Intel Ivy Bridge micro-architecture, instructions are
despatched to different "execution ports", depending on their
type. For example, floating-point multiplication and division is
handled by port 0 while addition and subtraction are sent to port 1.
Provided there are no dependencies between them, operations that are
mapped to different ports may be executed in parallel.

The number of different execution ports is specified in
`NUM_EXECUTION_PORTS` and the mapping of instructions to them is
supplied to Habakkuk as a dictionary, `CPU_EXECUTION_PORTS`. The keys
in this dictionary are the same as those in `OPERATORS` and the associated
entries are simply the integer index of the corresponding execution port.

Instruction Overlapping
-----------------------

In addition to the instruction-level parallelism offered by having
instructions despatched to different execution ports, the differing
cycle count of the various f.p. operations provides further scope for
overlapping them. In particular, f.p. division on Ivy Bridge takes
eight times longer than e.g. multiplication and addition/subtraction.
Even though multiplication and division operations are mapped to the
same execution port, the results of micro-benchmarks show that the
hardware is able to perform several multiplications while a single
division is in progress. Similarly, multiple addition/subtraction
operations may be performed on port 1 while a single division is in
progress on port 0.

Whether or not this overlapping is supported by the CPU is configured
by setting `SUPPORTS_DIV_MUL_OVERLAP` to `True` or `False`, as
appropriate. If overlapping is supported then further data is
required on the degree of overlapping permitted and the cost in cycles.

`MAX_DIV_OVERLAP` is a dictionary with keys "*" and "+". The entries
are the maximum number of each of those operations that may be
overlapped with a single division.

`div_overlap_mul_cost(overlaps)` is a routine that returns the cost of
a division (in cycles) as a function of the number of other operations
that it is overlapped with. `overlaps` is a dictionary holding the
number of each type of operation ("*" and "+") that has been
overlapped with the division. (Subtraction operations are counted as
addition operations here.)

Support for Fused Multiply-Add Operations
-----------------------------------------

Some micro-architectures (but not Intel Ivy Bridge) have support for
fused multiply-add instructions. i.e. ``a*x + b`` can be performed in
a single operation. `SUPPORTS_FMA` must be set to `True`
or `False`, as appropriate.

Cache-line Size
---------------

The number of bytes contained in a single cache line must be supplied
in ``CACHE_LINE_BYTES``. This is used when estimating memory-bandwidth
requirements.

Clock Speed
-----------

A representative clock-speed for the CPU being modelled is supplied in
`EXAMPLE_CLOCK_GHZ`. This is used to generate concrete performance
figures. Note that on CPU cores with frequency-stepping and turbo
boost enabled, there is no single clock speed!

