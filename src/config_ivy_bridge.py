
''' Hardware details for the Intel Ivy Bridge microarchitecture. '''

# Types of floating-point operation with their cost in cycles
# (from http://www.agner.org/optimize/instruction_tables.pdf).
# Operations are in order of decreasing cost (for use when
# searching for duplicated sub-graphs).
# TODO these costs are microarchitecture specific.
OPERATORS = {"/": {"latency": 15, "cost": 8},
             "+": {"latency": 3, "cost": 1},
             "-": {"latency": 3, "cost": 1},
             "*": {"latency": 5, "cost": 1}}

# Which execution port each f.p. operation is mapped to on the CPU
# (from http://www.agner.org/optimize/microarchitecture.pdf).
NUM_EXECUTION_PORTS = 2
CPU_EXECUTION_PORTS = {"/": 0, "*": 0, "+": 1, "-": 1}

# Size of a cache line in bytes
CACHE_LINE_BYTES = 64

# Clock speed to use when computing example performance figures
EXAMPLE_CLOCK_GHZ = 3.85

# Fortran intrinsics that we recognise, with their cost in cycles
# (as obtained from micro-benchmarks: dl_microbench).
# TODO these costs are microarchitecture (and compiler+flags) specific.
FORTRAN_INTRINSICS = {"SIGN": 3, "SIN": 49, "COS": 49, "**": 49}

# Whether this microarchitecture supports the Fused Multiply Add op
# TODO check on this before we attempt to generate FMAs.
SUPPORTS_FMA = False
