
''' Hardware details for the Intel Ivy Bridge microarchitecture. '''

# Types of floating-point operation with their cost in cycles (from
# http://www.agner.org/optimize/instruction_tables.pdf).  Operations
# are in order of decreasing cost (for use when searching for
# duplicated sub-graphs).  TODO these costs are microarchitecture
# specific.  Those operations that are listed in FORTRAN_INTRINSICS
# are calls to intrinsic routines provided by the Fortran
# run-time. Costs for these are obtained by running micro-benchmarks
# (dl_microbench).
OPERATORS = {"**": {"latency": 0, "cost": 75},
             "SIN": {"latency": 0, "cost": 49},
             "COS": {"latency": 0, "cost": 49},
             "/": {"latency": 15, "cost": 8},
             "SIGN": {"latency": 0, "cost": 3},
             "+": {"latency": 3, "cost": 1},
             "-": {"latency": 3, "cost": 1},
             "*": {"latency": 5, "cost": 1}}

# Which execution port each f.p. operation is mapped to on the CPU
# (from http://www.agner.org/optimize/microarchitecture.pdf).
NUM_EXECUTION_PORTS = 2
CPU_EXECUTION_PORTS = {"/": 0, "*": 0, "+": 1, "-": 1,
                       # Which port the intrinsics will utilise
                       "**": 0, "SIN": 0, "COS": 0, "SIGN": 1}

# Size of a cache line in bytes
CACHE_LINE_BYTES = 64

# Clock speed to use when computing example performance figures
EXAMPLE_CLOCK_GHZ = 3.85

# Fortran intrinsics that we recognise. All uppercase.
FORTRAN_INTRINSICS = ["SIGN", "SIN", "COS", "**"]

# Whether this microarchitecture supports the Fused Multiply Add op
# TODO check on this before we attempt to generate FMAs.
SUPPORTS_FMA = False
