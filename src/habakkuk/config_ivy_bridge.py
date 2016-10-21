
''' Hardware details for the Intel Ivy Bridge microarchitecture. '''

# Types of floating-point operation with their cost in cycles (from
# http://www.agner.org/optimize/instruction_tables.pdf).  Operations
# are in order of decreasing cost (for use when searching for
# duplicated sub-graphs).  TODO these costs are microarchitecture
# specific.  Those operations that are listed in FORTRAN_INTRINSICS
# are calls to intrinsic routines provided by the Fortran
# run-time. Costs and FLOP-counts for these are obtained by running
# micro-benchmarks (dl_microbench) using a tool such as likwid.
OPERATORS = {"**": {"latency": 0, "cost": 75, "flops": 28},
             # TODO measure actual values for EXP on ivy bridge
             "EXP": {"latency": 0, "cost": 50, "flops": 20},
             "SIN": {"latency": 0, "cost": 49, "flops": 40},
             "COS": {"latency": 0, "cost": 49, "flops": 40},
             "/": {"latency": 15, "cost": 8, "flops": 1},
             "SIGN": {"latency": 0, "cost": 3, "flops": 1},
             "+": {"latency": 3, "cost": 1, "flops": 1},
             "-": {"latency": 3, "cost": 1, "flops": 1},
             "*": {"latency": 5, "cost": 1, "flops": 1},
             "MIN": {"latency": 0, "cost": 1, "flops": 0},
             "MAX": {"latency": 0, "cost": 1, "flops": 0},
             # TODO how to handle an intrinsic when the cost depends
             # on the argument?
             "SUM": {"latency": 0, "cost": 1, "flops": 1},
             # String intrinsics. Obviously these don't account
             # for any FLOPs but need to be here so that we can
             # grok the Fortran
             "TRIM": {"latency": 0, "cost": 1, "flops": 0},
             "NINT": {"latency": 0, "cost": 1, "flops": 0}}

# Which execution port each f.p. operation is mapped to on the CPU
# (from http://www.agner.org/optimize/microarchitecture.pdf).
NUM_EXECUTION_PORTS = 6
CPU_EXECUTION_PORTS = {"/": 0, "*": 0, "+": 1, "-": 1,
                       # Which port the intrinsics will utilise
                       "**": 0, "SIN": 0, "COS": 0, "SIGN": 1, "EXP": 0,
                       "SUM": 0,
                       # The CMP instruction can execute on 0, 1 or 5
                       # so specify 5 here as 0 and 1 are likely to be busy
                       "MAX": 5, "MIN": 5,
                       # String manipulation is integer
                       "TRIM": 5, "NINT": 5}

# Size of a cache line in bytes
CACHE_LINE_BYTES = 64

# Clock speed to use when computing example performance figures
EXAMPLE_CLOCK_GHZ = 3.85

# Fortran intrinsics that we recognise. All uppercase.
FORTRAN_INTRINSICS = ["SIGN", "SIN", "COS", "**", "MAX", "MIN", "EXP",
                      "TRIM", "NINT", "SUM"]

# Whether this microarchitecture supports the Fused Multiply Add op
# TODO check on this before we attempt to generate FMAs.
SUPPORTS_FMA = False
