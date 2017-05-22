
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
             # TODO measure actual values for EXP, TAN*, SQRT, ABS
             # on ivy bridge
             "SQRT": {"latency": 0, "cost": 75, "flops": 28},
             "EXP": {"latency": 0, "cost": 50, "flops": 20},
             "LOG": {"latency": 0, "cost": 50, "flops": 20},
             "SIN": {"latency": 0, "cost": 49, "flops": 40},
             "COS": {"latency": 0, "cost": 49, "flops": 40},
             "ACOS": {"latency": 0, "cost": 49, "flops": 40},
             "ATAN": {"latency": 0, "cost": 49, "flops": 40},
             "TAN": {"latency": 0, "cost": 49, "flops": 40},
             "TANH": {"latency": 0, "cost": 49, "flops": 40},
             "/": {"latency": 15, "cost": 8, "flops": 1},
             "MOD": {"latency": 15, "cost": 8, "flops": 1},
             "SIGN": {"latency": 0, "cost": 3, "flops": 1},
             "ABS": {"latency": 0, "cost": 3, "flops": 1},
             "+": {"latency": 3, "cost": 1, "flops": 1},
             "-": {"latency": 3, "cost": 1, "flops": 1},
             "*": {"latency": 5, "cost": 1, "flops": 1},
             "MIN": {"latency": 0, "cost": 1, "flops": 0},
             "MAX": {"latency": 0, "cost": 1, "flops": 0},
             "INT": {"latency": 0, "cost": 1, "flops": 0},
             "DBLE": {"latency": 0, "cost": 1, "flops": 0},
             "REAL": {"latency": 0, "cost": 1, "flops": 0},
             # TODO how to handle an intrinsic when the cost depends
             # on the argument?
             "SUM": {"latency": 0, "cost": 1, "flops": 1},
             "PRESENT": {"latency": 0, "cost": 1, "flops": 0},
             # String intrinsics. Obviously these don't account
             # for any FLOPs but need to be here so that we can
             # grok the Fortran
             "TRIM": {"latency": 0, "cost": 1, "flops": 0},
             "COUNT": {"latency": 0, "cost": 1, "flops": 0},
             "IACHAR": {"latency": 0, "cost": 1, "flops": 0},
             "NINT": {"latency": 0, "cost": 1, "flops": 0}}

# Which execution port each f.p. operation is mapped to on the CPU
# (from http://www.agner.org/optimize/microarchitecture.pdf).
NUM_EXECUTION_PORTS = 6
CPU_EXECUTION_PORTS = {"/": 0, "*": 0, "+": 1, "-": 1,
                       # Which port the intrinsics will utilise
                       "**": 0, "SIN": 0, "COS": 0, "ACOS": 0, "SIGN": 1,
                       "EXP": 0, "LOG": 0, "MOD": 0,
                       "SUM": 0, "ATAN": 0, "TAN": 0, "TANH": 0, "SQRT": 0,
                       # The CMP instruction can execute on 0, 1 or 5
                       # so specify 5 here as 0 and 1 are likely to be busy
                       "MAX": 5, "MIN": 5, "ABS": 5, "PRESENT": 5,
                       "DBLE": 1, "REAL": 1, "INT": 1, "NINT": 1,
                       # String manipulation is integer
                       # TODO handle string manipulation separately as
                       # it's not really something we want to have to
                       # care about when thinking about number-crunching
                       # performance
                       "TRIM": 1, "COUNT": 1, "IACHAR": 1}

# Size of a cache line in bytes
CACHE_LINE_BYTES = 64

# Clock speed to use when computing example performance figures
EXAMPLE_CLOCK_GHZ = 3.85

FORTRAN_STRING_INTRINSICS = ["TRIM", "COUNT", "IACHAR"]

# Fortran intrinsics that we recognise. All uppercase.
FORTRAN_INTRINSICS = ["SIGN", "SIN", "COS", "ACOS", "**", "MAX", "MIN", "EXP",
                      "MOD", "NINT", "SUM", "ATAN", "TAN", "TANH", "SQRT",
                      "ABS", "LOG", "INT", "REAL", "DBLE", "PRESENT"] + \
    FORTRAN_STRING_INTRINSICS

# Whether this microarchitecture supports the Fused Multiply Add op
# TODO check on this before we attempt to generate FMAs.
SUPPORTS_FMA = False

# Whether this microarchitecture supports overlapping multiplication
# operations with (independent) division operations. If True then
# the cost (as a function of the number of overlapped multiplications)
# is obtained by calling div_overlap_mul_cost()
SUPPORTS_DIV_MUL_OVERLAP = True

def div_overlap_mul_cost(num_mul):
    ''' Returns the cost of a division operation as a function of the
    number of (independent) multiplications with which it is overlapped.

    The values returned by this routine were determined by experiment. See
    https://bitbucket.org/apeg/dl_microbench for details of the code used
    to perform the measurements. '''
    if num_mul < 7:
        return OPERATORS["/"]["cost"]
    elif num_mul < 12:
        return OPERATORS["/"]["cost"] + 5
    else:
        return OPERATORS["/"]["cost"] + 5 + \
            (num_mul - 11)*OPERATORS["*"]["cost"]
