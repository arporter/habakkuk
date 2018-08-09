Getting going
=============

Download
--------

Habakkuk is available from the Python Package Index (pypi). The
project itself is hosted on github
(https://github.com/arporter/habakkuk).

Dependencies
------------

Habakkuk is written in Python and so needs Python (either 2.7 or 3.6+)
to be installed on the target machine. It also requires the (Python)
fparser and six packages.  In order to run the test suite you will
require py.test.

Installation
------------

Using pip
^^^^^^^^^

The recommended way of installing Habakkuk is to use pip. This will
obtain the package from pypi as well as any required dependencies:
::
    $ pip install Habakkuk

By default, pip attempts to perform a system-wide installation which
requires root privileges. Alternatively, a user-local installation
may be requested by specifying the `--user` flag:
::
    $ pip install --user Habakkuk

This will install the package(s) under `${HOME}/.local`. Depending on
your linux distribution, you may need to add `${HOME}/.local/bin` to
your `PATH` and `${HOME}/.local/lib/pythonX.Y/site-packages/` to your
`PYTHONPATH`. (X.Y is the version of python your system is running,
e.g. 2.7.)

From tarball
^^^^^^^^^^^^

If pip is not available then tarballs of each of the releases of
Habakkuk are available on github
(https://github.com/arporter/habakkuk/releases).
Once the tarball has been downloaded and unpacked, change to the
resulting habakkuk directory and do:
::
   $ python setup.py install

If you do not have root access then, as with using pip (above), you can
specify the prefix for the install path like so:
::
   $ python setup.py install --prefix ${HOME}/.local


Running
-------

Habakkuk is run from the command line. The `-h`/`--help` flag will
produce a list of the various available options::

  $ habakkuk -h
  
  Usage: habakkuk [options] <Fortran file(s)>

  Options:
    -h, --help            show this help message and exit
    --no-prune            Do not attempt to prune duplicate operations from the
                          graph
    --no-fma              Do not attempt to generate fused multiply-add
                          operations
    --rm-scalar-tmps      Remove scalar temporaries from the DAG
    --show-weights        Display node weights in the DAG
    --unroll=UNROLL_FACTOR
                          No. of times to unroll a loop. (Applied to every loop
                          that is encountered.)

    Fortran code options:
      Specify information about Fortran codes.

      --mode=MODE         Specify Fortran code mode. Default: auto.

Habakkuk analyses Fortran source files, provided as arguments on the
command line, e.g.::

   $ habakkuk my_fortran_file1.f90 my_fortran_file2.F90

If all is well, you should see output similar to the following::
   
   $ habakkuk tra_adv.F90 
   Habakkuk processing file 'tra_adv.F90'
   Wrote DAG to tra_adv_loop1.gv
   Stats for DAG tra_adv_loop1:
      0 addition operators.
      0 subtraction operators.
      0 multiplication operators.
      1 division operators.
      1 FLOPs in total.
      8 array references.
      8 distinct cache-line references.
      Naive FLOPs/byte = 0.016
      Whole DAG in serial:
        Sum of cost of all nodes = 8 (cycles)
        1 FLOPs in 8 cycles => 0.1250*CLOCK_SPEED FLOPS
        Associated mem bandwidth = 8.00*CLOCK_SPEED bytes/s
      Everything in parallel to Critical path:
        Critical path contains 4 nodes, 1 FLOPs and is 8 cycles long
        FLOPS (ignoring memory accesses) = 0.1250*CLOCK_SPEED
        Associated mem bandwidth = 8.00*CLOCK_SPEED bytes/s
    Schedule contains 1 steps:
                Execution Port        
          0    1    2    3    4    5  
    0   /    None None None None None (cost = 8)
      Estimate using computed schedule:
        Cost of schedule as a whole = 8 cycles
        FLOPS from schedule (ignoring memory accesses) = 0.1250*CLOCK_SPEED
        Associated mem bandwidth = 8.00*CLOCK_SPEED bytes/s
      Estimate using perfect schedule:
        Cost if all ops on different execution ports are perfectly overlapped = 8 cycles
      e.g. at 3.85 GHz, these different estimates give (GFLOPS): 
      No ILP  |  Computed Schedule  |  Perfect Schedule | Critical path
       0.48   |          0.48       |        0.48       |    0.48
     with associated BW of 30.80,30.80,30.80,30.80 GB/s


Testing
-------

The Habakkuk source contains a test-suite written to use py.test. In
order to run it you will need to obtain the Habakkuk source - either
by downloading a tarball of one of the
[releases](https://github.com/arporter/habakkuk/releases) or by
cloning the git repository. Assuming you have Habakkuk and py.test
installed you can then do:
::
    $ cd habakkuk/src/habakkuk/tests
    $ py.test
