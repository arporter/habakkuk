Getting going
=============

Download
--------

Habbakuk is hosted on github.

Heron, the location that you downloaded Habbakuk to (including the
habbakuk directory itself) will be referred to as <HABBAKUK_HOME>.

Dependencies
------------

Habbakuk is written in python and so needs python to be installed on
the target machine. It requires the f2py package and that in turn
requires the numpy package. In order to run the test suite you will
require py.test.

Environment set-up
------------------

The source code of f2py (revision 93) is provided with Habbakuk in the
sub-directory ``f2py_93``.

To use this version of f2py you can simply set up your PYTHONPATH
variable to include this directory. In order to run Habbakuk
(including running the test suite and building the documentation) you
will also need to add the location of its src directory to your
PYTHONPATH. The simplest way to achieve this is to do: ::

    > cd <HABBAKUK_HOME>
    > export PYTHONPATH=${PWD}/f2py_93:${PWD}/src:${PYTHONPATH}

If for some reason you need to install f2py yourself then 
see :ref:`sec_f2py_install`.

Running
-------

Testing
-------
