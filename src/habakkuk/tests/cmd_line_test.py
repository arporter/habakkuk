''' This module tests aspects of Habakkuk related to handling of
    command-line arguments '''

import os
import subprocess

# constants - set the location of the Habakkuk script (so that we
# don't just pick-up the one on our PATH)
_DIR = os.path.dirname(os.path.abspath(__file__))
while "src" in _DIR:
    (_DIR, _) = os.path.split(_DIR)
HABAKKUK_SCRIPT = os.path.join(_DIR, 'bin', 'habakkuk')

PWD = os.path.dirname(os.path.abspath(__file__))
BASE_PATH = os.path.join(PWD, "test_files")


def test_usage_message():
    ''' Check that we get a usage message if no command-line arguments
    are supplied '''
    (output, _) = subprocess.Popen([HABAKKUK_SCRIPT],
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT).communicate()
    assert "Usage: habakkuk [options] <Fortran files>" in output


def test_missing_file():
    ''' Check that we get the expected message if the user specifies a
    Fortran source file that cannot be found '''
    # One argument given and it cannot be found...
    (output, _) = subprocess.Popen([HABAKKUK_SCRIPT, 'not_a_file.f90'],
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT).communicate()
    assert "The specified source file ('not_a_file.f90') cannot be found" in \
        output
    # and now when it is the second of two files that does not exist...
    (output, _) = subprocess.Popen([HABAKKUK_SCRIPT,
                                    os.path.join(BASE_PATH, "basic_loop.f90"),
                                    'not_a_file.f90'],
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT).communicate()
    assert "The specified source file ('not_a_file.f90') cannot be found" in \
        output


def test_help_message():
    ''' Check that a help message is produced when the --help flag is
    supplied '''
    (output, _) = subprocess.Popen([HABAKKUK_SCRIPT, '--help'],
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT).communicate()
    assert (
        "Options:\n"
        "  -h, --help            show this help message and exit\n"
        "  --task=TASK           Specify parsing result task. Default: show.\n"
        "  --no-prune            Do not attempt to prune duplicate "
        "operations from the\n"
        "                        graph\n"
        "  --no-fma              Do not attempt to generate fused "
        "multiply-add\n"
        "                        operations\n"
        "  --rm-scalar-tmps      Remove scalar temporaries from the DAG\n"
        "  --show-weights        Display node weights in the DAG\n"
        "  --unroll=UNROLL_FACTOR\n"
        "                        No. of times to unroll a loop. (Applied to "
        "every loop\n"
        "                        that is encountered.)") in output
