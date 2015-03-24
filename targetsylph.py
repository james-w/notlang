"""Execute ./sylph-c [-Derror] <filename>
"""

import sys
from rpython.rlib.parsing.parsing import ParseError
from rpython.rlib.streamio import open_file_as_stream
from rpython.jit.codewriter.policy import JitPolicy
from sylph.interpreter import interpret
from sylph.typer import SylphNameError, SylphTypeError


def main(argv):
    Derror = False
    if len(argv) < 2 or len(argv) > 3:
        print __doc__
        return 1
    if len(argv) > 2:
        assert argv[1] == "-Derror"
        Derror = True
        fname = argv[2]
    else:
        fname = argv[1]
    f = open_file_as_stream(fname)
    data = f.readall()
    f.close()
    try:
        interpret(data)
    except ParseError as e:
        print(e.nice_error_message(filename=fname, source=data))
        if Derror:
            raise
    except SylphNameError as e:
        print(e.nice_error_message(filename=fname, source=data))
        if Derror:
            raise
    except SylphTypeError as e:
        print(e.nice_error_message(filename=fname, source=data))
        if Derror:
            raise
    return 0


def target(driver, args):
    return main, None


def jitpolicy(driver):
    return JitPolicy()
    

if __name__ == '__main__':
    main(sys.argv)
