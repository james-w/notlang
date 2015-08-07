"""Execute ./targetnotlang-c [-Derror] <filename>
"""

from argparse import ArgumentParser, FileType
import sys

from rpython.rlib.parsing.parsing import ParseError
from rpython.jit.codewriter.policy import JitPolicy
from notlang.debug import add_debug_args, show_errors, trace_interp, trace_typer
from notlang.interpreter import interpret
from notlang.typer import NotNameError, NotTypeError


def main(argv):
    parser = ArgumentParser(description=__doc__, epilog="Thanks for trying notlang. Sorry for the bugs.")
    parser.add_argument('file', type=FileType('r'), help="The file to execute.")
    add_debug_args(parser)
    opts = parser.parse_args()
    data = opts.file.read()
    opts.file.close()
    do_raise = show_errors(opts)
    try:
        interpret(data, trace=trace_interp(opts), trace_typer=trace_typer(opts))
    except (ParseError, NotNameError, NotTypeError) as e:
        print(e.nice_error_message(source=data, filename=opts.file.name))
        if do_raise:
            raise
    return 0


def target(driver, args):
    return main, None


def jitpolicy(driver):
    return JitPolicy()


if __name__ == '__main__':
    main(sys.argv)
