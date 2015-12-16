"""Execute ./targetnotlang-c [-Derror] <filename>
"""

from argparse import ArgumentParser, FileType
import logging
import sys

from rpython.rlib.parsing.parsing import ParseError
from rpython.jit.codewriter.policy import JitPolicy
from notlang.debug import add_debug_args, show_errors, make_debug_handler
from notlang.interpreter import interpret
from notlang.typer import NotNameError, NotTypeError


def setup_logging(opts):
    logger = logging.getLogger('notlang')
    logger.setLevel(logging.INFO)
    base_handler = logging.StreamHandler()
    base_handler.setLevel(logging.INFO)
    logger.addHandler(base_handler)
    debug_handler = make_debug_handler(opts.debug)
    if debug_handler is not None:
        logger.addHandler(debug_handler)


def main(argv):
    parser = ArgumentParser(description=__doc__, epilog="Thanks for trying notlang. Sorry for the bugs.")
    parser.add_argument('file', type=FileType('r'), help="The file to execute.")
    add_debug_args(parser)
    opts = parser.parse_args()
    data = opts.file.read()
    opts.file.close()
    do_raise = show_errors(opts)
    setup_logging(opts)
    try:
        interpret(data)
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
