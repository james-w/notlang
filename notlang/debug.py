import logging


ERROR = 'error'
INTERP = 'interp'
TYPE = 'type'
LEXER = 'lexer'


DEBUG_TARGETS = {
    ERROR: 'Turn on full exception reporting, including tracebacks.',
    INTERP: 'Trace execution of the interpreter.',
    TYPE: 'Trace steps of the type-checker.',
    LEXER: 'Trace steps of the lexer.',
}

LOGGER_NAMES = {
    INTERP: ['interpreter'],
    TYPE: ['typer'],
    LEXER: ['lexer'],
}


def pretty_print():
    def _one_opt(keyval):
        return "   - %s: %s" % keyval
    return "\n".join(map(_one_opt, DEBUG_TARGETS.items()))


def add_debug_args(parser):
    parser.add_argument(
        '-D', '--debug',
        help="Turn on debugging options. Options are:\n\n" + pretty_print(),
        choices=DEBUG_TARGETS,
        action='append',
        default=[])


def show_errors(opts):
    return ERROR in opts.debug


def enable_debug_logging(name):
    logger = logging.getLogger('notlang.' + name)
    logger.setLevel(logging.DEBUG)


def make_debug_handler(opts, stream=None, all_targets=False):
    enable = False
    for key, names in LOGGER_NAMES.items():
        if all_targets or key in opts:
            enable = True
            for name in names:
                enable_debug_logging(name)
    debug_handler = logging.StreamHandler(stream=stream)
    if enable:
        debug_handler.setLevel(logging.DEBUG)
        debug_handler.setFormatter(logging.Formatter('%(asctime)s:%(name)s:%(levelname)s: %(message)s'))
        return debug_handler
    else:
        return None


class colours:
    PURPLE = '\033[0;35m'
    BLUE = '\033[0;34m'
    GREEN = '\033[0;32m'
    YELLOW = '\033[1;33m'
    RED = '\033[0;31m'
    BROWN = '\033[0;33m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


def coloured(text, colour):
    return colour + str(text) + colours.ENDC


def colour_list(l, colour):
    return "[" + ", ".join(map(lambda x: coloured(x, colour), l)) + "]"


def colour_dict(l, key_colour, value_colour, value_fn=None):
    if value_fn is None:
        value_fn = lambda x: coloured(repr(x), value_colour)
    return "{" + ", ".join(map(lambda x: coloured(repr(x[0]), key_colour) + ": " + value_fn(x[1]), l.items())) + "}"
