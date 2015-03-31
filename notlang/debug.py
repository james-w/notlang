ERROR = 'error'
INTERP = 'interp'


DEBUG_TARGETS = {
    ERROR: 'Turn on full exception reporting, including tracebacks.',
    INTERP: 'Trace execution of the interpreter.',
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


def trace_interp(opts):
    return INTERP in opts.debug
