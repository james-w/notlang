from testtools.matchers import (
    AfterPreprocessing,
    Equals,
    Is,
    IsInstance,
    MatchesAll,
    MatchesListwise,
)

from . import bytecode, typer


class BytecodeMatches(object):

    def __init__(self, expected):
        self.matcher = Equals(map(self.name, self.pair_up(expected)))

    def pair_up(self, l):
        return zip(l[0::2], l[1::2])

    def name(self, pair):
        return (bytecode.reverse_map.get(pair[0]), pair[1])

    def __str__(self):
        return str(self.matcher)

    def match(self, actual):
        return self.matcher.match(map(self.name, self.pair_up(map(ord, actual))))


class IsType(object):

    def __init__(self, name):
        def get_name(a):
            return getattr(a, 'name', None)
        def get_class(a):
            return getattr(a, '__class__', None)
        self.matcher = MatchesAll(
            AfterPreprocessing(get_class, Is(typer.Type)),
            AfterPreprocessing(get_name, Equals(name))
        )

    def __str__(self):
        return str(self.matcher)

    def match(self, actual):
        return self.matcher.match(actual)


class IsTypeExpr(object):

    def __init__(self, name):
        def get_name(a):
            return getattr(a, 'name', None)
        self.matcher = MatchesAll(
            IsInstance(typer.TypeExpr),
            AfterPreprocessing(get_name, Equals(name))
        )

    def __str__(self):
        return str(self.matcher)

    def match(self, actual):
        return self.matcher.match(actual)


class IsTypeVariable(object):

    def __init__(self, name):
        def get_name(a):
            return getattr(a, 'name', None)
        self.matcher = MatchesAll(
            IsInstance(typer.TypeVariable),
            AfterPreprocessing(get_name, Equals(name))
        )

    def __str__(self):
        return str(self.matcher)

    def match(self, actual):
        return self.matcher.match(actual)


class IsFunctionType(object):

    def __init__(self, name, args, rtype):
        def get_name(a):
            return getattr(a, 'name', None)
        def get_args(a):
            return getattr(a, 'args', [])
        def get_rtype(a):
            return getattr(a, 'rtype', None)
        self.matcher = MatchesAll(
            IsInstance(typer.FunctionType),
            AfterPreprocessing(get_name, name),
            AfterPreprocessing(get_args, MatchesListwise(args)),
            AfterPreprocessing(get_rtype, rtype),
        )

    def __str__(self):
        return str(self.matcher)

    def match(self, actual):
        return self.matcher.match(actual)


class IsInstantiate(object):

    def __init__(self, ftype):
        def get_ftype(a):
            return getattr(a, 'ftype', None)
        self.matcher = MatchesAll(
            IsInstance(typer.Instantiate),
            AfterPreprocessing(get_ftype, ftype),
        )

    def __str__(self):
        return str(self.matcher)

    def match(self, actual):
        return self.matcher.match(actual)


class ConstraintMatches(object):

    def __init__(self, a, constraint, b, positions):
        def get_a(o):
            return getattr(o, 'a', None)
        def get_b(o):
            return getattr(o, 'b', None)
        def get_constraint(o):
            return getattr(o, 'constraint', None)
        def get_positions(o):
            return getattr(o, 'positions', None)
        self.matcher = MatchesAll(
            IsInstance(typer.Constraint),
            AfterPreprocessing(get_a, a),
            AfterPreprocessing(get_b, b),
            AfterPreprocessing(get_constraint, Equals(constraint)),
            AfterPreprocessing(get_positions, MatchesListwise(positions)),
        )

    def __str__(self):
        return str(self.matcher)

    def match(self, actual):
        return self.matcher.match(actual)
