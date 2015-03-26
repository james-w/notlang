from testtools.matchers import Equals, IsInstance, MatchesAll, AfterPreprocessing, MatchesListwise

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
