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
        self.matcher = MatchesAll(
            IsInstance(typer.TypeExpr),
            AfterPreprocessing(lambda a: a.name, Equals(name))
        )

    def __str__(self):
        return str(self.matcher)

    def match(self, actual):
        return self.matcher.match(actual)


class IsFunctionType(object):

    def __init__(self, name, args, rtype):
        self.matcher = MatchesAll(
            IsInstance(typer.FunctionType),
            AfterPreprocessing(lambda a: a.name, name),
            AfterPreprocessing(lambda a: a.args, MatchesListwise(args)),
            AfterPreprocessing(lambda a: a.rtype, rtype),
        )

    def __str__(self):
        return str(self.matcher)

    def match(self, actual):
        return self.matcher.match(actual)
