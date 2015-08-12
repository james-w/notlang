from rpython.rlib.parsing.lexer import SourcePos
from testtools.matchers import (
    AfterPreprocessing,
    Equals,
    Is,
    IsInstance,
    MatchesAll,
    MatchesListwise,
)

from . import ast, bytecode, typer


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
            AfterPreprocessing(get_name, Equals(name)),
        )

    def __str__(self):
        return str(self.matcher)

    def match(self, actual):
        return self.matcher.match(actual)


class IsParametricType(object):

    def __init__(self, types):
        def get_types(a):
            return getattr(a, 'types', None)
        def get_class(a):
            return getattr(a, '__class__', None)
        self.matcher = MatchesAll(
            AfterPreprocessing(get_class, Is(typer.ParameterisedType)),
            AfterPreprocessing(get_types, MatchesListwise(types)),
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

    def __init__(self, args, rtype):
        def get_args(a):
            return getattr(a, 'args', [])
        def get_rtype(a):
            return getattr(a, 'rtype', None)
        self.matcher = MatchesAll(
            IsInstance(typer.FunctionType),
            AfterPreprocessing(get_args, MatchesListwise(args)),
            AfterPreprocessing(get_rtype, rtype),
        )

    def __str__(self):
        return str(self.matcher)

    def match(self, actual):
        return self.matcher.match(actual)


class IsAttributeAccess(object):

    def __init__(self, type, name):
        def get_name(a):
            return getattr(a, 'name', None)
        def get_type(a):
            return getattr(a, 'type', None)
        self.matcher = MatchesAll(
            IsInstance(typer.AttributeAccess),
            AfterPreprocessing(get_type, type),
            AfterPreprocessing(get_name, Equals(name)),
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


class ASTFactory(object):

    spos = SourcePos(0, 0, 0)

    def __init__(self, testcase):
        self.testcase = testcase

    def variable(self, name=None):
        if name is None:
            name = self.testcase.getUniqueString()
        return ast.Variable(name, self.spos)

    def int(self):
        return ast.ConstantInt(self.testcase.getUniqueInteger(), self.spos)

    def assignment(self, target=None, source=None):
        if target is None:
            target = self.variable()
        if source is None:
            source = self.int()
        return ast.Assignment(target, source, self.spos)

    def pass_(self):
        return ast.Pass(self.spos)

    def conditional(self, true_block=None, false_block=None):
        if true_block is None:
            true_block = self.pass_()
        return ast.Conditional(self.int(), true_block, false_block, self.spos)

    def while_(self, block=None):
        if block is None:
            block = self.pass_()
        return ast.While(self.int(), block, self.spos)

    def binop(self):
        return ast.BinOp('+', self.int(), self.int(), self.spos)

    def newtype(self, block=None):
        if block is None:
            block = self.pass_()
        return ast.NewType(block, "Type", self.spos)

    def funcdef(self, name=None, body=None, args=None):
        if name is None:
            name = self.testcase.getUniqueString()
        if body is None:
            body = self.pass_()
        if args is None:
            args = []
        return ast.FuncDef(name, args, body, self.spos)

    def block(self, children=None):
        if children is None:
            children = []
        return ast.Block(children, self.spos)

    def stmt(self, child=None):
        if child is None:
            child = self.pass_()
        return ast.Stmt(child, self.spos)
