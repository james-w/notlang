from rpython.rlib.parsing.lexer import SourcePos
from testtools.matchers import (
    AfterPreprocessing,
    Equals,
    Is,
    IsInstance,
    MatchesAll,
    MatchesListwise,
)

from . import ast, bytecode, objectspace, typer


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


class IsW_Int(object):

    def __init__(self, value):
        def get_val(a):
            return getattr(a, 'intval', None)
        self.matcher = MatchesAll(
            IsInstance(objectspace.W_Int),
            AfterPreprocessing(get_val, Equals(value)),
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

    def int(self, value=None):
        if value is None:
            value = self.testcase.getUniqueInteger()
        return ast.ConstantInt(value, self.spos)

    def assignment(self, target=None, source=None):
        if target is None:
            target = self.variable()
        if source is None:
            source = self.int()
        return ast.Assignment(target, source, self.spos)

    def pass_(self):
        return ast.Pass(self.spos)

    def conditional(self, condition=None, true_block=None, false_block=None):
        if condition is None:
            condition = self.int()
        if true_block is None:
            true_block = self.pass_()
        return ast.Conditional(condition, true_block, false_block, self.spos)

    def while_(self, condition=None, block=None):
        if block is None:
            block = self.pass_()
        if condition is None:
            condition = self.int()
        return ast.While(condition, block, self.spos)

    def binop(self, op=None, a=None, b=None):
        if op is None:
            op = '+'
        if a is None:
            a = self.int()
        if b is None:
            b = self.int()
        return ast.BinOp(op, a, b, self.spos)

    def newtype(self, block=None, options=None, type_type='Type', type_params=None):
        if block is None:
            block = self.pass_()
        if options is None:
            options = []
        return ast.NewType(block, type_type, self.spos, options=options, type_params=type_params)

    def enum(self, block=None, options=None):
        if options is None:
            options = [self.type_option()]
        return self.newtype(block=block, options=options, type_type='Enum')

    def funcdef(self, name=None, body=None, args=None, rtype=None, argtypes=None, type_params=None):
        if name is None:
            name = self.testcase.getUniqueString()
        if body is None:
            body = self.pass_()
        if args is None:
            args = []
        return ast.FuncDef(name, args, body, self.spos, rtype=rtype, argtypes=argtypes, type_params=type_params)

    def function_call(self, function=None, args=None, type_params=None):
        if function is None:
            function = self.variable()
        if args is None:
            args = []
        return ast.Function(function, args, self.spos, type_params=type_params)

    def block(self, children=None):
        if children is None:
            children = []
        return ast.Block(children, self.spos)

    def stmt(self, child=None):
        if child is None:
            child = self.pass_()
        return ast.Stmt(child, self.spos)

    def case(self, target=None, cases=None):
        if target is None:
            target = self.variable()
        if cases is None:
            cases = [self.case_case()]
        return ast.Case(target, cases, self.spos)

    def case_case(self, label=None, block=None):
        if label is None:
            label = self.variable()
        if block is None:
            block = self.block(children=[self.pass_()])
        return ast.CaseCase(label, block, self.spos)

    def type_option(self, name=None, members=None):
        if name is None:
            name = self.testcase.getUniqueString()
        return ast.TypeOption(name, self.spos, members=members)

    def tuple(self, types=None, block=None):
        if types is None:
            types = [self.type_option()]
        if block is None:
            block = self.block()
        return ast.NewType(block, "Tuple", self.spos, options=types)

    def return_(self, arg=None):
        return ast.Return(arg, self.spos)

    def attribute(self, target=None, name=None):
        if target is None:
            target = self.variable()
        if name is None:
            name = self.testcase.getUniqueString()
        return ast.Attribute(target, name, self.spos)
