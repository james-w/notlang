from contextlib import contextmanager
from cStringIO import StringIO
import logging

from hypothesis.control import current_build_context
from rpython.rlib.parsing.lexer import SourcePos
from testtools import TestCase as _TestCase
from testtools.content import text_content
from testtools.matchers import (
    AfterPreprocessing,
    Equals,
    Is,
    IsInstance,
    MatchesAll,
    MatchesListwise,
)

from .. import ast, bytecode, objectspace, typer
from ..debug import make_debug_handler


def capture_logs():
    logfile = StringIO()
    logger = logging.getLogger('notlang')
    logger.addHandler(make_debug_handler([], all_targets=True, stream=logfile, include_time=False))
    return logfile


def if_final(f):
    context = current_build_context()
    if context.is_final:
        f()


class TestCase(_TestCase):

    def setUp(self):
        super(TestCase, self).setUp()
        logfile = capture_logs()
        def attach_log():
            log_content = logfile.getvalue()
            self.addDetail('log', text_content(log_content))
        self.addCleanup(attach_log)


class HypothesisTestCase(_TestCase):

    @contextmanager
    def capture_logs(self):
        logfile = capture_logs()
        try:
            yield
        finally:
            def attach_log():
                log_content = logfile.getvalue()
                self.addDetail('log', text_content(log_content))
            if_final(attach_log)


class BytecodeMatches(object):

    def __init__(self, expected):
        self.matcher = Equals(map(self.name, self.pair_up(expected)))

    def pair_up(self, l):
        return zip(l[0::2], l[1::2])

    def name(self, pair):
        return (bytecode.reverse_map.get(pair[0]), pair[1])

    def bc_to_py(self, bc):
        for i in range(0, len(bc), bytecode.INSTRUCTION_SIZE):
            opcode = ord(bc[i])
            high = ord(bc[i+1])
            low = ord(bc[i+2])
            arg = (high << 8) + low
            yield (opcode, arg)

    def __str__(self):
        return str(self.matcher)

    def match(self, actual):
        return self.matcher.match(map(self.name, self.bc_to_py(actual)))


def IsType(name):
    def get_name(a):
        return getattr(a, 'name', None)
    def get_class(a):
        return getattr(a, '__class__', None)
    return MatchesAll(
        AfterPreprocessing(get_class, Is(typer.Type)),
        AfterPreprocessing(get_name, Equals(name)),
    )


def IsParametricType(types):
    def get_types(a):
        return getattr(a, 'types', None)
    def get_class(a):
        return getattr(a, '__class__', None)
    return MatchesAll(
        AfterPreprocessing(get_class, Is(typer.ParameterisedType)),
        AfterPreprocessing(get_types, MatchesListwise(types)),
    )


def IsTypeExpr(name):
    def get_name(a):
        return getattr(a, 'name', None)
    return MatchesAll(
        IsInstance(typer.TypeExpr),
        AfterPreprocessing(get_name, Equals(name))
    )


def IsTypeVariable(name):
    def get_name(a):
        return getattr(a, 'name', None)
    return MatchesAll(
        IsInstance(typer.TypeVariable),
        AfterPreprocessing(get_name, Equals(name))
    )


def IsFunctionType(args, rtype):
    def get_args(a):
        return getattr(a, 'args', [])
    def get_rtype(a):
        return getattr(a, 'rtype', None)
    return MatchesAll(
        IsInstance(typer.FunctionType),
        AfterPreprocessing(get_args, MatchesListwise(args)),
        AfterPreprocessing(get_rtype, rtype),
    )


def IsUnionType(subtypes):
    def get_subtypes(a):
        return getattr(a, 'subtypes', [])
    return MatchesAll(
        IsInstance(typer.UnionType),
        AfterPreprocessing(get_subtypes, MatchesListwise(subtypes)),
    )


def IsW_Int(value):
    def get_val(a):
        return getattr(a, 'intval', None)
    return MatchesAll(
        IsInstance(objectspace.W_Int),
        AfterPreprocessing(get_val, Equals(value)),
    )


def IsAttributeAccess(type, name):
    def get_name(a):
        return getattr(a, 'name', None)
    def get_type(a):
        return getattr(a, 'type', None)
    return MatchesAll(
        IsInstance(typer.AttributeAccess),
        AfterPreprocessing(get_type, type),
        AfterPreprocessing(get_name, Equals(name)),
    )


def ConstraintMatches(a, constraint, b, positions):
    def get_a(o):
        return getattr(o, 'a', None)
    def get_b(o):
        return getattr(o, 'b', None)
    def get_constraint(o):
        return getattr(o, 'constraint', None)
    def get_positions(o):
        return getattr(o, 'positions', None)
    return MatchesAll(
        IsInstance(typer.Constraint),
        AfterPreprocessing(get_a, a),
        AfterPreprocessing(get_b, b),
        AfterPreprocessing(get_constraint, Equals(constraint)),
        AfterPreprocessing(get_positions, MatchesListwise(positions)),
    )


def IsTypeReference(name, type_params=None):
    def get_name(a):
        return getattr(a, 'name', None)
    def get_type_params(a):
        return getattr(a, 'type_params', [])
    if type_params is None:
        type_params = []
    return MatchesAll(
        IsInstance(ast.TypeReference),
        AfterPreprocessing(get_name, Equals(name)),
        AfterPreprocessing(get_type_params, MatchesListwise(type_params)),
    )


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

    def case(self, target=None, cases=None, else_case=None):
        if target is None:
            target = self.variable()
        if cases is None:
            cases = [self.case_case()]
        return ast.Case(target, cases, else_case, self.spos)

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

    def type_reference(self, name=None):
        if name is None:
            name = self.testcase.getUniqueString()
        return ast.TypeReference(name, self.spos)
