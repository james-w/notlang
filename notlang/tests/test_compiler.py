from operator import attrgetter

from testtools import TestCase
from rpython.rlib.parsing.lexer import SourcePos

from .. import ast, bytecode, objectspace
from ..compiler import Compiler
from ..compilercontext import CompilerContext
from ..testing import BytecodeMatches


def compile(node, locals=None):
    ctx = CompilerContext()
    if locals is None:
        locals = []
    ctx.locals = locals
    Compiler(ctx).dispatch(node)
    return ctx


class TestCompiler(TestCase):

    spos = SourcePos(0, 0, 0)

    def test_variable(self):
        vname = "foo"
        node = ast.Variable(vname, self.spos)
        ctx = compile(node, locals=[vname])
        self.assertEqual([vname], ctx.names)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_VAR, 0]))

    def test_global_variable(self):
        vname = "foo"
        node = ast.Variable(vname, self.spos)
        ctx = compile(node)
        self.assertEqual([vname], ctx.names)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_GLOBAL, 0]))

    def test_constant_int(self):
        value = 2
        node = ast.ConstantInt(value, self.spos)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(value, ctx.constants[0].intval)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_CONSTANT, 0]))

    def test_binary_operation(self):
        left = ast.ConstantInt(1, self.spos)
        right = ast.ConstantInt(2, self.spos)
        node = ast.BinOp("+", left, right, self.spos)
        ctx = compile(node)
        self.assertEqual(2, len(ctx.constants))
        self.assertEqual(1, ctx.constants[0].intval)
        self.assertEqual(2, ctx.constants[1].intval)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.LOAD_CONSTANT, 1,
                             bytecode.BINARY_ADD, 0]))

    def test_assignment(self):
        vname = "foo"
        var = ast.Variable(vname, self.spos)
        right = ast.ConstantInt(2, self.spos)
        node = ast.Assignment(var, right, self.spos)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(2, ctx.constants[0].intval)
        self.assertEqual([vname], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.ASSIGN, 0]))

    def test_print(self):
        arg = ast.ConstantInt(2, self.spos)
        node = ast.Function(ast.Variable("print", self.spos), [arg], self.spos)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(2, ctx.constants[0].intval)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.PRINT, 0]))

    def test_function(self):
        fname = "foo"
        arg1 = ast.ConstantInt(2, self.spos)
        arg2 = ast.ConstantInt(99, self.spos)
        node = ast.Function(ast.Variable(fname, self.spos), [arg1, arg2], self.spos)
        ctx = compile(node)
        self.assertEqual(2, len(ctx.constants))
        self.assertEqual(99, ctx.constants[0].intval)
        self.assertEqual(2, ctx.constants[1].intval)
        self.assertEqual([fname], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_GLOBAL, 0,
                             bytecode.LOAD_CONSTANT, 0,
                             bytecode.LOAD_CONSTANT, 1,
                             bytecode.CALL_FUNCTION, 2]))

    def test_conditional(self):
        condition = ast.ConstantInt(1, self.spos)
        true_block = ast.ConstantInt(2, self.spos)
        node = ast.Conditional(condition, true_block, None, self.spos)
        ctx = compile(node)
        self.assertEqual(2, len(ctx.constants))
        self.assertEqual(1, ctx.constants[0].intval)
        self.assertEqual(2, ctx.constants[1].intval)
        # TODO: optomize out the JUMP_FORWARD
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.JUMP_IF_FALSE, 4,
                             bytecode.LOAD_CONSTANT, 1,
                             bytecode.JUMP_FORWARD, 0]))

    def test_conditional_with_else(self):
        condition = ast.ConstantInt(1, self.spos)
        true_block = ast.ConstantInt(2, self.spos)
        else_block = ast.ConstantInt(3, self.spos)
        node = ast.Conditional(condition, true_block, else_block, self.spos)
        ctx = compile(node)
        self.assertEqual(3, len(ctx.constants))
        self.assertEqual(1, ctx.constants[0].intval)
        self.assertEqual(2, ctx.constants[1].intval)
        self.assertEqual(3, ctx.constants[2].intval)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.JUMP_IF_FALSE, 4,
                             bytecode.LOAD_CONSTANT, 1,
                             bytecode.JUMP_FORWARD, 2,
                             bytecode.LOAD_CONSTANT, 2]))

    def test_while(self):
        condition = ast.ConstantInt(1, self.spos)
        block = ast.ConstantInt(2, self.spos)
        node = ast.While(condition, block, self.spos)
        ctx = compile(node)
        self.assertEqual(2, len(ctx.constants))
        self.assertEqual(1, ctx.constants[0].intval)
        self.assertEqual(2, ctx.constants[1].intval)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.JUMP_IF_FALSE, 4,
                             bytecode.LOAD_CONSTANT, 1,
                             bytecode.JUMP_BACK, 6]))


    def test_function_defn(self):
        fname = "foo"
        block = ast.ConstantInt(2, self.spos)
        args = ["a", "b"]
        node = ast.FuncDef(fname, args, block, self.spos)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertIsInstance(ctx.constants[0], objectspace.W_Code)
        self.assertEqual([fname], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.MAKE_FUNCTION, 0,
                             bytecode.ASSIGN, 0]))
        self.assertThat(ctx.constants[0].bytecode,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.LOAD_CONSTANT, 1,
                             bytecode.RETURN, 0]))

    def test_return(self):
        arg = ast.ConstantInt(2, self.spos)
        node = ast.Return(arg, self.spos)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(2, ctx.constants[0].intval)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.RETURN, 0]))

    def test_return_no_arg(self):
        node = ast.Return(None, self.spos)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertIs(objectspace.TheNone, ctx.constants[0])
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.RETURN, 0]))

    def test_new_type(self):
        var = ast.Variable("a", self.spos)
        attrname = 'b'
        attrval = 2
        attr = ast.Variable(attrname, self.spos)
        right = ast.ConstantInt(attrval, self.spos)
        block = ast.Assignment(attr, right, self.spos)
        t = ast.NewType(block, "Type", self.spos)
        node = ast.Assignment(var, t, self.spos)
        ctx = compile(node)
        self.assertEqual(2, len(ctx.constants))
        self.assertIsInstance(ctx.constants[0], objectspace.W_String)
        self.assertEqual('a', ctx.constants[0].strval)
        self.assertIsInstance(ctx.constants[1], objectspace.W_Code)
        self.assertEqual(['Type', 'a'], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.LOAD_GLOBAL, 0,
                             bytecode.BUILD_TUPLE, 1,
                             bytecode.LOAD_CONSTANT, 1,
                             bytecode.MAKE_FUNCTION, 0,
                             bytecode.CALL_FUNCTION, 0,
                             bytecode.MAKE_TYPE, 0,
                             bytecode.ASSIGN, 1]))

    def test_enum(self):
        var = ast.Variable("a", self.spos)
        block = ast.Pass(self.spos)
        options = ["A"]
        t = ast.NewType(block, "Enum", self.spos, options=options)
        node = ast.Assignment(var, t, self.spos)
        ctx = compile(node)
        self.assertEqual(4, len(ctx.constants))
        self.assertIsInstance(ctx.constants[0], objectspace.W_String)
        self.assertEqual('a', ctx.constants[0].strval)
        self.assertIsInstance(ctx.constants[1], objectspace.W_Code)
        self.assertEqual(['Enum', 'A', 'a'], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.LOAD_GLOBAL, 0,
                             bytecode.BUILD_TUPLE, 1,
                             bytecode.LOAD_CONSTANT, 1,
                             bytecode.MAKE_FUNCTION, 0,
                             bytecode.CALL_FUNCTION, 0,
                             bytecode.MAKE_TYPE, 0,
                             bytecode.DUP_TOP, 0,
                             bytecode.DUP_TOP, 0,
                             bytecode.LOAD_CONSTANT, 2,
                             bytecode.ROT_TWO, 0,
                             bytecode.BUILD_TUPLE, 1,
                             bytecode.LOAD_CONSTANT, 3,
                             bytecode.MAKE_TYPE, 0,
                             bytecode.CALL_FUNCTION, 0,
                             bytecode.SET_ATTR, 1,
                             bytecode.ASSIGN, 2]))

    def test_Pass(self):
        node = ast.Pass(self.spos)
        ctx = compile(node)
        self.assertEqual([], ctx.constants)
        self.assertEqual([], ctx.names)
        self.assertEqual([], ctx.data)

    # TODO: tests for Attribute

    def test_Case(self):
        var = ast.Variable("a", self.spos)
        cases = [
            ast.CaseCase(ast.Variable("B", self.spos), ast.ConstantInt(1, self.spos), self.spos),
            ast.CaseCase(ast.Variable("C", self.spos), ast.ConstantInt(2, self.spos), self.spos),
        ]
        node = ast.Case(var, cases, self.spos)
        ctx = compile(node)
        self.assertEqual([1, 2], map(attrgetter('intval'), ctx.constants))
        self.assertEqual(["B", "a", "C"], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([
                bytecode.LOAD_GLOBAL, 0,
                bytecode.LOAD_GLOBAL, 1,
                bytecode.BINARY_IS, 0,
                bytecode.JUMP_IF_FALSE, 4,
                bytecode.LOAD_CONSTANT, 0,
                bytecode.JUMP_FORWARD, 12,
                bytecode.LOAD_GLOBAL, 2,
                bytecode.LOAD_GLOBAL, 1,
                bytecode.BINARY_IS, 0,
                bytecode.JUMP_IF_FALSE, 4,
                bytecode.LOAD_CONSTANT, 1,
                bytecode.JUMP_FORWARD, 0,
                ]))
