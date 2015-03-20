from testtools import TestCase

from .. import ast, bytecode, objectspace
from ..compiler import Compiler
from ..compilercontext import CompilerContext
from ..testing import BytecodeMatches


def compile(node):
    ctx = CompilerContext()
    Compiler(ctx).dispatch(node)
    return ctx


class TestCompiler(TestCase):

    def test_variable(self):
        vname = "foo"
        node = ast.Variable(vname)
        ctx = compile(node)
        self.assertEqual([vname], ctx.names)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_VAR, 0]))

    def test_constant_int(self):
        value = 2
        node = ast.ConstantInt(value)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(value, ctx.constants[0].intval)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_CONSTANT, 0]))

    def test_binary_operation(self):
        left = ast.ConstantInt(1)
        right = ast.ConstantInt(2)
        node = ast.BinOp("+", left, right)
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
        var = ast.Variable(vname)
        right = ast.ConstantInt(2)
        node = ast.Assignment(var, right)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(2, ctx.constants[0].intval)
        self.assertEqual([vname], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.ASSIGN, 0]))

    def test_print(self):
        arg = ast.ConstantInt(2)
        node = ast.Function("print", [arg])
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(2, ctx.constants[0].intval)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.PRINT, 0]))

    def test_function(self):
        fname = "foo"
        arg = ast.ConstantInt(2)
        node = ast.Function(fname, [arg])
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(2, ctx.constants[0].intval)
        self.assertEqual([fname], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_VAR, 0,
                             bytecode.LOAD_CONSTANT, 0,
                             bytecode.CALL_FUNCTION, 1]))

    def test_conditional(self):
        condition = ast.ConstantInt(1)
        true_block = ast.ConstantInt(2)
        node = ast.Conditional(condition, true_block)
        ctx = compile(node)
        self.assertEqual(2, len(ctx.constants))
        self.assertEqual(1, ctx.constants[0].intval)
        self.assertEqual(2, ctx.constants[1].intval)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.JUMP_IF_FALSE, 2,
                             bytecode.LOAD_CONSTANT, 1]))

    def test_while(self):
        condition = ast.ConstantInt(1)
        block = ast.ConstantInt(2)
        node = ast.While(condition, block)
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
        block = ast.ConstantInt(2)
        args = ["a", "b"]
        node = ast.FuncDef(fname, args, block)
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
        arg = ast.ConstantInt(2)
        node = ast.Return(arg)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(2, ctx.constants[0].intval)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.RETURN, 0]))

    def test_return_no_arg(self):
        node = ast.Return(None)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertIs(objectspace.TheNone, ctx.constants[0])
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.RETURN, 0]))
