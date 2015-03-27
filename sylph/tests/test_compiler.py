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
        node = ast.Function("print", [arg], self.spos)
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
        node = ast.Function(fname, [arg1, arg2], self.spos)
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
        t = ast.NewType(self.spos)
        node = ast.Assignment(var, t, self.spos)
        ctx = compile(node)
        self.assertEqual(0, len(ctx.constants))
        self.assertThat(ctx.data,
            BytecodeMatches([]))
