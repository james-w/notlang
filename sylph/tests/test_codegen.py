from testtools import TestCase

from .. import bytecode, codegen, objectspace
from ..compilercontext import CompilerContext
from ..testing import BytecodeMatches


class CodeGenTests(TestCase):


    def test_load_var(self):
        varname = "foo"
        ctx = CompilerContext()
        ctx.locals = [varname]
        vnum = codegen.load_var(ctx, varname)
        self.assertEqual([varname], ctx.names)
        self.assertEqual(0, vnum)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_VAR, 0]))

    def test_load_global(self):
        varname = "foo"
        ctx = CompilerContext()
        vnum = codegen.load_var(ctx, varname)
        self.assertEqual([varname], ctx.names)
        self.assertEqual(0, vnum)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_GLOBAL, 0]))

    def test_load_constant_int(self):
        ctx = CompilerContext()
        vnum = codegen.load_constant_int(ctx, 2)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(2, ctx.constants[0].intval)
        self.assertEqual(0, vnum)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_CONSTANT, 0]))

    def test_make_function(self):
        fname = "foo"
        ctx = CompilerContext()
        def code_cb(cctx):
            pass
        fvar = codegen.make_function(ctx, fname, code_cb, [])
        self.assertEqual(0, fvar)
        self.assertEqual(1, len(ctx.constants))
        self.assertIsInstance(ctx.constants[0], objectspace.W_Code)
        self.assertEqual([fname], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.MAKE_FUNCTION, 0,
                             bytecode.ASSIGN, 0]))

    def test_binary_operation(self):
        ctx = CompilerContext()
        codegen.binary_operation(ctx, "+")
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.BINARY_ADD, 0]))

    def test_assignment(self):
        varname = "foo"
        ctx = CompilerContext()
        vnum = codegen.assignment(ctx, varname)
        self.assertEqual([varname], ctx.names)
        self.assertEqual(0, vnum)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.ASSIGN, 0]))

    def test_print(self):
        ctx = CompilerContext()
        codegen.do_print(ctx)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.PRINT, 0]))

    def test_function_call(self):
        fname = "foo"
        numargs = 2
        ctx = CompilerContext()
        def args_cb(cctx):
            cctx.emit(bytecode.LOAD_CONSTANT, 99)
        fnum = codegen.function_call(ctx, fname, numargs, args_cb)
        self.assertEqual(0, fnum)
        self.assertEqual([fname], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_GLOBAL, 0,
                             bytecode.LOAD_CONSTANT, 99,
                             bytecode.CALL_FUNCTION, numargs]))

    def test_conditional(self):
        ctx = CompilerContext()
        def true_block_cb(cctx):
            cctx.emit(bytecode.LOAD_CONSTANT, 99)
        def false_block_cb(cctx):
            cctx.emit(bytecode.LOAD_CONSTANT, 98)
        codegen.conditional(ctx, true_block_cb, false_block_cb)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.JUMP_IF_FALSE, 4,
                             bytecode.LOAD_CONSTANT, 99,
                             bytecode.JUMP_FORWARD, 2,
                             bytecode.LOAD_CONSTANT, 98]))

    def test_while(self):
        ctx = CompilerContext()
        def conditional_cb(cctx):
            cctx.emit(bytecode.LOAD_CONSTANT, 88)
        def block_cb(cctx):
            cctx.emit(bytecode.LOAD_CONSTANT, 99)
        codegen.while_loop(ctx, conditional_cb, block_cb)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 88,
                             bytecode.JUMP_IF_FALSE, 4,
                             bytecode.LOAD_CONSTANT, 99,
                             bytecode.JUMP_BACK, 6]))

    def test_return(self):
        ctx = CompilerContext()
        codegen.do_return(ctx)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.RETURN, 0]))

    def test_load_none(self):
        ctx = CompilerContext()
        var = codegen.load_none(ctx)
        self.assertEqual(var, 0)
        self.assertEqual(1, len(ctx.constants))
        self.assertIs(objectspace.TheNone, ctx.constants[0])
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_CONSTANT, 0]))
