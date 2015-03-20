from testtools import TestCase

from .. import bytecode, codegen, compilercontext, interpreter


def interpret(bcode):
    frame = interpreter.Frame(bcode)
    return frame.execute()


def make_simple_function(ctx, name):
    def code_cb(cctx):
        codegen.load_constant_int(cctx, 0)
        cctx.emit(bytecode.RETURN)
    return codegen.make_function(ctx, name, code_cb)


class CALL_FUNCTIONTests(TestCase):

    def test_noargs(self):
        fname = "foo"
        ctx = compilercontext.CompilerContext()
        fvar = make_simple_function(ctx, fname)
        ctx.emit(bytecode.LOAD_VAR, fvar)
        ctx.emit(bytecode.CALL_FUNCTION, 0)
        ctx.emit(bytecode.RETURN)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(0, ret.intval)

    def test_one_arg(self):
        fname = "foo"
        ctx = compilercontext.CompilerContext()
        def code_cb(cctx):
            cctx.emit(bytecode.LOAD_VAR, cctx.register_var('a'))
            cctx.emit(bytecode.RETURN)
        fvar = codegen.make_function(ctx, fname, code_cb)
        ctx.emit(bytecode.LOAD_VAR, fvar)
        codegen.load_constant_int(ctx, 1)
        ctx.emit(bytecode.CALL_FUNCTION, 1)
        ctx.emit(bytecode.RETURN)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(1, ret.intval)

    def test_two_args(self):
        fname = "foo"
        ctx = compilercontext.CompilerContext()
        def code_cb(cctx):
            cctx.emit(bytecode.LOAD_VAR, cctx.register_var('a'))
            cctx.emit(bytecode.LOAD_VAR, cctx.register_var('b'))
            cctx.emit(bytecode.BINARY_SUB)
            cctx.emit(bytecode.RETURN)
        fvar = codegen.make_function(ctx, fname, code_cb)
        ctx.emit(bytecode.LOAD_VAR, fvar)
        codegen.load_constant_int(ctx, 1)
        codegen.load_constant_int(ctx, 2)
        ctx.emit(bytecode.CALL_FUNCTION, 2)
        ctx.emit(bytecode.RETURN)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(1, ret.intval)
