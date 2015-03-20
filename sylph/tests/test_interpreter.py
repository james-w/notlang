from testtools import TestCase

from .. import bytecode, compiler, compilercontext, interpreter


def interpret(bcode):
    frame = interpreter.Frame(bcode)
    return frame.execute()


def load_int(ctx, val):
    vnum = ctx.register_constant(interpreter.W_Int(val))
    ctx.emit(bytecode.LOAD_CONSTANT, vnum)
    return vnum


def make_function(ctx, name, code_cb):
    cctx = compilercontext.CompilerContext()
    code_cb(cctx)
    code = cctx.create_bytecode()
    code_const = ctx.register_constant(code)
    ctx.emit(bytecode.LOAD_CONSTANT, code_const)
    ctx.emit(bytecode.MAKE_FUNCTION)
    fvar = ctx.register_var(name)
    ctx.emit(bytecode.ASSIGN, fvar)
    return fvar


def make_simple_function(ctx, name):
    def code_cb(cctx):
        load_int(cctx, 0)
        cctx.emit(bytecode.RETURN)
    return make_function(ctx, name, code_cb)


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
        fvar = make_function(ctx, fname, code_cb)
        ctx.emit(bytecode.LOAD_VAR, fvar)
        load_int(ctx, 1)
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
        fvar = make_function(ctx, fname, code_cb)
        ctx.emit(bytecode.LOAD_VAR, fvar)
        load_int(ctx, 1)
        load_int(ctx, 2)
        ctx.emit(bytecode.CALL_FUNCTION, 2)
        ctx.emit(bytecode.RETURN)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(1, ret.intval)
