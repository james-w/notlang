from operator import attrgetter
from testtools import TestCase

from .. import bytecode, codegen, compilercontext, objectspace, interpreter


def interpret(bcode):
    space = interpreter.Space()
    return space.call_function(bcode, [], {}, {})


def make_simple_function(ctx, name):
    def code_cb(cctx):
        codegen.load_constant_int(cctx, 0)
        cctx.emit(bytecode.RETURN)
    return codegen.make_function(ctx, name, code_cb, [])


class VariableTests(TestCase):

    def test_store_load(self):
        varname = 'a'
        ctx = compilercontext.CompilerContext()
        ctx.locals = [varname]
        codegen.load_constant_int(ctx, 99)
        codegen.assignment(ctx, varname)
        codegen.load_var(ctx, varname)
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(99, ret.intval)

    def test_load_before_store(self):
        varname = 'a'
        ctx = compilercontext.CompilerContext()
        ctx.locals = [varname]
        codegen.load_var(ctx, varname)
        codegen.do_return(ctx)
        self.assertRaises(AssertionError, interpret, ctx.create_bytecode())


class BinaryOperationTests(TestCase):

    def test_add(self):
        ctx = compilercontext.CompilerContext()
        codegen.load_constant_int(ctx, 99)
        codegen.load_constant_int(ctx, 1)
        codegen.binary_operation(ctx, '+')
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(100, ret.intval)

    def test_sub(self):
        ctx = compilercontext.CompilerContext()
        codegen.load_constant_int(ctx, 99)
        codegen.load_constant_int(ctx, 1)
        codegen.binary_operation(ctx, '-')
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(98, ret.intval)

    def test_mult(self):
        ctx = compilercontext.CompilerContext()
        codegen.load_constant_int(ctx, 99)
        codegen.load_constant_int(ctx, 2)
        codegen.binary_operation(ctx, '*')
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(198, ret.intval)

    def test_lt_false(self):
        ctx = compilercontext.CompilerContext()
        codegen.load_constant_int(ctx, 99)
        codegen.load_constant_int(ctx, 2)
        codegen.binary_operation(ctx, '<')
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(0, ret.intval)

    def test_lt_true(self):
        ctx = compilercontext.CompilerContext()
        codegen.load_constant_int(ctx, 2)
        codegen.load_constant_int(ctx, 99)
        codegen.binary_operation(ctx, '<')
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(1, ret.intval)

    def test_gt_false(self):
        ctx = compilercontext.CompilerContext()
        codegen.load_constant_int(ctx, 2)
        codegen.load_constant_int(ctx, 99)
        codegen.binary_operation(ctx, '>')
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(0, ret.intval)

    def test_gt_true(self):
        ctx = compilercontext.CompilerContext()
        codegen.load_constant_int(ctx, 99)
        codegen.load_constant_int(ctx, 2)
        codegen.binary_operation(ctx, '>')
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(1, ret.intval)

    def test_eq_true(self):
        ctx = compilercontext.CompilerContext()
        codegen.load_constant_int(ctx, 99)
        codegen.load_constant_int(ctx, 99)
        codegen.binary_operation(ctx, '==')
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(1, ret.intval)

    def test_eq_false(self):
        ctx = compilercontext.CompilerContext()
        codegen.load_constant_int(ctx, 99)
        codegen.load_constant_int(ctx, 2)
        codegen.binary_operation(ctx, '==')
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(0, ret.intval)


class JumpTests(TestCase):

    def test_jump_if_false__false(self):
        varname = 'a'
        ctx = compilercontext.CompilerContext()
        ctx.locals = [varname]
        codegen.load_constant_int(ctx, 99)
        codegen.assignment(ctx, varname)
        codegen.load_constant_int(ctx, 99)
        codegen.load_constant_int(ctx, 2)
        codegen.binary_operation(ctx, '==')
        def block_cb(cctx):
            codegen.load_constant_int(cctx, 2)
            codegen.assignment(cctx, varname)
        def else_cb(cctx):
            pass
        codegen.conditional(ctx, block_cb, else_cb)
        codegen.load_var(ctx, varname)
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(99, ret.intval)

    def test_jump_if_false__true(self):
        varname = 'a'
        ctx = compilercontext.CompilerContext()
        ctx.locals = [varname]
        codegen.load_constant_int(ctx, 99)
        codegen.assignment(ctx, varname)
        codegen.load_constant_int(ctx, 99)
        codegen.load_constant_int(ctx, 99)
        codegen.binary_operation(ctx, '==')
        def block_cb(cctx):
            codegen.load_constant_int(cctx, 2)
            codegen.assignment(cctx, varname)
        def else_cb(cctx):
            pass
        codegen.conditional(ctx, block_cb, else_cb)
        codegen.load_var(ctx, varname)
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(2, ret.intval)

    def test_jump_back(self):
        varname = 'a'
        ctx = compilercontext.CompilerContext()
        ctx.locals = [varname]
        codegen.load_constant_int(ctx, 1)
        codegen.assignment(ctx, varname)
        def condition_cb(cctx):
            codegen.load_var(ctx, varname)
            codegen.load_constant_int(ctx, 0)
            codegen.binary_operation(ctx, '>')
        def block_cb(cctx):
            codegen.load_var(cctx, varname)
            codegen.load_constant_int(cctx, 1)
            codegen.binary_operation(cctx, '-')
            codegen.assignment(cctx, varname)
        codegen.while_loop(ctx, condition_cb, block_cb)
        codegen.load_var(ctx, varname)
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(0, ret.intval)


class FunctionTests(TestCase):

    def test_noargs(self):
        fname = "foo"
        ctx = compilercontext.CompilerContext()
        fvar = make_simple_function(ctx, fname)
        ctx.emit(bytecode.LOAD_VAR, fvar)
        ctx.emit(bytecode.CALL_FUNCTION, 0)
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(0, ret.intval)

    def test_one_arg(self):
        fname = "foo"
        ctx = compilercontext.CompilerContext()
        def code_cb(cctx):
            codegen.load_var(cctx, 'a')
            codegen.do_return(cctx)
        fvar = codegen.make_function(ctx, fname, code_cb, ['a'])
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
            codegen.load_var(cctx, 'a')
            codegen.load_var(cctx, 'b')
            codegen.binary_operation(cctx, '-')
            codegen.do_return(cctx)
        fvar = codegen.make_function(ctx, fname, code_cb, ['a', 'b'])
        ctx.emit(bytecode.LOAD_VAR, fvar)
        codegen.load_constant_int(ctx, 1)
        codegen.load_constant_int(ctx, 2)
        ctx.emit(bytecode.CALL_FUNCTION, 2)
        ctx.emit(bytecode.RETURN)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(1, ret.intval)


class TypeTests(TestCase):

    def test_instantiate(self):
        cname = "foo"
        attrname = "bar"
        varname = "a"
        ctx = compilercontext.CompilerContext()
        ctx.locals = [cname, varname]
        def code_cb(ctx):
            codegen.load_constant_int(ctx, 1)
            codegen.assignment(ctx, attrname)
        codegen.new_type(ctx, cname, code_cb)
        codegen.assignment(ctx, cname)
        codegen.load_var(ctx, cname)
        codegen.function_call(ctx, 0, lambda x: None)
        codegen.assignment(ctx, varname)
        codegen.load_var(ctx, varname)
        codegen.load_attr(ctx, attrname)
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertEqual(1, ret.intval)



class LocalsTests(TestCase):

    def test_load_locals(self):
        varname1 = "foo"
        varname2 = "bar"
        ctx = compilercontext.CompilerContext()
        ctx.locals = [varname1, varname2]
        codegen.load_constant_int(ctx, 1)
        codegen.assignment(ctx, varname1)
        codegen.load_locals(ctx)
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertIsInstance(ret, objectspace.W_Dict)
        self.assertEqual(1, len(ret.dictval))
        self.assertEqual(1, ret.dictval[varname1].intval)



class TupleTests(TestCase):

    def test_build_tuple(self):
        varname1 = "foo"
        varname2 = "bar"
        ctx = compilercontext.CompilerContext()
        ctx.locals = [varname1, varname2]
        codegen.load_constant_int(ctx, 1)
        codegen.load_constant_int(ctx, 2)
        codegen.build_tuple(ctx, 2)
        codegen.do_return(ctx)
        ret = interpret(ctx.create_bytecode())
        self.assertIsInstance(ret, objectspace.W_Tuple)
        self.assertEqual([2, 1], map(attrgetter('intval'), ret.val))
