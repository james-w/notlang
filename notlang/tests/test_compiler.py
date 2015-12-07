from operator import attrgetter, methodcaller

from testtools import TestCase
from rpython.rlib.parsing.lexer import SourcePos

from .. import bytecode, objectspace
from ..compiler import Compiler
from ..compilercontext import CompilerContext
from ..testing import BytecodeMatches, ASTFactory


def compile(node, locals=None):
    ctx = CompilerContext()
    if locals is None:
        locals = []
    ctx.locals = locals
    Compiler(ctx).dispatch(node)
    return ctx


class TestCompiler(TestCase):

    spos = SourcePos(0, 0, 0)

    def setUp(self):
        super(TestCompiler, self).setUp()
        self.factory = ASTFactory(self)

    def test_variable(self):
        vname = "foo"
        node = self.factory.variable(name=vname)
        ctx = compile(node, locals=[vname])
        self.assertEqual([vname], ctx.names)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_VAR, 0]))

    def test_global_variable(self):
        vname = "foo"
        node = self.factory.variable(name=vname)
        ctx = compile(node)
        self.assertEqual([vname], ctx.names)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_GLOBAL, 0]))

    def test_constant_int(self):
        value = 2
        node = self.factory.int(value=value)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(value, ctx.constants[0].intval)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_CONSTANT, 0]))

    def test_binary_operation(self):
        left = self.factory.int(value=1)
        right = self.factory.int(value=2)
        node = self.factory.binop(op='+', a=left, b=right)
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
        var = self.factory.variable(name=vname)
        right = self.factory.int(value=2)
        node = self.factory.assignment(source=right, target=var)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(2, ctx.constants[0].intval)
        self.assertEqual([vname], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.ASSIGN, 0]))

    def test_print(self):
        arg = self.factory.int(value=2)
        node = self.factory.function_call(function=self.factory.variable(name="print"), args=[arg])
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(2, ctx.constants[0].intval)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.PRINT, 0]))

    def test_function(self):
        fname = "foo"
        arg1 = self.factory.int(value=2)
        arg2 = self.factory.int(value=99)
        node = self.factory.function_call(function=self.factory.variable(name=fname), args=[arg1, arg2])
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
        condition = self.factory.int(1)
        true_block = self.factory.int(2)
        node = self.factory.conditional(condition=condition, true_block=true_block)
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
        condition = self.factory.int(1)
        true_block = self.factory.int(2)
        else_block = self.factory.int(3)
        node = self.factory.conditional(condition=condition, true_block=true_block, false_block=else_block)
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
        condition = self.factory.int(value=1)
        block = self.factory.int(value=2)
        node = self.factory.while_(condition=condition, block=block)
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
        block = self.factory.int(value=2)
        args = ["a", "b"]
        node = self.factory.funcdef(name=fname, body=block, args=args)
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
        arg = self.factory.int(value=2)
        node = self.factory.return_(arg=arg)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(2, ctx.constants[0].intval)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.RETURN, 0]))

    def test_return_no_arg(self):
        node = self.factory.return_(arg=None)
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertIs(objectspace.TheNone, ctx.constants[0])
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.RETURN, 0]))

    def test_new_type(self):
        var = self.factory.variable(name="a")
        attrname = 'b'
        attrval = 2
        attr = self.factory.variable(name=attrname)
        right = self.factory.int(value=attrval)
        block = self.factory.assignment(target=attr, source=right)
        t = self.factory.newtype(block=block)
        node = self.factory.assignment(target=var, source=t)
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
        var = self.factory.variable(name="a")
        block = self.factory.pass_()
        options = [self.factory.type_option(name="A")]
        t = self.factory.enum(block=block, options=options)
        node = self.factory.assignment(target=var, source=t)
        ctx = compile(node)
        self.assertEqual(4, len(ctx.constants))
        self.assertIsInstance(ctx.constants[0], objectspace.W_String)
        self.assertEqual('a', ctx.constants[0].strval)
        self.assertIsInstance(ctx.constants[1], objectspace.W_Code)
        self.assertEqual(['Enum', 'Type', 'A', 'a'], ctx.names)
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
                             bytecode.LOAD_GLOBAL, 1,
                             bytecode.ROT_TWO, 0,
                             bytecode.LOAD_GLOBAL, 0,
                             bytecode.ROT_TWO, 0,
                             bytecode.BUILD_TUPLE, 3,
                             bytecode.LOAD_CONSTANT, 2,
                             bytecode.ROT_TWO, 0,
                             bytecode.LOAD_CONSTANT, 3,
                             bytecode.MAKE_TYPE, 0,
                             bytecode.CALL_FUNCTION, 0,
                             bytecode.SET_ATTR, 2,
                             bytecode.ASSIGN, 3]))

    def test_destructuring_enum(self):
        var = self.factory.variable(name="a")
        block = self.factory.pass_()
        options = [self.factory.type_option(name="A", members=["x"])]
        t = self.factory.enum(block=block, options=options)
        node = self.factory.assignment(target=var, source=t)
        ctx = compile(node)
        self.assertEqual(4, len(ctx.constants))
        self.assertIsInstance(ctx.constants[0], objectspace.W_String)
        self.assertEqual('a', ctx.constants[0].strval)
        self.assertIsInstance(ctx.constants[1], objectspace.W_Code)
        self.assertEqual(['Enum', 'Type', 'Tuple', 'A', 'a'], ctx.names)
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
                             bytecode.LOAD_GLOBAL, 1,
                             bytecode.ROT_TWO, 0,
                             bytecode.LOAD_GLOBAL, 0,
                             bytecode.ROT_TWO, 0,
                             bytecode.LOAD_GLOBAL, 2,
                             bytecode.ROT_TWO, 0,
                             bytecode.BUILD_TUPLE, 4,
                             bytecode.LOAD_CONSTANT, 2,
                             bytecode.ROT_TWO, 0,
                             bytecode.LOAD_CONSTANT, 3,
                             bytecode.MAKE_TYPE, 0,
                             bytecode.SET_ATTR, 3,
                             bytecode.ASSIGN, 4]))

    def test_Pass(self):
        node = self.factory.pass_()
        ctx = compile(node)
        self.assertEqual([], ctx.constants)
        self.assertEqual([], ctx.names)
        self.assertEqual([], ctx.data)

    # TODO: tests for Attribute

    def test_Case(self):
        var = self.factory.variable(name="a")
        cases = [
            self.factory.case_case(label=self.factory.variable(name="B"), block=self.factory.int(value=1)),
            self.factory.case_case(label=self.factory.variable(name="C"), block=self.factory.int(value=2)),
        ]
        node = self.factory.case(target=var, cases=cases)
        ctx = compile(node)
        self.assertEqual([repr(1), repr(2), repr("Pattern match failure: ")], map(methodcaller('repr'), ctx.constants))
        self.assertEqual(["B", "a", "C", "add", "repr"], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([
                bytecode.LOAD_GLOBAL, 0,
                bytecode.LOAD_GLOBAL, 1,
                bytecode.BINARY_IS, 0,
                bytecode.JUMP_IF_FALSE, 4,
                bytecode.LOAD_CONSTANT, 0,
                bytecode.JUMP_FORWARD, 26,
                bytecode.LOAD_GLOBAL, 2,
                bytecode.LOAD_GLOBAL, 1,
                bytecode.BINARY_IS, 0,
                bytecode.JUMP_IF_FALSE, 4,
                bytecode.LOAD_CONSTANT, 1,
                bytecode.JUMP_FORWARD, 14,
                bytecode.LOAD_CONSTANT, 2,
                bytecode.LOAD_ATTR, 3,
                bytecode.LOAD_GLOBAL, 1,
                bytecode.LOAD_ATTR, 4,
                bytecode.CALL_FUNCTION, 0,
                bytecode.CALL_FUNCTION, 1,
                bytecode.PANIC, 0,
                ]))

    def test_Case_with_else(self):
        var = self.factory.variable(name="a")
        cases = [
            self.factory.case_case(label=self.factory.variable(name="B"), block=self.factory.int(value=1)),
        ]
        else_case = self.factory.case_case(label=self.factory.variable(name="else"), block=self.factory.int(value=2))
        node = self.factory.case(target=var, cases=cases, else_case=else_case)
        print node.children
        ctx = compile(node)
        self.assertEqual([1, 2], map(attrgetter('intval'), ctx.constants))
        self.assertEqual(["B", "a"], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([
                bytecode.LOAD_GLOBAL, 0,
                bytecode.LOAD_GLOBAL, 1,
                bytecode.BINARY_IS, 0,
                bytecode.JUMP_IF_FALSE, 4,
                bytecode.LOAD_CONSTANT, 0,
                bytecode.JUMP_FORWARD, 2,
                bytecode.LOAD_CONSTANT, 1,
                ]))
