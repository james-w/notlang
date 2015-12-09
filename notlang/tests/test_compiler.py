from operator import attrgetter, methodcaller

from hypothesis import given, Settings
from testtools import TestCase
from rpython.rlib.parsing.lexer import SourcePos

from .. import ast, bytecode, objectspace
from ..compiler import Compiler
from ..compilercontext import CompilerContext
from ..testing import BytecodeMatches, ASTFactory
from ..testing.strategies import ast as ast_strats


def compile(node, locals=None, constants=None, names=None, names_to_numbers=None):
    ctx = CompilerContext()
    if locals is None:
        locals = []
    ctx.locals = locals
    if constants is None:
        constants = []
    ctx.constants = constants
    if names is None:
        names = []
    ctx.names = names
    if names_to_numbers is None:
        names_to_numbers = {}
    ctx.names_to_numbers = names_to_numbers
    Compiler(ctx).dispatch(node)
    return ctx


def chain_compile(nodes, locals=None, names=None, names_to_numbers=None):
    if len(nodes) < 1:
        return []
    ctx = compile(nodes[0], locals=locals, names=names, names_to_numbers=names_to_numbers)
    contexts = [ctx]
    for node in nodes[1:]:
        ctx = compile(node, locals=ctx.locals, constants=ctx.constants, names=ctx.names, names_to_numbers=ctx.names_to_numbers)
        contexts.append(ctx)
    return contexts


def bytecode_to_expected(bc):
    expected = []
    for i in range(len(bc), step=bytecode.INSTRUCTION_SIZE):
        opcode = ord(bc[i])
        high = ord(bc[i+1])
        low = ord(bc[i+2])
        arg = (high << 8) + low
        expected.extend([opcode, arg])
    return expected


class TestCompiler(TestCase):

    spos = SourcePos(0, 0, 0)

    def setUp(self):
        super(TestCompiler, self).setUp()
        self.factory = ASTFactory(self)

    @given(ast_strats.VariableStrategy())
    def test_variable(self, node):
        ctx = compile(node, locals=[node.varname])
        self.assertEqual([node.varname], ctx.names)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_VAR, 0]))

    @given(ast_strats.VariableStrategy())
    def test_global_variable(self, node):
        ctx = compile(node)
        self.assertEqual([node.varname], ctx.names)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_GLOBAL, 0]))

    @given(ast_strats.IntStrategy())
    def test_constant_int(self, node):
        ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(node.intval, ctx.constants[0].intval)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_CONSTANT, 0]))

    @given(ast_strats.BinOpStrategy())
    def test_binary_operation(self, node):
        ctx = compile(node)
        args_ctxs = chain_compile(node.args)
        self.assertEqual(len(args_ctxs[-1].constants), len(ctx.constants))
        self.assertThat(ctx.data,
            BytecodeMatches(bytecode_to_expected(args_ctxs[0].data)
                            + bytecode_to_expected(args_ctxs[1].data)
                            + [bytecode.BINOP[node.op], 0]))

    @given(ast_strats.AssignmentStrategy())
    def test_assignment(self, node):
        ctx = compile(node)
        source_ctx = compile(node.source)
        self.assertEqual(len(source_ctx.constants), len(ctx.constants))
        self.assertIn(node.target.varname, ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches(bytecode_to_expected(source_ctx.data)
                            + [bytecode.ASSIGN, ctx.names.index(node.target.varname)]))

    @given(ast_strats.PrintStrategy())
    def test_print(self, node):
        ctx = compile(node)
        args_ctx = compile(node.args[0])
        self.assertEqual(len(args_ctx.constants), len(ctx.constants))
        self.assertThat(ctx.data,
            BytecodeMatches(bytecode_to_expected(args_ctx.data)
                            + [bytecode.PRINT, 0]))

    @given(ast_strats.FunctionCallStrategy())
    def test_function(self, node):
        ctx = compile(node)
        contexts = chain_compile(list(reversed(node.args)), names=[node.fname.varname], names_to_numbers={node.fname.varname: 0})
        if node.args:
            self.assertEqual(len(contexts[-1].constants), len(ctx.constants))
        else:
            self.assertEqual([], ctx.constants)
        self.assertIn(node.fname.varname, ctx.names)
        expected_bytecode = [bytecode.LOAD_GLOBAL, 0]
        for arg_ctx in contexts:
            expected_bytecode += bytecode_to_expected(arg_ctx.data)
        expected_bytecode += [bytecode.CALL_FUNCTION, len(node.args)]
        self.assertThat(ctx.data, BytecodeMatches(expected_bytecode))

    @given(ast_strats.ConditionalStrategy(), settings=Settings(max_examples=10))
    def test_conditional(self, node):
        ctx = compile(node)
        blocks = [node.condition, node.true_block]
        if node.false_block is not None:
            blocks.append(node.false_block)
        contexts = chain_compile(blocks)
        self.assertEqual(len(contexts[-1].constants), len(ctx.constants))
        # TODO: optomize out the JUMP_FORWARD if no false_block
        expected_bytecode = (bytecode_to_expected(contexts[0].data)
                            + [bytecode.JUMP_IF_FALSE, len(contexts[1].data) + bytecode.INSTRUCTION_SIZE]
                            + bytecode_to_expected(contexts[1].data))
        if node.false_block is not None:
            expected_bytecode += [bytecode.JUMP_FORWARD, len(contexts[2].data)]
            expected_bytecode += bytecode_to_expected(contexts[2].data)
        else:
            # TODO: optomize out the JUMP_FORWARD if no false_block
            expected_bytecode += [bytecode.JUMP_FORWARD, 0]
        self.assertThat(ctx.data, BytecodeMatches(expected_bytecode))

    @given(ast_strats.WhileStrategy())
    def test_while(self, node):
        ctx = compile(node)
        condition_ctx, block_ctx = chain_compile([node.condition, node.block])
        self.assertEqual(len(block_ctx.constants), len(ctx.constants))
        self.assertThat(ctx.data,
            BytecodeMatches(bytecode_to_expected(condition_ctx.data)
                            + [bytecode.JUMP_IF_FALSE, len(block_ctx.data) + bytecode.INSTRUCTION_SIZE]
                            + bytecode_to_expected(block_ctx.data)
                            + [bytecode.JUMP_BACK, len(block_ctx.data) + len(condition_ctx.data) + bytecode.INSTRUCTION_SIZE]))


    @given(ast_strats.FuncDefStrategy())
    def test_function_defn(self, node):
        ctx = compile(node)
        code_ctx = CompilerContext()
        for arg in node.args:
            code_ctx.register_var(arg)
        code_ctx.locals = list(node.args)
        for local in ast.GatherAssignedNames().dispatch(node.code):
            if local not in code_ctx.locals:
                code_ctx.locals.append(local)
        Compiler(code_ctx).dispatch(node.code)
        self.assertEqual(1, len(ctx.constants))
        self.assertIsInstance(ctx.constants[0], objectspace.W_Code)
        self.assertEqual([node.name], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                             bytecode.MAKE_FUNCTION, 0,
                             bytecode.ASSIGN, 0]))
        self.assertThat(ctx.constants[0].bytecode,
            BytecodeMatches(bytecode_to_expected(code_ctx.data)
                            + [bytecode.LOAD_CONSTANT, len(code_ctx.constants),
                               bytecode.RETURN, 0]))

    @given(ast_strats.ReturnStrategy())
    def test_return(self, node):
        ctx = compile(node)
        if node.arg is not None:
            arg_ctx = compile(node.arg)
            num_constants = len(arg_ctx.constants)
            expected_bytecode = bytecode_to_expected(arg_ctx.data)
        else:
            num_constants = 1
            expected_bytecode = [bytecode.LOAD_CONSTANT, 0]
        self.assertEqual(num_constants, len(ctx.constants))
        self.assertThat(ctx.data,
            BytecodeMatches(expected_bytecode + [bytecode.RETURN, 0]))

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
                bytecode.JUMP_IF_FALSE, 2 * bytecode.INSTRUCTION_SIZE,
                bytecode.LOAD_CONSTANT, 0,
                bytecode.JUMP_FORWARD, 13 * bytecode.INSTRUCTION_SIZE,
                bytecode.LOAD_GLOBAL, 2,
                bytecode.LOAD_GLOBAL, 1,
                bytecode.BINARY_IS, 0,
                bytecode.JUMP_IF_FALSE, 2 * bytecode.INSTRUCTION_SIZE,
                bytecode.LOAD_CONSTANT, 1,
                bytecode.JUMP_FORWARD, 7 * bytecode.INSTRUCTION_SIZE,
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
        ctx = compile(node)
        self.assertEqual([1, 2], map(attrgetter('intval'), ctx.constants))
        self.assertEqual(["B", "a"], ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches([
                bytecode.LOAD_GLOBAL, 0,
                bytecode.LOAD_GLOBAL, 1,
                bytecode.BINARY_IS, 0,
                bytecode.JUMP_IF_FALSE, 2 * bytecode.INSTRUCTION_SIZE,
                bytecode.LOAD_CONSTANT, 0,
                bytecode.JUMP_FORWARD, bytecode.INSTRUCTION_SIZE,
                bytecode.LOAD_CONSTANT, 1,
                ]))
