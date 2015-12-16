from hypothesis import given, Settings
from rpython.rlib.parsing.lexer import SourcePos

from .. import ast, bytecode, objectspace
from ..compiler import Compiler
from ..compilercontext import CompilerContext
from ..testing import BytecodeMatches, ASTFactory, HypothesisTestCase
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


class TestCompiler(HypothesisTestCase):

    def setUp(self):
        super(TestCompiler, self).setUp()
        self.factory = ASTFactory(self)

    @given(ast_strats.VariableStrategy())
    def test_variable(self, node):
        with self.capture_logs():
            ctx = compile(node, locals=[node.varname])
        self.assertEqual([node.varname], ctx.names)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_VAR, 0]))

    @given(ast_strats.VariableStrategy())
    def test_global_variable(self, node):
        with self.capture_logs():
            ctx = compile(node)
        self.assertEqual([node.varname], ctx.names)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_GLOBAL, 0]))

    @given(ast_strats.IntStrategy())
    def test_constant_int(self, node):
        with self.capture_logs():
            ctx = compile(node)
        self.assertEqual(1, len(ctx.constants))
        self.assertEqual(node.intval, ctx.constants[0].intval)
        self.assertThat(ctx.data, BytecodeMatches([bytecode.LOAD_CONSTANT, 0]))

    @given(ast_strats.BinOpStrategy())
    def test_binary_operation(self, node):
        with self.capture_logs():
            ctx = compile(node)
        args_ctxs = chain_compile(node.args)
        self.assertEqual(len(args_ctxs[-1].constants), len(ctx.constants))
        self.assertThat(ctx.data,
            BytecodeMatches(bytecode_to_expected(args_ctxs[0].data)
                            + bytecode_to_expected(args_ctxs[1].data)
                            + [bytecode.BINOP[node.op], 0]))

    @given(ast_strats.AssignmentStrategy())
    def test_assignment(self, node):
        with self.capture_logs():
            ctx = compile(node)
        source_ctx = compile(node.source)
        self.assertEqual(len(source_ctx.constants), len(ctx.constants))
        self.assertIn(node.target.varname, ctx.names)
        self.assertThat(ctx.data,
            BytecodeMatches(bytecode_to_expected(source_ctx.data)
                            + [bytecode.ASSIGN, ctx.names.index(node.target.varname)]))

    @given(ast_strats.PrintStrategy())
    def test_print(self, node):
        with self.capture_logs():
            ctx = compile(node)
        args_ctx = compile(node.args[0])
        self.assertEqual(len(args_ctx.constants), len(ctx.constants))
        self.assertThat(ctx.data,
            BytecodeMatches(bytecode_to_expected(args_ctx.data)
                            + [bytecode.PRINT, 0]))

    @given(ast_strats.FunctionCallStrategy())
    def test_function(self, node):
        with self.capture_logs():
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
        with self.capture_logs():
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
        with self.capture_logs():
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
        with self.capture_logs():
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
        with self.capture_logs():
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

    @given(ast_strats.NewTypeStrategy())
    def test_new_type(self, node):
        with self.capture_logs():
            ctx = compile(node)
        self.assertEqual(2 + 2 * len(node.source.options) if node.source.type_type == 'Enum' else 2, len(ctx.constants))
        self.assertIsInstance(ctx.constants[0], objectspace.W_String)
        self.assertEqual(node.target.varname, ctx.constants[0].strval)
        self.assertIsInstance(ctx.constants[1], objectspace.W_Code)
        bases = [node.source.type_type]
        cctx = CompilerContext()
        if node.source.type_type == 'Enum':
            bases.append('Type')
            if any(map(lambda x: len(x.members) > 0, node.source.options)):
                bases.append('Tuple')
            [cctx.register_var(x.name) for x in node.source.options]
        cctx.register_var(node.target.varname)
        opt_names = cctx.names
        self.assertEqual(sorted(bases + opt_names), sorted(ctx.names))
        if node.source.type_type == 'Enum':
            expected_bytecode = [bytecode.LOAD_CONSTANT, 0,
                                 bytecode.LOAD_GLOBAL, 0,
                                 bytecode.BUILD_TUPLE, 1,
                                 bytecode.LOAD_CONSTANT, 1,
                                 bytecode.MAKE_FUNCTION, 0,
                                 bytecode.CALL_FUNCTION, 0,
                                 bytecode.MAKE_TYPE, 0]
            for i, option in enumerate(node.source.options):
                expected_bytecode += [bytecode.DUP_TOP, 0,
                                      bytecode.DUP_TOP, 0,
                                      bytecode.LOAD_GLOBAL, 1,
                                      bytecode.ROT_TWO, 0,
                                      bytecode.LOAD_GLOBAL, 0,
                                      bytecode.ROT_TWO, 0]
                if len(option.members) > 0:
                    expected_bytecode += [bytecode.LOAD_GLOBAL, ctx.names.index('Tuple'),
                                          bytecode.ROT_TWO, 0]
                expected_bytecode += [bytecode.BUILD_TUPLE, 3 if len(option.members) == 0 else 4,
                                      bytecode.LOAD_CONSTANT, (i+1)*2,
                                      bytecode.ROT_TWO, 0,
                                      bytecode.LOAD_CONSTANT, (i+1)*2+1,
                                      bytecode.MAKE_TYPE, 0]
                if len(option.members) == 0:
                    expected_bytecode += [bytecode.CALL_FUNCTION, 0]
                expected_bytecode += [bytecode.SET_ATTR, ctx.names.index(option.name)]
            expected_bytecode += [bytecode.ASSIGN, ctx.names.index(node.target.varname)]
            self.assertThat(ctx.data, BytecodeMatches(expected_bytecode))
        else:
            self.assertThat(ctx.data,
                BytecodeMatches([bytecode.LOAD_CONSTANT, 0,
                                 bytecode.LOAD_GLOBAL, 0,
                                 bytecode.BUILD_TUPLE, 1,
                                 bytecode.LOAD_CONSTANT, 1,
                                 bytecode.MAKE_FUNCTION, 0,
                                 bytecode.CALL_FUNCTION, 0,
                                 bytecode.MAKE_TYPE, 0,
                                 bytecode.ASSIGN, 1]))

    @given(ast_strats.PassStrategy())
    def test_Pass(self, node):
        with self.capture_logs():
            ctx = compile(node)
        self.assertEqual([], ctx.constants)
        self.assertEqual([], ctx.names)
        self.assertEqual([], ctx.data)

    # TODO: tests for Attribute

    @given(ast_strats.CaseStrategy())
    def test_Case(self, node):
        with self.capture_logs():
            ctx = compile(node)
        parts = []
        extra_names = []
        for case in node.cases:
            parts.append(case.label)
            parts.append(node.target)
            parts.append(case.block)
        if node.else_case:
            parts.append(node.else_case.block)
        else:
            parts.append(self.factory.int())
            parts.append(node.target)
        contexts = chain_compile(parts)
        if node.else_case is None:
            extra_names = []
            for name in ['add', 'repr']:
                if name not in contexts[-1].names:
                    extra_names.append(name)
        self.assertEqual(contexts[-1].names + extra_names, ctx.names)
        expected_bytecode = []
        remainings = []
        if node.else_case:
            x = len(contexts[-1].data)
        else:
            x = 6*bytecode.INSTRUCTION_SIZE + len(contexts[-1].data)
        for i, case in enumerate(reversed(node.cases)):
            remainings.insert(0, x)
            x += 3*bytecode.INSTRUCTION_SIZE
            context_index = (len(node.cases)-i-1) * 3
            x += len(contexts[context_index].data)
            x += len(contexts[context_index+1].data)
            x += len(contexts[context_index+2].data)
        for i, case in enumerate(node.cases):
            expected_bytecode += bytecode_to_expected(contexts[i*3].data)
            expected_bytecode += bytecode_to_expected(contexts[i*3+1].data)
            expected_bytecode += [bytecode.BINARY_IS, 0,
                    bytecode.JUMP_IF_FALSE, len(contexts[i*3+2].data) + bytecode.INSTRUCTION_SIZE]
            expected_bytecode += bytecode_to_expected(contexts[i*3+2].data)
            # Not reducing on 0 because we add 2 ops per case
            expected_bytecode += [bytecode.JUMP_FORWARD, remainings[i]]
        if node.else_case:
            expected_bytecode += bytecode_to_expected(contexts[-1].data)
        else:
            expected_bytecode += bytecode_to_expected(contexts[-2].data)
            expected_bytecode += [bytecode.LOAD_ATTR, ctx.names.index('add')]
            expected_bytecode += bytecode_to_expected(contexts[-1].data)
            expected_bytecode += [bytecode.LOAD_ATTR, ctx.names.index('repr'),
                                  bytecode.CALL_FUNCTION, 0,
                                  bytecode.CALL_FUNCTION, 1,
                                  bytecode.PANIC, 0,
                                  ]
        self.assertThat(ctx.data, BytecodeMatches(expected_bytecode))
