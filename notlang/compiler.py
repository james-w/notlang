from . import ast, bytecode, codegen, compilercontext, debug, objectspace, typer


class Compiler(ast.ASTVisitor):

    def __init__(self, ctx):
        self.ctx = ctx

    def general_nonterminal_visit(self, node):
        [self.dispatch(c) for c in node.children]

    def visit_Attribute(self, node):
        self.dispatch(node.target)
        codegen.load_attr(self.ctx, node.name)

    def visit_Variable(self, node):
        codegen.load_var(self.ctx, node.varname)

    def visit_ConstantInt(self, node):
        codegen.load_constant_int(self.ctx, node.intval)

    def visit_BinOp(self, node):
        self.general_nonterminal_visit(node)
        codegen.binary_operation(self.ctx, node.op)

    def visit_Assignment(self, node):
        if isinstance(node.source, ast.NewType):
            if node.source.type_type == 'Enum':
                codegen.enum(self.ctx, node.var.varname, node.source.options,
                             CallbackHelper(node.source).type_code_cb)
            else:
                codegen.new_type(self.ctx, node.var.varname, [node.source.type_type], CallbackHelper(node.source).type_code_cb)
        else:
            self.general_nonterminal_visit(node)
        codegen.assignment(self.ctx, node.var.varname)

    def visit_Function(self, node):
        # FIXME: doesn't handle aliasing the print function
        if isinstance(node.fname, ast.Variable) and node.fname.varname == 'print':
            self.dispatch(node.args[0])
            codegen.do_print(self.ctx)
        else:
            self.dispatch(node.fname)
            codegen.function_call(self.ctx, len(node.args), CallbackHelper(node).function_args_cb)

    def visit_Conditional(self, node):
        self.dispatch(node.condition)
        helper = CallbackHelper(node)
        codegen.conditional(self.ctx, helper.conditional_true_block_cb, helper.conditional_false_block_cb)

    def visit_While(self, node):
        helper = CallbackHelper(node)
        codegen.while_loop(self.ctx, helper.while_condition_cb, helper.while_block_cb)

    def visit_FuncDef(self, node):
        codegen.make_function(self.ctx, node.name, CallbackHelper(node).funcdef_code_cb, node.args)

    def visit_Case(self, node):
        helper = CallbackHelper(node.cases[0], cases=node.cases, case_parent=node, else_case=node.else_case)
        helper.case_no_match_cb(self.ctx)

    def visit_Return(self, node):
        if node.arg:
            self.dispatch(node.arg)
        else:
            codegen.load_none(self.ctx)
        codegen.do_return(self.ctx)

    def visit_Pass(self, node):
        # How appropriate
        pass


class CallbackHelper(object):

    def __init__(self, node, cases=None, case_parent=None, else_case=None):
        self.node = node
        self.cases = cases
        self.case_parent = case_parent
        self.else_case = else_case

    def funcdef_code_cb(self, ctx):
        for local in ast.GatherAssignedNames().dispatch(self.node.code):
            if local not in ctx.locals:
                ctx.locals.append(local)
        Compiler(ctx).dispatch(self.node.code)

    def function_args_cb(self, ctx):
        [Compiler(ctx).dispatch(c) for c in reversed(self.node.args)]

    def conditional_true_block_cb(self, ctx):
        Compiler(ctx).dispatch(self.node.true_block)

    def conditional_false_block_cb(self, ctx):
        false_block = self.node.false_block
        if false_block is not None:
            Compiler(ctx).dispatch(false_block)

    def while_condition_cb(self, ctx):
        Compiler(ctx).dispatch(self.node.condition)

    def while_block_cb(self, ctx):
        Compiler(ctx).dispatch(self.node.block)

    def type_code_cb(self, ctx):
        Compiler(ctx).dispatch(self.node.block)

    def case_self_cb(self, ctx):
        Compiler(ctx).dispatch(self.case_parent.target)

    def case_match_cb(self, ctx):
        if not(self.node.is_simple()):
            functions = {0: 'first', 1: 'second'}
            for i, arg in enumerate(self.node.label.args):
                Compiler(ctx).dispatch(self.case_parent.target)
                codegen.load_attr(ctx, functions[i])
                codegen.function_call(ctx, 1, self.case_self_cb)
                codegen.assignment(ctx, arg.varname)
        Compiler(ctx).dispatch(self.node.block)

    def case_no_match_cb(self, ctx):
        if self.cases:
            case = self.cases[0]
            if case.is_simple():
                Compiler(ctx).dispatch(case.label)
                Compiler(ctx).dispatch(self.case_parent.target)
                codegen.is_(ctx)
            else:
                # XXX: hardcoding
                codegen.load_global(ctx, "isinstance")
                codegen.function_call(ctx, 2, self.case_is_instance_function_args_cb)
            helper = CallbackHelper(case, cases=self.cases[1:], case_parent=self.case_parent, else_case=self.else_case)
            codegen.conditional(ctx, helper.case_match_cb, helper.case_no_match_cb)
        elif self.else_case:
            Compiler(ctx).dispatch(self.else_case.block)
        else:
            # TODO: exceptions, better error message
            codegen.load_constant_string(ctx, "Pattern match failure: ")
            codegen.load_attr(ctx, 'add')
            def load_target(ctx):
                Compiler(ctx).dispatch(self.case_parent.target)
                codegen.load_attr(ctx, 'repr')
                codegen.function_call(ctx, 0, lambda x: None)
            codegen.function_call(ctx, 1, load_target)
            codegen.panic(ctx)

    def case_is_instance_function_args_cb(self, ctx):
        case = self.cases[0]
        Compiler(ctx).dispatch(self.case_parent.target)
        Compiler(ctx).dispatch(case.label.fname)


def dump_instr(index, inst, arg, context=None):
    line = debug.coloured(str(index), debug.colours.BOLD) + " "
    line += debug.coloured(bytecode.reverse_map[inst], debug.colours.BROWN)
    if inst not in bytecode.unary_ops:
        line += " " + debug.coloured(str(arg), debug.colours.BLUE)
    if context is not None:
        if inst in (bytecode.LOAD_VAR, bytecode.ASSIGN, bytecode.LOAD_GLOBAL, bytecode.LOAD_ATTR):
            line += " (" + debug.coloured(str(context.names[arg]), debug.colours.GREEN) + ")"
        if inst in (bytecode.LOAD_CONSTANT,):
            line += " (" + debug.coloured(context.constants[arg].repr(), debug.colours.GREEN) + ")"
    return line


def dump(code, context=None):
    lines = []
    i = 0
    stacksize = 0
    for i in range(0, len(code.bytecode), 2):
        c = ord(code.bytecode[i])
        c2 = ord(code.bytecode[i + 1])
        stacksize += compilercontext.get_stack_change(c, c2)
        lines.append(dump_instr(i, c, c2, context=context) + " (stacksize: %d)" % stacksize)
    return '\n'.join(lines)


def max_stacksize(code):
    stacksize = 0
    maximum = 0
    i = 0
    for i in range(0, len(code.bytecode), 2):
        c = ord(code.bytecode[i])
        c2 = ord(code.bytecode[i + 1])
        stacksize += compilercontext.get_stack_change(c, c2)
        if stacksize > maximum:
            maximum = stacksize
            max_instr = c
            max_line = i
    return maximum, max_line, bytecode.reverse_map.get(max_instr)


def get_compiler(astnode, trace_typer=False):
    typer.typecheck(astnode, trace=trace_typer)
    c = compilercontext.CompilerContext()
    c.locals = ast.GatherAssignedNames().dispatch(astnode)
    # XXX: needs to be generalised
    c.locals.append("List")
    Compiler(c).dispatch(astnode)
    c.emit(bytecode.LOAD_CONSTANT, c.register_constant(objectspace.TheNone))
    c.emit(bytecode.RETURN)
    return c

def compile_ast(astnode, trace_typer=False):
    c = get_compiler(astnode, trace_typer=trace_typer)
    return c.create_bytecode()
