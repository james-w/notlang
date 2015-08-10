from . import ast, bytecode, codegen, compilercontext, objectspace, typer


class Compiler(ast.ASTVisitor):

    def __init__(self, ctx):
        self.ctx = ctx

    def general_nonterminal_visit(self, node):
        [self.dispatch(c) for c in node.children]

    def visit_Attribute(self, node):
        self.dispatch(node.children[0])
        codegen.load_attr(self.ctx, node.name)

    def visit_Variable(self, node):
        codegen.load_var(self.ctx, node.varname)

    def visit_ConstantInt(self, node):
        codegen.load_constant_int(self.ctx, node.intval)

    def visit_BinOp(self, node):
        self.general_nonterminal_visit(node)
        codegen.binary_operation(self.ctx, node.op)

    def visit_Assignment(self, node):
        if isinstance(node.children[0], ast.NewType):
            codegen.new_type(self.ctx, node.var.varname, CallbackHelper(node).type_code_cb)
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
        self.dispatch(node.children[0])
        helper = CallbackHelper(node)
        codegen.conditional(self.ctx, helper.conditional_true_block_cb, helper.conditional_false_block_cb)

    def visit_While(self, node):
        helper = CallbackHelper(node)
        codegen.while_loop(self.ctx, helper.while_condition_cb, helper.while_block_cb)

    def visit_FuncDef(self, node):
        codegen.make_function(self.ctx, node.name, CallbackHelper(node).funcdef_code_cb, node.args)

    def visit_Return(self, node):
        if node.children:
            self.dispatch(node.children[0])
        else:
            codegen.load_none(self.ctx)
        codegen.do_return(self.ctx)

    def visit_Pass(self, node):
        # How appropriate
        pass


class CallbackHelper(object):

    def __init__(self, node):
        self.node = node

    def funcdef_code_cb(self, ctx):
        for local in ast.GatherAssignedNames().dispatch(self.node.children[0]):
            if local not in ctx.locals:
                ctx.locals.append(local)
        Compiler(ctx).dispatch(self.node.children[0])

    def function_args_cb(self, ctx):
        [Compiler(ctx).dispatch(c) for c in reversed(self.node.args)]

    def conditional_true_block_cb(self, ctx):
        Compiler(ctx).dispatch(self.node.children[1])

    def conditional_false_block_cb(self, ctx):
        false_block = self.node.children[2]
        if false_block is not None:
            Compiler(ctx).dispatch(false_block)

    def while_condition_cb(self, ctx):
        Compiler(ctx).dispatch(self.node.children[0])

    def while_block_cb(self, ctx):
        Compiler(ctx).dispatch(self.node.children[1])

    def type_code_cb(self, ctx):
        Compiler(ctx).dispatch(self.node.children[0])


def dump_instr(index, inst, arg, context=None):
    line = "%d " % index
    line += bytecode.reverse_map[inst]
    if inst not in bytecode.unary_ops:
        line += " " + str(arg)
    if context is not None:
        if inst in (bytecode.LOAD_VAR, bytecode.ASSIGN, bytecode.LOAD_GLOBAL, bytecode.LOAD_ATTR):
            line += " (" + str(context.names[arg]) + ")"
        if inst in (bytecode.LOAD_CONSTANT,):
            line += " (" + context.constants[arg].repr() + ")"
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
