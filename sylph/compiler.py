from . import ast, bytecode, codegen, compilercontext, typer
from .objectspace import TheNone


class Compiler(ast.ASTVisitor):

    def __init__(self, ctx):
        self.ctx = ctx

    def general_nonterminal_visit(self, node):
        [self.dispatch(c) for c in node.children]

    def visit_Variable(self, node):
        codegen.load_var(self.ctx, node.varname)

    def visit_ConstantInt(self, node):
        codegen.load_constant_int(self.ctx, node.intval)

    def visit_BinOp(self, node):
        self.general_nonterminal_visit(node)
        codegen.binary_operation(self.ctx, node.op)

    def visit_Assignment(self, node):
        self.general_nonterminal_visit(node)
        codegen.assignment(self.ctx, node.var.varname)

    def visit_Function(self, node):
        if node.fname == 'print':
            self.dispatch(node.children[0])
            codegen.do_print(self.ctx)
        else:
            codegen.function_call(self.ctx, node.fname, len(node.children), CallbackHelper(node).function_args_cb)

    def visit_Conditional(self, node):
        self.dispatch(node.children[0])
        codegen.conditional(self.ctx, CallbackHelper(node).conditional_true_block_cb)

    def visit_While(self, node):
        helper = CallbackHelper(node)
        codegen.while_loop(self.ctx, helper.while_condition_cb, helper.while_block_cb)

    def visit_FuncDef(self, node):
        codegen.make_function(self.ctx, node.name, CallbackHelper(node).funcdef_code_cb)

    def visit_Return(self, node):
        if node.children:
            self.dispatch(node.children[0])
        else:
            codegen.load_none(self.ctx)
        codegen.do_return(self.ctx)


class CallbackHelper(object):

    def __init__(self, node):
        self.node = node

    def funcdef_code_cb(self, ctx):
        [ctx.register_var(a) for a in self.node.args]
        Compiler(ctx).dispatch(self.node.children[0])

    def function_args_cb(self, ctx):
        Compiler(ctx).general_nonterminal_visit(self.node)

    def conditional_true_block_cb(self, ctx):
        Compiler(ctx).dispatch(self.node.children[1])

    def while_condition_cb(self, ctx):
        Compiler(ctx).dispatch(self.node.children[0])

    def while_block_cb(self, ctx):
        Compiler(ctx).dispatch(self.node.children[1])


def dump(code, context=None):
    lines = []
    i = 0
    stacksize = 0
    for i in range(0, len(code.bytecode), 2):
        c = ord(code.bytecode[i])
        c2 = ord(code.bytecode[i + 1])
        stacksize += compilercontext.get_stack_change(c, c2)
        line = "%d " % i
        line += bytecode.reverse_map[c]
        if c not in bytecode.unary_ops:
            line += " " + str(c2)
        if context is not None:
            if c in (bytecode.LOAD_VAR, bytecode.ASSIGN, bytecode.LOAD_GLOBAL):
                line += " (" + str(context.names[c2]) + ")"
            if c in (bytecode.LOAD_CONSTANT,):
                line += " (" + context.constants[c2].repr() + ")"
        line += " (stacksize: %d)" % stacksize
        lines.append(line)
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


def get_compiler(astnode):
    typer.typecheck(astnode)
    c = compilercontext.CompilerContext()
    Compiler(c).dispatch(astnode)
    c.emit(bytecode.LOAD_CONSTANT, c.register_constant(TheNone))
    c.emit(bytecode.RETURN)
    return c

def compile_ast(astnode):
    c = get_compiler(astnode)
    return c.create_bytecode()
