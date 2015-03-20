from . import bytecode, compilercontext
from .objectspace import TheNone


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
    #Typer().dispatch(astnode)
    c = compilercontext.CompilerContext()
    astnode.compile(c)
    c.emit(bytecode.LOAD_CONSTANT, c.register_constant(TheNone))
    c.emit(bytecode.RETURN)
    return c

def compile_ast(astnode):
    c = get_compiler(astnode)
    return c.create_bytecode()
