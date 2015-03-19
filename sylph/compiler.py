from . import bytecode, compilercontext
from .objectspace import TheNone
from .typer import Typer


def dump(code, context=None):
    lines = []
    i = 0
    for i in range(0, len(code.bytecode), 2):
        c = ord(code.bytecode[i])
        c2 = ord(code.bytecode[i + 1])
        line = "%d " % i
        line += reverse_map[c]
        if c not in unary_ops:
            line += " " + str(c2)
        if context is not None:
            if c in (bytecode.LOAD_VAR, bytecode.ASSIGN, bytecode.LOAD_GLOBAL):
                line += " (" + str(context.names[c2]) + ")"
            if c in (bytecode.LOAD_CONSTANT,):
                line += " (" + context.constants[c2].repr() + ")"
        lines.append(line)
    return '\n'.join(lines)


def get_compiler(astnode):
    Typer().dispatch(astnode)
    c = compilercontext.CompilerContext()
    astnode.compile(c)
    c.emit(bytecode.LOAD_CONSTANT, c.register_constant(TheNone))
    c.emit(bytecode.RETURN)
    return c

def compile_ast(astnode):
    c = get_compiler(astnode)
    return c.create_bytecode()
