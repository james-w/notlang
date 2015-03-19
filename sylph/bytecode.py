from .objectspace import W_Code, TheNone


reverse_map = {}

def register_code(bytecode, i):
    assert i not in globals()['reverse_map'], "Opcode %s already used" % i
    globals()[bytecode] = i
    globals()['reverse_map'][i] = bytecode


register_code('LOAD_CONSTANT', 1)
register_code('LOAD_VAR', 2)
register_code('LOAD_GLOBAL', 3)
register_code('ASSIGN', 4)
register_code('BINARY_ADD', 5)
register_code('BINARY_SUB', 6)
register_code('BINARY_MULT', 7)
register_code('BINARY_EQ', 8)
register_code('BINARY_LT', 9)
register_code('JUMP_IF_FALSE', 10)
register_code('CALL_FUNCTION', 11)
register_code('MAKE_FUNCTION', 12)
register_code('RETURN', 13)
register_code('PRINT', 14)

BINOP = {'+': BINARY_ADD, '-': BINARY_SUB, '*': BINARY_MULT, '==': BINARY_EQ, '<': BINARY_LT}
unary_ops = [BINARY_ADD, BINARY_SUB, BINARY_MULT, BINARY_EQ, BINARY_LT, RETURN, PRINT]


class CompilerContext(object):
    def __init__(self):
        self.data = []
        self.constants = []
        self.names = []
        self.names_to_numbers = {}

    def register_constant(self, v):
        self.constants.append(v)
        return len(self.constants) - 1

    def register_var(self, name):
        try:
            return self.names_to_numbers[name]
        except KeyError:
            self.names_to_numbers[name] = len(self.names)
            self.names.append(name)
            return len(self.names) - 1

    def emit(self, bc, arg=0):
        self.data.append(chr(bc))
        self.data.append(chr(arg))

    def create_bytecode(self):
        return W_Code("".join(self.data), self.constants[:], self.names)

    def next_instruction_index(self):
        return len(self.data)

    def adjust_arg(self, index, new_arg):
        self.data[index+1] = chr(new_arg)


def get_compiler(astnode):
    c = CompilerContext()
    astnode.compile(c)
    c.emit(LOAD_CONSTANT, c.register_constant(TheNone))
    c.emit(RETURN)
    return c

def compile_ast(astnode):
    c = get_compiler(astnode)
    return c.create_bytecode()
