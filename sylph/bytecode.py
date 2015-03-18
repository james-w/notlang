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
register_code('BINARY_EQ', 7)
register_code('BINARY_LT', 8)
register_code('JUMP_IF_FALSE', 9)
register_code('FUNCTION', 10)
register_code('RETURN', 11)
register_code('PRINT', 12)

BINOP = {'+': BINARY_ADD, '-': BINARY_SUB, '==': BINARY_EQ, '<': BINARY_LT}
unary_ops = [BINARY_ADD, BINARY_SUB, BINARY_EQ, BINARY_LT, RETURN, PRINT]


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
        return ByteCode("".join(self.data), self.constants[:], self.names)

    def next_instruction_index(self):
        return len(self.data)

    def adjust_arg(self, index, new_arg):
        self.data[index+1] = chr(new_arg)


class ByteCode(object):
    _immutable_fields_ = ['code', 'constants[*]', 'names']
    
    def __init__(self, code, constants, names):
        self.code = code
        self.constants = constants
        self.names = names

    def dump(self, context=None):
        lines = []
        i = 0
        for i in range(0, len(self.code), 2):
            c = ord(self.code[i])
            c2 = ord(self.code[i + 1])
            line = "%d " % i
            line += reverse_map[c]
            if c not in unary_ops:
                line += " " + str(c2)
            if context is not None:
                if c in (LOAD_VAR, ASSIGN, LOAD_GLOBAL):
                    line += " (" + str(context.names[c2]) + ")"
                if c in (LOAD_CONSTANT,):
                    line += " (" + context.constants[c2].str() + ")"
            lines.append(line)
        return '\n'.join(lines)


def get_compiler(astnode):
    c = CompilerContext()
    astnode.compile(c)
    c.emit(RETURN, 0)
    return c

def compile_ast(astnode):
    c = get_compiler(astnode)
    return c.create_bytecode()
