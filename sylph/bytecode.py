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
register_code('BINARY_GT', 10)
register_code('JUMP_IF_FALSE', 11)
register_code('JUMP_BACK', 12)
register_code('CALL_FUNCTION', 13)
register_code('MAKE_FUNCTION', 14)
register_code('RETURN', 15)
register_code('PRINT', 16)

BINOP = {'+': BINARY_ADD, '-': BINARY_SUB, '*': BINARY_MULT, '==': BINARY_EQ, '<': BINARY_LT, '>': BINARY_GT}
unary_ops = [BINARY_ADD, BINARY_SUB, BINARY_MULT, BINARY_EQ, BINARY_LT, BINARY_GT, RETURN, PRINT]
