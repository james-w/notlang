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
register_code('JUMP_FORWARD', 12)
register_code('JUMP_BACK', 13)
register_code('CALL_FUNCTION', 14)
register_code('MAKE_FUNCTION', 15)
register_code('RETURN', 16)
register_code('PRINT', 17)

BINOP = {'+': BINARY_ADD, '-': BINARY_SUB, '*': BINARY_MULT, '==': BINARY_EQ, '<': BINARY_LT, '>': BINARY_GT}
unary_ops = [BINARY_ADD, BINARY_SUB, BINARY_MULT, BINARY_EQ, BINARY_LT, BINARY_GT, RETURN, PRINT]
STACK_CHANGE = {
    LOAD_CONSTANT: 1,
    LOAD_VAR: 1,
    LOAD_GLOBAL: 1,
    ASSIGN: -1,
    BINARY_ADD: -1,
    BINARY_SUB: -1,
    BINARY_MULT: -1,
    BINARY_EQ: -1,
    BINARY_LT: -1,
    BINARY_GT: -1,
    JUMP_IF_FALSE: -1,
    JUMP_FORWARD: 0,
    JUMP_BACK: 0,
    CALL_FUNCTION: 999, # sentinel meaning -arg
    MAKE_FUNCTION: 0,
    RETURN: -1,
    PRINT: -1,
}
