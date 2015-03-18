class W_Root(object):

    __slots__ = ()
    _settled_ = True

    def add(self, other):
        raise NotImplementedError

    def eq(self, other):
        raise NotImplementedError

    def str(self):
        raise NotImplementedError

    def lt(self, other):
        raise NotImplementedError

    def is_true(self):
        raise NotImplementedError


class W_Code(W_Root):
    _immutable_ = True
    _immutable_fields_ = ['bytecode', 'constants[*]', 'names']
   
    def __init__(self, code, constants, names):
        self.bytecode = code
        self.constants = constants
        self.names = names

    def dump(self, context=None):
        lines = []
        i = 0
        for i in range(0, len(self.bytecode), 2):
            c = ord(self.bytecode[i])
            c2 = ord(self.bytecode[i + 1])
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


class W_None(W_Root):
    pass


TheNone = W_None()
