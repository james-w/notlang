from . import bytecode
from .objectspace import W_Code


def get_stack_change(inst, arg):
    change = bytecode.STACK_CHANGE.get(inst, 1000)
    if change == 1000:
        raise AssertionError(bytecode.reverse_map[inst] + " not in STACK_CHANGE")
    if change == 999:
        change = -1*arg
    return change


class CompilerContext(object):
    def __init__(self):
        self.data = []
        self.constants = []
        self.names = []
        self.names_to_numbers = {}
        self.stacksize = 0
        self.max_stacksize = 0
        self.locals = []

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

    def is_local(self, name):
        return name in self.locals

    def emit(self, bc, arg=0):
        self.data.append(chr(bc))
        self.data.append(chr(arg))
        self.stacksize += get_stack_change(bc, arg)
        if self.stacksize > self.max_stacksize:
            self.max_stacksize = self.stacksize

    def create_bytecode(self):
        return W_Code("".join(self.data), self.constants[:], self.names, self.max_stacksize)

    def next_instruction_index(self):
        return len(self.data)

    def adjust_arg(self, index, new_arg):
        self.data[index+1] = chr(new_arg)

