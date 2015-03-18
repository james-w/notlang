import sys

from rpython.rlib import jit

from . import bytecode
from .parsing import parse


def printable_loc(pc, code, prog):
    name = bytecode.reverse_map[ord(code[pc])]
    return str(pc) + " " + name


driver = jit.JitDriver(greens = ['pc', 'code', 'prog'],
                       reds = ['frame'],
                       virtualizables=['frame'],
                       get_printable_location=printable_loc)

class W_Root(object):
    pass


class W_Int(W_Root):
    def __init__(self, intval):
        assert(isinstance(intval, int))
        self.intval = intval

    def add(self, other):
        if not isinstance(other, W_Int):
            raise Exception("wrong type")
        return W_Int(self.intval + other.intval)

    def lt(self, other): 
        if not isinstance(other, W_Int):
            raise Exception("wrong type")
        return W_Int(self.intval < other.intval)

    def is_true(self):
        return self.intval != 0

    def eq(self, other):
        if not isinstance(other, W_Int):
            raise Exception("wrong type")
        return W_Int(self.intval == other.intval)

    def int(self):
        return self.intval

    def str(self):
        return str(self.intval)

    def __repr__(self):
        return "<%s.%s object at %s value:%d>" % (self.__class__.__module__,
                self.__class__.__name__, id(self), self.intval)


class W_Func(W_Root):

    def __init__(self, name, code):
        self.name = name
        self.code = code

    def str(self):
        return "<function %s>" % self.name

    def call(self, arg):
        return self.code(arg)


def double(arg):
    return W_Int(arg.int() * 2)


class Frame(object):
    _virtualizable_ = ['valuestack[*]', 'valuestack_pos', 'vars[*]', 'names[*]']
    
    def __init__(self, prog):
        self = jit.hint(self, fresh_virtualizable=True, access_directly=True)
        self.valuestack = [None] * 3 # safe estimate!
        self.vars = [None] * len(prog.names)
        self.names = prog.names
        self.valuestack_pos = 0
        self.globals = {
            'double': W_Func('double', double),
        }

    def push(self, v):
        pos = jit.hint(self.valuestack_pos, promote=True)
        assert pos >= 0
        self.valuestack[pos] = v
        self.valuestack_pos = pos + 1
    
    def pop(self):
        pos = jit.hint(self.valuestack_pos, promote=True)
        new_pos = pos - 1
        assert new_pos >= 0
        v = self.valuestack[new_pos]
        self.valuestack_pos = new_pos
        return v


def execute(frame, prog):
    code = prog.code
    pc = 0
    while True:
        # required hint indicating this is the top of the opcode dispatch
        driver.jit_merge_point(pc=pc, code=code, prog=prog, frame=frame)
        c = ord(code[pc])
        arg = ord(code[pc + 1])
        pc += 2
        if c == bytecode.LOAD_CONSTANT:
            w_constant = prog.constants[arg]
            frame.push(w_constant)
        elif c == bytecode.RETURN:
            return
        elif c == bytecode.BINARY_ADD:
            right = frame.pop()
            left = frame.pop()
            w_res = left.add(right)
            frame.push(w_res)
        elif c == bytecode.BINARY_LT:
            right = frame.pop()
            left = frame.pop()
            frame.push(left.lt(right))
        elif c == bytecode.BINARY_EQ:
            right = frame.pop()
            left = frame.pop()
            frame.push(left.eq(right))
        elif c == bytecode.ASSIGN:
            frame.vars[arg] = frame.pop()
        elif c == bytecode.LOAD_VAR:
            frame.push(frame.vars[arg])
        elif c == bytecode.LOAD_GLOBAL:
            fname = frame.names[arg]
            fobj = frame.globals.get(fname, None)
            if fobj is None:
                raise SyntaxError("Unknown function: %s" % fname)
            else:
                frame.push(fobj)
        elif c == bytecode.FUNCTION:
            farg = frame.pop()
            fobj = frame.pop()
            ret = fobj.call(farg)
            frame.push(ret)
        elif c == bytecode.PRINT:
            print frame.pop().str()
        elif c == bytecode.JUMP_IF_FALSE:
            if not frame.pop().is_true():
                pc += arg-2
        else:
            assert False, "Unknown opcode: %d" % c


def get_bytecode(source):
    return bytecode.compile_ast(parse(source))


def interpret(source):
    prog = get_bytecode(source)
    frame = Frame(prog)
    execute(frame, prog)
    return frame
