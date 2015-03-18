from rpython.rlib import jit

from . import bytecode
from .objectspace import W_Root, W_Code
from .parsing import parse


def printable_loc(pc, code):
    name = bytecode.reverse_map[ord(code[pc])]
    return str(pc) + " " + name


driver = jit.JitDriver(greens = ['pc', 'code'],
                       reds = ['self'],
                       virtualizables=['self'],
                       get_printable_location=printable_loc)

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

    __slots__ = ['code']
    _immutable_fields_ = ['code']

    def __init__(self, code):
        self.code = code


class Frame(object):
    _virtualizable_ = ['valuestack[*]', 'valuestack_pos', 'vars[*]', 'names[*]']
    
    def __init__(self, prog):
        self = jit.hint(self, fresh_virtualizable=True, access_directly=True)
        self.valuestack = [None] * 3 # safe estimate!
        self.vars = [None] * len(prog.names)
        self.names = prog.names
        self.valuestack_pos = 0
        self.globals = {
        }
        self.code = prog.bytecode
        self.constants = prog.constants

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

    def execute(self):
        code = self.code
        pc = 0
        while True:
            # required hint indicating this is the top of the opcode dispatch
            driver.jit_merge_point(pc=pc, code=code, self=self)
            c = ord(code[pc])
            arg = ord(code[pc + 1])
            pc += 2
            if c == bytecode.LOAD_CONSTANT:
                w_constant = self.constants[arg]
                self.push(w_constant)
            elif c == bytecode.RETURN:
                return self.pop()
            elif c == bytecode.BINARY_ADD:
                right = self.pop()
                left = self.pop()
                w_res = left.add(right)
                self.push(w_res)
            elif c == bytecode.BINARY_LT:
                right = self.pop()
                left = self.pop()
                self.push(left.lt(right))
            elif c == bytecode.BINARY_EQ:
                right = self.pop()
                left = self.pop()
                self.push(left.eq(right))
            elif c == bytecode.ASSIGN:
                self.vars[arg] = self.pop()
            elif c == bytecode.LOAD_VAR:
                self.push(self.vars[arg])
            elif c == bytecode.LOAD_GLOBAL:
                fname = self.names[arg]
                fobj = self.globals.get(fname, None)
                if fobj is None:
                    raise SyntaxError("Unknown function: %s" % fname)
                else:
                    self.push(fobj)
            elif c == bytecode.CALL_FUNCTION:
                farg = self.pop()
                fobj = self.pop()
                if not isinstance(fobj, W_Func):
                    raise AssertionError
                child_f = Frame(fobj.code)
                child_f.vars[0] = farg
                ret = child_f.execute()
                self.push(ret)
            elif c == bytecode.MAKE_FUNCTION:
                code_obj = self.pop()
                if not isinstance(code_obj, W_Code):
                    raise AssertionError
                self.push(W_Func(code_obj))
            elif c == bytecode.PRINT:
                print self.pop().str()
            elif c == bytecode.JUMP_IF_FALSE:
                if not self.pop().is_true():
                    pc += arg-2
            else:
                assert False, "Unknown opcode: %d" % c


def get_bytecode(source):
    return bytecode.compile_ast(parse(source))


def interpret(source):
    prog = get_bytecode(source)
    frame = Frame(prog)
    frame.execute()
    return frame
