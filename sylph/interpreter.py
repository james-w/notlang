import sys

from rpython.rlib import jit
from rpython.rlib.debug import make_sure_not_resized

from . import bytecode, compiler
from .objectspace import W_Code, W_Dict, W_Func, W_Type
from .parsing import parse


def printable_loc(pc, code):
    name = bytecode.reverse_map[ord(code[pc])]
    return str(pc) + " " + name


driver = jit.JitDriver(greens = ['pc', 'code'],
                       reds = ['frame'],
                       virtualizables=['frame'],
                       get_printable_location=printable_loc)


def make_type(name, attrs):
    return type(name, (W_Type,), attrs.dictval)


class Space(object):

    def call_function(self, code, args, globals, trace=False):
        child_f = Frame(self, code, globals)
        for i, arg in enumerate(args):
            child_f.vars[i] = arg
        return child_f.execute(trace=trace)


class Frame(object):
    _virtualizable_ = ['valuestack[*]', 'valuestack_pos', 'vars[*]', 'names[*]']
    
    def __init__(self, space, prog, globals):
        self = jit.hint(self, fresh_virtualizable=True, access_directly=True)
        self.valuestack = [None] * prog.max_stacksize
        make_sure_not_resized(self.valuestack)
        self.vars = [None] * len(prog.names)
        self.names = prog.names[:]
        make_sure_not_resized(self.names)
        self.valuestack_pos = 0
        self.globals = globals
        self.code = prog.bytecode
        self.constants = prog.constants
        self.space = space

    def push(self, v):
        pos = jit.hint(self.valuestack_pos, promote=True)
        assert pos >= 0
        self.valuestack[pos] = v
        self.valuestack_pos = pos + 1

    def popmany(self, count):
        v = []
        for i in range(count):
            pos = jit.hint(self.valuestack_pos, promote=True)
            new_pos = pos - 1
            assert new_pos >= 0
            v.append(self.valuestack[new_pos])
            self.valuestack_pos = new_pos
        return v
    
    def pop(self):
        return self.popmany(1)[0]

    def execute(self, trace=False):
        code = self.code
        pc = 0
        while True:
            # required hint indicating this is the top of the opcode dispatch
            driver.jit_merge_point(pc=pc, code=code, frame=self)
            c = ord(code[pc])
            arg = ord(code[pc + 1])
            pc += 2
            if trace:
                sys.stderr.write("instr: ")
                sys.stderr.write(compiler.dump_instr(pc-2, c, arg, context=self))
                sys.stderr.write(" " + repr([a for a in reversed(self.valuestack[:self.valuestack_pos])]))
                sys.stderr.write("\n")
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
            elif c == bytecode.BINARY_SUB:
                right = self.pop()
                left = self.pop()
                w_res = left.sub(right)
                self.push(w_res)
            elif c == bytecode.BINARY_MULT:
                right = self.pop()
                left = self.pop()
                w_res = left.multiply(right)
                self.push(w_res)
            elif c == bytecode.BINARY_LT:
                right = self.pop()
                left = self.pop()
                self.push(left.lt(right))
            elif c == bytecode.BINARY_GT:
                right = self.pop()
                left = self.pop()
                self.push(left.gt(right))
            elif c == bytecode.BINARY_EQ:
                right = self.pop()
                left = self.pop()
                self.push(left.eq(right))
            elif c == bytecode.ASSIGN:
                self.vars[arg] = self.pop()
            elif c == bytecode.LOAD_VAR:
                var = self.vars[arg]
                if var is None:
                    raise AssertionError("Variable referenced before assignment")
                self.push(var)
            elif c == bytecode.LOAD_GLOBAL:
                gname = self.names[arg]
                glob = self.globals.get(gname, None)
                if glob is None:
                    raise SyntaxError("Unknown variable: %s" % gname)
                else:
                    self.push(glob)
            elif c == bytecode.LOAD_ATTR:
                attrname = self.names[arg]
                obj = self.pop()
                attr = getattr(obj, attrname, None)
                if attr is None:
                    raise AssertionError("%s has no attribute %s" % (obj, attrname))
                self.push(attr)
            elif c == bytecode.LOAD_LOCALS:
                dictval = {}
                for i, val in enumerate(self.vars):
                    if val is not None:
                        dictval[self.names[i]] = val
                self.push(W_Dict(dictval))
            elif c == bytecode.CALL_FUNCTION:
                fargs = self.popmany(arg)
                fobj = self.pop()
                if getattr(fobj, 'call', None) is None:
                    raise AssertionError("%s is not callable" % fobj)
                globals = self.globals
                if globals is None:
                    globals = {}
                    for i, name in enumerate(self.names):
                        globals[name] = self.vars[i]
                self.push(fobj.call(self.space, fargs, globals, trace=trace))
            elif c == bytecode.MAKE_FUNCTION:
                code_obj = self.pop()
                if not isinstance(code_obj, W_Code):
                    raise AssertionError("Is not a code object %s" % code_obj)
                self.push(W_Func(code_obj))
            elif c == bytecode.MAKE_TYPE:
                attrs = self.pop()
                name = self.pop().strval
                self.push(make_type(name, attrs))
            elif c == bytecode.PRINT:
                print self.pop().str()
            elif c == bytecode.JUMP_IF_FALSE:
                if not self.pop().is_true():
                    pc += arg
            elif c == bytecode.JUMP_BACK:
                pc -= arg+2
                # required hint indicating this is the end of a loop
                driver.can_enter_jit(pc=pc, code=code, frame=self)
            elif c == bytecode.JUMP_FORWARD:
                pc += arg
            else:
                assert False, "Unknown opcode: %d" % c


def get_bytecode(source):
    return compiler.compile_ast(parse(source))


def interpret(source, trace=False):
    prog = get_bytecode(source)
    space = Space()
    return space.call_function(prog, [], None, trace=trace)
