import logging

from rpython.rlib import jit
from rpython.rlib.debug import make_sure_not_resized

from . import bytecode, compiler
from .objectspace import W_Code, W_Dict, W_Func, W_List, W_Tuple, W_Type, W_Int, W_IsInstance, W_Enum, is_builtin
from .parsing import parse


logger = logging.getLogger(__name__)


def printable_loc(pc, code):
    name = bytecode.reverse_map[ord(code[pc])]
    return str(pc) + " " + name


driver = jit.JitDriver(greens = ['pc', 'code'],
                       reds = ['frame'],
                       virtualizables=['frame'],
                       get_printable_location=printable_loc)


def make_type(name, bases, attrs):
    return type(name, bases.val, attrs.dictval)


class Space(object):

    def call_function(self, code, args, globals, locals):
        child_f = Frame(self, code, globals, locals)
        for i, arg in enumerate(args):
            child_f.vars[i] = arg
        return child_f.execute()


class Panic(Exception):

    def __init__(self, msg):
        self.msg = msg


class Frame(object):
    _virtualizable_ = ['valuestack[*]', 'valuestack_pos', 'vars[*]', 'names[*]']

    def __init__(self, space, prog, globals, locals):
        self = jit.hint(self, fresh_virtualizable=True, access_directly=True)
        self.valuestack = [None] * prog.max_stacksize
        make_sure_not_resized(self.valuestack)
        self.vars = [None] * len(prog.names)
        self.names = prog.names[:]
        make_sure_not_resized(self.names)
        self.valuestack_pos = 0
        self.globals = globals
        self.locals = locals
        self.locals2fast()
        self.code = prog.bytecode
        self.constants = prog.constants
        self.space = space

    def locals2fast(self):
        for i, name in enumerate(self.names):
            if name in self.locals:
                self.vars[i] = self.locals[name]

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

    def execute(self):
        code = self.code
        pc = 0
        while True:
            # required hint indicating this is the top of the opcode dispatch
            driver.jit_merge_point(pc=pc, code=code, frame=self)
            c = ord(code[pc])
            higharg = ord(code[pc + 1])
            lowarg = ord(code[pc + 2])
            arg = (higharg << 8) + lowarg
            pc += 3
            logger.debug("instr: " + compiler.dump_instr(pc-3, c, arg, context=self) + " " + repr([a for a in reversed(self.valuestack[:self.valuestack_pos])]))
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
            elif c == bytecode.BINARY_IS:
                right = self.pop()
                left = self.pop()
                self.push(W_Int(int(left is right)))
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
            elif c == bytecode.SET_ATTR:
                attrname = self.names[arg]
                val = self.pop()
                obj = self.pop()
                setattr(obj, attrname, val)
            elif c == bytecode.CALL_FUNCTION:
                fargs = self.popmany(arg)
                fobj = self.pop()
                callable = getattr(fobj, 'call', None)
                if callable is None:
                    if getattr(fobj, '__call__', None) is None:
                        raise AssertionError("%s is not callable" % fobj)
                    else:
                        callable = fobj
                if is_builtin(callable):
                    self.push(callable(*fargs))
                else:
                    globals = self.globals.copy()
                    for i, name in enumerate(self.names):
                        if self.vars[i] is not None:
                            globals[name] = self.vars[i]
                    self.push(callable(self.space, fargs, globals))
            elif c == bytecode.MAKE_FUNCTION:
                code_obj = self.pop()
                if not isinstance(code_obj, W_Code):
                    raise AssertionError("Is not a code object %s" % code_obj)
                self.push(W_Func(code_obj))
            elif c == bytecode.MAKE_TYPE:
                attrs = self.pop()
                bases = self.pop()
                name = self.pop().strval
                self.push(make_type(name, bases, attrs))
            elif c == bytecode.PRINT:
                print self.pop().str()
            elif c == bytecode.JUMP_IF_FALSE:
                if not self.pop().is_true():
                    pc += arg
            elif c == bytecode.JUMP_BACK:
                pc -= arg+bytecode.INSTRUCTION_SIZE
                # required hint indicating this is the end of a loop
                driver.can_enter_jit(pc=pc, code=code, frame=self)
            elif c == bytecode.JUMP_FORWARD:
                pc += arg
            elif c == bytecode.DUP_TOP:
                val = self.pop()
                self.push(val)
                self.push(val)
            elif c == bytecode.ROT_TWO:
                val1 = self.pop()
                val2 = self.pop()
                self.push(val1)
                self.push(val2)
            elif c == bytecode.BUILD_TUPLE:
                fargs = self.popmany(arg)
                self.push(W_Tuple(tuple(fargs)))
            elif c == bytecode.PANIC:
                msg = self.pop()
                raise Panic(msg)
            else:
                assert False, "Unknown opcode: %d" % c


def get_bytecode(source):
    return compiler.compile_ast(parse(source))


def interpret(source):
    prog = get_bytecode(source)
    space = Space()
    globals = dict(List=W_List, Type=W_Type, Enum=W_Enum, Tuple=W_Tuple, isinstance=W_IsInstance)
    return space.call_function(prog, [], globals, globals)
