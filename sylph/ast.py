from . import bytecode, compilercontext
from .objectspace import TheNone


class Node(object):
    """ The abstract AST node
    """

    def __init__(self):
        pass

    def __eq__(self, other):
        return (self.__class__ == other.__class__ and
                self.__dict__ == other.__dict__)

    def __ne__(self, other):
        return not self == other

    def get_extra_dot_info(self):
        return ""


class NonTerminal(Node):

    def __init__(self):
        super(NonTerminal, self).__init__()
        self.children = []

    def compile(self, ctx):
        for stmt in self.children:
            stmt.compile(ctx)


class Block(NonTerminal):
    """ A list of statements
    """
    def __init__(self, stmts):
        super(Block, self).__init__()
        self.children = stmts


class Stmt(NonTerminal):
    """ A single statement
    """
    def __init__(self, expr):
        super(Stmt, self).__init__()
        self.children = [expr]


class ConstantInt(Node):
    """ Represent a constant
    """
    def __init__(self, intval):
        super(ConstantInt, self).__init__()
        self.intval = intval

    def compile(self, ctx):
        from .interpreter import W_Int
        w = W_Int(self.intval)
        ctx.emit(bytecode.LOAD_CONSTANT, ctx.register_constant(w))

    def get_extra_dot_info(self):
        return str(self.intval)


class BinOp(NonTerminal):
    """ A binary operation
    """
    def __init__(self, op, left, right):
        super(BinOp, self).__init__()
        self.op = op
        self.children = [left, right]

    def compile(self, ctx):
        self.children[0].compile(ctx)
        self.children[1].compile(ctx)
        ctx.emit(bytecode.BINOP[self.op])

    def get_extra_dot_info(self):
        return str(self.op)


class Variable(Node):
    """ Variable reference
    """
    def __init__(self, varname):
        super(Variable, self).__init__()
        self.varname = varname

    def compile(self, ctx):
        ctx.emit(bytecode.LOAD_VAR, ctx.register_var(self.varname))

    def get_extra_dot_info(self):
        return str(self.varname)


class Assignment(NonTerminal):
    """ Assign to a variable
    """
    def __init__(self, var, expr):
        super(Assignment, self).__init__()
        self.var = var
        self.children = [expr]

    def compile(self, ctx):
        self.children[0].compile(ctx)
        ctx.emit(bytecode.ASSIGN, ctx.register_var(self.var.varname))

    def get_extra_dot_info(self):
        return self.var.varname


class Function(NonTerminal):
    """Call a function"""

    def __init__(self, fname, args):
        super(Function, self).__init__()
        self.fname = fname
        self.children = args

    def compile(self, ctx):
        # XXX: print with multiple args?
        if self.fname == 'print':
            self.children[0].compile(ctx)
            ctx.emit(bytecode.PRINT)
        else:
            ctx.emit(bytecode.LOAD_VAR, ctx.register_var(self.fname))
            for child in reversed(self.children):
                child.compile(ctx)
            ctx.emit(bytecode.CALL_FUNCTION, 2)

    def get_extra_dot_info(self):
        return self.fname


class Conditional(NonTerminal):

    def __init__(self, condition, true_block):
        super(Conditional, self).__init__()
        self.children = [condition, true_block]

    def compile(self, ctx):
        self.children[0].compile(ctx)
        jump_instr = ctx.next_instruction_index()
        ctx.emit(bytecode.JUMP_IF_FALSE)
        self.children[1].compile(ctx)
        ctx.adjust_arg(jump_instr, ctx.next_instruction_index()-jump_instr)


class While(NonTerminal):

    def __init__(self, condition, block):
        super(While, self).__init__()
        self.children = [condition, block]

    def compile(self, ctx):
        start_instr = ctx.next_instruction_index()
        self.children[0].compile(ctx)
        jump_instr = ctx.next_instruction_index()
        ctx.emit(bytecode.JUMP_IF_FALSE)
        self.children[1].compile(ctx)
        ctx.emit(bytecode.JUMP_BACK, ctx.next_instruction_index()-start_instr)
        ctx.adjust_arg(jump_instr, ctx.next_instruction_index()-jump_instr)


class FuncDef(NonTerminal):

    def __init__(self, name, args, code, rtype=None, argtypes=None):
        super(FuncDef, self).__init__()
        self.name = name
        self.args = args
        self.children = [code]
        self.rtype = rtype
        if argtypes is None:
            argtypes = [None]
        self.argtypes = argtypes

    def get_extra_dot_info(self):
        rtype = self.rtype or "ANY"
        return self.name + " ( -> " + rtype + ")"

    def compile(self, ctx):
        closure_context = compilercontext.CompilerContext()
        map(closure_context.register_var, self.args)
        self.children[0].compile(closure_context)
        closure_context.emit(bytecode.LOAD_CONSTANT, closure_context.register_constant(TheNone))
        closure_context.emit(bytecode.RETURN)
        code = closure_context.create_bytecode()
        # TODO: record number of args on the code object
        code_const = ctx.register_constant(code)
        ctx.emit(bytecode.LOAD_CONSTANT, code_const)
        ctx.emit(bytecode.MAKE_FUNCTION)
        ctx.emit(bytecode.ASSIGN, ctx.register_var(self.name))


class Return(NonTerminal):

    def __init__(self, arg):
        super(Return, self).__init__()
        self.children = []
        if arg:
            self.children = [arg]

    def compile(self, ctx):
        if self.children:
            self.children[0].compile(ctx)
        else:
            ctx.emit(bytecode.LOAD_CONSTANT, ctx.register_constant(TheNone))
        ctx.emit(bytecode.RETURN)


class VisitError(Exception):
    def __init__(self, node):
        self.node = node
        self.args = (node, )

    def __str__(self):
        return "could not visit %s" % (self.node, )


def make_dispatch_function(__general_nonterminal_visit=None,
                           __general_terminal_visit=None,
                           __general_visit=None,
                           **dispatch_table):
    def dispatch(self, node):
        name = node.__class__.__name__
        if isinstance(node, NonTerminal):
            func = dispatch_table.get(name, None)
            if func is None:
                if __general_nonterminal_visit:
                    return __general_nonterminal_visit(self, node)
            else:
                return func(self, node)
        else:
            func = dispatch_table.get(name, None)
            if func is None:
                if __general_terminal_visit:
                    return __general_terminal_visit(self, node)
            else:
                return func(self, node)
        if __general_visit:
            return __general_visit(self, node)
        raise VisitError(node)
    return dispatch


class CreateDispatchDictionaryMetaclass(type):
    def __new__(cls, name_, bases, dct):
        dispatch_table = {}
        for name, value in dct.iteritems():
            if name.startswith("visit_"):
                dispatch_table[name[len("visit_"):]] = value
        for special in ["general_terminal_visit",
                        "general_nonterminal_visit",
                        "general_visit"]:
            if special in dct:
                dispatch_table["__" + special] = dct[special]
        dct["dispatch"] = make_dispatch_function(**dispatch_table)
        return type.__new__(cls, name_, bases, dct)


class ASTVisitor(object):
    __metaclass__ = CreateDispatchDictionaryMetaclass



