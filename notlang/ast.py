

class Node(object):
    """ The abstract AST node
    """

    def __eq__(self, other):
        return (self.__class__ == other.__class__ and
                self.__dict__ == other.__dict__)

    def __ne__(self, other):
        return not self == other

    def get_extra_dot_info(self):
        return ""


class NonTerminal(Node):

    children = []


class Block(NonTerminal):
    """ A list of statements
    """
    def __init__(self, stmts, sourcepos):
        self.children = stmts
        self.sourcepos = sourcepos

    @property
    def statements(self):
        return self.children


class Stmt(NonTerminal):
    """ A single statement
    """
    def __init__(self, expr, sourcepos):
        self.children = [expr]
        self.sourcepos = sourcepos

    @property
    def expr(self):
        return self.children[0]


class Pass(Node):
    """A no-op.
    """
    def __init__(self, sourcepos):
        self.sourcepos = sourcepos


class ConstantInt(Node):
    """ Represent a constant
    """
    def __init__(self, intval, sourcepos):
        self.intval = intval
        self.sourcepos = sourcepos

    def get_extra_dot_info(self):
        return str(self.intval)


class Variable(Node):
    """ Variable reference
    """
    def __init__(self, varname, sourcepos):
        self.varname = varname
        self.sourcepos = sourcepos

    def get_extra_dot_info(self):
        return str(self.varname)


class Assignment(NonTerminal):
    """ Assign to a variable
    """
    def __init__(self, var, expr, sourcepos):
        self.var = var
        self.children = [expr]
        self.sourcepos = sourcepos

    def get_extra_dot_info(self):
        return self.var.varname

    @property
    def source(self):
        return self.children[0]

    @property
    def target(self):
        return self.var


class Function(NonTerminal):
    """Call a function"""

    def __init__(self, fname, args, sourcepos, type_params=None):
        self.children = [fname] + args
        self.sourcepos = sourcepos
        if type_params is None:
            type_params = []
        self.type_params = type_params

    def get_extra_dot_info(self):
        return self.fname

    @property
    def fname(self):
        return self.children[0]

    @property
    def args(self):
        return self.children[1:]


class BinOp(Function):
    """ A binary operation
    """
    def __init__(self, op, left, right, sourcepos):
        self.op = op
        self.children = [left, right]
        self.sourcepos = sourcepos
        self.type_params = []

    def get_extra_dot_info(self):
        return str(self.op)

    @property
    def fname(self):
        return self.op

    @property
    def args(self):
        return self.children


class Conditional(NonTerminal):

    def __init__(self, condition, true_block, false_block, sourcepos):
        self.children = [condition, true_block, false_block]
        self.sourcepos = sourcepos

    @property
    def condition(self):
        return self.children[0]

    @property
    def true_block(self):
        return self.children[1]

    @property
    def false_block(self):
        return self.children[2]


class While(NonTerminal):

    def __init__(self, condition, block, sourcepos):
        self.children = [condition, block]
        self.sourcepos = sourcepos

    @property
    def condition(self):
        return self.children[0]

    @property
    def block(self):
        return self.children[1]


class FuncDef(NonTerminal):

    def __init__(self, name, args, code, sourcepos, rtype=None, argtypes=None, type_params=None):
        self.name = name
        self.args = args
        self.children = [code]
        self.rtype = rtype
        if argtypes is None:
            argtypes = [None] * len(self.args)
        self.argtypes = argtypes
        if type_params is None:
            type_params = []
        self.type_params = type_params
        self.sourcepos = sourcepos

    def get_extra_dot_info(self):
        rtype = self.rtype or "ANY"
        return self.name + " ( " + ", ".join(self.args) + " -> " + rtype + ")"

    @property
    def code(self):
        return self.children[0]


class Return(NonTerminal):

    def __init__(self, arg, sourcepos):
        self.children = []
        if arg:
            self.children = [arg]
        self.sourcepos = sourcepos

    @property
    def arg(self):
        if self.children:
            return self.children[0]
        return None


class NewType(NonTerminal):

    def __init__(self, block, type_type, sourcepos, type_params=None, options=None):
        self.children = [block]
        self.type_type = type_type
        self.sourcepos = sourcepos
        if type_params is None:
            type_params = []
        self.type_params = type_params
        if options is None:
            options = []
        self.options = options

    @property
    def block(self):
        return self.children[0]


class Case(NonTerminal):

    def __init__(self, target, cases, sourcepos):
        self.children = [target] + cases
        self.sourcepos = sourcepos

    @property
    def target(self):
        return self.children[0]

    @property
    def cases(self):
        return self.children[1:]


class CaseCase(NonTerminal):

    def __init__(self, label, block, sourcepos):
        self.children = [label, block]
        self.sourcepos = sourcepos

    @property
    def label(self):
        return self.children[0]

    @property
    def block(self):
        return self.children[1]


class Attribute(NonTerminal):

    def __init__(self, target, name, sourcepos):
        self.children = [target]
        self.name = name
        self.sourcepos = sourcepos

    def get_extra_dot_info(self):
        return self.name

    @property
    def target(self):
        return self.children[0]


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


class GatherNames(ASTVisitor):

    def visit_Variable(self, node):
        return [node.varname]

    def visit_FuncDef(self, node):
        return [node.name]

    def general_terminal_visit(self, node):
        return []

    def general_nonterminal_visit(self, node):
        names = []
        for child in node.children:
            cnames = self.dispatch(child)
            for name in cnames:
                if name not in names:
                    names.append(name)
        return names


class GatherAssignedNames(ASTVisitor):

    def visit_Variable(self, node):
        return []

    def visit_Assignment(self, node):
        return [node.var.varname]

    def visit_FuncDef(self, node):
        return [node.name]

    def general_terminal_visit(self, node):
        return []

    def general_nonterminal_visit(self, node):
        names = []
        for child in node.children:
            cnames = self.dispatch(child)
            for name in cnames:
                if name not in names:
                    names.append(name)
        return names
