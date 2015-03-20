

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
    def __init__(self, stmts):
        self.children = stmts


class Stmt(NonTerminal):
    """ A single statement
    """
    def __init__(self, expr):
        self.children = [expr]


class ConstantInt(Node):
    """ Represent a constant
    """
    def __init__(self, intval):
        self.intval = intval

    def get_extra_dot_info(self):
        return str(self.intval)


class BinOp(NonTerminal):
    """ A binary operation
    """
    def __init__(self, op, left, right):
        self.op = op
        self.children = [left, right]

    def get_extra_dot_info(self):
        return str(self.op)


class Variable(Node):
    """ Variable reference
    """
    def __init__(self, varname):
        self.varname = varname

    def get_extra_dot_info(self):
        return str(self.varname)


class Assignment(NonTerminal):
    """ Assign to a variable
    """
    def __init__(self, var, expr):
        self.var = var
        self.children = [expr]

    def get_extra_dot_info(self):
        return self.var.varname


class Function(NonTerminal):
    """Call a function"""

    def __init__(self, fname, args):
        self.fname = fname
        self.children = args

    def get_extra_dot_info(self):
        return self.fname


class Conditional(NonTerminal):

    def __init__(self, condition, true_block):
        self.children = [condition, true_block]


class While(NonTerminal):

    def __init__(self, condition, block):
        self.children = [condition, block]


class FuncDef(NonTerminal):

    def __init__(self, name, args, code, rtype=None, argtypes=None):
        self.name = name
        self.args = args
        self.children = [code]
        self.rtype = rtype
        if argtypes is None:
            argtypes = [None]
        self.argtypes = argtypes

    def get_extra_dot_info(self):
        rtype = self.rtype or "ANY"
        return self.name + " ( " + ", ".join(self.args) + " -> " + rtype + ")"


class Return(NonTerminal):

    def __init__(self, arg):
        self.children = []
        if arg:
            self.children = [arg]


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

