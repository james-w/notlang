

class Node(object):
    """ The abstract AST node
    """

    def __eq__(self, other):
        return self.__class__ == other.__class__

    def __ne__(self, other):
        return not self == other

    def get_extra_dot_info(self):
        return ""


class NonTerminal(Node):

    children = []

    def __eq__(self, other):
        return super(NonTerminal, self).__eq__(other) and self.children == other.children


class Block(NonTerminal):
    """ A list of statements
    """
    def __init__(self, stmts, sourcepos):
        self.children = stmts
        self.sourcepos = sourcepos

    @property
    def statements(self):
        return self.children

    def __repr__(self):
        return u"<Block: {} at 0x{}>".format(repr(self.children), id(self))


class Stmt(NonTerminal):
    """ A single statement
    """
    def __init__(self, expr, sourcepos):
        self.children = [expr]
        self.sourcepos = sourcepos

    @property
    def expr(self):
        return self.children[0]

    def __repr__(self):
        return u"<Stmt: {} at 0x{}>".format(repr(self.children[0]), id(self))


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

    def __repr__(self):
        return u"<ConstantInt: {} from {} at 0x{}>".format(self.intval, self.sourcepos.i, id(self))

    def __eq__(self, other):
        return super(ConstantInt, self).__eq__(other) and self.intval == other.intval


class Variable(Node):
    """ Variable reference
    """
    def __init__(self, varname, sourcepos):
        self.varname = varname
        self.sourcepos = sourcepos

    def get_extra_dot_info(self):
        return str(self.varname)

    def __repr__(self):
        return u"<Variable: {} from {} at 0x{}>".format(repr(self.varname), self.sourcepos.i, id(self))

    def __eq__(self, other):
        return super(Variable, self).__eq__(other) and self.varname == other.varname


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

    def __repr__(self):
        return u"<Assignment: {} = {} from {} at 0x{}>".format(repr(self.var), repr(self.children[0]), self.sourcepos.i, id(self))

    def __eq__(self, other):
        return super(Assignment, self).__eq__(other) and self.target == other.target


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

    def __repr__(self):
        return u"<Function: {}({}) from {} at 0x{}>".format(repr(self.fname), repr(self.args), self.sourcepos.i, id(self))

    def __eq__(self, other):
        return super(Function, self).__eq__(other) and self.type_params == other.type_params


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

    def __repr__(self):
        return u"<BinOp: {} {} {} from {} at 0x{}>".format(repr(self.children[0]), self.op, repr(self.children[1]), self.sourcepos.i, id(self))

    def __eq__(self, other):
        return super(BinOp, self).__eq__(other) and self.op == other.op


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

    def __repr__(self):
        return u"<Conditional: if {} then {} else {} from {} at 0x{}>".format(repr(self.condition), repr(self.true_block), repr(self.false_block), self.sourcepos.i, id(self))


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

    def __repr__(self):
        return u"<While: while {} then {} from {} at 0x{}>".format(repr(self.condition), repr(self.block), self.sourcepos.i, id(self))


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

    def __repr__(self):
        return u"<FuncDef: {} ({}) {} from {} at 0x{}>".format(repr(self.name), repr(self.args), repr(self.children[0]), self.sourcepos.i, id(self))

    def __eq__(self, other):
        return (super(FuncDef, self).__eq__(other)
                and self.name == other.name
                and self.args == other.args
                and self.rtype == other.rtype
                and self.argtypes == other.argtypes
                and self.type_params == other.type_params)


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

    def __repr__(self):
        return u"<Return: {} from {} at 0x{}>".format(repr(self.arg), self.sourcepos.i, id(self))


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

    def __repr__(self):
        return u"<NewType: {}<{}>({}) {} from {} at 0x{}>".format(repr(self.type_type), repr(self.type_params), repr(self.options), repr(self.block), self.sourcepos.i, id(self))

    def __eq__(self, other):
        return (super(NewType, self).__eq__(other)
                and self.type_type == other.type_type
                and self.options == other.options
                and self.type_params == other.type_params)


class TypeOption(Node):

    def __init__(self, name, sourcepos, members=None):
        self.name = name
        self.sourcepos = sourcepos
        if members is None:
            members = []
        self.members = members

    def get_extra_dot_info(self):
        if self.members:
            return str(self.name) + "(" + ", ".join(self.members) + ")"
        else:
            return str(self.name)

    def __repr__(self):
        return u"<TypeOption: {} from {} at 0x{}>".format(repr(self.get_extra_dot_info()), self.sourcepos.i, id(self))

    def __eq__(self, other):
        return (super(TypeOption, self).__eq__(other)
                and self.name == other.name
                and self.members == other.members)


class Case(NonTerminal):

    def __init__(self, target, cases, else_case, sourcepos):
        self.children = [target] + cases + [else_case]
        self.sourcepos = sourcepos

    @property
    def target(self):
        return self.children[0]

    @property
    def cases(self):
        return self.children[1:-1]

    @property
    def else_case(self):
        return self.children[-1]

    def __repr__(self):
        return u"<Case: {} {} else {} from {} at 0x{}>".format(repr(self.target), repr(self.cases), repr(self.else_case), self.sourcepos.i, id(self))


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

    def is_simple(self):
        """True if the pattern doesn't destructure.

        case a:
             Nothing:
                 pass
             Just(x):
                 pass

        The first case is simple, the second is not,
        as it destructures to bind x.
        """
        return isinstance(self.label, Variable) or isinstance(self.label, Attribute)

    def __repr__(self):
        return u"<CaseCase: {} {} from {} at 0x{}>".format(repr(self.label), repr(self.block), self.sourcepos.i, id(self))


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

    def __repr__(self):
        return u"<Attribute: {}.{} from {} at 0x{}>".format(repr(self.target), repr(self.name), self.sourcepos.i, id(self))

    def __eq__(self, other):
        return (super(Attribute, self).__eq__(other)
                and self.name == other.name)


class TypeReference(Node):

    def __init__(self, name, sourcepos):
        self.name = name
        self.type_params = []
        self.sourcepos = sourcepos

    def get_extra_dot_info(self):
        return self.name

    def __str__(self):
        return self.name

    def __eq__(self, other):
        return (super(TypeReference, self).__eq__(other)
                and self.name == other.name
                and self.type_params == other.type_params)


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

    def visit_CaseCase(self, node):
        cnames = self.dispatch(node.block)
        if not node.is_simple():
            cnames.extend([n.varname for n in node.label.args])
        return cnames

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
