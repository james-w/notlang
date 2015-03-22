from .ast import ASTVisitor


class Type(object):

    def __init__(self, name):
        self.name = name

    def __eq__(self, other):
        return (self.__class__ == other.__class__ and
                self.name == other.name)

    def __ne__(self, other):
        return not self == other

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<Type:%s>" % str(self)


class FunctionCallType(Type):

    def __init__(self, name, args):
        self.name = name
        self.args = args

    def __eq__(self, other):
        return (self.__class__ == other.__class__ and
                self.name == other.name and
                self.args == other.args)

    def __ne__(self, other):
        return not self == other

    def __str__(self):
        if self.args:
            arg_str = " -> ".join(map(str, self.args))
        else:
            arg_str = "(noargs)"
        return self.name + " " + arg_str

    def __repr__(self):
        return "<FunctionCall:%s>" % str(self)


ANY = Type("ANY")
INT = Type("Int")
NONE = Type("None")
BOOL = Type("Boolean")


class TypeCollector(ASTVisitor):

    def __init__(self):
        self.varmap = {}
        self.equalities = []
        self.rtype = Type("return")
        self.child_contexts = []

    def visit_ConstantInt(self, node):
        return INT

    def visit_Assignment(self, node):
        source = node.children[0]
        target = node.var
        source_type = self.dispatch(source)
        target_type = self.varmap.setdefault(target.varname, Type(target.varname))
        self.equalities.append((target_type, source_type))
        return target_type

    def visit_Return(self, node):
        if node.children:
            source_type = self.dispatch(node.children[0])
        else:
            source_type = NONE
        self.equalities.append((self.rtype, source_type))
        return None

    def visit_Variable(self, node):
        return self.varmap.setdefault(node.varname, Type(node.varname))

    def visit_BinOp(self, node):
        args = map(self.dispatch, node.children)
        return FunctionCallType(node.op, args)

    def visit_Conditional(self, node):
        condition, block = node.children
        self.equalities.append((BOOL, self.dispatch(condition)))
        self.dispatch(block)
        return None

    visit_While = visit_Conditional

    def visit_Function(self, node):
        return FunctionCallType(node.fname, map(self.dispatch, node.children))

    def visit_FuncDef(self, node):
        child = TypeCollector()
        self.child_contexts.append(child)
        if node.rtype:
            child.rtype = node.rtype
        for i, argtype in enumerate(node.argtypes):
            if argtype is not None:
                child.varmap[node.args[i].varname] = argtype
        child.dispatch(node.children[0])
        return None

    def general_nonterminal_visit(self, node):
        map(self.dispatch, node.children)
