from .parsing import ASTVisitor


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


class FunctionType(Type):

    def __init__(self, rtype, argtypes):
        #TODO: how to encode unknown number of args?
        self.rtype = rtype
        self.argtypes = argtypes

    def __eq__(self, other):
        return (self.__class__ == other.__class__ and
                self.rtype == other.rtype and
                self.argtypes == other.argtypes)

    def __ne__(self, other):
        return not self == other

    def __str__(self):
        return " -> ".join(map(str, self.argtypes)) + " -> " + str(self.rtype)

    def __repr__(self):
        return "<FunctionType:%s>" % str(self)


ANY = Type("ANY")


class Typer(ASTVisitor):

    def __init__(self):
        self.varmap = {}
        self.rtype = ANY

    def infer_type(self, source_type, target_type, explanation):
        if source_type is not ANY:
            if (target_type is not ANY and target_type != source_type):
                raise AssertionError("Type mismatch %s vs %s " % (source_type, target_type) + explanation)
            return source_type
        return target_type

    def visit_ConstantInt(self, node):
        return Type("int")

    def visit_Assignment(self, node):
        source = node.children[0]
        target = node.var
        source_type = self.dispatch(source)
        target_type = self.varmap.get(target.varname, ANY)
        explanation = "assigning %s to %s" % (source, target)
        new_target_type = self.infer_type(source_type, target_type, explanation)
        if new_target_type is not None:
            self.varmap[target.varname] = source_type
        return new_target_type

    def visit_Return(self, node):
        self.rtype = self.infer_type(self.dispatch(node.children[0]), self.rtype, "Inconsistent return types")
        return self.rtype

    def visit_FuncDef(self, node):
        subtyper = Typer()
        if node.rtype:
            rtype = Type(node.rtype)
        else:
            rtype = ANY
        subtyper.varmap[node.name] = FunctionType(ANY, [ANY])
        subtyper.rtype = rtype
        subtyper.dispatch(node.children[0])
        self.varmap[node.name] = FunctionType(subtyper.rtype, [ANY])

    def visit_Variable(self, node):
        return self.varmap.get(node.varname, ANY)

    def visit_Function(self, node):
        ftype = self.varmap.get(node.fname, ANY)
        if ftype is ANY:
            ftype = FunctionType(ANY, [ANY])
            self.varmap[node.fname] = ftype
        if not isinstance(ftype, FunctionType):
            raise AssertionError("Not a function %s (%s)" % (node.fname, ftype))
        return ftype.rtype

    def visit_BinOp(self, node):
        left, right = map(self.dispatch, node.children)
        explanation = "for %s %s and %s" % (node.op, node.children[0], node.children[1])
        # TODO: needs to really check if the op is defined for the two types,
        # rather than just matching
        new_target_type = self.infer_type(left, right, explanation)
        return new_target_type

    def general_nonterminal_visit(self, node):
        map(self.dispatch, node.children)

    def general_terminal_visit(self, node):
        pass
