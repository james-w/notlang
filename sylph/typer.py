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

    def __init__(self, rtype, argtype=None):
        self.rtype = rtype
        self.argtype = argtype

    def __eq__(self, other):
        return (self.__class__ == other.__class__ and
                self.rtype == other.rtype and
                self.argtype == other.argtype)

    def __ne__(self, other):
        return not self == other

    def __str__(self):
        if self.argtype:
            return str(self.argtype) + " -> " + str(self.rtype)
        else:
            return " -> " + str(self.rtype)

    def __repr__(self):
        return "<FunctionType:%s>" % str(self)


ANY = Type("ANY")


def build_ftype(rtype, argtypes):
    ftype = rtype
    if not argtypes:
        ftype = FunctionType(rtype)
    else:
        for argtype in reversed(argtypes):
            ftype = FunctionType(ftype, argtype)
    return ftype


def type_from_str(argtype_str):
    if argtype_str is None:
        return ANY
    return Type(argtype_str)


class Typer(ASTVisitor):

    def __init__(self):
        self.varmap = {}
        self.rtype = ANY

    def infer_type(self, source_type, target_type, explanation):
        if source_type is not ANY:
            if (target_type is not ANY and target_type != source_type):
                raise TypeError("Type mismatch %s vs %s " % (source_type, target_type) + explanation)
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
        rtype = type_from_str(node.rtype)
        argtypes = map(type_from_str, node.argtypes)
        ftype = build_ftype(rtype, argtypes)
        subtyper.varmap[node.name] = ftype
        for i, arg in enumerate(node.args):
            subtyper.varmap[arg] = argtypes[i]
        subtyper.rtype = rtype
        subtyper.dispatch(node.children[0])
        # XXX: variable length argument type
        argtypes = []
        for arg in node.args:
            argtypes.append(subtyper.varmap[arg])
        self.varmap[node.name] = build_ftype(subtyper.rtype, argtypes)

    def visit_Variable(self, node):
        return self.varmap.get(node.varname, ANY)

    def visit_Function(self, node):
        ftype = self.varmap.get(node.fname, ANY)
        if ftype is ANY:
            ftype = FunctionType(ANY, ANY)
        else:
            if not isinstance(ftype, FunctionType):
                raise TypeError("Not a function %s (%s)" % (node.fname, ftype))
        print node.children
        curried_ftype = ftype
        for child in node.children:
            print "Applying child to curried_ftype"
            if curried_ftype.argtype is None:
                raise TypeError("Too many args to %s" % node.fname)
            declared_argtype = curried_ftype.argtype
            calculated_argtype = self.dispatch(node.children[0])
            # XXX: is this the correct way to handle passing a
            # function to itself?
            if calculated_argtype == ftype:
                if not (declared_argtype is ANY or declared_argtype == calculated_argtype):
                    raise TypeError("Mismatched types in function arg: %s vs %s" % (declared_argtype, calculated_argtype))
                # skip narrowing ANY for the declared_argtype because
                # of infinite recursion
            else:
                declared_argtype = self.infer_type(declared_argtype, calculated_argtype, "used in arg of %s" % node.fname)
            curried_ftype.argtype = declared_argtype
            curried_ftype = curried_ftype.rtype
        # XXX: check for too few arguments
        self.varmap[node.fname] = ftype
        return curried_ftype.rtype

    def visit_BinOp(self, node):
        left, right = map(self.dispatch, node.children)
        explanation = "for %s %s and %s" % (node.op, node.children[0], node.children[1])
        print explanation
        # TODO: needs to really check if the op is defined for the two types,
        # rather than just matching
        new_target_type = self.infer_type(left, right, explanation)
        return new_target_type

    def general_nonterminal_visit(self, node):
        map(self.dispatch, node.children)

    def general_terminal_visit(self, node):
        pass
