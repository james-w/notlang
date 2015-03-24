from .ast import ASTVisitor, GatherNames


class Type(object):

    def __init__(self, name):
        self.name = name

    def __key(self):
        return (self.__class__, self.name)

    def __eq__(self, other):
        return self.__key() == other.__key()

    def __ne__(self, other):
        return not self == other

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<Type:%s>" % str(self)

    def __hash__(self):
        return hash(self.__key())


class TypeVariable(Type):

    def __init__(self, name):
        self.name = name

    def __key(self):
        return (self.__class__, self.name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<TypeVariable:%s>" % str(self)


class FunctionCallType(Type):

    def __init__(self, name, args):
        self.name = name
        self.args = args

    def __key(self):
        return (self.__class__, self.name, self.args)

    def __str__(self):
        if self.args:
            arg_str = " -> ".join([str(a) for a in self.args])
        else:
            arg_str = "(noargs)"
        return str(self.name) + " " + arg_str

    def __repr__(self):
        return "<FunctionCall:%s>" % str(self)


ANY = Type("ANY")
INT = Type("int")
NONE = Type("None")
BOOL = Type("Boolean")


def create_typevars(arg, sourcepos):
    equalities = []
    if isinstance(arg, FunctionCallType):
        new_tvar = TypeVariable("(rtype of %s)" % str(arg))
        equalities.append((new_tvar, arg, [sourcepos]))
        return new_tvar, equalities
    else:
        return arg, equalities


class TypeCollector(ASTVisitor):

    def __init__(self):
        self.varmap = {}
        self.equalities = []
        self.rtype = TypeVariable("return")
        self.child_contexts = {}
        self.args = []
        self.fname = None

    def get_typevar(self, name):
        return self.varmap.setdefault(name, TypeVariable(name))

    def visit_ConstantInt(self, node):
        return INT

    def visit_Assignment(self, node):
        source = node.children[0]
        target = node.var
        source_type = self.dispatch(source)
        target_type = self.get_typevar(target.varname)
        self.equalities.append((target_type, source_type, [node.sourcepos]))
        return target_type

    def visit_Return(self, node):
        if node.children:
            source_type = self.dispatch(node.children[0])
        else:
            source_type = NONE
        self.equalities.append((self.rtype, source_type, [node.sourcepos]))
        return None

    def visit_Variable(self, node):
        # XXX: check use before assignment here or in the checker?
        return self.get_typevar(node.varname)

    def visit_Conditional(self, node):
        condition, true_block, false_block = node.children
        self.equalities.append((BOOL, self.dispatch(condition), [condition.sourcepos]))
        self.dispatch(true_block)
        if false_block is not None:
            self.dispatch(false_block)
        return None

    def visit_While(self, node):
        condition, block = node.children
        self.equalities.append((BOOL, self.dispatch(condition), [condition.sourcepos]))
        self.dispatch(block)
        return None

    def visit_BinOp(self, node):
        args = [self.dispatch(c) for c in node.children]
        new_args = []
        for arg in args:
            new_arg, new_eqs = create_typevars(arg, node.sourcepos)
            self.equalities.extend(new_eqs)
            new_args.append(new_arg)
        return FunctionCallType(node.op, new_args)

    def visit_Function(self, node):
        args = [self.dispatch(c) for c in node.children]
        ftype = self.varmap.setdefault(node.fname, TypeVariable(node.fname))
        new_args = []
        for arg in args:
            new_arg, new_eqs = create_typevars(arg, node.sourcepos)
            self.equalities.extend(new_eqs)
            new_args.append(new_arg)
        call = FunctionCallType(ftype, new_args)
        rtype = TypeVariable("return of " + str(call))
        self.equalities.append((rtype, call, [node.sourcepos]))
        return rtype

    def visit_FuncDef(self, node):
        child = TypeCollector()
        # XXX: catch redfinition of functions
        self.child_contexts[node.name] = child
        if node.rtype:
            child.rtype = Type(node.rtype)
        for i, argtype_str in enumerate(node.argtypes):
            argtype = TypeVariable(node.args[i])
            if argtype_str is not None:
                # XXX: should look up type based on name
                argtype = Type(argtype_str)
            child.varmap[node.args[i]] = argtype
        child.args = node.args
        child.dispatch(node.children[0])
        return None

    def general_nonterminal_visit(self, node):
        [self.dispatch(c) for c in node.children]


# XXX: These need to match the functions that are in scope
# at the time
FUNCTIONS = {
    '+': (INT, INT, INT),
    '-': (INT, INT, INT),
    '*': (INT, INT, INT),
    '>': (INT, INT, BOOL),
    '==': (INT, INT, BOOL),
    'true': (BOOL,),
    'print': (INT, NONE),
}


class SylphNameError(Exception):

    def __init__(self, message, positions):
        self.message = message
        self.positions = positions

    def __str__(self):
        return "%s at %s" % (self.message, self.positions)

    def nice_error_message(self, source=None, filename="<string>"):
        lines = []
        lines.append("NameError: " + self.message)
        lines.append("at line %d of %s" % (self.positions[0].lineno, filename))
        if source:
            lines.append(source.splitlines()[self.positions[0].lineno])
            lines.append(" " * (self.positions[0].columnno) + "^~~~~")
        return "\n".join(lines)


class SylphTypeError(Exception):

    def __init__(self, message, positions):
        self.message = message
        self.positions = positions

    def __str__(self):
        return "%s at %s" % (self.message, self.positions)

    def nice_error_message(self, source=None, filename="<string>"):
        lines = []
        lines.append("TypeError: " + self.message)
        lines.append("at line %d of %s" % (self.positions[0].lineno, filename))
        if source:
            lines.append(source.splitlines()[self.positions[0].lineno])
            lines.append(" " * (self.positions[0].columnno) + "^~~~~")
        return "\n".join(lines)


def find_function(rtype, ftype, varmap, positions, functions):
    if isinstance(ftype.name, TypeVariable):
        if not ftype.name in varmap:
            raise SylphNameError("%s used before assignment" % ftype.name, positions)
        name = varmap[ftype.name].name
        candidate = functions.get(name, [])
    else:
        candidate = functions.get(ftype.name, [])
    argtypes = ()
    for arg in ftype.args:
        if isinstance(arg, TypeVariable):
            if arg in varmap:
                argtypes += (varmap[arg],)
            else:
                raise SylphNameError("%s used before assignment" % arg, positions)
        else:
            argtypes += (arg,)
    if candidate[:-1] == argtypes:
        if rtype is None or candidate[-1] == rtype:
            return candidate
        else:
            raise SylphTypeError("Function %s over %s returns %s not %s" % (ftype.name, " -> ".join([str(a) for a in argtypes]), candidate[-1], rtype), positions)
    else:
        raise SylphTypeError("Can't find function for %s with signature (%s)" % (ftype.name, " -> ".join([str(a) for a in argtypes])), positions)


def unify_types(a, b, varmap, positions, functions):
    if isinstance(b, FunctionCallType):
        find_function(a, b, varmap, positions, functions)
    else:
        if a != b:
            raise SylphTypeError("%s != %s" % (a, b), positions)


def satisfy_equalities(equalities, varmap, functions):
    for equality in equalities:
        lhs, rhs, positions = equality
        if isinstance(lhs, TypeVariable):
            if isinstance(rhs, TypeVariable):
                if rhs in varmap:
                    if lhs in varmap:
                        unify_types(varmap[lhs], rhs, varmap, positions, functions)
                    else:
                        varmap[lhs] = varmap[rhs]
                else:
                    raise SylphNameError("%s used before assignment" % rhs.name, positions)
            elif isinstance(rhs, FunctionCallType):
                if lhs in varmap:
                    find_function(varmap[lhs], rhs, varmap, positions, functions)
                else:
                    ftype = find_function(None, rhs, varmap, positions, functions)
                    varmap[lhs] = ftype[-1]
            else:
                if lhs in varmap:
                    unify_types(varmap[lhs], rhs, varmap, positions, functions)
                varmap[lhs] = rhs
        else:
            unify_types(lhs, rhs, varmap, positions, functions)


def _typecheck(t):
    functions = FUNCTIONS.copy()
    varmap = {}
    varmap[TypeVariable('print')] = TypeVariable('print')
    if t.fname is not None:
        args = ()
        for arg in t.args:
            args += (t.varmap[arg],)
        args += (t.rtype,)
        functions[t.fname] = args
        varmap[TypeVariable(t.fname)] = TypeVariable(t.fname)
        #varmap[t.fname] = FunctionCallType(TypeVariable(t.fname), args)
    for name, context in t.child_contexts.items():
        context.varmap[TypeVariable(name)] = FunctionCallType(TypeVariable(name), [])
        context.fname = name
        _typecheck(context)
        args = ()
        for arg in context.args:
            args += (context.varmap[arg],)
        functions[name] = args + (context.rtype,)
        varmap[TypeVariable(name)] = FunctionCallType(name, [])
    satisfy_equalities(t.equalities, varmap, functions)


def typecheck(node):
    names = GatherNames().dispatch(node)
    t = TypeCollector()
    for name in names:
        t.varmap[name] = TypeVariable(name)
    t.dispatch(node)
    _typecheck(t)
