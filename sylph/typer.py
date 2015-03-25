from .ast import ASTVisitor, GatherNames


class Type(object):

    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<Type:%s>" % str(self)


class TypeExpr(Type):

    def __init__(self, name):
        self.name = name

    def __str__(self):
        return str(self.name)

    def __repr__(self):
        return "<TypeExpr:%s>" % str(self)


class TypeVariable(Type):

    def __init__(self, name):
        self.name = name

    def __str__(self):
        return str(self.name)

    def __repr__(self):
        return "<TypeVariable:%s>" % str(self)


class FunctionType(Type):

    def __init__(self, name, args, rtype):
        self.name = name
        self.args = args
        self.rtype = rtype

    def __str__(self):
        if self.args:
            arg_str = " -> ".join([str(a) for a in self.args])
        else:
            arg_str = "(noargs)"
        return "(" + str(self.name) + " " + arg_str + " -> " + str(self.rtype) + ")"

    def __repr__(self):
        return "<FunctionType:%s>" % str(self)


ANY = Type("ANY")
INT = Type("int")
NONE = Type("None")
BOOL = Type("bool")

BASE_TYPES = {
    "any": ANY,
    "int": INT,
    "None": NONE,
    "bool": BOOL,
}


def type_from_decl(type_str):
    try:
        return BASE_TYPES[type_str]
    except KeyError:
        raise AssertionError("Unknown type: %s" % type_str)


SUPERTYPE_OF = "supertype of"
SUBTYPE_OF = "subtype of"

INVERSE_CONSTRAINT = {
    SUPERTYPE_OF: SUBTYPE_OF,
    SUBTYPE_OF: SUPERTYPE_OF,
}

class TypeCollector(ASTVisitor):

    def __init__(self):
        self.varmap = {}
        self.constraints = []
        self.rtype = TypeExpr("return")
        self.child_contexts = {}
        self.args = []
        self.fname = None

    def get_typevar(self, name):
        return self.varmap.setdefault(name, TypeExpr(name))

    def visit_ConstantInt(self, node):
        return INT

    def visit_Assignment(self, node):
        source = node.children[0]
        target = node.var
        source_type = self.dispatch(source)
        target_type = self.get_typevar(target.varname)
        self.constraints.append((target_type, SUPERTYPE_OF, source_type, [node.sourcepos]))
        return target_type

    def visit_Return(self, node):
        if node.children:
            source_type = self.dispatch(node.children[0])
        else:
            source_type = NONE
        self.constraints.append((self.rtype, SUPERTYPE_OF, source_type, [node.sourcepos]))
        return None

    def visit_Variable(self, node):
        # XXX: check use before assignment here or in the checker?
        return self.get_typevar(node.varname)

    def visit_Conditional(self, node):
        condition, true_block, false_block = node.children
        self.constraints.append((self.dispatch(condition), SUBTYPE_OF, BOOL, [condition.sourcepos]))
        self.dispatch(true_block)
        if false_block is not None:
            self.dispatch(false_block)
        return None

    def visit_While(self, node):
        condition, block = node.children
        self.constraints.append((self.dispatch(condition), SUBTYPE_OF, BOOL, [condition.sourcepos]))
        self.dispatch(block)
        return None

    def _handle_function(self, node, name):
        # TODO: this needs to carry the sourcepos for the args somehow
        args = [self.dispatch(c) for c in node.children]
        type_name = self.get_typevar(name)
        rtype = TypeExpr("r" + str(type_name))
        self.constraints.append((type_name, SUPERTYPE_OF, FunctionType(type_name, args, rtype), [node.sourcepos]))
        return rtype

    def visit_BinOp(self, node):
        return self._handle_function(node, node.op)

    def visit_Function(self, node):
        return self._handle_function(node, node.fname)

    def visit_FuncDef(self, node):
        child = TypeCollector()
        # XXX: catch redfinition of functions
        self.child_contexts[node.name] = child
        if node.rtype:
            child.rtype = type_from_decl(node.rtype)
        for i, argtype_str in enumerate(node.argtypes):
            argtype = TypeExpr(node.args[i])
            if argtype_str is not None:
                # XXX: should look up type based on name
                argtype = type_from_decl(argtype_str)
            child.varmap[node.args[i]] = argtype
        ftype = self.get_typevar(node.name)
        child.varmap[node.name] = ftype
        self.varmap[node.name] = ftype
        child.args = node.args
        child.dispatch(node.children[0])
        # XXX: catch function redefinition/shadowing?
        return None

    def general_nonterminal_visit(self, node):
        [self.dispatch(c) for c in node.children]


FUNCTIONS = {
    '+': FunctionType("+", [INT, INT], INT),
    '-': FunctionType("-", [INT, INT], INT),
    '*': FunctionType("*", [INT, INT], INT),
    '>': FunctionType(">", [INT, INT], BOOL),
    '==': FunctionType("==", [INT, INT], BOOL),
    'true': FunctionType("true", [], BOOL), 
    'print': FunctionType("print", [ANY], NONE), 
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
            for pos in self.positions:
                lines.append(source.splitlines()[pos.lineno])
                lines.append(" " * (pos.columnno) + "^~~~~")
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
            for pos in self.positions:
                lines.append(source.splitlines()[pos.lineno])
                lines.append(" " * (pos.columnno) + "^~~~~")
        return "\n".join(lines)


def unify_types(a, b, constraint):
    if constraint == SUPERTYPE_OF and a is ANY:
        return ANY
    if constraint == SUBTYPE_OF and b is ANY:
        return ANY
    if a == b:
        return a


def occurs(lhs, rhs):
    if isinstance(rhs, TypeExpr):
        return False
    elif isinstance(rhs, FunctionType):
        if lhs == rhs:
            return True
        for arg in rhs.args:
            if occurs(lhs, arg):
                return True
        if occurs(lhs, rhs.rtype):
            return True


def update_substitution(substition, lhs, rhs, positions):
    if occurs(lhs, rhs):
        raise SylphTypeError("Recursive type definition: %s = %s" % (lhs, rhs), positions)
    for other_lhs, (candidate, pos) in substition.items():
        # TODO: recurse in to function types in rhs?
        if candidate == lhs:
            substition[other_lhs] = (rhs, pos + positions)
        if other_lhs == lhs:
            substition[rhs] = (candidate, pos + positions)
    substition[lhs] = (rhs, positions)


def satisfy_constraints(constraints):
    substition = dict()
    while constraints:
        equality = constraints.pop(0)
        lhs, constraint, rhs, positions = equality
        if isinstance(lhs, TypeExpr):
            if lhs in substition:
                # TODO: store positions in the substition and update
                # the list as things are replaced to get full list of
                # involved lines?
                newlhs, newpos = substition[lhs]
                constraints.insert(0, (newlhs, constraint, rhs, positions + newpos))
                continue
            else:
                if lhs != rhs:
                    update_substitution(substition, lhs, rhs, positions)
        elif isinstance(lhs, FunctionType):
            if not isinstance(rhs, FunctionType):
                raise SylphTypeError("Types don't match %s != %s" % (lhs, rhs), positions)
            if len(lhs.args) != len(rhs.args):
                raise SylphTypeError("Types don't match %s != %s, argument lengths differ" % (lhs, rhs), positions)
            for i, arg in enumerate(lhs.args):
                constraints.insert(0, (arg, constraint, rhs.args[i], positions))
            constraints.insert(0, (lhs.rtype, constraint, rhs.rtype, positions))
        else:
            if isinstance(rhs, TypeExpr):
                if rhs in substition:
                    newrhs, newpos = substition[rhs]
                    constraints.insert(0, (lhs, constraint, newrhs, positions + newpos))
                    continue
                else:
                    if lhs != rhs:
                        update_substitution(substition, rhs, lhs, positions)
                    continue
            newtype = unify_types(lhs, rhs, constraint)
            if newtype is None:
                raise SylphTypeError("Types don't match %s is not a %s of %s" % (lhs, constraint, rhs), positions)
    return substition


def functions_from_vars(varmap):
    functions = {}
    for name, t in varmap.items():
        if isinstance(t, FunctionType):
            functions[TypeExpr(name)] = t
    return functions


def get_substituted(var, substitions):
    while var in substitions:
        var = substitions[var][0]
    return var


def function_type_from_context(t, prefix_names=True, substitions=None):
    if substitions is None:
        substitions = {}
    vars = {}
    def get_substituted_and_generalised(var):
        var = get_substituted(var, substitions)
        if isinstance(var, TypeExpr):
            var = vars.setdefault(var, TypeVariable(var.name))
        elif isinstance(var, FunctionType):
            var = FunctionType(var.name, [get_substituted_and_generalised(a) for a in var.args], get_substituted_and_generalised(var.rtype))
        return var
    rtype = get_substituted_and_generalised(t.rtype)
    args = [get_substituted_and_generalised(t.varmap[arg]) for arg in t.args]
    return FunctionType(t.fname, args, rtype)


def instantiated(ftype):
    instantiated_vars = {}
    def instantiate(arg):
        if isinstance(arg, TypeVariable):
            return instantiated_vars.setdefault(arg, TypeExpr(arg))
        if isinstance(arg, FunctionType):
            return instantiated(arg)
        return arg
    return FunctionType(ftype.name, [instantiate(a) for a in ftype.args], instantiate(ftype.rtype))


def check_functions_exist(substitions, functions, args):
    constraints = []
    ftypes = {}
    for s, (t, pos) in substitions.items():
        if isinstance(t, FunctionType):
            ftypes.setdefault(t, []).append((s, pos))
    for t, names in ftypes.items():
        for s, pos in names:
            assert isinstance(s, TypeExpr), "Hope this doesn't fail!"
            if s.name in args:
                break
            if s.name in functions:
                defined = functions[s.name]
                constraints.append((t, SUBTYPE_OF, instantiated(defined), pos))
                break
        else:
            if s.name not in functions:
                raise AssertionError("%s is not a function" % s.name)
    return constraints


def _typecheck(t):
    functions = FUNCTIONS.copy()
    if t.fname:
        functions[t.fname] = function_type_from_context(t, prefix_names=False)
    varmap = {}
    for name, context in t.child_contexts.items():
        context.fname = name
        child_subs = _typecheck(context)
        functions[name] = function_type_from_context(context, substitions=child_subs)
    functions.update(functions_from_vars(t.varmap))
    substitions = satisfy_constraints(t.constraints[:])
    new_constraints = check_functions_exist(substitions, functions, t.args)
    satisfy_constraints(t.constraints[:] + new_constraints)
    return substitions


def typecheck(node):
    t = TypeCollector()
    t.dispatch(node)
    _typecheck(t)
