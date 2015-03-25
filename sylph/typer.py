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
        return self.name

    def __repr__(self):
        return "<TypeExpr:%s>" % str(self)


class TypeVariable(Type):

    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

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
        return str(self.name) + "(" + arg_str + ") -> " + str(self.rtype)

    def __repr__(self):
        return "<FunctionCall:%s>" % str(self)


class ReturnType(TypeExpr):

    def __str__(self):
        return "return of " + str(self.name)

    def __repr__(self):
        return "<ReturnType:%s>" % str(self)


class Apply(Type):

    def __init__(self, ftype, args):
        self.ftype = ftype
        self.args = args

    def __str__(self):
        return "Application of %s to %s" % (str(self.ftype), str(self.args))

    def __repr__(self):
        return "<Apply:%s(%s)>" % (str(self.ftype), str(self.args))


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


SUPERTYPE_OF = ">>>"
SUBTYPE_OF = "<<<"

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
        ftype = self.get_typevar(name)
        rtype = TypeExpr("return of %s" % ftype)
        self.constraints.append((rtype, SUPERTYPE_OF, ReturnType(Apply(ftype, args)), [node.sourcepos]))
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


def unify_types(a, b, constraint):
    if constraint == SUPERTYPE_OF and a is ANY:
        return ANY
    if constraint == SUBTYPE_OF and b is ANY:
        return ANY
    if a == b:
        return a


def occurs(lhs, rhs):
    if isinstance(rhs, ReturnType):
        if lhs == rhs:
            return True
        if occurs(lhs, rhs.name):
            return True
    elif isinstance(rhs, TypeExpr):
        pass
    elif isinstance(rhs, FunctionType):
        if lhs == rhs:
            return True
        for arg in rhs.args:
            if occurs(lhs, arg):
                return True
        if occurs(lhs, rhs.rtype):
            return True
    elif isinstance(rhs, Apply):
        if lhs == rhs:
            return True
        if occurs(lhs, rhs.ftype):
            return True
        for arg in rhs.args:
            if occurs(lhs, arg):
                return True


def update_substitution(substition, lhs, rhs, positions):
    if occurs(lhs, rhs):
        raise SylphTypeError("Recursive type definition: %s = %s" % (lhs, rhs), positions)
    for other_lhs, candidate in substition.items():
        # TODO: recurse in to function types in rhs?
        if candidate == lhs:
            substition[other_lhs] = rhs
        if other_lhs == lhs:
            substition[rhs] = candidate
    substition[lhs] = rhs


def handle_return_type(rtype, required_rtype, constraint, constraints, functions, substition, positions):
    tapply = rtype.name
    ftype = tapply.ftype
    defined = ftype
    if defined in substition:
        defined = substition[ftype]
    if not isinstance(defined, FunctionType):
        if defined.name not in functions:
            raise SylphNameError("Unknown function: %s" % defined.name, positions)
        defined = functions.get(defined.name)
    if not isinstance(defined, FunctionType):
        raise SylphTypeError("Attempted to call a non-function %s" % (defined.name), positions)
    if len(defined.args) != len(tapply.args):
        raise SylphTypeError("Incorrect number of args for function %s, expected %d, got %d" % (defined.name, len(defined.args), len(tapply.args)), positions)
    instantiated_vars = {}
    def instantiate(arg):
        if isinstance(arg, TypeVariable):
            return instantiated_vars.setdefault(arg, TypeExpr(arg1.name))
        return arg
    for arg1, arg2 in zip(defined.args, tapply.args):
        constraints.insert(0, (arg2, SUBTYPE_OF, instantiate(arg1), positions))
    constraints.insert(0, (required_rtype, INVERSE_CONSTRAINT[constraint], instantiate(defined.rtype), positions))


def satisfy_constraints(constraints, varmap, functions):
    substition = dict()
    while constraints:
        equality = constraints.pop(0)
        lhs, constraint, rhs, positions = equality
        if isinstance(lhs, TypeExpr):
            if lhs in substition:
                # TODO: store positions in the substition and update
                # the list as things are replaced to get full list of
                # involved lines?
                constraints.insert(0, (substition[lhs], constraint, rhs, positions))
                continue
            else:
                if isinstance(lhs, ReturnType):
                    handle_return_type(lhs, rhs, constraint, constraints, functions, substition, positions)
                elif isinstance(rhs, ReturnType):
                    handle_return_type(rhs, lhs, INVERSE_CONSTRAINT[constraint], constraints, functions, substition, positions)
                if lhs != rhs:
                    update_substitution(substition, lhs, rhs, positions)
        elif isinstance(lhs, FunctionType):
            if not isinstance(rhs, FunctionType):
                raise SylphTypeError("Types don't match %s != %s" % (lhs, rhs), positions)
            if len(lhs.args) != len(rhs.args):
                raise SylphTypeError("Types don't match %s != %s, argument lengths differ" % (lhs, rhs), positions)
            for i, arg in enumerate(lhs.args):
                constraints.insert(0, (arg, constraint, rhs.args[i], positions))
        else:
            if isinstance(rhs, TypeExpr):
                if rhs in substition:
                    constraints.insert(0, (lhs, constraint, substition[rhs], positions))
                    continue
                else:
                    if isinstance(rhs, ReturnType):
                        handle_return_type(rhs, lhs, INVERSE_CONSTRAINT[constraint], constraints, functions, substition, positions)
                    if lhs != rhs:
                        update_substitution(substition, rhs, lhs, positions)
                    continue
            newtype = unify_types(lhs, rhs, constraint)
            if newtype is None:
                raise SylphTypeError("Types don't match %s !%s %s" % (lhs, constraint, rhs), positions)
    return substition


def functions_from_vars(varmap):
    functions = {}
    for name, t in varmap.items():
        if isinstance(t, FunctionType):
            functions[TypeExpr(name)] = t
    return functions


def function_type_from_context(t, prefix_names=True, substitions=None):
    if substitions is None:
        substitions = {}
    vars = {}
    def get_substituted(var):
        while var in substitions:
            var = substitions[var]
        if prefix_names and isinstance(var, TypeExpr):
            var = vars.setdefault(var, TypeVariable(t.fname + ":" + var.name))
        return var
    rtype = get_substituted(t.rtype)
    args = [get_substituted(t.varmap[arg]) for arg in t.args]
    return FunctionType(t.fname, args, rtype)


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
    return satisfy_constraints(t.constraints, varmap, functions)


def typecheck(node):
    t = TypeCollector()
    t.dispatch(node)
    _typecheck(t)
