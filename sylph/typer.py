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


class Instantiate(Type):

    def __init__(self, ftype):
        self.ftype = ftype

    def __str__(self):
        return str(self.ftype)

    def __repr__(self):
        return "<Instantiate:%s>" % str(self)


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


class FirstPass(ASTVisitor):

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


class TypeCollector(ASTVisitor):

    def __init__(self, functions):
        self.varmap = {}
        self.constraints = []
        self.rtype = TypeExpr("return")
        self.child_contexts = {}
        self.args = []
        self.fname = None
        self.functions = functions

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
        if node.varname not in self.varmap:
            raise SylphNameError("%s referenced before assignment" % node.varname, [node.sourcepos])
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
        if name not in self.varmap:
            if name not in self.functions:
                raise SylphNameError("Unknown function %s" % name, [node.sourcepos])
            ftype = self.functions[name]
        else:
            ftype = self.varmap[name]
        args = [self.dispatch(c) for c in node.children]
        rtype = TypeExpr("r" + name)
        self.constraints.append((ftype, SUPERTYPE_OF, Instantiate(FunctionType(name, args, rtype)), [node.sourcepos]))
        return rtype

    def visit_BinOp(self, node):
        return self._handle_function(node, node.op)

    def visit_Function(self, node):
        return self._handle_function(node, node.fname)

    def visit_FuncDef(self, node):
        child = TypeCollector(self.functions)
        self.child_contexts[node.name] = child
        child.fname = node.name
        if node.rtype:
            child.rtype = type_from_decl(node.rtype)
        for i, argtype_str in enumerate(node.argtypes):
            argtype = TypeExpr(node.args[i])
            if argtype_str is not None:
                argtype = type_from_decl(argtype_str)
            child.varmap[node.args[i]] = argtype
        ftype = self.get_typevar(node.name)
        child.varmap[node.name] = ftype
        child.args = node.args
        child.dispatch(node.children[0])
        # XXX: catch function redefinition/shadowing?
        substitions = satisfy_constraints(child.constraints[:])
        self.constraints.extend(child.constraints)
        newftype = function_type_from_context(child, substitions=substitions)
        self.varmap[node.name] = newftype
        self.functions[node.name] = newftype
        self.constraints.append((ftype, SUPERTYPE_OF, newftype, [node.sourcepos]))
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
    elif isinstance(rhs, Instantiate):
        return occurs(lhs, rhs.ftype)


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
            if isinstance(rhs, Instantiate):
                constraints.insert(0, (instantiated(lhs), constraint, rhs.ftype, positions))
                continue
            if not isinstance(rhs, FunctionType):
                raise SylphTypeError("Types mismatch: %s != %s" % (lhs, rhs), positions)
            if len(lhs.args) != len(rhs.args):
                raise SylphTypeError("Types mismatch: %s != %s, argument lengths differ" % (lhs, rhs), positions)
            for i, arg in enumerate(lhs.args):
                constraints.insert(0, (arg, constraint, rhs.args[i], positions))
            constraints.insert(0, (lhs.rtype, constraint, rhs.rtype, positions))
        elif isinstance(lhs, Instantiate):
            if isinstance(rhs, FunctionType):
                constraints.insert(0, (lhs.ftype, constraint, instantiated(rhs), positions))
            else:
                constraints.insert(0, (lhs.ftype, constraint, rhs, positions))
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
                raise SylphTypeError("Type mismatch: %s is not a %s of %s" % (lhs, constraint, rhs), positions)
    return substition


def get_substituted(var, substitions):
    while var in substitions:
        var = substitions[var][0]
    return var


def function_type_from_context(t, substitions=None):
    if substitions is None:
        substitions = {}
    vars = {}
    def get_substituted_and_generalised(var, generalise=True):
        var = get_substituted(var, substitions)
        if isinstance(var, TypeExpr):
            if generalise:
                var = vars.setdefault(var, TypeVariable(var.name))
            else:
                var = vars.get(var, var)
        elif isinstance(var, FunctionType):
            var = FunctionType(var.name, [get_substituted_and_generalised(a, generalise=generalise) for a in var.args], get_substituted_and_generalised(var.rtype, generalise=generalise))
        elif isinstance(var, Instantiate):
            var = get_substituted_and_generalised(var.ftype)
        return var
    args = [get_substituted_and_generalised(t.varmap[arg]) for arg in t.args]
    rtype = get_substituted_and_generalised(t.rtype, generalise=False)
    return FunctionType(t.fname, args, rtype)


def instantiated(ftype):
    instantiated_vars = {}
    def instantiate(arg):
        if isinstance(arg, TypeVariable):
            return instantiated_vars.setdefault(arg, TypeExpr(arg))
        if isinstance(arg, FunctionType):
            return instantiated(arg)
        return arg
    new_args = []
    for arg in ftype.args:
        new_args.append(instantiate(arg))
    new_rtype = instantiate(ftype.rtype)
    return FunctionType(ftype.name, new_args, new_rtype)


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
                raise SylphTypeError("%s is not a function" % s.name, pos)
    return constraints


def _typecheck(t):
    varmap = {}
    for context in t.child_contexts.values():
        child_subs = _typecheck(context)
    return satisfy_constraints(t.constraints[:])


def typecheck(node):
    fnames = FirstPass().dispatch(node)
    t = TypeCollector(FUNCTIONS.copy())
    for fname in fnames:
        func = t.get_typevar(fname)
        t.functions[fname] = func
    t.dispatch(node)
    return t, _typecheck(t)
