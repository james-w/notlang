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


def type_from_decl(type_str, vartypes):
    if type_str in vartypes:
        return vartypes[type_str]
    try:
        return BASE_TYPES[type_str]
    except KeyError:
        if len(type_str) == 1:
            vartype = TypeVariable(type_str)
            vartypes[type_str] = vartype
            return vartype
        raise AssertionError("Unknown type: %s" % type_str)


SUPERTYPE_OF = "supertype of"
SUBTYPE_OF = "subtype of"

INVERSE_CONSTRAINT = {
    SUPERTYPE_OF: SUBTYPE_OF,
    SUBTYPE_OF: SUPERTYPE_OF,
}


class Constraint(object):

    def __init__(self, a, constraint, b, positions):
        self.a = a
        self.constraint = constraint
        self.b = b
        self.positions = positions

    def __str__(self):
        return "%r %s %r" % (self.a, self.constraint, self.b)


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
        self.called_functions = []

    def get_typevar(self, name):
        return self.varmap.setdefault(name, TypeExpr(name))

    def visit_ConstantInt(self, node):
        return INT

    def visit_Assignment(self, node):
        source = node.children[0]
        target = node.var
        source_type = self.dispatch(source)
        target_type = self.get_typevar(target.varname)
        self.constraints.append(Constraint(target_type, SUPERTYPE_OF, source_type, [node.sourcepos]))
        return target_type

    def visit_Return(self, node):
        if node.children:
            source_type = self.dispatch(node.children[0])
        else:
            source_type = NONE
        self.constraints.append(Constraint(self.rtype, SUPERTYPE_OF, source_type, [node.sourcepos]))
        return None

    def visit_Variable(self, node):
        if node.varname not in self.varmap:
            raise SylphNameError("%s referenced before assignment" % node.varname, [node.sourcepos])
        return self.get_typevar(node.varname)

    def visit_Conditional(self, node):
        condition, true_block, false_block = node.children
        self.constraints.append(Constraint(self.dispatch(condition), SUBTYPE_OF, BOOL, [condition.sourcepos]))
        self.dispatch(true_block)
        if false_block is not None:
            self.dispatch(false_block)
        return None

    def visit_While(self, node):
        condition, block = node.children
        self.constraints.append(Constraint(self.dispatch(condition), SUBTYPE_OF, BOOL, [condition.sourcepos]))
        self.dispatch(block)
        return None

    def _handle_function(self, node, name):
        # TODO: this needs to carry the sourcepos for the args somehow
        if name not in self.varmap:
            if name not in self.functions:
                raise SylphNameError("Unknown function %s" % name, [node.sourcepos])
            else:
                ftype = self.functions[name]
        else:
            ftype = self.varmap[name]
        if name not in self.called_functions and name not in self.args and name != self.fname:
            self.called_functions.append(name)
        args = [self.dispatch(c) for c in node.children]
        rtype = TypeExpr("r" + name)
        self.constraints.append(Constraint(ftype, SUPERTYPE_OF, FunctionType(name, args, rtype), [node.sourcepos]))
        return rtype

    def visit_BinOp(self, node):
        return self._handle_function(node, node.op)

    def visit_Function(self, node):
        return self._handle_function(node, node.fname)

    def visit_FuncDef(self, node):
        child = TypeCollector(self.functions)
        self.child_contexts[node.name] = child
        child.fname = node.name
        vartypes = {}
        if node.rtype:
            child.rtype = type_from_decl(node.rtype, vartypes)
        argtypes = []
        for i, argtype_str in enumerate(node.argtypes):
            argtype = TypeExpr(node.args[i])
            if argtype_str is not None:
                argtype = type_from_decl(argtype_str, vartypes)
            child.varmap[node.args[i]] = argtype
            argtypes.append(argtype)
        ftype = self.get_typevar(node.name)
        child.varmap[node.name] = ftype
        child.args = node.args
        child.dispatch(node.children[0])
        # XXX: catch function redefinition/shadowing?
        #substitutions = satisfy_constraints(child.constraints[:])
        #self.constraints.extend(child.constraints)
        #newftype = function_type_from_context(child, substitutions=substitutions, generalise=False)
        #self.functions[node.name] = newftype
        #self.varmap[node.name] = ftype
        newftype = FunctionType(node.name, argtypes, child.rtype)
        child.constraints.append(Constraint(ftype, SUPERTYPE_OF, newftype, [node.sourcepos]))
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


def update_substitution(substitution, lhs, rhs, positions):
    if occurs(lhs, rhs):
        raise SylphTypeError("Recursive type definition: %s = %s" % (lhs, rhs), positions)
    for other_lhs, (candidate, pos) in substitution.items():
        # TODO: recurse in to function types in rhs?
        if candidate == lhs:
            substitution[other_lhs] = (rhs, pos + positions)
        if other_lhs == lhs:
            substitution[rhs] = (candidate, pos + positions)
    substitution[lhs] = (rhs, positions)


def satisfy_constraints(constraints):
    substitution = dict()
    while constraints:
        equality = constraints.pop(0)
        lhs, constraint, rhs, positions = equality.a, equality.constraint, equality.b, equality.positions
        #print "%r %s %r" % (lhs, constraint, rhs)
        if isinstance(lhs, TypeExpr):
            if lhs in substitution:
                # TODO: store positions in the substitution and update
                # the list as things are replaced to get full list of
                # involved lines?
                newlhs, newpos = substitution[lhs]
                constraints.insert(0, Constraint(newlhs, constraint, rhs, positions + newpos))
                continue
            else:
                if lhs != rhs:
                    update_substitution(substitution, lhs, rhs, positions)
        elif isinstance(lhs, FunctionType):
            if not isinstance(rhs, FunctionType):
                raise SylphTypeError("Types mismatch: %s != %s" % (lhs, rhs), positions)
            if len(lhs.args) != len(rhs.args):
                raise SylphTypeError("Types mismatch: %s != %s, argument lengths differ" % (lhs, rhs), positions)
            for i, arg in enumerate(lhs.args):
                constraints.insert(0, Constraint(arg, constraint, rhs.args[i], positions))
            constraints.insert(0, Constraint(lhs.rtype, constraint, rhs.rtype, positions))
        else:
            if isinstance(rhs, TypeExpr):
                if rhs in substitution:
                    newrhs, newpos = substitution[rhs]
                    constraints.insert(0, Constraint(lhs, constraint, newrhs, positions + newpos))
                    continue
                else:
                    if lhs != rhs:
                        update_substitution(substitution, rhs, lhs, positions)
                    continue
            newtype = unify_types(lhs, rhs, constraint)
            if newtype is None:
                raise SylphTypeError("Type mismatch: %s is not a %s of %s" % (lhs, constraint, rhs), positions)
    return substitution


def get_substituted(var, substitutions):
    while var in substitutions:
        var = substitutions[var][0]
    return var


def function_type_from_context(t, substitutions=None, generalise=True):
    if substitutions is None:
        substitutions = {}
    vars = {}
    def get_substituted_and_generalised(var, generalise=True):
        var = get_substituted(var, substitutions)
        if isinstance(var, TypeExpr):
            if generalise:
                var = vars.setdefault(var, TypeVariable(var.name))
            else:
                var = vars.get(var, var)
        elif isinstance(var, FunctionType):
            var = FunctionType(var.name, [get_substituted_and_generalised(a, generalise=generalise) for a in var.args], get_substituted_and_generalised(var.rtype, generalise=generalise))
        return var
    args = [get_substituted_and_generalised(t.varmap[arg], generalise=generalise) for arg in t.args]
    rtype = get_substituted_and_generalised(t.rtype, generalise=False)
    return FunctionType(t.fname, args, rtype)


def instantiated(ftype):
    instantiated_vars = {}
    def instantiate(arg):
        if isinstance(arg, TypeVariable):
            return instantiated_vars.setdefault(arg, TypeExpr(arg.name))
        if isinstance(arg, FunctionType):
            return instantiated(arg)
        return arg
    new_args = []
    for arg in ftype.args:
        new_args.append(instantiate(arg))
    new_rtype = instantiate(ftype.rtype)
    return FunctionType(ftype.name, new_args, new_rtype)


def _typecheck(t):
    calls = {}
    for name, child in t.child_contexts.items():
        for f in child.called_functions:
            if f not in FUNCTIONS:
                calls.setdefault(name, []).append(f)
    changed = True
    while changed:
        changed = False
        for a, b in calls.items():
            for c in b:
                if c in calls:
                    for d in calls[c]:
                        if d not in b:
                            b.append(d)
                            changed = True
    deleted = set()
    for a, b in calls.items():
        for c in b:
            if c in calls and c not in deleted:
                if c != a:
                    del calls[c]
                deleted.add(c)
    sets = calls.values()
    for name in t.child_contexts:
        for s in sets:
            if name in s:
                break
        else:
            sets.append([name])
    base_constraints = []
    new_ftypes = []
    def update_constraints(constraints):
        cs = constraints[:]
        for c in cs:
            for ftype in new_ftypes:
                if isinstance(c.a, FunctionType) and c.a.name == ftype.name:
                    c.a = instantiated(ftype)
                if isinstance(c.b, FunctionType) and c.b.name == ftype.name:
                    c.b = instantiated(ftype)
        return cs
    for s in sets:
        constraints = []
        for child in s:
            constraints.extend(update_constraints(t.child_contexts[child].constraints))
        constraints.extend(base_constraints[:])
        child_subs = satisfy_constraints(constraints)
        for child in s:
            newftype = function_type_from_context(t.child_contexts[child], substitutions=child_subs)
            new_ftypes.append(newftype)
            ftype = t.get_typevar(child)
            base_constraints.append(Constraint(ftype, SUPERTYPE_OF, newftype, []))
    main_constraints = update_constraints(t.constraints)
    main_constraints.extend(base_constraints[:])
    return satisfy_constraints(main_constraints)


def typecheck(node):
    fnames = FirstPass().dispatch(node)
    t = TypeCollector(FUNCTIONS.copy())
    for fname in fnames:
        func = t.get_typevar(fname)
        t.varmap[fname] = func
        t.functions[fname] = func
    t.dispatch(node)
    return t, _typecheck(t)
