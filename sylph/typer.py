
from . import graph
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


def type_from_decl(type_str, vartypes, types):
    if type_str in vartypes:
        return vartypes[type_str]
    try:
        return types[type_str]
    except KeyError:
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

    def __init__(self, functions, types):
        self.varmap = {}
        self.constraints = []
        self.rtype = TypeExpr("return")
        self.child_contexts = {}
        self.args = []
        self.fname = None
        self.functions = functions
        self.types = types
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
        # Not isinstance, as we don't want to match TypeExpr etc.
        if source_type.__class__ == Type and source_type.name == "<anonymous>":
            source_type.name = target.varname
            self.types[target.varname] = source_type
        self.constraints.append(Constraint(target_type, SUPERTYPE_OF, source_type, [node.sourcepos]))
        # XXX: catch shadowing?
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
                if name not in self.types:
                    raise SylphNameError("Unknown function %s" % name, [node.sourcepos])
                else:
                    ftype = self.types[name]
            else:
                ftype = self.functions[name]
        else:
            ftype = self.varmap[name]
        if name not in self.called_functions and name not in self.args and name != self.fname:
            self.called_functions.append(name)
        args = [self.dispatch(c) for c in node.children]
        if name in self.types:
            return ftype
        else:
            rtype = TypeExpr("r" + name)
            self.constraints.append(Constraint(ftype, SUPERTYPE_OF, FunctionType(name, args, rtype), [node.sourcepos]))
            return rtype

    def visit_BinOp(self, node):
        return self._handle_function(node, node.op)

    def visit_Function(self, node):
        return self._handle_function(node, node.fname)

    def visit_FuncDef(self, node):
        child = TypeCollector(self.functions, self.types)
        self.child_contexts[node.name] = child
        child.fname = node.name
        vartypes = {}
        for type_param in node.type_params:
            vartypes[type_param] = TypeVariable(type_param)
        if node.rtype:
            child.rtype = type_from_decl(node.rtype, vartypes, self.types)
        argtypes = []
        for i, argtype_str in enumerate(node.argtypes):
            argtype = TypeExpr(node.args[i])
            if argtype_str is not None:
                argtype = type_from_decl(argtype_str, vartypes, self.types)
            child.varmap[node.args[i]] = argtype
            argtypes.append(argtype)
        ftype = self.get_typevar(node.name)
        child.varmap[node.name] = ftype
        child.args = node.args
        child.dispatch(node.children[0])
        # XXX: catch function redefinition/shadowing?
        newftype = FunctionType(node.name, argtypes, child.rtype)
        child.constraints.append(Constraint(ftype, SUPERTYPE_OF, newftype, [node.sourcepos]))
        return None

    def visit_NewType(self, node):
        return Type("<anonymous>")

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
    if lhs == rhs:
        return True
    if isinstance(rhs, FunctionType):
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
                if lhs != get_substituted(rhs, substitution):
                    update_substitution(substitution, lhs, get_substituted(rhs, substitution), positions)
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


def function_type_from_collector(t, substitutions):
    """Create a FunctionType from a TypeCollector.

    Looks at the attributes of TypeCollector related to functions
    (fname, args, rtype) and builds a FunctionType, taking in to
    account substitutions.
    """
    args = [get_substituted(t.varmap[arg], substitutions) for arg in t.args]
    rtype = get_substituted(t.rtype, substitutions)
    return FunctionType(t.fname, args, rtype)


def generalise(ftype):
    """Take a FunctionType and generalise it.

    This transforms any unconstrained variables in the type
    signature and turns them in to TypeVariables.

    It will recurse in to nested FunctionTypes.

    A repeated TypeExpr will be assiged the same TypeVariables for
    each occurence.

    Raises an error if the return type is unconstrained at the
    end.
    """
    vars = {}
    def _generalise(var, generalise=True):
        if isinstance(var, TypeExpr):
            if generalise:
                var = vars.setdefault(var, TypeVariable(var.name))
            else:
                var = vars.get(var, var)
        elif isinstance(var, FunctionType):
            # Not directly recursing, as the
            # restriction on generalising the rtype
            # depends on whether you are at the top
            # level or not
            var = FunctionType(
                var.name,
                [_generalise(a, generalise=generalise) for a in var.args],
                _generalise(var.rtype, generalise=generalise))
        return var
    args = [_generalise(arg, generalise=True) for arg in ftype.args]
    rtype = _generalise(ftype.rtype, generalise=False)
    if isinstance(rtype, TypeExpr):
        raise AssertionError("%s has an unconstrained return type." % ftype.name)
    return FunctionType(ftype.name, args, rtype)


def instantiate(ftype):
    """Instantiate a function type, i.e. create new TypeExpr for each TypeVariable.

    Given a function like TypeVariable(a) -> TypeVariable(a) it will return
    a function like TypeExpr(a) -> TypeExpr(a).

    It will recurse down to any function arguments.

    A type variable will expand to the same expression if used multiple times.

    Use this when "calling" a function, so that each call can use
    independent variables. Without doing this then every call
    to a function with type variables would have to use the same
    argument types.
    """
    _vars = {}
    def do_instantiate(ftype, vars):
        def _instantiate(arg):
            if isinstance(arg, TypeVariable):
                newvar = vars.setdefault(arg, TypeExpr(arg.name))
                return newvar
            elif isinstance(arg, FunctionType):
                return do_instantiate(arg, vars)
            return arg
        new_args = []
        for arg in ftype.args:
            new_args.append(_instantiate(arg))
        new_rtype = _instantiate(ftype.rtype)
        return FunctionType(ftype.name, new_args, new_rtype)
    return do_instantiate(ftype, _vars)


def _typecheck(t):
    calls = {}
    for name, child in t.child_contexts.items():
        for f in child.called_functions:
            if f not in FUNCTIONS:
                calls.setdefault(name, []).append(f)
    sets = graph.get_disjoint_sets(calls, t.child_contexts.keys())
    new_ftypes = {}
    def replace(var, old, new):
        if var == old:
            return instantiate(new)
        elif isinstance(var, FunctionType):
            return FunctionType(var.name, [replace(a, old, new) for a in var.args], replace(var.rtype, old, new))
        return var
    def update_constraints(constraints):
        cs = constraints[:]
        for c in cs:
            for var, ftype in new_ftypes.items():
                c.a = replace(c.a, var, ftype)
                c.b = replace(c.b, var, ftype)
        return cs
    for s in sets:
        constraints = []
        for child in s:
            constraints.extend(update_constraints(t.child_contexts[child].constraints))
        child_subs = satisfy_constraints(constraints)
        for child in s:
            newftype = generalise(function_type_from_collector(t.child_contexts[child], child_subs))
            ftype = t.get_typevar(child)
            new_ftypes[ftype] = newftype
            t.varmap[child] = newftype
    main_constraints = update_constraints(t.constraints)
    return satisfy_constraints(main_constraints)


def typecheck(node):
    fnames = FirstPass().dispatch(node)
    t = TypeCollector(FUNCTIONS.copy(), BASE_TYPES.copy())
    for fname in fnames:
        func = t.get_typevar(fname)
        t.varmap[fname] = func
        t.functions[fname] = func
    t.dispatch(node)
    return t, _typecheck(t)
