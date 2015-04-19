
from . import graph
from .ast import ASTVisitor, NewType


class Type(object):

    def __init__(self, name, attrs=None):
        self.name = name
        if attrs is None:
            attrs = {}
        self.attrs = attrs

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<Type:%s>" % str(self)


class ParameterisedType(object):

    def __init__(self, types):
        self.types = types

    def __str__(self):
        return "%s<%s>" % (self.types[0], ",".join([str(a) for a in self.types[1:]]))

    def __repr__(self):
        return "<ParameterisedType:%s>" % str(self)

    def __eq__(self, other):
        return self.__class__ == other.__class__ and self.types == other.types

    def __ne__(self, other):
        return not(self.__eq__(other))


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

    def __init__(self, args, rtype):
        self.args = args
        self.rtype = rtype

    def __str__(self):
        if self.args:
            arg_str = " -> ".join([str(a) for a in self.args])
        else:
            arg_str = "(noargs)"
        return "(" + arg_str + " -> " + str(self.rtype) + ")"

    def __repr__(self):
        return "<FunctionType:%s>" % str(self)


class AttributeAccess(Type):

    def __init__(self, type, name):
        self.type = type
        self.name = name

    def __str__(self):
        return "%s.%s" % (str(self.type), str(self.name))

    def __repr__(self):
        return "<AttributeAccess:%s>" % str(self)


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
    """Collect the names of functions and types declared each level."""

    def __init__(self):
        self.functions = set()
        self.types = set()
        self.assignments = set()
        self.children = {}

    def child_visit(self, node, name):
        child = FirstPass()
        self.children[name] = child
        child.dispatch(node)

    def visit_FuncDef(self, node):
        if node.name in self.children:
            raise NotNameError("Redefinition of %s" % node.name, [node.sourcepos])
        self.functions.add(node.name)
        self.child_visit(node.children[0], node.name)

    def visit_Assignment(self, node):
        if isinstance(node.children[0], NewType):
            if node.var.varname in self.children:
                raise NotNameError("Redefinition of %s" % node.var.varname, [node.sourcepos])
            self.types.add(node.var.varname)
            self.functions.add(node.var.varname)
            self.child_visit(node.children[0].children[0], node.var.varname)
        else:
            self.assignments.add(node.var.varname)

    def general_terminal_visit(self, node):
        pass

    def general_nonterminal_visit(self, node):
        for child in node.children:
            self.dispatch(child)


class SecondPass(ASTVisitor):
    """Find the call graph."""

    def __init__(self, prefix, functions, name_graph):
        self.calls = set()
        self.callgraph = {}
        self.prefix = prefix
        self.functions = functions
        self.name_graph = name_graph

    def visit_Variable(self, node):
        # Maybe we should track use of the variable,
        # and only if it is invoked include it in the
        # calls, but passing the function to
        # something else could well influence
        # the type signature.
        if node.varname in self.functions:
            self.calls.add(self.functions[node.varname])

    def child_visit(self, node, name, argtypes):
        functions = self.functions.copy()

        def prefix(k):
            prefixed = name + '.' + k
            if self.prefix:
                prefixed = self.prefix + '.'
            return prefixed
        child_graph = self.name_graph.children[name]
        for sibling in self.name_graph.functions:
            functions[sibling] = sibling
        for child_f in child_graph.functions:
            functions[child_f] = prefix(child_f)
        new_prefix = name
        if self.prefix:
            new_prefix = self.prefix + '.' + name
        child = SecondPass(new_prefix, functions, child_graph)
        child.dispatch(node)
        for argtype_str in argtypes:
            if argtype_str in functions:
                child.calls.add(functions[argtype_str])
        if child.calls:
            self.callgraph[name] = child.calls
        for k, v in child.callgraph.items():
            self.callgraph[prefix(k)] = v

    def visit_FuncDef(self, node):
        self.child_visit(node.children[0], node.name, node.argtypes)

    def visit_Assignment(self, node):
        if isinstance(node.children[0], NewType):
            self.child_visit(node.children[0].children[0], node.var.varname, [])

    def general_terminal_visit(self, node):
        pass

    def general_nonterminal_visit(self, node):
        for child in node.children:
            self.dispatch(child)


class TypeEnv(object):

    def __init__(self, prefix):
        self.env = {}
        self.counter = -1
        self.prefix = prefix
        self.children = {}
        self.types = {}
        self.rtype = self.newvar()

    def newvar(self):
        self.counter += 1
        return TypeExpr(self.prefix + ":" + str(self.counter))

    def register(self, k, inherit=False):
        if k in self.env:
            return self.env[k][0]
        else:
            return self.env.setdefault(k, (self.newvar(), inherit))[0]

    def lookup(self, name, sourcepos, require=True):
        if name not in self.env:
            if require:
                raise NotNameError("%s referenced before assignment" % name, [sourcepos])
            return None
        return self.env[name][0]

    def extend(self, k, v, inherit=True):
        self.env[k] = (v, inherit)

    def subenv(self, name):
        new = TypeEnv(self.prefix + "." + name)
        for name, (var, inherit) in self.env.items():
            if inherit:
                new.extend(name, var)
        new.types = self.types.copy()
        self.children[name] = new
        return new

    def get_type(self, name):
        return self.types.get(name, None)

    def register_type(self, name, t):
        self.types[name] = t


class ThirdPass(ASTVisitor):

    def __init__(self, env, active, name_graph, only_process=None, skip=None):
        self.env = env
        self.active = active
        if only_process is None:
            only_process = []
        self.only_process = only_process
        if skip is None:
            skip = set()
        self.skip = skip
        self.name_graph = name_graph

    def should_handle(self, name):
        only = any(filter(lambda x: x == name or x.startswith(name + '.'), self.only_process))
        return only and not name in self.skip

    def get_child_only_process(self, name):
        return map(lambda x: x[len(name + '.'):], filter(lambda x: x.startswith(name + '.'), self.only_process))

    def visit_FuncDef(self, node):
        if not self.should_handle(node.name):
            return [], None
        child_process = self.get_child_only_process(node.name)
        env = self.env.subenv(node.name)
        argtypes = []
        for i, arg in enumerate(node.args):
            argtype_str = node.argtypes[i]
            if argtype_str is None:
                argtype = env.newvar()
            else:
                argtype = self.env.get_type(argtype_str)
                if argtype is None:
                    argtype = env.newvar()
            env.extend(arg, argtype, False)
            argtypes.append(argtype)
        for fname in child_process:
            if "." not in fname:
                env.register(fname, inherit=True)
        child = ThirdPass(env, node.name in self.only_process, self.name_graph.children[node.name], only_process=child_process)
        constraints = child.dispatch(node.children[0])[0]
        constraints.append(Constraint(
            self.env.lookup(node.name, node.sourcepos),
            SUPERTYPE_OF,
            FunctionType(argtypes, env.rtype),
            [node.sourcepos]))
        return constraints, None

    def _handle_new_type(self, node, name):
        if not self.should_handle(name):
            return [], None
        child_process = self.get_child_only_process(name)
        env = self.env.subenv(name)
        attrs = {}
        names = self.name_graph.children[name]
        for n in names.functions.union(names.types):
            attrs[n] = env.register(n, inherit=True)
        for n in names.assignments:
            attrs[n] = env.register(n, inherit=False)
        new_t = Type(name, attrs=attrs)
        if node.type_params:
            t_params = [new_t]
            for param in node.type_params:
                t_params.append(TypeVariable(param))
            new_t = ParameterisedType(t_params)
        self.env.register_type(name, new_t)
        child = ThirdPass(env, name in self.only_process, child_process)
        constraints, _ = child.dispatch(node.children[0].children[0])
        ftype = FunctionType([], new_t)
        constraints.append(Constraint(
            self.env.lookup(name, node.sourcepos),
            SUPERTYPE_OF,
            ftype,
            [node.sourcepos]))
        return constraints, ftype

    def visit_ConstantInt(self, node):
        if not self.active:
            return [], None
        return [], INT

    def visit_Variable(self, node):
        if not self.active:
            return [], None
        return [], instantiate(self.env.lookup(node.varname, node.sourcepos))

    def visit_Assignment(self, node):
        if isinstance(node.children[0], NewType):
            return self._handle_new_type(node.children[0], node.var.varname)
        if not self.active:
            return [], None
        # TODO: handle Attribute
        t = self.env.register(node.var.varname)
        constraints, child_t = self.dispatch(node.children[0])
        constraints.append(Constraint(
            t, SUPERTYPE_OF,
            child_t,
            [node.sourcepos]))
        return constraints, t

    def visit_Return(self, node):
        if not self.active:
            return [], None
        constraints, child_t = self.dispatch(node.children[0])
        constraints.append(Constraint(
            self.env.rtype,
            SUPERTYPE_OF,
            child_t,
            [node.sourcepos]))
        return constraints, None

    def _handle_function(self, node, target):
        rtype = self.env.newvar()
        constraints = []
        argtypes = []
        for i, arg in enumerate(node.args):
            arg_c, arg_t = self.dispatch(arg)
            constraints.extend(arg_c)
            argtypes.append(arg_t)
        constraints.append(Constraint(target,
            SUPERTYPE_OF, FunctionType(argtypes, rtype),
            [node.sourcepos]))
        if node.type_params:
            ptypes = [self.env.newvar()]
            for type_str in node.type_params:
                t = self.env.get_type(type_str)
                if t is None:
                    raise NotTypeError(
                        "Unknown type: %s" % type_str, [node.sourcepos])
                ptypes.append(t)
            constraints.append(Constraint(
                rtype, SUBTYPE_OF, ParameterisedType(ptypes), [node.sourcepos]))
        return constraints, rtype

    def visit_BinOp(self, node):
        if not self.active:
            return [], None
        return self._handle_function(node, self.env.lookup(node.op, node.sourcepos))

    def visit_Function(self, node):
        if not self.active:
            return [], None
        constraints, target = self.dispatch(node.fname)
        fcall_c, fcall_t = self._handle_function(node, target)
        return constraints + fcall_c, fcall_t

    def visit_Attribute(self, node):
        new_t = self.env.newvar()
        child_c, child_t = self.dispatch(node.children[0])
        constraints = child_c
        constraints.append(Constraint(AttributeAccess(child_t, node.name), SUBTYPE_OF, new_t, [node.sourcepos]))
        return constraints, new_t

    def general_terminal_visit(self, node):
        return [], None

    def general_nonterminal_visit(self, node):
        constraints = []
        for child in node.children:
            constraints.extend(self.dispatch(child)[0])
        return constraints, None


FUNCTIONS = {
    '+': FunctionType([INT, INT], INT),
    '-': FunctionType([INT, INT], INT),
    '*': FunctionType([INT, INT], INT),
    '>': FunctionType([INT, INT], BOOL),
    '==': FunctionType([INT, INT], BOOL),
    'true': FunctionType([], BOOL),
    'print': FunctionType([ANY], NONE),
}


class NotNameError(Exception):

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


class NotTypeError(Exception):

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


def satisfy_constraint(constraint, substitution):
    """Attempt to satisfy a single constraint based on a substitution.

    This will raise an error if it can't do it, or update the substitution
    if there is new information.

    The function will return a list of additional constraints that
    should be checked.
    """
    #print constraint
    new_constraints = []
    if isinstance(constraint.a, TypeExpr):
        if constraint.a in substitution:
            newlhs, newpos = substitution[constraint.a]
            new_constraints.insert(0, Constraint(newlhs, constraint.constraint, constraint.b, constraint.positions + newpos))
            return new_constraints
        else:
            if isinstance(constraint.b, TypeExpr):
                if constraint.b in substitution:
                    newrhs, newpos = substitution[constraint.b]
                    new_constraints.insert(0, Constraint(constraint.a, constraint.constraint, newrhs, constraint.positions + newpos))
                    return new_constraints
            if constraint.a != get_substituted(constraint.b, substitution):
                update_substitution(substitution, constraint.a, get_substituted(constraint.b, substitution), constraint.positions)
    elif isinstance(constraint.a, AttributeAccess):
        t = constraint.a.type
        t = get_substituted(t, substitution)
        if constraint.a.name not in t.attrs:
            raise NotTypeError("%s has no attribute %s" % (t, constraint.a.name), constraint.positions)
        new_constraints.append(Constraint(t.attrs[constraint.a.name], constraint.constraint, constraint.b, constraint.positions))
    elif isinstance(constraint.a, FunctionType):
        if isinstance(constraint.b, TypeExpr):
            if constraint.b in substitution:
                newrhs, newpos = substitution[constraint.b]
                new_constraints.insert(0, Constraint(constraint.a, constraint.constraint, newrhs, constraint.positions + newpos))
                return new_constraints
        if not isinstance(constraint.b, FunctionType):
            raise NotTypeError("Types mismatch: %s != %s" % (constraint.a, constraint.b), constraint.positions)
        if len(constraint.a.args) != len(constraint.b.args):
            raise NotTypeError("Types mismatch: %s != %s, argument lengths differ" % (constraint.a, constraint.b), constraint.positions)
        a = constraint.a
        b = constraint.b
        for i, arg in enumerate(a.args):
            new_constraints.insert(0, Constraint(arg, constraint.constraint, b.args[i], constraint.positions))
        new_constraints.insert(0, Constraint(a.rtype, constraint.constraint, b.rtype, constraint.positions))
    elif isinstance(constraint.a, ParameterisedType):
        if isinstance(constraint.b, TypeExpr):
            if constraint.b in substitution:
                newrhs, newpos = substitution[constraint.b]
                new_constraints.insert(0, Constraint(constraint.a, constraint.constraint, newrhs, constraint.positions + newpos))
                return new_constraints
            else:
                update_substitution(substitution, constraint.b, constraint.a, constraint.positions)
                return new_constraints
        if not isinstance(constraint.b, ParameterisedType):
            raise NotTypeError("Types mismatch: %s != %s" % (constraint.a, constraint.b), constraint.positions)
        if len(constraint.a.types) != len(constraint.b.types):
            raise NotTypeError("Types mismatch: %s != %s, wrong number of type params" % (constraint.a, constraint.b), constraint.positions)
        a = constraint.a
        b = constraint.b
        for i, arg in enumerate(a.types):
            new_constraints.insert(0, Constraint(arg, constraint.constraint, b.types[i], constraint.positions))
    else:
        if isinstance(constraint.b, TypeExpr):
            if constraint.b in substitution:
                newrhs, newpos = substitution[constraint.b]
                new_constraints.insert(0, Constraint(constraint.a, constraint.constraint, newrhs, constraint.positions + newpos))
                return new_constraints
            else:
                update_substitution(substitution, constraint.b, constraint.a, constraint.positions)
                return new_constraints
        newtype = unify_types(constraint.a, constraint.b, constraint.constraint)
        if newtype is None:
            raise NotTypeError("Type mismatch: %s is not a %s of %s" % (constraint.a, constraint.constraint, constraint.b), constraint.positions)
    return new_constraints


def satisfy_constraints(constraints, initial_subtitution=None):
    """Given a set of constraints, this will attempt to satisfy them all.

    If it fails it will raise a NotTypeError or NotNameError.

    If it succeeds it will return a substitution map that provides the
    most information possible about the types of the various expressions.
    """
    if initial_subtitution is None:
        substitution = dict()
    else:
        substitution = initial_subtitution
    while constraints:
        constraint = constraints.pop(0)
        # We put the new constraints at the front, but it probably
        # doesn't matter
        constraints = satisfy_constraint(constraint, substitution) + constraints
    return substitution


def unify_types(a, b, constraint):
    """Return the most general type that contains both types.

    Given two types and a constraint (SUPERTYPE_OF/SUBTYPE_OF)
    this will return the most general type that contains both
    types and satisfies the constraint.

    If no such type exists, then it will return None instead.
    """
    if constraint == SUPERTYPE_OF and a is ANY:
        return ANY
    if constraint == SUBTYPE_OF and b is ANY:
        return ANY
    if a == b:
        return a


def occurs(lhs, rhs):
    """Checks if `lhs` occurs in `rhs`.

    This is true if they are the same type, or if `rhs` is a
    compound type (FunctionType, ParameterisedType), and
    `lhs` is one of the subtypes.
    """
    if lhs == rhs:
        return True
    if isinstance(rhs, FunctionType):
        for arg in rhs.args:
            if occurs(lhs, arg):
                return True
        if occurs(lhs, rhs.rtype):
            return True
    if isinstance(rhs, ParameterisedType):
        for subtype in rhs.types:
            if occurs(lhs, subtype):
                return True
    return False


def update_substitution(substitution, lhs, rhs, positions):
    """Update the substitution to set lhs == rhs.

    First performs an occurs check on the pair, to avoid
    looping on recursive definitions. If the check fails
    then this function will raise a NotTypeError.

    If the check passes then the substitution will be updated
    with the new information.
    """
    if occurs(lhs, rhs):
        raise NotTypeError("Recursive type definition: %s = %s" % (lhs, rhs), positions)
    substitution[lhs] = (rhs, positions)
    def replace_in(a, old, new):
        if a == old:
            return new
        if isinstance(a, FunctionType):
            a.rtype = replace_in(a.rtype, old, new)
            args = []
            for arg in a.args:
                args.append(replace_in(arg, old, new))
            a.args = args
        elif isinstance(a, ParameterisedType):
            types = []
            for t in a.types:
                types.append(replace_in(t, old, new))
            a.types = types
        return a
    for other_lhs, (other_rhs, other_positions) in substitution.items():
        if other_lhs == rhs:
            raise NotTypeError("Circular inference %s %s" % (lhs, rhs), positions)
        if lhs != other_lhs:
            substitution[other_lhs] = (replace_in(other_rhs, lhs, rhs), other_positions)


def get_substituted(var, substitutions):
    """Get the substituted form of var.

    This applies any substituations for var, recursing if var is a function
    type after substitutions.

    Given a type, this will get the best information that the substitution
    has about what type it is.
    """
    while var in substitutions:
        var = substitutions[var][0]
    if isinstance(var, FunctionType):
        var = FunctionType([get_substituted(a, substitutions) for a in var.args], get_substituted(var.rtype, substitutions))
    return var


def generalise(ftype):
    """Take a Type and generalise it.

    This transforms any unconstrained variables in the type
    signature and turns them in to TypeVariables.

    It will recurse in to nested FunctionTypes.

    A repeated TypeExpr will be assiged the same TypeVariables for
    each occurence.

    Raises an error if the return type is unconstrained at the
    end.
    """
    vars = {}
    next_name = {True: 'a'}
    def _generalise(var, generalise=True):
        if isinstance(var, TypeExpr):
            if generalise:
                if var in vars:
                    var = vars[var]
                else:
                    var = vars.setdefault(var, TypeVariable(next_name[True]))
                    next_name[True] = chr(ord(next_name[True]) + 1)
            else:
                var = vars.get(var, var)
        elif isinstance(var, FunctionType):
            # Not directly recursing, as the
            # restriction on generalising the rtype
            # depends on whether you are at the top
            # level or not
            var = FunctionType(
                [_generalise(a, generalise=generalise) for a in var.args],
                _generalise(var.rtype, generalise=generalise))
        return var
    if isinstance(ftype, FunctionType):
        args = [_generalise(arg, generalise=True) for arg in ftype.args]
        rtype = _generalise(ftype.rtype, generalise=False)
        if isinstance(rtype, TypeExpr):
            raise AssertionError("unconstrained return type: %s" % ftype)
        return FunctionType(args, rtype)
    return ftype


def instantiate(ftype, vars=None):
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
    if vars is None:
        vars = {}
    def do_instantiate(ftype, vars):
        def _instantiate(arg):
            if isinstance(arg, TypeVariable):
                newvar = vars.setdefault(arg, TypeExpr(arg.name))
                return newvar
            elif isinstance(arg, FunctionType):
                return do_instantiate(arg, vars)
            elif isinstance(arg, ParameterisedType):
                return do_instantiate(arg, vars)
            return arg
        if isinstance(ftype, FunctionType):
            new_args = []
            for arg in ftype.args:
                new_args.append(_instantiate(arg))
            new_rtype = _instantiate(ftype.rtype)
            return FunctionType(new_args, new_rtype)
        elif isinstance(ftype, ParameterisedType):
            new_types = []
            for arg in ftype.types:
                new_types.append(_instantiate(arg))
            return ParameterisedType(new_types)
        return ftype
    return do_instantiate(ftype, vars)


def get_all_functions(p, prefix=None):
    def add_prefix(name):
        if prefix:
            return prefix + '.' + name
        return name
    fs = map(add_prefix, p.functions)
    def recurse(arg):
        return get_all_functions(arg[1], add_prefix(arg[0]))
    fs.extend(sum(map(recurse, p.children.items()), []))
    return fs


def typecheck(node, trace=False):
    pass1= FirstPass()
    pass1.dispatch(node)
    pass2 = SecondPass(None, {a: a for a in pass1.functions}, pass1)
    pass2.dispatch(node)
    all_functions = get_all_functions(pass1)
    sets = graph.get_disjoint_sets(pass2.callgraph, all_functions)
    if trace:
        print("Context sets: {}".format(str(sets)))
    handled = set()
    base_env = TypeEnv('main')
    for name, ftype in FUNCTIONS.items():
        base_env.extend(name, ftype)
    for name, t in BASE_TYPES.items():
        base_env.register_type(name, t)
    subst = {}
    for s in sets:
        if trace:
            print("Processing {}".format(s))
        handled.update(s)
        ftypes = {}
        for name in s:
            if "." not in name:
                ftypes[name] = base_env.register(name, inherit=True)
        pass3 = ThirdPass(base_env, False, pass1, only_process=s)
        constraints, t = pass3.dispatch(node)
        if trace:
            print("Constraints: {}".format(map(str, constraints)))
        subst = satisfy_constraints(constraints, initial_subtitution=subst)
        for name, ftype in ftypes.items():
            ftype = generalise(get_substituted(ftype, subst))
            base_env.env[name] = (ftype, True)
            if trace:
                print("{} has type {}".format(name, ftype))
    if trace:
        print("Processing remainder")
    pass3 = ThirdPass(base_env, True, pass1, skip=handled)
    constraints, t = pass3.dispatch(node)
    if trace:
        print("Constraints: {}".format(map(str, constraints)))
    subst = satisfy_constraints(constraints, initial_subtitution=subst)
    if trace:
        print("Substitution: {}".format(subst))
    return base_env, subst
