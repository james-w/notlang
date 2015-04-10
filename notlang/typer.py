
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

    def child_visit(self, node, name):
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
        if child.calls:
            self.callgraph[name] = child.calls
        for k, v in child.callgraph.items():
            self.callgraph[prefix(k)] = v

    def visit_FuncDef(self, node):
        self.child_visit(node.children[0], node.name)

    def visit_Assignment(self, node):
        if isinstance(node.children[0], NewType):
            self.child_visit(node.children[0].children[0], node.var.varname)

    def general_terminal_visit(self, node):
        pass

    def general_nonterminal_visit(self, node):
        for child in node.children:
            self.dispatch(child)


class ThirdPass(ASTVisitor):

    def __init__(self, active, only_process):
        self.active = active
        self.only_process = only_process

    def should_handle(self, name):
        return any(filter(lambda x: x == name or x.startswith(name + '.'), self.only_process))

    def visit_FuncDef(self, node):
        if self.should_handle(node.name):
            child_process = map(lambda x: x[len(node.name + '.'):], filter(lambda x: x.startswith(node.name + '.'), self.only_process))
            child = ThirdPass(node.name in self.only_process, child_process)
            child.dispatch(node.children[0])

    def general_terminal_visit(self, node):
        pass

    def general_nonterminal_visit(self, node):
        for child in node.children:
            self.dispatch(child)


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
        target_type = self.get_typevar(target.varname)
        if isinstance(source, NewType):
            newt = self._handle_new_type(source, target.varname)
            self.constraints.append(Constraint(target_type, SUPERTYPE_OF, newt, [node.sourcepos]))
        else:
            source_type = self.dispatch(source)
            self.constraints.append(Constraint(target_type, SUPERTYPE_OF, source_type, [node.sourcepos]))
        return target_type

    def _handle_new_type(self, node, tname):
        baset = Type(tname)
        t = baset
        if node.type_params:
            t = ParameterisedType([t] + [TypeVariable(a) for a in node.type_params])
        child = TypeCollector(self.functions, self.types)
        self.child_contexts[tname] = child
        vartypes = {}
        for type_param in node.type_params:
            vartypes[type_param] = TypeVariable(type_param)
        child.dispatch(node.children[0])
        # XXX: varmap probably contains the wrong things, really want top
        # level names
        for name in child.varmap:
            baset.attrs[name] = child.varmap[name]
        self.types[tname] = t
        return t

    def visit_Return(self, node):
        if node.children:
            source_type = self.dispatch(node.children[0])
        else:
            source_type = NONE
        self.constraints.append(Constraint(self.rtype, SUPERTYPE_OF, source_type, [node.sourcepos]))
        return None

    def visit_Variable(self, node):
        varname = node.varname
        if varname not in self.varmap:
            raise NotNameError("%s referenced before assignment" % varname, [node.sourcepos])
        main =  self.get_typevar(varname)
        return main

    def visit_Attribute(self, node):
        target = self.dispatch(node.children[0])
        new = TypeExpr(str(target) + "." + node.name)
        self.constraints.append(Constraint(AttributeAccess(target, node.name), SUPERTYPE_OF, new, [node.sourcepos]))
        return new

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
        if name not in self.types:
            if name not in self.varmap:
                if name not in self.functions:
                    raise NotNameError("%s referenced before assignment." % name, [node.sourcepos])
                else:
                    ftype = self.functions[name]
            else:
                ftype = self.varmap[name]
        else:
            ftype = self.types[name]
        # XXX: should this refer to the fully qualified name, or just the top level?
        if name not in self.called_functions and name not in self.args and name != self.fname:
            self.called_functions.append(name)
        args = [self.dispatch(c) for c in node.args]
        if name in self.types:
            if node.type_params:
                assert isinstance(ftype, ParameterisedType) and len(node.type_params) == len(ftype.types) - 1
                stypes = []
                vartypes = {}
                for param in node.type_params:
                    stypes.append(type_from_decl(param, vartypes, self.types))
                ftype = ParameterisedType([ftype.types[0]] + stypes)
            return ftype
        else:
            assert getattr(node, 'type_params', []) == [], "Can't define type params for normal function"
            rtype = TypeExpr("r" + name)
            self.constraints.append(Constraint(ftype, SUPERTYPE_OF, FunctionType(name, args, rtype), [node.sourcepos]))
            return rtype

    def visit_BinOp(self, node):
        return self._handle_function(node, node.op)

    def visit_Function(self, node):
        return self._handle_function(node, node.fname.varname)

    def visit_FuncDef(self, node):
        child = TypeCollector(self.functions, self.types)
        # XXX: catch function redefinition/shadowing?
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
        newftype = FunctionType(node.name, argtypes, child.rtype)
        child.constraints.append(Constraint(ftype, SUPERTYPE_OF, newftype, [node.sourcepos]))
        return None

    def visit_Pass(self, node):
        pass

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
        a = instantiate(constraint.a)
        b = instantiate(constraint.b)
        for i, arg in enumerate(a.args):
            new_constraints.insert(0, Constraint(arg, constraint.constraint, b.args[i], constraint.positions))
        new_constraints.insert(0, Constraint(a.rtype, constraint.constraint, b.rtype, constraint.positions))
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
        var = FunctionType(var.name, [get_substituted(a, substitutions) for a in var.args], get_substituted(var.rtype, substitutions))
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


def type_from_collector(collector, t, substitutions):
    """Create a Type from a TypeCollector.

    Looks at the attributes of TypeCollector related to types
    and builds a Type, taking in to account substitutions.
    """
    baset = t
    if isinstance(t, ParameterisedType):
        t = t.types[0]
    for attr in t.attrs:
        t.attrs[attr] = get_substituted(t.attrs[attr], substitutions)
    return baset


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
    if isinstance(ftype, FunctionType):
        args = [_generalise(arg, generalise=True) for arg in ftype.args]
        rtype = _generalise(ftype.rtype, generalise=False)
        if isinstance(rtype, TypeExpr):
            raise AssertionError("%s has an unconstrained return type." % ftype.name)
        return FunctionType(ftype.name, args, rtype)
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
            return arg
        if isinstance(ftype, FunctionType):
            new_args = []
            for arg in ftype.args:
                new_args.append(_instantiate(arg))
            new_rtype = _instantiate(ftype.rtype)
            return FunctionType(ftype.name, new_args, new_rtype)
        return ftype
    return do_instantiate(ftype, vars)


def satisfy_set(contexts, initial_subtitution=None):
    constraints = []
    for context in contexts:
        constraints.extend(context.constraints)
    return satisfy_constraints(constraints, initial_subtitution=initial_subtitution)


def _typecheck(t):
    calls = {}
    for name, child in t.child_contexts.items():
        for f in child.called_functions:
            if f not in FUNCTIONS:
                calls.setdefault(name, []).append(f)
    sets = graph.get_disjoint_sets(calls, t.child_contexts.keys())
    context_sets = []
    for s in sets:
        context_set = []
        context_sets.append(context_set)
        for child in s:
            context_set.append((child, t.child_contexts[child]))
    substitution = {}
    for s in context_sets:
        satisfy_set([a[1] for a in s], initial_subtitution=substitution)
        for name, child in s:
            if child.fname is not None:
                newftype = generalise(function_type_from_collector(child, substitution))
            else:
                newftype = generalise(type_from_collector(child, t.types[name], substitution))
            ftype = t.get_typevar(name)
            update_substitution(substitution, ftype, newftype, [])
            t.varmap[name] = newftype
    return satisfy_constraints(t.constraints, initial_subtitution=substitution)


def typecheck(node):
    pass1 = FirstPass()
    pass1.dispatch(node)
    t = TypeCollector(FUNCTIONS.copy(), BASE_TYPES.copy())
    for fname in pass1.functions:
        func = t.get_typevar(fname)
        t.varmap[fname] = func
        t.functions[fname] = func
    t.dispatch(node)
    return t, _typecheck(t)


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


def typecheck2(node):
    pass1 = FirstPass()
    pass1.dispatch(node)
    pass2 = SecondPass(None, {}, pass1)
    pass2.dispatch(node)
    all_functions = get_all_functions(pass1)
    sets = graph.get_disjoint_sets(pass2.callgraph, all_functions)
    print(sets)
    handled = set()
    for s in sets:
        print("Processing {}".format(s))
        handled.update(s)
        pass3 = ThirdPass(False, s)
        pass3.dispatch(node)
