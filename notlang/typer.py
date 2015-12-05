import os
import sys

from . import debug, graph, parsing
from .ast import ASTVisitor, NewType


class Type(object):

    def __init__(self, name, attrs=None, bases=None):
        self.name = name
        if attrs is None:
            attrs = {}
        self.attrs = attrs
        if bases is None:
            bases = ()
        self.bases = bases

    def __str__(self):
        return self.name

    def __eq__(self, other):
        return (self.__class__ == other.__class__
                and self.name == other.name
                and self.attrs == other.attrs
                and self.bases == other.bases)

    def __ne__(self, other):
        return not(self.__eq__(other))

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

    @property
    def attrs(self):
        return self.types[0].attrs

    @property
    def bases(self):
        return self.types[0].bases


class TypeExpr(Type):

    def __init__(self, name):
        self.name = name

    def __str__(self):
        return str(self.name)

    def __eq__(self, other):
        return id(self) == id(other)

    def __repr__(self):
        return "<TypeExpr:%s>" % str(self)


class TypeVariable(Type):

    def __init__(self, name):
        self.name = name

    def __str__(self):
        return str(self.name)

    def __repr__(self):
        return "<TypeVariable:%s>" % str(self)

    def __eq__(self, other):
        return id(self) == id(other)


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

    def __eq__(self, other):
        return id(self) == id(other)


class UnionType(Type):

    def __init__(self, subtypes):
        assert subtypes, "can't union no types"
        self.subtypes = subtypes

    def __str__(self):
        subtype_str = " | ".join([str(a) for a in self.subtypes])
        return "[" + subtype_str + "]"

    def __repr__(self):
        return "<UnionType:%s>" % str(self)

    def __eq__(self, other):
        return type(self) == type(other) and self.subtypes == other.subtypes

    def reduce(self):
        new_subs = []
        def include(s):
            for sub in s:
                if sub in new_subs:
                    continue
                if isinstance(sub, UnionType):
                    include(sub.subtypes)
                else:
                    new_subs.append(sub)
        include(self.subtypes)
        if len(new_subs) < 2:
            return new_subs[0]
        return UnionType(new_subs)


class AttributeAccess(Type):

    def __init__(self, type, name):
        self.type = type
        self.name = name

    def __str__(self):
        return "%s.%s" % (str(self.type), str(self.name))

    def __repr__(self):
        return "<AttributeAccess:%s>" % str(self)

    def __eq__(self, other):
        return id(self) == id(other)


ANY = Type("ANY")


METATYPES = {
    'Type': Type('Type'),
    'Enum': Type('Enum'),
    'Tuple': Type('Tuple'),
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

    def __init__(self, a, constraint, b, positions, why):
        self.a = a
        self.constraint = constraint
        self.b = b
        self.positions = positions
        self.why = why

    def __str__(self):
        return "%r %s %r (%s)" % (self.a, self.constraint, self.b, self.why)


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
        self.child_visit(node.code, node.name)

    def visit_Assignment(self, node):
        if isinstance(node.source, NewType):
            if node.var.varname in self.children:
                raise NotNameError("Redefinition of %s" % node.var.varname, [node.sourcepos])
            self.types.add(node.var.varname)
            self.functions.add(node.var.varname)
            self.child_visit(node.source.block, node.var.varname)
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
        for argtype_ref in argtypes:
            if argtype_ref is None:
                continue
            if argtype_ref.name in functions:
                child.calls.add(functions[argtype_ref.name])
        if child.calls:
            self.callgraph[name] = child.calls
        for k, v in child.callgraph.items():
            self.callgraph[prefix(k)] = v

    def visit_FuncDef(self, node):
        self.child_visit(node.code, node.name, node.argtypes)

    def visit_Assignment(self, node):
        if isinstance(node.source, NewType):
            self.child_visit(node.source.block, node.var.varname, [])
            if node.source.type_type == 'Tuple':
                cgraph = self.callgraph.setdefault(node.var.varname, set())
                for opt in node.source.options:
                    cgraph.add(opt.name)
            if node.source.type_type == 'Enum':
                cgraph = self.callgraph.setdefault(node.var.varname, set())
                for opt in node.source.options:
                    for member in opt.members:
                        if member not in node.source.type_params:
                            cgraph.add(member)

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
        # Can this be from the prelude?
        self.rtype = Type("None")
        self.returned = False
        self.has_been_filled = False

    def newvar(self):
        self.counter += 1
        varname = self.prefix + ":" + str(self.counter)
        expr = TypeExpr(varname)
        return expr

    def register(self, name, reference, sourcepos, inherit=False):
        self.env[name] = (reference, sourcepos, inherit)

    def register_with_new_expr(self, name, sourcepos, inherit=False):
        expr = self.newvar()
        self.register(name, expr, sourcepos, inherit=inherit)
        return expr

    def lookup(self, name, sourcepos, require=True):
        if name not in self.env:
            if require:
                raise NotNameError("%s referenced before assignment" % name, [sourcepos])
            return None
        return self.env[name][0]

    def extend(self, k, v, sourcepos, inherit=True):
        self.env[k] = (v, sourcepos, inherit)

    def subenv(self, name):
        if name in self.children:
            return self.children[name]
        new = TypeEnv(self.prefix + "." + name)
        for n, (var, sourcepos, inherit) in self.env.items():
            if inherit:
                new.extend(n, var, sourcepos)
        new.types = self.types.copy()
        self.children[name] = new
        return new

    def subenv_same_scope(self):
        self.counter += 1
        new = TypeEnv(self.prefix + "." + str(self.counter))
        for n, (var, sourcepos, inherit) in self.env.items():
            new.extend(n, var, sourcepos)
        new.types = self.types.copy()
        new.rtype = self.rtype
        return new

    def get_type(self, name):
        return self.types.get(name, None)

    def get_type_from_ref(self, ref):
        base = self.get_type(ref.name)
        if base is None:
            return base
        if ref.type_params:
            if not isinstance(base, ParameterisedType):
                raise NotTypeError("Can't parameterise {} with {}".format(base, ref.type_params), [ref.sourcepos])
            base = ParameterisedType([base.types[0]] + [self.get_type_from_ref(r) for r in ref.type_params])
        return base

    def register_type(self, name, t):
        self.types[name] = t


class ThirdPass(ASTVisitor):

    def __init__(self, env, active, name_graph, only_process=None, skip=None, self_type=None, has_attrs=False):
        self.env = env
        self.active = active
        if only_process is None:
            only_process = []
        self.only_process = only_process
        if skip is None:
            skip = set()
        self.skip = skip
        self.name_graph = name_graph
        self.self_type = self_type
        self.has_attrs = has_attrs

    def should_handle(self, name):
        only = any(filter(lambda x: x == name or x.startswith(name + '.'), self.only_process))
        return only and not name in self.skip

    def should_handle_direct(self, name):
        only = any(filter(lambda x: x == name, self.only_process))
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
            argtype_ref = node.argtypes[i]
            if i == 0 and self.self_type is not None:
                # Handle the super arg in a method signature
                # XXX: need to check conflict with declared type
                argtype = self.self_type
            elif argtype_ref is None:
                argtype = env.newvar()
            else:
                argtype = self.env.get_type_from_ref(argtype_ref)
                if argtype is None:
                    raise NotTypeError("Unable to resolve type name {}".format(str(argtype_ref)), [node.sourcepos])
            env.extend(arg, argtype, [node.sourcepos], False)
            if i > 0 or self.self_type is None:
                argtypes.append(argtype)
        if node.rtype is not None:
            # TODO: should be env.declared_rtype or something
            env.rtype = self.env.get_type_from_ref(node.rtype)
            if env.rtype is None:
                raise NotTypeError("Unable to resolve {} to a valid type".format(node.rtype), [node.sourcepos])
        # XXX: missing considering node.type_params?
        for fname in child_process:
            if "." not in fname:
                # This should probably refer to the sourcepos for the siblings
                env.register_with_new_expr(fname, [], inherit=True)
        child = ThirdPass(env, node.name in self.only_process, self.name_graph.children[node.name], only_process=child_process)
        constraints = child.dispatch(node.code)[0]
        constraints.append(Constraint(
            self.env.lookup(node.name, node.sourcepos),
            SUPERTYPE_OF,
            FunctionType(argtypes, env.rtype),
            [node.sourcepos],
            "Function {} is used in the right way".format(node.name)))
        return constraints, None

    def _handle_new_type(self, node, name):
        if not self.should_handle(name):
            return [], None
        child_process = self.get_child_only_process(name)
        env = self.env.subenv(name)
        attrs = {}
        subtypes = []
        if not env.has_been_filled:
            names = self.name_graph.children[name]
            for n in names.functions.union(names.types):
                # This should include the sourcepos for the
                # elements
                attrs[n] = env.register_with_new_expr(n, [], inherit=True)
            for n in names.assignments:
                # This should include the sourcepos for the
                # elements
                attrs[n] = env.register_with_new_expr(n, [], inherit=False)
            if node.type_type == 'Enum':
                for val in node.options:
                    attrs[val.name] = env.register_with_new_expr('{}.{}'.format(name, val.name), [val.sourcepos], inherit=True)
            for n in node.type_params:
                env.register_type(n.name, TypeExpr(n.name))
            env.has_been_filled = True
        if self.should_handle_direct(name):
            if node.type_type == 'Tuple':
                for i, a_name in enumerate(['first', 'second']):
                    if len(node.options) > i:
                        t_name = node.options[i].name
                        argtype = self.env.get_type(t_name)
                        attrs[a_name] = FunctionType([], argtype)
            new_t = Type(name, attrs=attrs, bases=(METATYPES.get(node.type_type),))
            if node.type_params:
                t_params = [new_t]
                for param in node.type_params:
                    t_params.append(TypeVariable(param.name))
                new_t = ParameterisedType(t_params)
            # Register in both parent env and one used
            # for children
            self.env.register_type(name, new_t)
            env.register_type(name, new_t)
            if node.type_type == 'Enum':
                for val in node.options:
                    opt_name = '{}.{}'.format(name, val.name)
                    opt_type = new_t
                    opt_rtype = opt_type
                    if val.members:
                        args = []
                        # Should be generalised to resolve type refs in the presence
                        # of type params.
                        type_param_names = [n.name for n in node.type_params]
                        for member in val.members:
                            if member in type_param_names:
                                t_param = new_t.types[type_param_names.index(member)+1]
                                args.append(t_param)
                            else:
                                args.append(self.env.get_type(member))
                        opt_rtype = FunctionType(args, opt_type)
                    env.register_type(opt_name, opt_rtype)
                    subtypes.append((opt_name, opt_rtype))
        else:
            ftype = self.env.lookup(name, node.sourcepos)
            if node.type_type == 'Enum':
                new_t = ftype
            else:
                new_t = ftype.rtype
        child = ThirdPass(env, name in self.only_process, self.name_graph.children[name], only_process=child_process, self_type=new_t, has_attrs=True)
        constraints, _ = child.dispatch(node.block)
        if self.should_handle_direct(name):
            if node.type_type == 'Enum':
                ftype = new_t
                for sname, subtype in subtypes:
                    constraints.append(Constraint(
                        instantiate(env.lookup(sname, node.sourcepos)),
                        SUPERTYPE_OF,
                        subtype,
                        [node.sourcepos],
                        "Use of constructor {} matches its types".format(sname),
                        ))
            else:
                if node.type_type == 'Tuple':
                    argtypes = []
                    for t_name in node.options:
                        argtype = self.env.get_type(t_name.name)
                        if argtype is None:
                            raise NotTypeError(
                                "Unknown type: %s" % t_name.name, [t_name.sourcepos])
                        argtypes.append(argtype)
                    ftype = FunctionType(argtypes, new_t)
                else:
                    ftype = FunctionType([], new_t)
            constraints.append(Constraint(
                instantiate(self.env.lookup(name, node.sourcepos)),
                SUPERTYPE_OF,
                ftype,
                [node.sourcepos],
                "Constructor of {} is used correctly".format(name),
                ))
        return constraints, ftype

    def visit_ConstantInt(self, node):
        if not self.active:
            return [], None
        return [], self.env.get_type('int')

    def visit_Variable(self, node):
        if not self.active:
            return [], None
        return [], instantiate(self.env.lookup(node.varname, node.sourcepos))

    def visit_Assignment(self, node):
        if isinstance(node.source, NewType):
            return self._handle_new_type(node.source, node.var.varname)
        if not self.active:
            return [], None
        if self.has_attrs:
            constraints, child_t = self.dispatch(node.source)
            var_t  = self.env.lookup(node.var.varname, node.sourcepos)
            constraints.append(Constraint(
                var_t,
                SUPERTYPE_OF,
                child_t,
                [node.sourcepos],
                "Use of attribute {} has correct type".format(node.var.varname),
                ))
            return constraints, var_t
        else:
            # TODO: handle setting attribute
            constraints, child_t = self.dispatch(node.source)
            self.env.register(node.var.varname, child_t, [node.sourcepos])
            return constraints, child_t

    def visit_Return(self, node):
        if not self.active:
            return [], None
        if node.arg:
            constraints, child_t = self.dispatch(node.arg)
        else:
            constraints = []
            child_t = None
        if child_t is not None:
            self.env.rtype = child_t
        self.active = False
        self.env.returned = True
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
            [node.sourcepos],
            "Function {} called in correct manner".format(target),
            ))
        if node.type_params:
            ptypes = [self.env.newvar()]
            for type_ref in node.type_params:
                t = self.env.get_type_from_ref(type_ref)
                if t is None:
                    raise NotTypeError(
                        "Unknown type: %s" % str(type_ref), [node.sourcepos])
                ptypes.append(t)
            constraints.append(Constraint(
                rtype, SUBTYPE_OF, ParameterisedType(ptypes), [node.sourcepos],
                "Declared type params when calling {} match function rtype".format(target)))
        return constraints, rtype

    def visit_Case(self, node):
        if not self.active:
            return [], None
        constraints, ttype = self.dispatch(node.target)
        constraints.append(Constraint(
            ttype, SUBTYPE_OF, METATYPES.get('Enum'), [node.sourcepos],
            "Case is called on an enum type"))
        returned = []
        rtypes = []
        leaked_vars = []
        for case in node.cases:
            subenv = self.env.subenv_same_scope()
            if case.is_simple():
                new_constraints, ltype = self.dispatch(case.label)
                constraints.extend(new_constraints)
                constraints.append(Constraint(
                    ttype, SUPERTYPE_OF, ltype, [node.sourcepos],
                    "case label is a subclass of the target"))
            else:
                argtypes = []
                for arg in case.label.args:
                    arg_t = subenv.register_with_new_expr(arg.varname, [arg.sourcepos])
                    argtypes.append(arg_t)
                new_constraints, ltype = self.dispatch(case.label.fname)
                # Need to bind the types of the args to the types in the
                # constructor, and also bind the type to the type of the target
                constraints.append(Constraint(
                    ttype, SUPERTYPE_OF, ltype, [node.sourcepos],
                    "case label is a subclass of the target"))
                case_type = self.dispatch(case.label.fname)[1]
                constraints.append(Constraint(
                    case_type, SUPERTYPE_OF, FunctionType(argtypes, ltype), [node.sourcepos],
                    "case label subtype matches use"))
            child_pass = ThirdPass(subenv, True, self.name_graph)
            constraints.extend(child_pass.dispatch(case.block)[0])
            returned.append(subenv.returned)
            if subenv.returned:
                # The branch returned, so leaks no vars,
                # put the parent vars back in.
                leaked_vars.append(self.env.subenv_same_scope().env.copy())
            else:
                leaked_vars.append(subenv.env.copy())
            rtypes.append(subenv.rtype)
        # We assume full cover and use a subsequent pass to check
        # that it didn't cause a mistaken inference
        # TODO: Implement that pass to check
        # is that even possible? It's not just whether a variable
        # is live, but what type it has too, as a full_cover
        # will perhaps miss unioning with a previous value.
        analyse_leaked_vars(self.env, returned, rtypes, leaked_vars, full_cover=True)
        return constraints, None

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
        if not self.active:
            return [], None
        new_t = self.env.newvar()
        child_c, child_t = self.dispatch(node.target)
        constraints = child_c
        constraints.append(Constraint(AttributeAccess(child_t, node.name), SUBTYPE_OF, new_t, [node.sourcepos],
            "Attribute {} is found on target type".format(node.name)))
        return constraints, new_t

    def visit_Conditional(self, node):
        if not self.active:
            return [], None
        constraints = []
        condition_cs, condition_t = self.dispatch(node.condition)
        # Use Truthy rather than bool
        constraints.append(Constraint(
            condition_t,
            SUBTYPE_OF,
            self.env.get_type('bool'),
            [node.condition.sourcepos],
            "Conditional is testing a bool"))
        constraints.extend(condition_cs)
        leaked_vars = []
        rtypes = []
        returned = []
        for child in [node.true_block, node.false_block]:
            if child is not None:
                env = self.env.subenv_same_scope()
                child_pass = ThirdPass(env, True, self.name_graph)
                constraints.extend(child_pass.dispatch(child)[0])
                returned.append(env.returned)
                if env.returned:
                    # The branch returned, so leaks no vars,
                    # put the parent vars back in.
                    leaked_vars.append(self.env.subenv_same_scope().env.copy())
                else:
                    leaked_vars.append(env.env.copy())
                rtypes.append(env.rtype)
        analyse_leaked_vars(self.env, returned, rtypes, leaked_vars, full_cover=all([node.true_block, node.false_block]))
        return constraints, None

    # TODO: visit_Conditional treatment for While too.

    def general_terminal_visit(self, node):
        return [], None

    def general_nonterminal_visit(self, node):
        constraints = []
        for child in node.children:
            constraints.extend(self.dispatch(child)[0])
        return constraints, None


def analyse_leaked_vars(env, returned, rtypes, leaked_vars, full_cover=False):
    if full_cover and all(returned):
        env.rtype = unionify_all(rtypes)
        env.returned = True
    else:
        for (include, rtype) in zip(returned, rtypes):
            if include:
                env.rtype = unionify(env.rtype, rtype)
    if full_cover:
        fully_leaked = leaked_vars[0].copy()
        start_index = 1
    else:
        fully_leaked = {}
        start_index = 0
    partially_leaked = {}
    for possible_leak_set in leaked_vars[start_index:]:
        for a in fully_leaked.keys():
            if a not in possible_leak_set:
                partially_leaked[a] = fully_leaked[a]
                del fully_leaked[a]
            else:
                fully_leaked[a] = (unionify(fully_leaked[a][0], possible_leak_set[a][0]), fully_leaked[a][1] + possible_leak_set[a][1], fully_leaked[a][2] and possible_leak_set[a][2])
                del possible_leak_set[a]
        for a in possible_leak_set.keys():
            if a in partially_leaked:
                partially_leaked[a] = (unionify(partially_leaked[a][0], possible_leak_set[a][0]), partially_leaked[a][1] + possible_leak_set[a][1], partially_leaked[a][2] and possible_leak_set[a][2])
            else:
                partially_leaked[a] = possible_leak_set[a]
            del possible_leak_set[a]
    for a, val in fully_leaked.items():
        env.register(a, val[0], val[1], val[2])
    for a, val in partially_leaked.items():
        if a not in env.env:
            # Don't register a leaked var that wasn't
            # already defined. Perhaps some way
            # of registering it with an invalid type
            # would lead to better errors.
            continue
        env.register(a, unionify(env.env[a][0], val[0]), env.env[a][1] + val[1], env.env[a][2] and val[2])


def unionify(a, b):
    """Union two types.

    In general this will return a UnionType of the two
    types, but reductions may be made if there is
    a simplification that can be done, e.g.

       unionify(a, a) == a
    """
    return unionify_all([a, b])


def unionify_all(ts):
    """Union all of the types in a list."""
    return UnionType(ts).reduce()


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


def satisfy_attribute_access(attr, other, direction, positions, substitution):
    t = attr.type
    t = get_substituted(t, substitution)
    ret = None
    for base in (t,) + t.bases:
        ret = base.attrs.get(attr.name, None)
        if ret is not None:
            break
    else:
        raise NotTypeError("%s has no attribute %s" % (get_substituted(t, substitution), attr.name), positions)
    return [Constraint(other, direction, instantiate(get_substituted(ret, substitution)), positions,
        "Use of attribute {} matches actual type".format(attr.name))]


def satisfy_expr(expr, other_expr, direction, positions, substitution):
    if isinstance(other_expr, UnionType):
        # Special case for A = [A | B], which
        # simplifies to A = B
        subtypes = other_expr.subtypes
        new_subtypes = filter(lambda x: x != expr, subtypes)
        if new_subtypes != subtypes:
            if len(new_subtypes) > 1:
                other_expr = UnionType(new_subtypes)
            else:
                other_expr = new_subtypes[0]
    if expr != other_expr:
        update_substitution(substitution, expr, other_expr, positions)
    return []


def satisfy_function(a, b, direction, positions, substitution):
    new_constraints = []
    if not isinstance(b, FunctionType):
        raise NotTypeError("Types mismatch: %s != %s" % (get_substituted(a, substitution), get_substituted(b, substitution)), positions)
    if len(a.args) != len(b.args):
        raise NotTypeError("Types mismatch: %s != %s, argument lengths differ" % (get_substituted(a, substitution), get_substituted(b, substitution)), positions)
    for i, arg in enumerate(a.args):
        new_constraints.insert(0, Constraint(arg, direction, b.args[i], positions, "Type of argument {} matches".format(i)))
    new_constraints.insert(0, Constraint(a.rtype, direction, b.rtype, positions, "Return type matches"))
    return new_constraints


def satisfy_parameterised_type(a, b, direction, positions, substitution):
    new_constraints = []
    if not isinstance(b, ParameterisedType):
        if b.__class__ == Type:
            if unify_types(a.types[0], b, direction) is not None:
                return []
        raise NotTypeError("Types mismatch: %s != %s" % (get_substituted(a, substitution), get_substituted(b, substitution)), positions)
    if len(a.types) != len(b.types):
        raise NotTypeError("Types mismatch: %s != %s, wrong number of type params" % (get_substituted(a, substitution), get_substituted(b, substitution)), positions)
    for i, arg in enumerate(a.types):
        new_constraints.insert(0, Constraint(arg, direction, b.types[i], positions, "subtype {} matches".format(i)))
    return new_constraints


def satisfy_union_type(u, other, direction, positions, substitution):
    new_constraints = []
    for t in u.subtypes:
        new_constraints.append(Constraint(t, direction, other, positions, "matches each of the types in the union"))
    return new_constraints


def satisfy_constraint(constraint, substitution, trace=False):
    """Attempt to satisfy a single constraint based on a substitution.

    This will raise an error if it can't do it, or update the substitution
    if there is new information.

    The function will return a list of additional constraints that
    should be checked.
    """
    if trace:
        _trace("Processing constraint " + debug.coloured(constraint, debug.colours.BROWN) + " against " + colour_substitution(substitution))
    direction = constraint.constraint
    positions = constraint.positions
    a = constraint.a
    b = constraint.b
    why = constraint.why
    while a in substitution:
        a, newpos = substitution[a]
        positions = positions + newpos
    if b in substitution:
        b, newpos = substitution[b]
        positions = positions + newpos
    if not isinstance(a, TypeExpr) and isinstance(b, TypeExpr):
        return [Constraint(b, INVERSE_CONSTRAINT[direction], a, positions, why)]
    if isinstance(b, AttributeAccess):
        return satisfy_attribute_access(b, a, direction, positions, substitution)
    elif isinstance(a, TypeExpr):
        return satisfy_expr(a, b, direction, positions, substitution)
    elif isinstance(a, FunctionType):
        return satisfy_function(a, b, direction, positions, substitution)
    elif isinstance(a, ParameterisedType):
        return satisfy_parameterised_type(a, b, direction, positions, substitution)
    elif isinstance(b, ParameterisedType):
        return satisfy_parameterised_type(b, a, INVERSE_CONSTRAINT[direction], positions, substitution)
    elif isinstance(b, UnionType):
        return satisfy_union_type(b, a, INVERSE_CONSTRAINT[direction], positions, substitution)
    else:
        newtype = unify_types(a, b, direction)
        if newtype is None:
            raise NotTypeError("Type mismatch: %s is not a %s %s" % (get_substituted(a, substitution), direction, get_substituted(b, substitution)), positions)
        if isinstance(constraint.a, TypeExpr):
            update_substitution(substitution, constraint.a, newtype, positions)
    return []


def satisfy_constraints(constraints, initial_subtitution=None, trace=False):
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
        constraints = satisfy_constraint(constraint, substitution, trace=trace) + constraints
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
    if a.__class__ != b.__class__:
        return None
    a_bases = (a,) + a.bases
    b_bases = (b,) + b.bases
    if constraint == SUPERTYPE_OF:
        target = a
        sources = b_bases
    else:
        target = b
        sources = a_bases
    for base in sources:
        if base == target:
            return base


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
    if isinstance(rhs, UnionType):
        for subtype in rhs.subtypes:
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
    assert rhs is not None, "{} is said to be None, not a valid type".format(lhs)
    if occurs(lhs, rhs):
        raise NotTypeError("Recursive type definition: %s = %s" % (get_substituted(lhs, substitution), get_substituted(rhs, substitution)), positions)
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
        # TODO: UnionType
        return a
    for other_lhs, (other_rhs, other_positions) in substitution.items():
        if lhs != other_lhs:
            substitution[other_lhs] = (replace_in(other_rhs, lhs, rhs), other_positions)


def get_substituted(var, substitutions):
    """Get the substituted form of var.

    This applies any substituations for var, recursing if var is a compound
    type after substitutions.

    Given a type, this will get the best information that the substitution
    has about what type it is.
    """
    while var in substitutions:
        var = substitutions[var][0]
    if isinstance(var, FunctionType):
        var = FunctionType([get_substituted(a, substitutions) for a in var.args], get_substituted(var.rtype, substitutions))
    elif isinstance(var, ParameterisedType):
        var = ParameterisedType([get_substituted(a, substitutions) for a in var.types])
    elif isinstance(var, UnionType):
        var = UnionType([get_substituted(a, substitutions) for a in var.subtypes]).reduce()
    return var


def make_letter_generator():
    count = 0
    while True:
        remainder = count
        string = ""
        mod = remainder % 26
        string = chr(mod + ord('a')) + string
        remainder /= 26
        while remainder > 0:
            mod = (remainder % 26) - 1
            string = chr(mod + ord('a')) + string
            remainder /= 26
        yield string
        count += 1


def generalise(ftype, vars=None):
    """Take a Type and generalise it.

    This transforms any unconstrained variables in the type
    signature and turns them in to TypeVariables.

    It will recurse in to nested FunctionTypes.

    A repeated TypeExpr will be assiged the same TypeVariables for
    each occurence.
    """
    if vars is None:
        vars = {}
    else:
        vars = vars.copy()
    next_name = make_letter_generator()
    def _generalise(var, generalise=True):
        if isinstance(var, TypeExpr):
            if generalise:
                if var in vars:
                    var = vars[var]
                else:
                    var = vars.setdefault(var, TypeVariable(next(next_name)))
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
            # Can we put this in the prelude?
            rtype = Type("None")
        return FunctionType(args, rtype)
    return ftype


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
    def do_instantiate(ftype, subs):
        newt = ftype
        if ftype in subs:
            newt = subs[ftype]
        elif isinstance(ftype, TypeVariable):
            newt = TypeExpr(ftype.name)
            subs[ftype] = newt
        elif isinstance(ftype, FunctionType):
            new_args = []
            for arg in ftype.args:
                new_args.append(do_instantiate(arg, subs))
            new_rtype = do_instantiate(ftype.rtype, subs)
            newt = FunctionType(new_args, new_rtype)
            subs[ftype] = newt
        elif isinstance(ftype, ParameterisedType):
            new_types = []
            base = ftype.types[0]
            new_base = Type(base.name, bases=base.bases)
            for arg in ftype.types[1:]:
                new_types.append(do_instantiate(arg, subs))
            newt = ParameterisedType([new_base] + new_types)
            subs[ftype] = newt
            new_attrs = {}
            for name, t in base.attrs.items():
                new_attrs[name] = do_instantiate(t, subs)
            new_base.attrs = new_attrs
        return newt
    return do_instantiate(ftype, {})


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


def read_prelude():
    basedir = os.path.dirname(os.path.abspath(__file__))
    prelude_path = os.path.join(basedir, "stdlib", "_prelude.not")
    with open(prelude_path) as f:
        return f.read()


def env_from_prelude(trace=False):
    base_env = TypeEnv('main')
    prelude_source = read_prelude()
    new_env, subst = _typecheck(parsing.parse(prelude_source), base_env, trace=trace)
    int_t = new_env.get_type('int')
    bool_t = new_env.get_type('bool')
    new_env.extend(">", FunctionType([int_t, int_t], bool_t), [])
    new_env.extend("-", FunctionType([int_t, int_t], int_t), [])
    new_env.extend("+", FunctionType([int_t, int_t], int_t), [])
    new_env.extend("*", FunctionType([int_t, int_t], int_t), [])
    new_env.extend("==", FunctionType([int_t, int_t], bool_t), [])
    return new_env


def typecheck(node, trace=False):
    base_env = env_from_prelude(trace=trace)
    return _typecheck(node, base_env, trace=trace)


def generalise_functions(base_env, subst, names, ftypes, trace=False):
    for name in names:
        name_parts = name.split('.')
        base_t = get_substituted(base_env.env[name_parts[0]][0], subst)
        if len(name_parts) > 1:
            parent = None
            target = base_t
            vars = {}
            env = base_env.children[name_parts[0]]
            for name_part in name_parts[1:]:
                env = env.children[name_part]
                if isinstance(target, FunctionType):
                    target = target.rtype
                if isinstance(target, ParameterisedType):
                    for t in target.types[1:]:
                        vars[env.types[t.name]] = t
                parent = target
                if name_part in target.attrs:
                    target = target.attrs[name_part]
                else:
                    target = None
                    break
            if target is not None:
                parent.attrs[name_parts[-1]] = generalise(get_substituted(target, subst), vars=vars)
                if trace:
                    _trace("{} has type {}".format(debug.coloured(name, debug.colours.BLUE), debug.coloured(parent.attrs[name_parts[-1]], debug.colours.GREEN)))
        else:
            base_env.env[name] = (generalise(base_t), [], True)
            if name in base_env.types:
                if isinstance(base_t, FunctionType):
                    base_env.types[name] = base_t.rtype
                else:
                    base_env.types[name] = base_t
            if trace:
                _trace("{} has type {}".format(name, base_env.env[name][0]))


def _typecheck(node, base_env, trace=False):
    pass1= FirstPass()
    pass1.dispatch(node)
    pass2 = SecondPass(None, {a: a for a in pass1.functions}, pass1)
    pass2.dispatch(node)
    all_functions = get_all_functions(pass1)
    sets = graph.get_disjoint_sets(pass2.callgraph, all_functions)
    if trace:
        _trace("Type-context sets: [" + ", ".join(map(lambda x: debug.colour_list(x, debug.colours.BLUE), sets)) + "]")
    handled = set()
    subst = {}
    for s in sets:
        if trace:
            _trace(debug.coloured("Processing type-context(s) ", debug.colours.BOLD) + debug.colour_list(s, debug.colours.BLUE))
        handled.update(s)
        ftypes = {}
        for name in s:
            if "." not in name:
                # Should use the sourcepos of the name
                ftypes[name] = base_env.register_with_new_expr(name, [], inherit=True)
        pass3 = ThirdPass(base_env, False, pass1, only_process=s)
        constraints, t = pass3.dispatch(node)
        if trace:
            _trace("Gathered constraints: " + debug.colour_list(constraints, debug.colours.BROWN))
        subst = satisfy_constraints(constraints, initial_subtitution=subst, trace=trace)
        generalise_functions(base_env, subst, s, ftypes, trace=trace)
    if trace:
        _trace(debug.coloured("Processing remainder", debug.colours.BOLD))
    pass3 = ThirdPass(base_env, True, pass1, skip=handled)
    constraints, t = pass3.dispatch(node)
    if trace:
        _trace("Gathered Constraints: " + debug.colour_list(constraints, debug.colours.BROWN))
    subst = satisfy_constraints(constraints, initial_subtitution=subst, trace=trace)
    if trace:
        _trace("Substitution: " + colour_substitution(subst))
    return base_env, subst


def colour_substitution(s):
    return debug.colour_dict(s, debug.colours.GREEN, debug.colours.GREEN, value_fn=lambda x: "(" + debug.coloured(repr(x[0]), debug.colours.BROWN) + ", " + repr(x[1]) + ")")


def _trace(message):
    sys.stderr.write("type: " + message + "\n")
