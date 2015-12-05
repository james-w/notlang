from testtools import TestCase
from testtools.content import text_content
from testtools.matchers import Equals, Is, MatchesListwise
from rpython.rlib.parsing.lexer import SourcePos
from rpython.rlib.parsing.parsing import ParseError

from .. import parsing, testing, typer


class SatisfyConstraintsTests(TestCase):

    spos = SourcePos(0, 0, 0)

    def satisfy_constraints(self, constraints):
        return typer.satisfy_constraints(constraints)

    def test_empty(self):
        self.satisfy_constraints([])

    def test_trivially_equal(self):
        t = typer.Type("a")
        self.satisfy_constraints([typer.Constraint(t, typer.SUPERTYPE_OF, t, [self.spos], "")])

    def test_trivially_not_equal(self):
        t1 = typer.Type("a")
        t2 = typer.Type("b")
        self.assertRaises(typer.NotTypeError, self.satisfy_constraints,
            [typer.Constraint(t1, typer.SUPERTYPE_OF, t2, [self.spos], "")])

    def test_not_equal_across_two(self):
        vartype = typer.TypeExpr("a")
        t1 = typer.Type("b")
        t2 = typer.Type("c")
        self.assertRaises(typer.NotTypeError, self.satisfy_constraints,
            [typer.Constraint(vartype, typer.SUPERTYPE_OF, t1, [self.spos], ""),
             typer.Constraint(vartype, typer.SUPERTYPE_OF, t2, [self.spos], "")])

    def test_not_equal_across_three(self):
        vartype1 = typer.TypeExpr("a")
        vartype2 = typer.TypeExpr("b")
        t1 = typer.Type("c")
        t2 = typer.Type("d")
        self.assertRaises(typer.NotTypeError, self.satisfy_constraints,
            [typer.Constraint(vartype1, typer.SUPERTYPE_OF, t1, [self.spos], ""),
             typer.Constraint(vartype2, typer.SUPERTYPE_OF, vartype1, [self.spos], ""),
             typer.Constraint(vartype2, typer.SUPERTYPE_OF, t2, [self.spos], "")])

    def test_basic_type_vs_incompatible(self):
        t1 = typer.Type("a")
        t2 = typer.Type("b")
        self.assertRaises(
            typer.NotTypeError,
            typer.satisfy_constraint,
            typer.Constraint(t1, typer.SUPERTYPE_OF, t2, [], ""),
            {})

    def test_basic_type_vs_compatible(self):
        t1 = typer.Type("a")
        substitution = {}
        ret = typer.satisfy_constraint(
                typer.Constraint(t1, typer.SUPERTYPE_OF, t1, [], ""),
                substitution)
        self.assertEqual([], ret)
        self.assertEqual({}, substitution)

    def test_function_type_vs_non_function(self):
        t1 = typer.Type("a")
        a = typer.FunctionType([], t1)
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        self.assertRaises(
            typer.NotTypeError,
            typer.satisfy_constraint,
            typer.Constraint(a, typer.SUPERTYPE_OF, t1, constraint_pos, ""),
            substitution)

    def test_function_type_vs_function_type(self):
        t1 = typer.Type("a")
        t2 = typer.Type("b")
        t3 = typer.Type("c")
        t4 = typer.Type("d")
        a = typer.FunctionType([t3], t1)
        b = typer.FunctionType([t4], t2)
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        ret = typer.satisfy_constraint(
            typer.Constraint(a, typer.SUPERTYPE_OF, b, constraint_pos, ""),
            substitution)
        self.assertEqual(2, len(ret))
        self.assertThat(
            ret[0],
            testing.ConstraintMatches(
                Is(t1),
                typer.SUPERTYPE_OF,
                Is(t2),
                [Is(constraint_pos[0])]))
        self.assertThat(
            ret[1],
            testing.ConstraintMatches(
                Is(t3),
                typer.SUPERTYPE_OF,
                Is(t4),
                [Is(constraint_pos[0])]))
        self.assertEqual({}, substitution)

    def test_function_type_different_arg_count(self):
        t1 = typer.Type("a")
        t2 = typer.Type("b")
        a = typer.FunctionType([], t1)
        b = typer.FunctionType([t1], t2)
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        self.assertRaises(
            typer.NotTypeError,
            typer.satisfy_constraint,
            typer.Constraint(a, typer.SUPERTYPE_OF, b, constraint_pos, ""),
            substitution)

    def test_type_expr_subsitute(self):
        t1 = typer.Type("a")
        a = typer.TypeExpr('a')
        b = typer.TypeExpr('b')
        subs_pos = [SourcePos(1, 1, 1)]
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {a: (b, subs_pos)}
        ret = typer.satisfy_constraint(
                typer.Constraint(a, typer.SUPERTYPE_OF, t1, constraint_pos, ""),
                substitution)
        self.assertEqual(0, len(ret))
        self.assertEqual(2, len(substitution))
        self.assertEqual((t1, constraint_pos + subs_pos), substitution[b])

    def test_type_expr_updates_substitution(self):
        a = typer.TypeExpr('a')
        b = typer.TypeExpr('b')
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        ret = typer.satisfy_constraint(
                typer.Constraint(a, typer.SUPERTYPE_OF, b, constraint_pos, ""),
                substitution)
        self.assertEqual([], ret)
        self.assertEqual({a: (b, constraint_pos)}, substitution)

    def test_swaps_type_expr_vs_anything(self):
        a = typer.Type('foo')
        b = typer.TypeExpr('bar')
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        ret = typer.satisfy_constraint(
                typer.Constraint(a, typer.SUPERTYPE_OF, b, constraint_pos, ""),
                substitution)
        self.assertThat(
            ret,
            MatchesListwise([
                testing.ConstraintMatches(Is(b), typer.SUBTYPE_OF, Is(a), [Is(constraint_pos[0])])]))
        self.assertEqual({}, substitution)

    def test_getattr_no_attr(self):
        t1 = typer.Type("a")
        a = typer.Type("b")
        b = typer.AttributeAccess(t1, 'a')
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        self.assertRaises(
            typer.NotTypeError,
            typer.satisfy_constraint,
                typer.Constraint(a, typer.SUPERTYPE_OF, b, constraint_pos, ""),
                substitution)

    def test_getattr_has_attr(self):
        t1 = typer.Type("a")
        c = typer.Type('c', {'a': t1})
        a = typer.AttributeAccess(c, 'a')
        b = t1
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        ret = typer.satisfy_constraint(
                typer.Constraint(b, typer.SUBTYPE_OF, a, constraint_pos, ""),
                substitution)
        self.assertEqual(1, len(ret))
        self.assertThat(
            ret[0],
            testing.ConstraintMatches(
                Is(b), typer.SUBTYPE_OF, Is(t1), [Is(constraint_pos[0])]))
        self.assertEqual({}, substitution)

    def test_getattr_has_attr_via_substitution(self):
        t1 = typer.Type("a")
        c = typer.Type('c', {'a': t1})
        d = typer.TypeExpr('d')
        a = typer.AttributeAccess(d, 'a')
        b = t1
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {d: (c, [])}
        ret = typer.satisfy_constraint(
                typer.Constraint(b, typer.SUBTYPE_OF, a, constraint_pos, ""),
                substitution)
        self.assertEqual(1, len(ret))
        self.assertThat(
            ret[0],
            testing.ConstraintMatches(
                Is(b), typer.SUBTYPE_OF, Is(t1), [Is(constraint_pos[0])]))
        self.assertEqual({d: (c, [])}, substitution)


def get_type_of(name, source):
    parsed = parsing.parse(source)
    env, substitutions = typer.typecheck(parsed, trace=True)
    return typer.get_substituted(env.env[name][0], substitutions), env


class IntegrationTests(TestCase):

    def test_int(self):
        t, env = self.get_type('a', 'a = 1\n')
        self.assertThat(t, Is(env.get_type('int')))

    def test_int_by_inference(self):
        t, env = self.get_type('a', 'b = 1\na = b\n')
        self.assertThat(t, Is(env.get_type('int')))

    def test_return_int(self):
        t, env = self.get_type('a', 'def a():\n    return 1\n\n')
        self.assertThat(t, testing.IsFunctionType([], Is(env.get_type('int'))))

    def test_return_arg(self):
        ftype, env = self.get_type('a', 'def a(b):\n    return b\n\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType([Is(ftype.rtype)], testing.IsTypeVariable('a')))

    def test_type_not_bound(self):
        ftype, env = self.get_type('a', 'def a(b):\n    return b\n\na(1)\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType([Is(ftype.rtype)], testing.IsTypeVariable('a')))

    def test_instantiate(self):
        ftype, env = self.get_type('a', 'def a(b):\n    return b\n\na(1)\na(a)\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType([Is(ftype.rtype)], testing.IsTypeVariable('a')))

    def test_higher_order(self):
        ftype, env = self.get_type('a', 'def a(b, c):\n    return b(c)\n\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                [testing.IsFunctionType(
                    [Is(ftype.args[1])],
                    Is(ftype.rtype)),
                testing.IsTypeVariable('a')],
                testing.IsTypeVariable('b')))

    def test_recursive(self):
        ftype, env = self.get_type('a', 'def a(b):\n    if b > 0:\n        return a(b-1)\n    else:\n        return b\n\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                [Is(env.get_type('int'))],
                Is(env.get_type('int'))))

    def test_mutually_recursive(self):
        ftype, env = self.get_type('a', """
def a(b):
    return c(b)

def c(x):
    if x > 0:
        return a(x-1)
    else:
        return x

""")
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                [Is(env.get_type('int'))],
                Is(env.get_type('int'))))

    def test_infinite_recursion(self):
        ftype, env = self.get_type('a', """
def a(b):
    return a(b)

""")
        self.assertThat(ftype, testing.IsFunctionType([testing.IsTypeVariable('a')], testing.IsType('None')))

    def test_inferred_from_other(self):
        ftype, env = self.get_type('a', """
def a(b):
    return c(b)

def c(x):
    return x + 1

""")
        self.assertThat(
            ftype,
            testing.IsFunctionType([Is(env.get_type('int'))], Is(env.get_type('int'))))

    def test_use_of_higher_order(self):
        ftype, env = self.get_type('e', """
def b(x):
    return x

def a(c, d):
    return c(d)

e = a(b, 1)
""")
        self.assertThat(ftype, Is(env.get_type('int')))

    def test_new_type(self):
        ftype, env = self.get_type('Dog', """
Dog = new Type:
    pass

""")
        self.assertThat(ftype, testing.IsFunctionType([], testing.IsType('Dog')))

    def test_new_type_in_signature(self):
        ftype, env = self.get_type('foo', """
Dog = new Type:
    pass

def foo(d: Dog):
    return d

""")
        self.assertThat(ftype, testing.IsFunctionType([Is(ftype.rtype)], testing.IsType("Dog")))

    def test_parameterised_type(self):
        ftype, env = self.get_type('foo', """
Thing = new Type<a>:
    pass

foo = Thing()
""")
        self.assertThat(ftype, testing.IsParametricType([testing.IsType('Thing'), testing.IsTypeExpr('a')]))

    def test_parameterised_type_instantiated(self):
        # Double define foo to be sure that Thing<int> == Thing<int>
        # when instatiated in different places
        ftype, env = self.get_type('foo', """
Thing = new Type<a>:
    pass

foo = Thing<int>()
foo = Thing<int>()
""")
        self.assertThat(ftype, testing.IsParametricType([testing.IsType('Thing'), Is(env.get_type('int'))]))

    def test_different_parameterised_types(self):
        ftype, env = self.get_type('foo', """
Thing = new Type<a>:
    pass

foo = Thing<int>()
foo = Thing<bool>()
""")
        self.assertThat(ftype, testing.IsParametricType([testing.IsType('Thing'), Is(env.get_type('bool'))]))

    def test_parameterised_type_as_argtype(self):
        ftype, env = self.get_type('foo', """
Thing = new Type<a>:
    pass


def foo(a: Thing<int>):
    return a
""")
        self.assertThat(ftype.args[0], testing.IsParametricType([testing.IsType('Thing'), Is(env.get_type('int'))]))

    def get_type(self, name, source):
        self.addDetail('source', text_content(source))
        try:
            return get_type_of(name, source)
        except ParseError as e:
            err = e.nice_error_message(source=source)
            self.addDetail('parse_error', text_content(err))
            raise
        except typer.NotTypeError as e:
            err = e.nice_error_message(source=source)
            self.addDetail('type_error', text_content(err))
            raise
        except typer.NotNameError as e:
            err = e.nice_error_message(source=source)
            self.addDetail('name_error', text_content(err))
            raise

    def test_attribute_access(self):
        source = """
Thing = new Type:
    a = 1

bar = Thing()
foo = bar.a
"""
        ftype, env = self.get_type('foo', source)
        self.assertThat(ftype, Is(env.get_type('int')))

    def test_attribute_access_on_returned_value(self):
        ftype, env = self.get_type('foo', """
Thing = new Type:
    a = 1

def thing():
    return Thing()

foo = thing().a
""")
        self.assertThat(ftype, Is(env.get_type('int')))

    def test_method_call(self):
        ftype, env = self.get_type('bar', """
Thing = new Type:

    def foo(self):
        return 1

bar = Thing().foo()
""")
        self.assertThat(ftype, Is(env.get_type('int')))

    def test_enum(self):
        ftype, env = self.get_type('Answer', """
Answer = new Enum(Y, N):
    pass

""")
        self.assertThat(ftype, testing.IsType('Answer'))

    def test_enum_value(self):
        # Assign both values to the same var to be sure
        # that they are considered the same type.
        ftype, env = self.get_type('y', """
Answer = new Enum(Y, N):
    pass

y = Answer.Y
y = Answer.N

""")
        self.assertThat(ftype, testing.IsType('Answer'))

    def test_tuple(self):
        ftype, env = self.get_type('f', """
Thing = new Tuple(int, int):
    pass

f = Thing(1, 1)
""")
        self.assertThat(ftype, testing.IsType('Thing'))

    def test_enum_return(self):
        ftype, env = self.get_type('f', """
Answer = new Enum(Y, N):
    pass


def f():
    if 1 == 1:
        return Answer.Y
    else:
        return Answer.N
""")
        self.assertThat(ftype, testing.IsFunctionType([], testing.IsType("Answer")))

    def test_branching_variable_type(self):
        # A branch with differing variable types
        # leads to an ad-hoc union type.
        ftype, env = self.get_type('foo', """
foo = 1
if foo > 0:
    foo = 1 == 2
""")
        self.assertThat(
            ftype,
            testing.IsUnionType([Is(env.get_type('int')), Is(env.get_type('bool'))]))

    def test_branching_variable_type_unifies(self):
        # A union type from branching is reduced
        # if each branch has the same type
        ftype, env = self.get_type('foo', """
foo = 1
if foo > 0:
    foo = 1 == 2
else:
    foo = 1 == 2
""")
        self.assertThat(
            ftype,
            Is(env.get_type('bool')))

    def test_branching_defines_undeclared(self):
        # A set of blocks with full coverage can
        # define a new variable
        ftype, env = self.get_type('foo', """
if 1 > 0:
    foo = 1 == 2
else:
    foo = 1 == 2
""")
        self.assertThat(
            ftype,
            Is(env.get_type('bool')))

    def test_branching_doesnt_leak_undeclared(self):
        # A leaked var from a block isn't available
        # afterwards if it doesn't have full
        # coverage
        e = self.assertRaises(typer.NotNameError, self.get_type, 'foo', """
if 1 > 0:
    foo = 1 == 2
foo + 1
""")
        self.assertEqual(28, e.positions[0].i)

    def test_use_a_union_type(self):
        ftype, env = self.get_type('bar', """
def id(a):
    return a

foo = 1
if 1 > 0:
    foo = 1 == 2
bar = id(foo)
""")
        self.assertThat(
            ftype,
            testing.IsUnionType([Is(env.get_type('int')), Is(env.get_type('bool'))]))

    def test_use_a_union_type_wrong(self):
        e = self.assertRaises(typer.NotTypeError, self.get_type, 'foo', """
foo = 1
if 1 > 0:
    foo = 1 == 2
foo + 1
""")
        self.assertEqual(36, e.positions[0].i)

    def test_return_ends_checking(self):
        ftype, env = self.get_type('foo', """
foo = 1
if 1 > 0:
    foo = 1 == 2
    return foo
foo + 1
""")
        self.assertThat(ftype, Is(env.get_type('int')))

    def test_return_from_a_case(self):
        ftype, env = self.get_type('x', """
Answer = new Enum(Y, N):

    def is_yes(self):
        case self:
            Answer.Y:
                return 1
            Answer.N:
                return 0

y = Answer.Y

x = y.is_yes()
""")
        self.assertThat(ftype, Is(env.get_type('int')))


class InstantiateTests(TestCase):

    def test_types(self):
        t1 = typer.Type("a")
        self.assertThat(
            typer.instantiate(typer.FunctionType([t1], t1)),
            testing.IsFunctionType(
                [Is(t1)],
                Is(t1))
            )

    def test_vars(self):
        tvar = typer.TypeVariable('a')
        ret = typer.instantiate(typer.FunctionType([tvar], tvar))
        self.assertThat(
            ret,
            testing.IsFunctionType(
                [Is(ret.rtype)],
                testing.IsTypeExpr(tvar.name))
            )

    def test_nested(self):
        tvar1 = typer.TypeVariable('a')
        tvar2 = typer.TypeVariable('b')
        ret = typer.instantiate(
            typer.FunctionType(
                [typer.FunctionType(
                    [tvar1],
                    tvar2),
                 tvar1,
                ],
                tvar2))
        argtype = ret.args[1]
        self.assertThat(
            ret,
            testing.IsFunctionType(
                [testing.IsFunctionType(
                    [Is(argtype)],
                    Is(ret.rtype)),
                 testing.IsTypeExpr(tvar1.name)],
                testing.IsTypeExpr(tvar2.name))
            )

    def test_recursive_attrs(self):
        t = typer.ParameterisedType([typer.Type('a'), typer.TypeVariable('b')])
        t.types[0].attrs = dict(child=t)
        ret = typer.instantiate(t)
        self.assertThat(
            ret,
            testing.IsParametricType(
                [testing.IsType(t.types[0].name), testing.IsTypeExpr(t.types[1].name)]
            ))
        self.assertThat(ret.types[0].attrs['child'], Is(ret))


class GeneraliseFunctionTests(TestCase):

    def test_no_vars(self):
        t1 = typer.Type("a")
        input = typer.FunctionType([t1], t1)
        ftype = typer.generalise(input)
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                [Is(t1)],
                Is(t1)))

    def test_one_var(self):
        t1 = typer.Type("a")
        argname = "bar"
        argtype = typer.TypeExpr(argname)
        input = typer.FunctionType([argtype], t1)
        ftype = typer.generalise(input)
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                [testing.IsTypeVariable('a')],
                Is(t1)))

    def test_one_var_repeated(self):
        argname = "bar"
        argtype = typer.TypeExpr(argname)
        input = typer.FunctionType([argtype], argtype)
        ftype = typer.generalise(input)
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                [Is(ftype.rtype)],
                testing.IsTypeVariable('a')))

    def test_nested_function(self):
        t1 = typer.Type("a")
        argname = "bar"
        argtype = typer.TypeExpr(argname)
        input = typer.FunctionType(
            [argtype, typer.FunctionType(
                [argtype],
                t1)],
            t1)
        ftype = typer.generalise(input)
        new_argtype = ftype.args[0]
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                [testing.IsTypeVariable('a'),
                testing.IsFunctionType([Is(new_argtype)], Is(t1))],
                Is(t1)))

    def test_unconstrained_rtype(self):
        t1 = typer.Type("a")
        input = typer.FunctionType([t1], typer.TypeExpr("bar"))
        ftype = typer.generalise(input)
        self.assertThat(ftype.rtype, testing.IsType('None'))


class SubstitutionTests(TestCase):

    def test_adds(self):
        substitution = {}
        positions = []
        t1 = typer.Type("t1")
        t2 = typer.Type("t2")
        typer.update_substitution(substitution, t1, t2, positions)
        self.assertEqual({t1: (t2, positions)}, substitution)

    def test_replaces(self):
        t1 = typer.Type("t1")
        t2 = typer.Type("t2")
        t3 = typer.Type("t3")
        substitution = {t1: (t2, [])}
        positions = []
        typer.update_substitution(substitution, t1, t3, positions)
        self.assertEqual({t1: (t3, positions)}, substitution)

    def test_updates_other_references(self):
        a = typer.TypeExpr('a')
        t1 = typer.Type("t1")
        t2 = typer.Type("t2")
        substitution = {t1: (a, [])}
        positions = []
        typer.update_substitution(substitution, a, t2, positions)
        self.assertEqual({t1: (t2, []), a: (t2, positions)}, substitution)

    def test_updates_other_references_in_ftype(self):
        a = typer.TypeExpr('a')
        f = typer.FunctionType([], a)
        t1 = typer.Type("t1")
        t2 = typer.Type("t2")
        substitution = {t1: (f, [])}
        positions = []
        typer.update_substitution(substitution, a, t2, positions)
        self.assertEqual(2, len(substitution))
        self.assertThat(substitution[a], Equals((t2, [])))
        self.assertThat(substitution[t1][0], testing.IsFunctionType([], Is(t2)))

    def test_occurs_check(self):
        substitution = {}
        positions = []
        t1 = typer.Type("t1")
        self.assertRaises(typer.NotTypeError, typer.update_substitution,
                substitution, t1, t1, positions)

    def test_get_substituded(self):
        t1 = typer.Type("t1")
        t2 = typer.Type("t2")
        substitution = {t1: (t2, [])}
        ret = typer.get_substituted(t1, substitution)
        self.assertIs(t2, ret)

    def test_get_substituded_transitive(self):
        t1 = typer.Type("t1")
        t2 = typer.Type("t2")
        t3 = typer.Type("t3")
        substitution = {t1: (t2, []), t2: (t3, [])}
        ret = typer.get_substituted(t1, substitution)
        self.assertIs(t3, ret)

    def test_get_substituded_function_type(self):
        t1 = typer.Type("t1")
        t2 = typer.Type("t2")
        t3 = typer.Type("t3")
        substitution = {t1: (typer.FunctionType([t2], t2), []), t2: (t3, [])}
        ret = typer.get_substituted(t1, substitution)
        self.assertThat(ret, testing.IsFunctionType([Is(t3)], Is(t3)))

    def test_occurs_same_type(self):
        t1 = typer.Type("t1")
        self.assertEqual(True, typer.occurs(t1, t1))

    def test_occurs_different_types(self):
        t1 = typer.Type("t1")
        t2 = typer.Type("t2")
        self.assertEqual(False, typer.occurs(t1, t2))

    def test_occurs_function_arg(self):
        t1 = typer.Type("t1")
        t2 = typer.Type("t2")
        self.assertEqual(True, typer.occurs(t1, typer.FunctionType([t1], t2)))

    def test_occurs_function_rtype(self):
        t1 = typer.Type("t1")
        t2 = typer.Type("t2")
        self.assertEqual(True, typer.occurs(t1, typer.FunctionType([t2], t1)))

    def test_occurs_parameterised_type(self):
        t1 = typer.Type("t1")
        t2 = typer.Type("t2")
        self.assertEqual(True, typer.occurs(t1, typer.ParameterisedType([t2, t1])))

    # TODO: test occurs UnionType


class UnifyTypeTests(TestCase):

    def test_same(self):
        t1 = typer.Type("a")
        self.assertIs(t1, typer.unify_types(t1, t1, typer.SUPERTYPE_OF))
        self.assertIs(t1, typer.unify_types(t1, t1, typer.SUBTYPE_OF))

    def test_different(self):
        t1 = typer.Type("a")
        t2 = typer.Type("b")
        self.assertIs(None, typer.unify_types(t1, t2, typer.SUPERTYPE_OF))
        self.assertIs(None, typer.unify_types(t1, t2, typer.SUBTYPE_OF))

    def test_different_classes(self):
        t1 = typer.Type("a")
        self.assertIs(None, typer.unify_types(t1, typer.TypeExpr("a"), typer.SUPERTYPE_OF))
        self.assertIs(None, typer.unify_types(t1, typer.TypeExpr("b"), typer.SUBTYPE_OF))

    def test_vs_any(self):
        t1 = typer.Type("a")
        t2 = typer.Type("b")
        self.assertIs(typer.ANY, typer.unify_types(typer.ANY, t1, typer.SUPERTYPE_OF))
        self.assertIs(None, typer.unify_types(t1, typer.ANY, typer.SUPERTYPE_OF))
        self.assertIs(typer.ANY, typer.unify_types(t2, typer.ANY, typer.SUBTYPE_OF))
        self.assertIs(None, typer.unify_types(typer.ANY, t1, typer.SUBTYPE_OF))

    def test_subtype(self):
        t1 = typer.Type('a')
        t2 = typer.Type('b', bases=(t1,))
        self.assertIs(t1, typer.unify_types(t1, t2, typer.SUPERTYPE_OF))
        self.assertIs(None, typer.unify_types(t1, t2, typer.SUBTYPE_OF))


class FirstPassTests(TestCase):

    def setUp(self):
        super(FirstPassTests, self).setUp()
        self.factory = testing.ASTFactory(self)

    def test_ConstantInt(self):
        node = self.factory.int()
        pass1 = typer.FirstPass()
        pass1.dispatch(node)
        self.assertEqual(set(), pass1.functions)
        self.assertEqual(set(), pass1.types)
        self.assertEqual({}, pass1.children)

    def test_NewType(self):
        varname = 'a'
        node = self.factory.assignment(target=self.factory.variable(name=varname), source=self.factory.newtype())
        pass1 = typer.FirstPass()
        pass1.dispatch(node)
        self.assertEqual(set([varname]), pass1.functions)
        self.assertEqual(set([varname]), pass1.types)
        self.assertEqual(1, len(pass1.children))
        self.assertIsInstance(pass1.children[varname], typer.FirstPass)

    def test_FuncDef(self):
        fname = 'a'
        node = self.factory.funcdef(name=fname)
        pass1 = typer.FirstPass()
        pass1.dispatch(node)
        self.assertEqual(set([fname]), pass1.functions)
        self.assertEqual(set(), pass1.types)
        self.assertEqual(1, len(pass1.children))
        self.assertIsInstance(pass1.children[fname], typer.FirstPass)


class SecondPassTests(TestCase):

    def setUp(self):
        super(SecondPassTests, self).setUp()
        self.factory = testing.ASTFactory(self)

    def test_ignores_non_function(self):
        pass2 = typer.SecondPass(None, {}, None)
        node = self.factory.variable(name='a')
        pass2.dispatch(node)
        self.assertEqual(set(), pass2.calls)

    def test_notes_function_call(self):
        varname = 'a'
        pass2 = typer.SecondPass(None, {varname: varname}, None)
        node = self.factory.variable(name=varname)
        pass2.dispatch(node)
        self.assertEqual(set([varname]), pass2.calls)

    def test_FuncDef(self):
        fname = "foo"
        pass1 = typer.FirstPass()
        pass1.children[fname] = typer.FirstPass()
        pass2 = typer.SecondPass(None, {}, pass1)
        node = self.factory.funcdef(name=fname)
        pass2.dispatch(node)
        self.assertEqual(set(), pass2.calls)
        self.assertEqual({}, pass2.callgraph)

    def test_FuncDef_that_makes_calls(self):
        fname = "foo"
        othername = "bar"
        pass1 = typer.FirstPass()
        pass1.children[fname] = typer.FirstPass()
        pass2 = typer.SecondPass(None, {othername: othername}, pass1)
        node = self.factory.funcdef(name=fname, body=self.factory.variable(name=othername))
        pass2.dispatch(node)
        self.assertEqual(set(), pass2.calls)
        self.assertEqual({fname: set([othername])}, pass2.callgraph)

    def test_FuncDef_that_makes_calls_to_nested(self):
        fname = "foo"
        othername = "bar"
        pass1 = typer.FirstPass()
        pass1.children[fname] = typer.FirstPass()
        pass1.children[fname].functions = {othername: othername}
        pass2 = typer.SecondPass(None, {}, pass1)
        node = self.factory.funcdef(name=fname, body=self.factory.variable(name=othername))
        pass2.dispatch(node)
        self.assertEqual(set(), pass2.calls)
        self.assertEqual({fname: set([fname + '.' + othername])}, pass2.callgraph)

    def test_Tuple_references_types(self):
        tname = "foo"
        othername = "bar"
        pass1 = typer.FirstPass()
        pass1.children[tname] = typer.FirstPass()
        pass1.children[tname].functions = {}
        pass2 = typer.SecondPass(None, {}, pass1)
        node = self.factory.assignment(
            target=self.factory.variable(tname),
            source=self.factory.tuple(types=[self.factory.type_option(name=othername)]))
        pass2.dispatch(node)
        self.assertEqual(set(), pass2.calls)
        self.assertEqual({tname: set([othername])}, pass2.callgraph)


class GetAllFunctionsTests(TestCase):

    def test_empty(self):
        pass1 = typer.FirstPass()
        self.assertEqual([], typer.get_all_functions(pass1))

    def test_direct_functions(self):
        fname = "foo"
        pass1 = typer.FirstPass()
        pass1.functions.add(fname)
        self.assertEqual([fname], typer.get_all_functions(pass1))

    def test_child_functions(self):
        fname = "foo"
        child_name = "bar"
        pass1 = typer.FirstPass()
        child = typer.FirstPass()
        child.functions.add(fname)
        pass1.children[child_name] = child
        self.assertEqual(
            [child_name + '.' + fname],
            typer.get_all_functions(pass1))


class ThirdPassTests(TestCase):

    def setUp(self):
        super(ThirdPassTests, self).setUp()
        self.factory = testing.ASTFactory(self)

    def get_third_pass(self, active, name_graph=None, only_handle=None):
        if only_handle is None:
            only_handle = []
        if name_graph is None:
            name_graph = typer.FirstPass()
        return typer.ThirdPass(
            typer.TypeEnv('main'), active, name_graph, only_process=only_handle)

    def test_ConstantInt(self):
        node = self.factory.int()
        checker = self.get_third_pass(True)
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(checker.env.get_type('int')))
        self.assertThat(constraints, Equals([]))

    def test_ConstantInt_disabled(self):
        node = self.factory.int()
        checker = self.get_third_pass(False)
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(constraints, Equals([]))

    def test_Variable_missing(self):
        node = self.factory.variable()
        checker = self.get_third_pass(True)
        self.assertRaises(typer.NotNameError, checker.dispatch, node)

    def test_Variable(self):
        node = self.factory.variable()
        checker = self.get_third_pass(True)
        t = typer.Type('a')
        checker.env.extend(node.varname, t, [])
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(t))
        self.assertThat(constraints, Equals([]))

    def test_Variable_missing_disabled(self):
        node = self.factory.variable()
        checker = self.get_third_pass(False)
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(constraints, Equals([]))

    def test_Variable_disabled(self):
        node = self.factory.variable()
        checker = self.get_third_pass(False)
        checker.env.extend(node.varname, typer.Type('a'), [])
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(constraints, Equals([]))

    def test_Pass(self):
        node = self.factory.pass_()
        checker = self.get_third_pass(True)
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(constraints, Equals([]))

    def test_Assignment(self):
        node = self.factory.assignment(source=self.factory.int())
        checker = self.get_third_pass(True)
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(checker.env.get_type('int')))
        self.assertThat(checker.env.env[node.var.varname][0], Is(t))
        self.assertThat(constraints, MatchesListwise([]))

    def test_Assignment_disabled(self):
        node = self.factory.assignment()
        checker = self.get_third_pass(False)
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(constraints, Equals([]))

    def test_Block(self):
        node = self.factory.block([self.factory.stmt(self.factory.assignment())])
        checker = self.get_third_pass(True)
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertEqual(0, len(constraints))

    def test_FuncDef_no_handle(self):
        fname = "foo"
        node = self.factory.funcdef(name=fname)
        checker = self.get_third_pass(False)
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(constraints, Equals([]))

    def test_FuncDef_should_handle(self):
        fname = "foo"
        varname = "bar"
        node = self.factory.funcdef(
            name=fname,
            body=self.factory.assignment(source=self.factory.variable(name=varname)),
            args=[varname])
        name_graph = typer.FirstPass()
        name_graph.children[fname] = typer.FirstPass()
        checker = self.get_third_pass(False, name_graph=name_graph, only_handle=[fname])
        ftype = checker.env.register_with_new_expr(fname, [])
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(len(constraints), Equals(1))
        child_env = checker.env.children[fname]
        self.assertThat(constraints[0], testing.ConstraintMatches(
            Is(ftype),
            typer.SUPERTYPE_OF,
            testing.IsFunctionType(
                [Is(child_env.env[varname][0])],
                Is(child_env.rtype)),
            [Is(self.factory.spos)],
            ))

    def test_Case(self):
        target = "a"
        label = "A"
        node = self.factory.case(
            target=self.factory.variable(name=target),
            cases=[self.factory.case_case(label=self.factory.variable(name=label))])
        checker = self.get_third_pass(True)
        ttype = checker.env.register_with_new_expr(target, [])
        ltype = checker.env.register_with_new_expr(label, [])
        constraints, t = checker.dispatch(node)
        self.assertThat(len(constraints), Equals(2))
        self.assertThat(constraints[0], testing.ConstraintMatches(
            Is(ttype),
            typer.SUBTYPE_OF,
            Is(typer.METATYPES['Enum']),
            [Is(self.factory.spos)],
            ))
        self.assertThat(constraints[1], testing.ConstraintMatches(
            Is(ttype),
            typer.SUPERTYPE_OF,
            Is(ltype),
            [Is(self.factory.spos)],
            ))

    def test_Tuple(self):
        tname = "A"
        aname = "B"
        atype = typer.Type(aname)
        node = self.factory.assignment(
            target=self.factory.variable(tname),
            source=self.factory.tuple(types=[self.factory.type_option(name=aname)]))
        name_graph = typer.FirstPass()
        name_graph.dispatch(node)
        checker = self.get_third_pass(True, only_handle=[tname], name_graph=name_graph)
        checker.env.register_type(aname, atype)
        placeholder_type = checker.env.register_with_new_expr(tname, [])
        constraints, t = checker.dispatch(node)
        self.assertThat(len(constraints), Equals(1))
        self.assertThat(t, testing.IsFunctionType([Is(atype)], Is(checker.env.get_type(tname))))
        self.assertThat(constraints[0], testing.ConstraintMatches(
            Is(placeholder_type),
            typer.SUPERTYPE_OF,
            Is(t),
            [Is(self.factory.spos)],
            ))
        self.assertIn('first', t.rtype.attrs)
        self.assertThat(t.rtype.attrs['first'], testing.IsFunctionType([], Is(atype)))


class LetterGeneratorTests(TestCase):

    def test_one(self):
        gen = typer.make_letter_generator()
        self.assertEqual('a', next(gen))

    def test_twenty_seven(self):
        gen = typer.make_letter_generator()
        for _ in range(26):
            next(gen)
        self.assertEqual('aa', next(gen))

    def test_twenty_eight(self):
        gen = typer.make_letter_generator()
        for _ in range(27):
            next(gen)
        self.assertEqual('ab', next(gen))

    def test_fifty_three(self):
        gen = typer.make_letter_generator()
        for _ in range(52):
            next(gen)
        self.assertEqual('ba', next(gen))
