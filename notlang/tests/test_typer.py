from testtools import TestCase
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
        self.satisfy_constraints([typer.Constraint(typer.INT, typer.SUPERTYPE_OF, typer.INT, [self.spos])])

    def test_trivially_not_equal(self):
        self.assertRaises(typer.NotTypeError, self.satisfy_constraints,
            [typer.Constraint(typer.NONE, typer.SUPERTYPE_OF, typer.INT, [self.spos])])

    def test_not_equal_across_two(self):
        vartype = typer.TypeVariable("a")
        self.assertRaises(typer.NotTypeError, self.satisfy_constraints,
            [typer.Constraint(vartype, typer.SUPERTYPE_OF, typer.INT, [self.spos]),
             typer.Constraint(vartype, typer.SUPERTYPE_OF, typer.NONE, [self.spos])])

    def test_not_equal_across_three(self):
        vartype1 = typer.TypeVariable("a")
        vartype2 = typer.TypeVariable("b")
        self.assertRaises(typer.NotTypeError, self.satisfy_constraints,
            [typer.Constraint(vartype1, typer.SUPERTYPE_OF, typer.INT, [self.spos]),
             typer.Constraint(vartype2, typer.SUPERTYPE_OF, vartype1, [self.spos]),
             typer.Constraint(vartype2, typer.SUPERTYPE_OF, typer.NONE, [self.spos])])

    def test_basic_type_vs_incompatible(self):
        self.assertRaises(
            typer.NotTypeError,
            typer.satisfy_constraint,
            typer.Constraint(typer.INT, typer.SUPERTYPE_OF, typer.BOOL, []),
            {})

    def test_basic_type_vs_compatible(self):
        substitution = {}
        ret = typer.satisfy_constraint(
                typer.Constraint(typer.INT, typer.SUPERTYPE_OF, typer.INT, []),
                substitution)
        self.assertEqual([], ret)
        self.assertEqual({}, substitution)

    def test_basic_type_vs_type_expr_substitute(self):
        a = typer.TypeExpr('a')
        b = typer.TypeExpr('b')
        subs_pos = [SourcePos(1, 1, 1)]
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {a: (b, subs_pos)}
        ret = typer.satisfy_constraint(
                typer.Constraint(typer.INT, typer.SUPERTYPE_OF, a, constraint_pos),
                substitution)
        self.assertEqual(1, len(ret))
        self.assertThat(
            ret[0],
            testing.ConstraintMatches(
                Is(typer.INT),
                typer.SUPERTYPE_OF,
                Is(b),
                [Is(constraint_pos[0]), Is(subs_pos[0])]))
        self.assertEqual(1, len(substitution))

    def test_basic_type_vs_type_expr_no_substitute(self):
        a = typer.TypeExpr('a')
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        ret = typer.satisfy_constraint(
                typer.Constraint(typer.INT, typer.SUPERTYPE_OF, a, constraint_pos),
                substitution)
        self.assertEqual([], ret)
        self.assertEqual({a: (typer.INT, constraint_pos)}, substitution)

    def test_function_type_vs_non_function(self):
        a = typer.FunctionType([], typer.INT)
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        self.assertRaises(
            typer.NotTypeError,
            typer.satisfy_constraint,
            typer.Constraint(a, typer.SUPERTYPE_OF, typer.INT, constraint_pos),
            substitution)

    def test_function_type_vs_expr(self):
        a = typer.FunctionType([], typer.INT)
        b = typer.TypeExpr('b')
        constraint_pos = [SourcePos(2, 2, 2)]
        subs_pos = [SourcePos(1, 1, 1)]
        substitution = {b: (typer.INT, subs_pos)}
        ret = typer.satisfy_constraint(
                typer.Constraint(a, typer.SUPERTYPE_OF, b, constraint_pos),
                substitution)
        self.assertEqual(1, len(ret))
        self.assertThat(ret[0], testing.ConstraintMatches(
            Is(a), typer.SUPERTYPE_OF, Is(typer.INT), [Is(constraint_pos[0]), Is(subs_pos[0])]))
        self.assertEqual({b: (typer.INT, subs_pos)}, substitution)

    def test_function_type_vs_function_type(self):
        a = typer.FunctionType([typer.NONE], typer.INT)
        b = typer.FunctionType([typer.ANY], typer.BOOL)
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        ret = typer.satisfy_constraint(
            typer.Constraint(a, typer.SUPERTYPE_OF, b, constraint_pos),
            substitution)
        self.assertEqual(2, len(ret))
        self.assertThat(
            ret[0],
            testing.ConstraintMatches(
                Is(typer.INT),
                typer.SUPERTYPE_OF,
                Is(typer.BOOL),
                [Is(constraint_pos[0])]))
        self.assertThat(
            ret[1],
            testing.ConstraintMatches(
                Is(typer.NONE),
                typer.SUPERTYPE_OF,
                Is(typer.ANY),
                [Is(constraint_pos[0])]))
        self.assertEqual({}, substitution)

    def test_function_type_different_arg_count(self):
        a = typer.FunctionType([], typer.INT)
        b = typer.FunctionType([typer.INT], typer.BOOL)
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        self.assertRaises(
            typer.NotTypeError,
            typer.satisfy_constraint,
            typer.Constraint(a, typer.SUPERTYPE_OF, b, constraint_pos),
            substitution)

    def test_type_expr_subsitute(self):
        a = typer.TypeExpr('a')
        b = typer.TypeExpr('b')
        subs_pos = [SourcePos(1, 1, 1)]
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {a: (b, subs_pos)}
        ret = typer.satisfy_constraint(
                typer.Constraint(a, typer.SUPERTYPE_OF, typer.INT, constraint_pos),
                substitution)
        self.assertEqual(1, len(ret))
        self.assertThat(
            ret[0],
            testing.ConstraintMatches(
                Is(b),
                typer.SUPERTYPE_OF,
                Is(typer.INT),
                [Is(constraint_pos[0]), Is(subs_pos[0])]))
        self.assertEqual(1, len(substitution))

    def test_type_expr_updates_substitution(self):
        a = typer.TypeExpr('a')
        b = typer.TypeExpr('b')
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        ret = typer.satisfy_constraint(
                typer.Constraint(a, typer.SUPERTYPE_OF, b, constraint_pos),
                substitution)
        self.assertEqual([], ret)
        self.assertEqual({a: (b, constraint_pos)}, substitution)

    def test_getattr_no_attr(self):
        a = typer.AttributeAccess(typer.INT, 'a')
        b = typer.BOOL
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        self.assertRaises(
            typer.NotTypeError,
            typer.satisfy_constraint,
                typer.Constraint(a, typer.SUPERTYPE_OF, b, constraint_pos),
                substitution)

    def test_getattr_has_attr(self):
        c = typer.Type('c', {'a': typer.BOOL})
        a = typer.AttributeAccess(c, 'a')
        b = typer.BOOL
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        ret = typer.satisfy_constraint(
                typer.Constraint(a, typer.SUPERTYPE_OF, b, constraint_pos),
                substitution)
        self.assertEqual(1, len(ret))
        self.assertThat(
            ret[0],
            testing.ConstraintMatches(
                Is(typer.BOOL), typer.SUPERTYPE_OF, Is(b), [Is(constraint_pos[0])]))
        self.assertEqual({}, substitution)

    def test_getattr_has_attr_via_substitution(self):
        c = typer.Type('c', {'a': typer.BOOL})
        d = typer.TypeExpr('d')
        a = typer.AttributeAccess(d, 'a')
        b = typer.BOOL
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {d: (c, [])}
        ret = typer.satisfy_constraint(
                typer.Constraint(a, typer.SUPERTYPE_OF, b, constraint_pos),
                substitution)
        self.assertEqual(1, len(ret))
        self.assertThat(
            ret[0],
            testing.ConstraintMatches(
                Is(typer.BOOL), typer.SUPERTYPE_OF, Is(b), [Is(constraint_pos[0])]))
        self.assertEqual({d: (c, [])}, substitution)


def get_type_of(name, source):
    try:
        parsed = parsing.parse(source)
    except ParseError as e:
        print e.nice_error_message(source=source)
        raise
    env, substitutions = typer.typecheck(parsed, trace=True)
    return typer.get_substituted(env.env[name][0], substitutions)


class IntegrationTests(TestCase):

    def test_int(self):
        self.assertThat(get_type_of('a', 'a = 1\n'), Is(typer.INT))

    def test_int_by_inference(self):
        self.assertThat(get_type_of('a', 'b = 1\na = b\n'), Is(typer.INT))

    def test_return_int(self):
        self.assertThat(
            get_type_of('a', 'def a():\n    return 1\n\n'),
            testing.IsFunctionType([], Is(typer.INT)))

    def test_return_arg(self):
        ftype = get_type_of('a', 'def a(b):\n    return b\n\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType([Is(ftype.rtype)], testing.IsTypeVariable('a')))

    def test_type_not_bound(self):
        ftype = get_type_of('a', 'def a(b):\n    return b\n\na(1)\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType([Is(ftype.rtype)], testing.IsTypeVariable('a')))

    def test_instantiate(self):
        ftype = get_type_of('a', 'def a(b):\n    return b\n\na(1)\na(true())\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType([Is(ftype.rtype)], testing.IsTypeVariable('a')))

    def test_higher_order(self):
        ftype = get_type_of('a', 'def a(b, c):\n    return b(c)\n\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                [testing.IsFunctionType(
                    [Is(ftype.args[1])],
                    Is(ftype.rtype)),
                testing.IsTypeVariable('a')],
                testing.IsTypeVariable('b')))

    def test_recursive(self):
        ftype = get_type_of('a', 'def a(b):\n    if b > 0:\n        return a(b-1)\n    else:\n        return b\n\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                [Is(typer.INT)],
                Is(typer.INT)))

    def test_mutually_recursive(self):
        ftype = get_type_of('a', """
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
                [Is(typer.INT)],
                Is(typer.INT)))

    def test_infinite_recursion(self):
        self.assertRaises(AssertionError, get_type_of, 'a', """
def a(b):
    return a(b)

""")

    def test_inferred_from_other(self):
        ftype = get_type_of('a', """
def a(b):
    return c(b)

def c(x):
    return x + 1

""")
        self.assertThat(
            ftype,
            testing.IsFunctionType([Is(typer.INT)], Is(typer.INT)))

    def test_use_of_higher_order(self):
        ftype = get_type_of('e', """
def b(x):
    return x

def a(c, d):
    return c(d)

e = a(b, 1)
""")
        self.assertThat(ftype, Is(typer.INT))

    def test_new_type(self):
        ftype = get_type_of('Dog', """
Dog = new Type:
    pass

""")
        self.assertThat(ftype, testing.IsFunctionType([], testing.IsType('Dog')))

    def test_new_type_in_signature(self):
        ftype = get_type_of('foo', """
Dog = new Type:
    pass

def foo(d: Dog):
    return d

""")
        self.assertThat(ftype, testing.IsFunctionType([Is(ftype.rtype)], testing.IsType("Dog")))

    def test_parameterised_type(self):
        ftype = get_type_of('foo', """
List = new Type<a>:
    pass

foo = List()
""")
        self.assertThat(ftype, testing.IsParametricType([testing.IsType('List'), testing.IsTypeExpr('a')]))

    def test_parameterised_type_instantiated(self):
        # Double define foo to be sure that List<int> == List<int>
        # when instatiated in different places
        ftype = get_type_of('foo', """
List = new Type<a>:
    pass

foo = List<int>()
foo = List<int>()
""")
        self.assertThat(ftype, testing.IsParametricType([testing.IsType('List'), Is(typer.INT)]))

    def test_different_parameterised_types(self):
        self.assertRaises(typer.NotTypeError, get_type_of, 'foo', """
List = new Type<a>:
    pass

foo = List<int>()
foo = List<bool>()
""")

    def test_attribute_access(self):
        ftype = get_type_of('foo', """
Thing = new Type:
    a = 1

bar = Thing()
foo = bar.a
""")
        self.assertThat(ftype, Is(typer.INT))

    def test_attribute_access_on_returned_value(self):
        ftype = get_type_of('foo', """
Thing = new Type:
    a = 1

def thing():
    return Thing()

foo = thing().a
""")
        self.assertThat(ftype, Is(typer.INT))


class InstantiateTests(TestCase):

    def test_types(self):
        self.assertThat(
            typer.instantiate(typer.FunctionType([typer.INT], typer.INT)),
            testing.IsFunctionType(
                [Is(typer.INT)],
                Is(typer.INT))
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


class GeneraliseFunctionTests(TestCase):

    def test_no_vars(self):
        input = typer.FunctionType([typer.INT], typer.INT)
        ftype = typer.generalise(input)
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                [Is(typer.INT)],
                Is(typer.INT)))

    def test_one_var(self):
        argname = "bar"
        argtype = typer.TypeExpr(argname)
        input = typer.FunctionType([argtype], typer.INT)
        ftype = typer.generalise(input)
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                [testing.IsTypeVariable('a')],
                Is(typer.INT)))

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
        argname = "bar"
        argtype = typer.TypeExpr(argname)
        input = typer.FunctionType(
            [argtype, typer.FunctionType(
                [argtype],
                typer.INT)],
            typer.INT)
        ftype = typer.generalise(input)
        new_argtype = ftype.args[0]
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                [testing.IsTypeVariable('a'),
                testing.IsFunctionType([Is(new_argtype)], Is(typer.INT))],
                Is(typer.INT)))

    def test_unconstrained_rtype(self):
        input = typer.FunctionType([typer.INT], typer.TypeExpr("bar"))
        e = self.assertRaises(AssertionError, typer.generalise, input)
        self.assertThat(str(e), Equals("unconstrained return type: %s" % input))


class SubstitutionTests(TestCase):

    def test_adds(self):
        substitution = {}
        positions = []
        typer.update_substitution(substitution, typer.INT, typer.BOOL, positions)
        self.assertEqual({typer.INT: (typer.BOOL, positions)}, substitution)

    def test_replaces(self):
        substitution = {typer.INT: (typer.NONE, [])}
        positions = []
        typer.update_substitution(substitution, typer.INT, typer.BOOL, positions)
        self.assertEqual({typer.INT: (typer.BOOL, positions)}, substitution)

    def test_updates_other_references(self):
        a = typer.TypeExpr('a')
        substitution = {typer.INT: (a, [])}
        positions = []
        typer.update_substitution(substitution, a, typer.BOOL, positions)
        self.assertEqual({typer.INT: (typer.BOOL, []), a: (typer.BOOL, positions)}, substitution)

    def test_updates_other_references_in_ftype(self):
        a = typer.TypeExpr('a')
        f = typer.FunctionType([], a)
        substitution = {typer.INT: (f, [])}
        positions = []
        typer.update_substitution(substitution, a, typer.BOOL, positions)
        self.assertEqual(2, len(substitution))
        self.assertThat(substitution[a], Equals((typer.BOOL, [])))
        self.assertThat(substitution[typer.INT][0], testing.IsFunctionType([], Is(typer.BOOL)))

    def test_occurs_check(self):
        substitution = {}
        positions = []
        self.assertRaises(typer.NotTypeError, typer.update_substitution,
                substitution, typer.INT, typer.INT, positions)

    def test_get_substituded(self):
        substitution = {typer.INT: (typer.NONE, [])}
        ret = typer.get_substituted(typer.INT, substitution)
        self.assertIs(typer.NONE, ret)

    def test_get_substituded_transitive(self):
        substitution = {typer.INT: (typer.NONE, []), typer.NONE: (typer.BOOL, [])}
        ret = typer.get_substituted(typer.INT, substitution)
        self.assertIs(typer.BOOL, ret)

    def test_get_substituded_function_type(self):
        substitution = {typer.INT: (typer.FunctionType([typer.NONE], typer.NONE), []), typer.NONE: (typer.BOOL, [])}
        ret = typer.get_substituted(typer.INT, substitution)
        self.assertThat(ret, testing.IsFunctionType([Is(typer.BOOL)], Is(typer.BOOL)))

    def test_occurs_same_type(self):
        self.assertEqual(True, typer.occurs(typer.INT, typer.INT))

    def test_occurs_different_types(self):
        self.assertEqual(False, typer.occurs(typer.INT, typer.NONE))

    def test_occurs_function_arg(self):
        self.assertEqual(True, typer.occurs(typer.INT, typer.FunctionType([typer.INT], typer.BOOL)))

    def test_occurs_function_rtype(self):
        self.assertEqual(True, typer.occurs(typer.INT, typer.FunctionType([typer.BOOL], typer.INT)))

    def test_occurs_parameterised_type(self):
        self.assertEqual(True, typer.occurs(typer.INT, typer.ParameterisedType([typer.BOOL, typer.INT])))


class UnifyTypeTests(TestCase):

    def test_same(self):
        self.assertIs(typer.INT, typer.unify_types(typer.INT, typer.INT, typer.SUPERTYPE_OF))
        self.assertIs(typer.INT, typer.unify_types(typer.INT, typer.INT, typer.SUBTYPE_OF))

    def test_different(self):
        self.assertIs(None, typer.unify_types(typer.INT, typer.BOOL, typer.SUPERTYPE_OF))
        self.assertIs(None, typer.unify_types(typer.INT, typer.BOOL, typer.SUBTYPE_OF))

    def test_vs_any(self):
        self.assertIs(typer.ANY, typer.unify_types(typer.ANY, typer.BOOL, typer.SUPERTYPE_OF))
        self.assertIs(None, typer.unify_types(typer.BOOL, typer.ANY, typer.SUPERTYPE_OF))
        self.assertIs(typer.ANY, typer.unify_types(typer.INT, typer.ANY, typer.SUBTYPE_OF))
        self.assertIs(None, typer.unify_types(typer.ANY, typer.BOOL, typer.SUBTYPE_OF))


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
        self.assertThat(t, Is(typer.INT))
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
        checker.env.extend(node.varname, typer.INT)
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(typer.INT))
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
        checker.env.extend(node.varname, typer.INT)
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
        node = self.factory.assignment()
        checker = self.get_third_pass(True)
        constraints, t = checker.dispatch(node)
        self.assertThat(t, testing.IsTypeExpr("main:1"))
        self.assertThat(checker.env.env[node.var.varname][0], Is(t))
        self.assertThat(
            constraints,
            MatchesListwise([
                testing.ConstraintMatches(
                    Is(t),
                    typer.SUPERTYPE_OF,
                    Is(typer.INT),
                    [Is(self.factory.spos)])]))

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
        self.assertEqual(1, len(constraints))

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
        ftype = checker.env.register(fname)
        constraints, t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(len(constraints), Equals(2))
        child_env = checker.env.children[fname]
        self.assertThat(constraints[1], testing.ConstraintMatches(
            Is(ftype),
            typer.SUPERTYPE_OF,
            testing.IsFunctionType(
                [Is(child_env.env[varname][0])],
                Is(child_env.rtype)),
            [Is(self.factory.spos)],
            ))
