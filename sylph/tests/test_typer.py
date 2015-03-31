from testtools import TestCase
from testtools.matchers import Equals, Is
from rpython.rlib.parsing.lexer import SourcePos
from rpython.rlib.parsing.parsing import ParseError

from .. import ast, parsing, testing, typer


class TypeCollectorTests(TestCase):

    spos = SourcePos(0, 0, 0)

    def get_typecollector(self):
        return typer.TypeCollector(typer.FUNCTIONS.copy(), typer.BASE_TYPES.copy())

    def test_ConstantInt(self):
        node = ast.ConstantInt(2, self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        self.assertIs(typer.INT, rtype)
        self.assertEqual([], t.constraints)

    def test_Assignment(self):
        varname = "a"
        rhs = ast.ConstantInt(2, self.spos)
        lhs = ast.Variable((varname,), self.spos)
        node = ast.Assignment(lhs, rhs, self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        self.assertThat(rtype, testing.IsTypeExpr(varname))
        self.assertEqual(1, len(t.constraints))
        self.assertThat(t.constraints[0], testing.ConstraintMatches(Is(rtype), typer.SUPERTYPE_OF, Is(typer.INT), [Is(self.spos)]))
        self.assertEqual(rtype, t.varmap[varname])

    def test_Return(self):
        rhs = ast.ConstantInt(2, self.spos)
        node = ast.Return(rhs, self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(1, len(t.constraints))
        self.assertThat(
            t.constraints[0],
            testing.ConstraintMatches(Is(t.rtype), typer.SUPERTYPE_OF, Is(typer.INT), [Is(self.spos)]))

    def test_Return_noarg(self):
        node = ast.Return(None, self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(1, len(t.constraints))
        self.assertThat(
            t.constraints[0],
            testing.ConstraintMatches(Is(t.rtype), typer.SUPERTYPE_OF, Is(typer.NONE), [Is(self.spos)]))

    def test_Variable_existing(self):
        varname = "a"
        node = ast.Variable((varname,), self.spos)
        t = self.get_typecollector()
        vartype = typer.TypeVariable(varname)
        t.varmap[varname] = vartype
        self.assertIs(vartype, t.dispatch(node))
        self.assertEqual([], t.constraints)

    def test_Variable_nonexisting(self):
        varname = "a"
        node = ast.Variable((varname,), self.spos)
        t = self.get_typecollector()
        self.assertRaises(typer.SylphNameError, t.dispatch, node)

    def test_Variable_attr(self):
        varname = "a"
        attrname = "b"
        node = ast.Variable((varname, attrname), self.spos)
        t = self.get_typecollector()
        vartype = typer.TypeVariable(varname)
        t.varmap[varname] = vartype
        ret = t.dispatch(node)
        self.assertThat(ret, testing.IsTypeExpr("a.b"))
        self.assertEqual(1, len(t.constraints))
        self.assertThat(t.constraints[0], testing.ConstraintMatches(
            testing.IsAttributeAccess(Is(vartype), 'b'), typer.SUPERTYPE_OF, Is(ret), [Is(self.spos)]))

    def test_Variable_attr_nested(self):
        varname = "a"
        attrname = "b"
        attrname2 = "c"
        node = ast.Variable((varname, attrname, attrname2), self.spos)
        t = self.get_typecollector()
        vartype = typer.TypeVariable(varname)
        t.varmap[varname] = vartype
        ret = t.dispatch(node)
        self.assertThat(ret, testing.IsTypeExpr("a.b.c"))
        self.assertEqual(2, len(t.constraints))
        self.assertThat(t.constraints[0], testing.ConstraintMatches(
            testing.IsAttributeAccess(Is(vartype), 'b'), typer.SUPERTYPE_OF, testing.IsTypeExpr("a.b"), [Is(self.spos)]))
        self.assertThat(t.constraints[1], testing.ConstraintMatches(
            testing.IsAttributeAccess(testing.IsTypeExpr("a.b"), 'c'), typer.SUPERTYPE_OF, Is(ret), [Is(self.spos)]))

    def test_BinOp(self):
        op = "+"
        lhs = ast.ConstantInt(1, self.spos)
        node = ast.BinOp(op, lhs, lhs, self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        self.assertIsInstance(rtype, typer.TypeExpr)
        self.assertEqual("r+", rtype.name)
        self.assertEqual(1, len(t.constraints))
        self.assertThat(
            t.constraints[0],
            testing.ConstraintMatches(
                testing.IsFunctionType(Equals("+"), [Is(typer.INT), Is(typer.INT)], Is(typer.INT)),
                typer.SUPERTYPE_OF,
                testing.IsFunctionType(Equals("+"), [Is(typer.INT), Is(typer.INT)], testing.IsTypeExpr("r+")),
                [Is(self.spos)]))

    def test_nested_BinOp(self):
        op = "+"
        lhs = ast.ConstantInt(1, self.spos)
        node = ast.BinOp(op, lhs, lhs, self.spos)
        node = ast.BinOp(op, lhs, node, self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        typer.TypeVariable("(rtype of + int -> a)")
        self.assertIsInstance(rtype, typer.TypeExpr)
        self.assertEqual("r+", rtype.name)
        self.assertEqual(2, len(t.constraints))
        self.assertThat(
            t.constraints[0],
            testing.ConstraintMatches(
                testing.IsFunctionType(Equals("+"), [Is(typer.INT), Is(typer.INT)], Is(typer.INT)),
                typer.SUPERTYPE_OF,
                testing.IsFunctionType(Equals("+"), [Is(typer.INT), Is(typer.INT)], testing.IsTypeExpr("r+")),
                [Is(self.spos)],
                )
            )
        self.assertThat(
            t.constraints[1],
            testing.ConstraintMatches(
                testing.IsFunctionType(Equals("+"), [Is(typer.INT), Is(typer.INT)], Is(typer.INT)),
                typer.SUPERTYPE_OF,
                testing.IsFunctionType(Equals("+"), [Is(typer.INT), testing.IsTypeExpr("r+")], testing.IsTypeExpr("r+")),
                [Is(self.spos)],
                )
            )
        # probably applies to foo(bar(baz)) too?
        # also test foo(bar) + 1 etc.
        # Maybe need to create a lot more typevars and add equalities?

    def test_Conditional(self):
        varname = "a"
        condition = ast.ConstantInt(1, self.spos)
        block = ast.Assignment(ast.Variable((varname,), self.spos), ast.ConstantInt(2, self.spos), self.spos)
        node = ast.Conditional(condition, block, None, self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(2, len(t.constraints))
        self.assertThat(
            t.constraints[0],
            testing.ConstraintMatches(
                Is(typer.INT),
                typer.SUBTYPE_OF,
                Is(typer.BOOL),
                [Is(self.spos)]))
        self.assertThat(
            t.constraints[1],
            testing.ConstraintMatches(
                testing.IsTypeExpr(varname),
                typer.SUPERTYPE_OF,
                Is(typer.INT),
                [Is(self.spos)]))

    def test_Conditional_with_else(self):
        varname = "a"
        condition = ast.ConstantInt(1, self.spos)
        block = ast.Assignment(ast.Variable((varname,), self.spos), ast.ConstantInt(2, self.spos), self.spos)
        node = ast.Conditional(condition, block, block, self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(3, len(t.constraints))
        self.assertThat(
            t.constraints[0],
            testing.ConstraintMatches(
                Is(typer.INT),
                typer.SUBTYPE_OF,
                Is(typer.BOOL),
                [Is(self.spos)]))
        self.assertThat(
            t.constraints[1],
            testing.ConstraintMatches(
                testing.IsTypeExpr(varname),
                typer.SUPERTYPE_OF,
                Is(typer.INT),
                [Is(self.spos)]))
        self.assertThat(
            t.constraints[2],
            testing.ConstraintMatches(
                testing.IsTypeExpr(varname),
                typer.SUPERTYPE_OF,
                Is(typer.INT),
                [Is(self.spos)]))

    def test_While(self):
        varname = "a"
        condition = ast.ConstantInt(1, self.spos)
        block = ast.Assignment(ast.Variable((varname,), self.spos), ast.ConstantInt(2, self.spos), self.spos)
        node = ast.While(condition, block, self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(2, len(t.constraints))
        self.assertThat(
            t.constraints[0],
            testing.ConstraintMatches(Is(typer.INT), typer.SUBTYPE_OF, Is(typer.BOOL), [Is(self.spos)]),
            )
        self.assertThat(
            t.constraints[1],
            testing.ConstraintMatches(testing.IsTypeExpr(varname), typer.SUPERTYPE_OF, Is(typer.INT), [Is(self.spos)]),
            )

    def test_Function_noargs(self):
        fname = "foo"
        node = ast.Function((fname,), [], self.spos)
        t = self.get_typecollector()
        t.functions[fname] = typer.TypeExpr(fname)
        rtype = t.dispatch(node)
        self.assertThat(rtype, testing.IsTypeExpr("rfoo"))
        self.assertEqual(1, len(t.constraints))
        self.assertThat(
            t.constraints[0],
            testing.ConstraintMatches(
                testing.IsTypeExpr(fname),
                typer.SUPERTYPE_OF,
                testing.IsFunctionType(Equals(fname), [], Is(rtype)),
                [Is(self.spos)]))

    # TODO: test recursion

    def test_Function_args(self):
        fname = "foo"
        arg = ast.ConstantInt(1, self.spos)
        node = ast.Function((fname,), [arg], self.spos)
        t = self.get_typecollector()
        t.functions[fname] = typer.TypeExpr(fname)
        rtype = t.dispatch(node)
        self.assertThat(rtype, testing.IsTypeExpr("rfoo"))
        self.assertEqual(1, len(t.constraints))
        self.assertThat(
            t.constraints[0],
            testing.ConstraintMatches(
                testing.IsTypeExpr(fname),
                typer.SUPERTYPE_OF,
                testing.IsFunctionType(Equals(fname), [Is(typer.INT)], Is(rtype)),
                [Is(self.spos)]))

    def test_FuncDef(self):
        fname = "foo"
        argname = "bar"
        code = ast.Return(ast.Variable((argname,), self.spos), self.spos)
        node = ast.FuncDef(fname, [argname], code, self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(0, len(t.constraints))
        self.assertEqual(1, len(t.child_contexts))
        self.assertIn(fname, t.child_contexts)
        context = t.child_contexts[fname]
        self.assertIsInstance(context, typer.TypeCollector)
        self.assertThat(
            context.constraints[0],
            testing.ConstraintMatches(
                Is(context.rtype),
                typer.SUPERTYPE_OF,
                Is(context.varmap[argname]),
                [Is(self.spos)]))
        self.assertEqual(2, len(context.varmap))
        self.assertThat(context.varmap[argname], testing.IsTypeExpr(argname))
        self.assertThat(context.varmap[fname], testing.IsTypeExpr(fname))

    def test_FuncDef_with_annotations(self):
        fname = "foo"
        argname = "bar"
        rtype = "int"
        argtype = "int"
        code = ast.Return(ast.Variable((argname,), self.spos), self.spos)
        node = ast.FuncDef(fname, [argname], code, self.spos, rtype=rtype, argtypes=[argtype])
        t = self.get_typecollector()
        self.assertIs(None, t.dispatch(node))
        self.assertEqual(0, len(t.constraints))
        self.assertEqual(1, len(t.child_contexts))
        self.assertIn(fname, t.child_contexts)
        context = t.child_contexts[fname]
        self.assertIsInstance(context, typer.TypeCollector)
        self.assertThat(
            context.constraints[0],
            testing.ConstraintMatches(
                Is(typer.INT),
                typer.SUPERTYPE_OF,
                Is(typer.INT),
                [Is(self.spos)]))
        self.assertEqual(typer.INT, context.rtype)
        self.assertEqual(typer.INT, context.varmap[argname])

    def test_NewType_assigned(self):
        varname = "atype"
        block = ast.ConstantInt(1, self.spos)
        node = ast.Assignment(ast.Variable((varname,), self.spos), ast.NewType(block, self.spos), self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        self.assertThat(rtype, testing.IsTypeExpr(varname))
        self.assertEqual(1, len(t.constraints))
        self.assertThat(
            t.constraints[0],
            testing.ConstraintMatches(Is(rtype), typer.SUPERTYPE_OF, testing.IsType(varname), [Is(self.spos)]))
        self.assertEqual(rtype, t.varmap[varname])

    def test_parameterised_NewType_assigned(self):
        varname = "atype"
        block = ast.ConstantInt(1, self.spos)
        node = ast.Assignment(ast.Variable((varname,), self.spos), ast.NewType(block, self.spos, ["a"]), self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        self.assertThat(rtype, testing.IsTypeExpr(varname))
        self.assertEqual(1, len(t.constraints))
        self.assertThat(
            t.constraints[0],
            testing.ConstraintMatches(
                Is(rtype),
                typer.SUPERTYPE_OF,
                testing.IsParametricType([testing.IsType(varname), testing.IsTypeVariable("a")]),
                [Is(self.spos)]))
        self.assertEqual(rtype, t.varmap[varname])

    def test_Block(self):
        # Test that children are dispatched to by default
        varname = "a"
        rhs = ast.ConstantInt(2, self.spos)
        lhs = ast.Variable((varname,), self.spos)
        node = ast.Assignment(lhs, rhs, self.spos)
        t = self.get_typecollector()
        t.dispatch(ast.Block([node], self.spos))
        self.assertEqual(1, len(t.constraints))


class SatisfyConstraintsTests(TestCase):

    spos = SourcePos(0, 0, 0)

    def satisfy_constraints(self, constraints):
        return typer.satisfy_constraints(constraints)

    def test_empty(self):
        self.satisfy_constraints([])

    def test_trivially_equal(self):
        self.satisfy_constraints([typer.Constraint(typer.INT, typer.SUPERTYPE_OF, typer.INT, [self.spos])])

    def test_trivially_not_equal(self):
        self.assertRaises(typer.SylphTypeError, self.satisfy_constraints,
            [typer.Constraint(typer.NONE, typer.SUPERTYPE_OF, typer.INT, [self.spos])])

    def test_not_equal_across_two(self):
        vartype = typer.TypeVariable("a")
        self.assertRaises(typer.SylphTypeError, self.satisfy_constraints,
            [typer.Constraint(vartype, typer.SUPERTYPE_OF, typer.INT, [self.spos]),
             typer.Constraint(vartype, typer.SUPERTYPE_OF, typer.NONE, [self.spos])])

    def test_not_equal_across_three(self):
        vartype1 = typer.TypeVariable("a")
        vartype2 = typer.TypeVariable("b")
        self.assertRaises(typer.SylphTypeError, self.satisfy_constraints,
            [typer.Constraint(vartype1, typer.SUPERTYPE_OF, typer.INT, [self.spos]),
             typer.Constraint(vartype2, typer.SUPERTYPE_OF, vartype1, [self.spos]),
             typer.Constraint(vartype2, typer.SUPERTYPE_OF, typer.NONE, [self.spos])])

    def test_basic_type_vs_incompatible(self):
        self.assertRaises(
            typer.SylphTypeError,
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
        a = typer.FunctionType('a', [], typer.INT)
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        self.assertRaises(
            typer.SylphTypeError,
            typer.satisfy_constraint,
            typer.Constraint(a, typer.SUPERTYPE_OF, typer.INT, constraint_pos),
            substitution)

    def test_function_type_vs_function_type(self):
        a = typer.FunctionType('a', [typer.NONE], typer.INT)
        b = typer.FunctionType('a', [typer.ANY], typer.BOOL)
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
        a = typer.FunctionType('a', [], typer.INT)
        b = typer.FunctionType('b', [typer.INT], typer.BOOL)
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {}
        self.assertRaises(
            typer.SylphTypeError,
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

    def test_type_expr_substituted_already_equal(self):
        # If when substituted the type exprs are already equal,
        # then don't do anything
        a = typer.TypeExpr('a')
        b = typer.TypeExpr('b')
        subs_pos = [SourcePos(1, 1, 1)]
        constraint_pos = [SourcePos(2, 2, 2)]
        substitution = {a: (b, subs_pos)}
        ret = typer.satisfy_constraint(
                typer.Constraint(b, typer.SUPERTYPE_OF, a, constraint_pos),
                substitution)
        self.assertEqual([], ret)
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
            typer.SylphTypeError,
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
    checker, substitutions = typer.typecheck(parsed)
    return typer.get_substituted(checker.varmap[name], substitutions)


class IntegrationTests(TestCase):

    def test_int(self):
        self.assertThat(get_type_of('a', 'a = 1\n'), Is(typer.INT))

    def test_int_by_inference(self):
        self.assertThat(get_type_of('a', 'b = 1\na = b\n'), Is(typer.INT))

    def test_return_int(self):
        self.assertThat(
            get_type_of('a', 'def a():\n    return 1\n\n'),
            testing.IsFunctionType(Equals('a'), [], Is(typer.INT)))

    def test_return_arg(self):
        ftype = get_type_of('a', 'def a(b):\n    return b\n\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType(Equals('a'), [Is(ftype.rtype)], testing.IsTypeVariable('b')))

    def test_type_not_bound(self):
        ftype = get_type_of('a', 'def a(b):\n    return b\n\na(1)\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType(Equals('a'), [Is(ftype.rtype)], testing.IsTypeVariable('b')))

    def test_higher_order(self):
        ftype = get_type_of('a', 'def a(b, c):\n    return b(c)\n\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                Equals('a'),
                [testing.IsFunctionType(
                    Equals('b'),
                    [Is(ftype.args[1])],
                    Is(ftype.rtype)),
                testing.IsTypeVariable('c')],
                testing.IsTypeVariable('rb')))

    def test_recursive(self):
        ftype = get_type_of('a', 'def a(b):\n    if b > 0:\n        return a(b-1)\n    else:\n        return b\n\n')
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                Equals('a'),
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
                Equals('a'),
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
            testing.IsFunctionType(Equals('a'), [Is(typer.INT)], Is(typer.INT)))

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
        self.assertThat(ftype, testing.IsType("Dog"))

    def test_new_type_in_signature(self):
        ftype = get_type_of('foo', """
Dog = new Type:
    pass

def foo(d: Dog):
    return d

""")
        self.assertThat(ftype, testing.IsFunctionType(Equals('foo'), [Is(ftype.rtype)], testing.IsType("Dog")))

    def test_parameterised_type(self):
        ftype = get_type_of('foo', """
List = new Type<a>:
    pass

foo = List()
""")
        self.assertThat(ftype, testing.IsParametricType([testing.IsType('List'), testing.IsTypeVariable('a')]))

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
        self.assertRaises(typer.SylphTypeError, get_type_of, 'foo', """
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


class InstantiateTests(TestCase):

    def test_types(self):
        fname = "foo"
        self.assertThat(
            typer.instantiate(typer.FunctionType(fname, [typer.INT], typer.INT)),
            testing.IsFunctionType(
                Equals(fname),
                [Is(typer.INT)],
                Is(typer.INT))
            )

    def test_vars(self):
        fname = "foo"
        tvar = typer.TypeVariable('a')
        ret = typer.instantiate(typer.FunctionType(fname, [tvar], tvar))
        self.assertThat(
            ret,
            testing.IsFunctionType(
                Equals(fname),
                [Is(ret.rtype)],
                testing.IsTypeExpr(tvar.name))
            )

    def test_nested(self):
        fname = "foo"
        nested_fname = "bar"
        tvar1 = typer.TypeVariable('a')
        tvar2 = typer.TypeVariable('b')
        ret = typer.instantiate(
            typer.FunctionType(
                fname,
                [typer.FunctionType(
                    nested_fname,
                    [tvar1],
                    tvar2),
                 tvar1,
                ],
                tvar2))
        argtype = ret.args[1]
        self.assertThat(
            ret,
            testing.IsFunctionType(
                Equals(fname),
                [testing.IsFunctionType(
                    Equals(nested_fname),
                    [Is(argtype)],
                    Is(ret.rtype)),
                 testing.IsTypeExpr(tvar1.name)],
                testing.IsTypeExpr(tvar2.name))
            )


class FunctionTypeFromContextTests(TestCase):

    def test_no_vars(self):
        t = typer.TypeCollector({}, {})
        t.fname = "foo"
        argname = "a"
        t.args = [argname]
        t.varmap[argname] = typer.INT
        t.rtype = typer.INT
        ftype = typer.function_type_from_collector(t, {})
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                Equals(t.fname),
                [Is(typer.INT)],
                Is(typer.INT)))

    def test_one_var(self):
        t = typer.TypeCollector({}, {})
        t.fname = "foo"
        argname = "a"
        t.args = [argname]
        t.varmap[argname] = typer.TypeExpr(argname)
        t.rtype = typer.INT
        ftype = typer.function_type_from_collector(t, {})
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                Equals(t.fname),
                [Is(t.varmap[argname])],
                Is(typer.INT)))

    def test_substituted(self):
        t = typer.TypeCollector({}, {})
        t.fname = "foo"
        argname = "a"
        t.args = [argname]
        t.varmap[argname] = typer.TypeExpr("nonsense")
        argtype = typer.TypeExpr(argname)
        t.rtype = typer.INT
        ftype = typer.function_type_from_collector(t, {t.varmap[argname]: (argtype, [])})
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                Equals(t.fname),
                [Is(argtype)],
                Is(typer.INT)))


class GeneraliseFunctionTests(TestCase):

    def test_no_vars(self):
        fname = "foo"
        input = typer.FunctionType(fname, [typer.INT], typer.INT)
        ftype = typer.generalise(input)
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                Equals(fname),
                [Is(typer.INT)],
                Is(typer.INT)))

    def test_one_var(self):
        fname = "foo"
        argname = "bar"
        argtype = typer.TypeExpr(argname)
        input = typer.FunctionType(fname, [argtype], typer.INT)
        ftype = typer.generalise(input)
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                Equals(fname),
                [testing.IsTypeVariable(argname)],
                Is(typer.INT)))

    def test_one_var_repeated(self):
        fname = "foo"
        argname = "bar"
        argtype = typer.TypeExpr(argname)
        input = typer.FunctionType(fname, [argtype], argtype)
        ftype = typer.generalise(input)
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                Equals(fname),
                [Is(ftype.rtype)],
                testing.IsTypeVariable(argname)))

    def test_nested_function(self):
        fname = "foo"
        nested_fname = "baz"
        argname = "bar"
        argtype = typer.TypeExpr(argname)
        input = typer.FunctionType(
            fname,
            [argtype, typer.FunctionType(
                nested_fname,
                [argtype],
                typer.INT)],
            typer.INT)
        ftype = typer.generalise(input)
        new_argtype = ftype.args[0]
        self.assertThat(
            ftype,
            testing.IsFunctionType(
                Equals(fname),
                [testing.IsTypeVariable(argname),
                testing.IsFunctionType(Equals(nested_fname), [Is(new_argtype)], Is(typer.INT))],
                Is(typer.INT)))

    def test_unconstrained_rtype(self):
        fname = "foo"
        input = typer.FunctionType(fname, [typer.INT], typer.TypeExpr("bar"))
        e = self.assertRaises(AssertionError, typer.generalise, input)
        self.assertThat(str(e), Equals("%s has an unconstrained return type." % fname))


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

    def test_occurs_check(self):
        substitution = {}
        positions = []
        self.assertRaises(typer.SylphTypeError, typer.update_substitution,
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
        substitution = {typer.INT: (typer.FunctionType('func', [typer.NONE], typer.NONE), []), typer.NONE: (typer.BOOL, [])}
        ret = typer.get_substituted(typer.INT, substitution)
        self.assertThat(ret, testing.IsFunctionType(Equals('func'), [Is(typer.BOOL)], Is(typer.BOOL)))

    def test_occurs_same_type(self):
        self.assertEqual(True, typer.occurs(typer.INT, typer.INT))

    def test_occurs_different_types(self):
        self.assertEqual(False, typer.occurs(typer.INT, typer.NONE))

    def test_occurs_function_arg(self):
        self.assertEqual(True, typer.occurs(typer.INT, typer.FunctionType('foo', [typer.INT], typer.BOOL)))

    def test_occurs_function_rtype(self):
        self.assertEqual(True, typer.occurs(typer.INT, typer.FunctionType('foo', [typer.BOOL], typer.INT)))

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
