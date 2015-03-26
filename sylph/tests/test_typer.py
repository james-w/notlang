from testtools import TestCase
from testtools.matchers import Equals, Is
from rpython.rlib.parsing.lexer import SourcePos

from .. import ast, testing, typer


class TypeCollectorTests(TestCase):

    spos = SourcePos(0, 0, 0)

    def get_typecollector(self):
        return typer.TypeCollector(typer.FUNCTIONS.copy())

    def test_ConstantInt(self):
        node = ast.ConstantInt(2, self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        self.assertIs(typer.INT, rtype)
        self.assertEqual([], t.constraints)

    def test_Assignment(self):
        varname = "a"
        rhs = ast.ConstantInt(2, self.spos)
        lhs = ast.Variable(varname, self.spos)
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
        node = ast.Variable(varname, self.spos)
        t = self.get_typecollector()
        vartype = typer.TypeVariable(varname)
        t.varmap[varname] = vartype
        self.assertIs(vartype, t.dispatch(node))
        self.assertEqual([], t.constraints)

    def test_Variable_nonexisting(self):
        varname = "a"
        node = ast.Variable(varname, self.spos)
        t = self.get_typecollector()
        self.assertRaises(typer.SylphNameError, t.dispatch, node)

    def test_BinOp(self):
        varname = "a"
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
        varname = "a"
        op = "+"
        lhs = ast.ConstantInt(1, self.spos)
        node = ast.BinOp(op, lhs, lhs, self.spos)
        node = ast.BinOp(op, lhs, node, self.spos)
        t = self.get_typecollector()
        rtype = t.dispatch(node)
        new_tvar = typer.TypeVariable("(rtype of + int -> a)")
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
        block = ast.Assignment(ast.Variable(varname, self.spos), ast.ConstantInt(2, self.spos), self.spos)
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
        block = ast.Assignment(ast.Variable(varname, self.spos), ast.ConstantInt(2, self.spos), self.spos)
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
        block = ast.Assignment(ast.Variable(varname, self.spos), ast.ConstantInt(2, self.spos), self.spos)
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
        node = ast.Function(fname, [], self.spos)
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
        node = ast.Function(fname, [arg], self.spos)
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
        code = ast.Return(ast.Variable(argname, self.spos), self.spos)
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
        code = ast.Return(ast.Variable(argname, self.spos), self.spos)
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

    def test_Block(self):
        # Test that children are dispatched to by default
        varname = "a"
        rhs = ast.ConstantInt(2, self.spos)
        lhs = ast.Variable(varname, self.spos)
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
