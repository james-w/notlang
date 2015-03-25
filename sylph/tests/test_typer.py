from testtools import TestCase
from testtools.matchers import Equals, Is
from rpython.rlib.parsing.lexer import SourcePos

from .. import ast, testing, typer


class TypeCollectorTests(TestCase):

    spos = SourcePos(0, 0, 0)

    def test_ConstantInt(self):
        node = ast.ConstantInt(2, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(typer.INT, rtype)
        self.assertEqual([], t.constraints)

    def test_Assignment(self):
        varname = "a"
        rhs = ast.ConstantInt(2, self.spos)
        lhs = ast.Variable(varname, self.spos)
        node = ast.Assignment(lhs, rhs, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertThat(rtype, testing.IsTypeExpr(varname))
        self.assertEqual(1, len(t.constraints))
        self.assertEqual((rtype, typer.SUPERTYPE_OF, typer.INT, [self.spos]), t.constraints[0])
        self.assertEqual(rtype, t.varmap[varname])

    def test_Return(self):
        rhs = ast.ConstantInt(2, self.spos)
        node = ast.Return(rhs, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(1, len(t.constraints))
        self.assertEqual((t.rtype, typer.SUPERTYPE_OF, typer.INT, [self.spos]), t.constraints[0])

    def test_Return_noarg(self):
        node = ast.Return(None, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(1, len(t.constraints))
        self.assertEqual((t.rtype, typer.SUPERTYPE_OF, typer.NONE, [self.spos]), t.constraints[0])

    def test_Variable_existing(self):
        varname = "a"
        node = ast.Variable(varname, self.spos)
        t = typer.TypeCollector()
        vartype = typer.TypeVariable(varname)
        t.varmap[varname] = vartype
        self.assertIs(vartype, t.dispatch(node))
        self.assertEqual([], t.constraints)

    def test_Variable_nonexisting(self):
        varname = "a"
        node = ast.Variable(varname, self.spos)
        t = typer.TypeCollector()
        self.assertRaises(typer.SylphNameError, t.dispatch, node)

    def test_BinOp(self):
        varname = "a"
        op = "+"
        lhs = ast.ConstantInt(1, self.spos)
        node = ast.BinOp(op, lhs, lhs, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIsInstance(rtype, typer.TypeExpr)
        self.assertEqual("r+", rtype.name)
        self.assertEqual(1, len(t.constraints))
        constraint = t.constraints[0]
        self.assertThat(constraint[0], testing.IsTypeExpr("+"))
        self.assertEqual(constraint[1], typer.SUPERTYPE_OF)
        self.assertThat(constraint[2], testing.IsFunctionType(testing.IsTypeExpr("+"), [Equals(typer.INT), Equals(typer.INT)], testing.IsTypeExpr("r+")))
        self.assertEqual([self.spos], constraint[3])

    def test_nested_BinOp(self):
        varname = "a"
        op = "+"
        lhs = ast.ConstantInt(1, self.spos)
        node = ast.BinOp(op, lhs, lhs, self.spos)
        node = ast.BinOp(op, lhs, node, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        new_tvar = typer.TypeVariable("(rtype of + int -> a)")
        self.assertIsInstance(rtype, typer.TypeExpr)
        self.assertEqual("r+", rtype.name)
        self.assertEqual(2, len(t.constraints))
        constraint = t.constraints[0]
        self.assertThat(constraint[0], testing.IsTypeExpr("+"))
        self.assertEqual(constraint[1], typer.SUPERTYPE_OF)
        self.assertThat(constraint[2], testing.IsFunctionType(testing.IsTypeExpr("+"), [Equals(typer.INT), Equals(typer.INT)], testing.IsTypeExpr("r+")))
        self.assertEqual([self.spos], constraint[3])
        constraint = t.constraints[1]
        self.assertThat(constraint[0], testing.IsTypeExpr("+"))
        self.assertEqual(constraint[1], typer.SUPERTYPE_OF)
        self.assertThat(constraint[2], testing.IsFunctionType(testing.IsTypeExpr("+"), [Equals(typer.INT), testing.IsTypeExpr("r+")], testing.IsTypeExpr("r+")))
        self.assertEqual([self.spos], constraint[3])
        # probably applies to foo(bar(baz)) too?
        # also test foo(bar) + 1 etc.
        # Maybe need to create a lot more typevars and add equalities?

    def test_Conditional(self):
        varname = "a"
        condition = ast.ConstantInt(1, self.spos)
        block = ast.Assignment(ast.Variable(varname, self.spos), ast.ConstantInt(2, self.spos), self.spos)
        node = ast.Conditional(condition, block, None, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(2, len(t.constraints))
        self.assertEqual((typer.INT, typer.SUBTYPE_OF, typer.BOOL, [self.spos]), t.constraints[0])
        self.assertEqual((typer.SUPERTYPE_OF, typer.INT, [self.spos]), t.constraints[1][1:])
        self.assertThat(t.constraints[1][0], testing.IsTypeExpr(varname))

    def test_Conditional_with_else(self):
        varname = "a"
        condition = ast.ConstantInt(1, self.spos)
        block = ast.Assignment(ast.Variable(varname, self.spos), ast.ConstantInt(2, self.spos), self.spos)
        node = ast.Conditional(condition, block, block, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(3, len(t.constraints))
        self.assertEqual((typer.INT, typer.SUBTYPE_OF, typer.BOOL, [self.spos]), t.constraints[0])
        self.assertEqual((typer.SUPERTYPE_OF, typer.INT, [self.spos]), t.constraints[1][1:])
        self.assertThat(t.constraints[1][0], testing.IsTypeExpr(varname))
        self.assertEqual((typer.SUPERTYPE_OF, typer.INT, [self.spos]), t.constraints[2][1:])
        self.assertThat(t.constraints[1][0], testing.IsTypeExpr(varname))

    def test_While(self):
        varname = "a"
        condition = ast.ConstantInt(1, self.spos)
        block = ast.Assignment(ast.Variable(varname, self.spos), ast.ConstantInt(2, self.spos), self.spos)
        node = ast.While(condition, block, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(2, len(t.constraints))
        constraint = t.constraints[0]
        self.assertEqual((typer.INT, typer.SUBTYPE_OF, typer.BOOL, [self.spos]), constraint)
        constraint = t.constraints[1]
        self.assertEqual((typer.SUPERTYPE_OF, typer.INT, [self.spos]), constraint[1:])
        self.assertThat(constraint[0], testing.IsTypeExpr(varname))

    def test_Function_noargs(self):
        fname = "foo"
        node = ast.Function(fname, [], self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertThat(rtype, testing.IsTypeExpr("rfoo"))
        constraint = t.constraints[0]
        self.assertThat(constraint[0], testing.IsTypeExpr(fname))
        self.assertEqual(typer.SUPERTYPE_OF, constraint[1])
        self.assertThat(constraint[2], testing.IsFunctionType(testing.IsTypeExpr(fname), [], Is(rtype)))
        self.assertEqual([self.spos], constraint[3])

    # TODO: test recursion

    def test_Function_args(self):
        fname = "foo"
        arg = ast.ConstantInt(1, self.spos)
        node = ast.Function(fname, [arg], self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertThat(rtype, testing.IsTypeExpr("rfoo"))
        constraint = t.constraints[0]
        self.assertThat(constraint[0], testing.IsTypeExpr(fname))
        self.assertEqual(typer.SUPERTYPE_OF, constraint[1])
        self.assertThat(constraint[2], testing.IsFunctionType(testing.IsTypeExpr(fname), [Is(typer.INT)], Is(rtype)))
        self.assertEqual([self.spos], constraint[3])

    def test_FuncDef(self):
        fname = "foo"
        argname = "bar"
        code = ast.Return(ast.Variable(argname, self.spos), self.spos)
        node = ast.FuncDef(fname, [argname], code, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual([], t.constraints)
        self.assertEqual(1, len(t.child_contexts))
        self.assertIn(fname, t.child_contexts)
        context = t.child_contexts[fname]
        self.assertIsInstance(context, typer.TypeCollector)
        self.assertEqual([(context.rtype, typer.SUPERTYPE_OF, context.varmap[argname], [self.spos])], context.constraints)
        self.assertEqual(2, len(context.varmap))
        self.assertThat(context.varmap[argname], testing.IsTypeExpr(argname))
        self.assertThat(context.varmap[fname], testing.IsTypeExpr(fname))

    def test_FuncDef_with_annotations(self):
        fname = "foo"
        argname = "bar"
        rtype = "bool"
        argtype = "int"
        code = ast.Return(ast.Variable(argname, self.spos), self.spos)
        node = ast.FuncDef(fname, [argname], code, self.spos, rtype=rtype, argtypes=[argtype])
        t = typer.TypeCollector()
        self.assertIs(None, t.dispatch(node))
        self.assertEqual([], t.constraints)
        self.assertEqual(1, len(t.child_contexts))
        self.assertIn(fname, t.child_contexts)
        context = t.child_contexts[fname]
        self.assertIsInstance(context, typer.TypeCollector)
        self.assertEqual([(typer.BOOL, typer.SUPERTYPE_OF, typer.INT, [self.spos])], context.constraints)
        self.assertEqual(typer.BOOL, context.rtype)
        self.assertEqual(typer.INT, context.varmap[argname])

    def test_Block(self):
        # Test that children are dispatched to by default
        varname = "a"
        rhs = ast.ConstantInt(2, self.spos)
        lhs = ast.Variable(varname, self.spos)
        node = ast.Assignment(lhs, rhs, self.spos)
        t = typer.TypeCollector()
        t.dispatch(ast.Block([node], self.spos))
        self.assertEqual(1, len(t.constraints))


class SatisfyConstraintsTests(TestCase):

    spos = SourcePos(0, 0, 0)

    def satisfy_constraints(self, constraints):
        return typer.satisfy_constraints(constraints)

    def test_empty(self):
        self.satisfy_constraints([])

    def test_trivially_equal(self):
        self.satisfy_constraints([(typer.INT, typer.SUPERTYPE_OF, typer.INT, [self.spos])])

    def test_trivially_not_equal(self):
        self.assertRaises(typer.SylphTypeError, self.satisfy_constraints,
            [(typer.NONE, typer.SUPERTYPE_OF, typer.INT, [self.spos])])

    def test_not_equal_across_two(self):
        vartype = typer.TypeVariable("a")
        self.assertRaises(typer.SylphTypeError, self.satisfy_constraints,
            [(vartype, typer.SUPERTYPE_OF, typer.INT, [self.spos]),
             (vartype, typer.SUPERTYPE_OF, typer.NONE, [self.spos])])

    def test_not_equal_across_three(self):
        vartype1 = typer.TypeVariable("a")
        vartype2 = typer.TypeVariable("b")
        self.assertRaises(typer.SylphTypeError, self.satisfy_constraints,
            [(vartype1, typer.SUPERTYPE_OF, typer.INT, [self.spos]),
             (vartype2, typer.SUPERTYPE_OF, vartype1, [self.spos]),
             (vartype2, typer.SUPERTYPE_OF, typer.NONE, [self.spos])])
