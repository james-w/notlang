from testtools import TestCase
from rpython.rlib.parsing.lexer import SourcePos

from .. import ast, typer


class TypeCollectorTests(TestCase):

    spos = SourcePos(0, 0, 0)

    def test_ConstantInt(self):
        node = ast.ConstantInt(2, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(typer.INT, rtype)
        self.assertEqual([], t.equalities)

    def test_Assignment(self):
        varname = "a"
        rhs = ast.ConstantInt(2, self.spos)
        lhs = ast.Variable(varname, self.spos)
        node = ast.Assignment(lhs, rhs, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        vartype = typer.Type(varname)
        self.assertEqual(vartype, rtype)
        self.assertEqual(1, len(t.equalities))
        self.assertEqual((vartype, typer.INT), t.equalities[0])
        self.assertEqual(vartype, t.varmap[varname])

    def test_Return(self):
        rhs = ast.ConstantInt(2, self.spos)
        node = ast.Return(rhs, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(1, len(t.equalities))
        self.assertEqual((t.rtype, typer.INT), t.equalities[0])

    def test_Return_noarg(self):
        node = ast.Return(None, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(1, len(t.equalities))
        self.assertEqual((t.rtype, typer.NONE), t.equalities[0])

    def test_Variable_existing(self):
        varname = "a"
        node = ast.Variable(varname, self.spos)
        t = typer.TypeCollector()
        vartype = typer.Type(varname)
        t.varmap[varname] = vartype
        self.assertIs(vartype, t.dispatch(node))
        self.assertEqual([], t.equalities)

    def test_Variable_nonexisting(self):
        varname = "a"
        node = ast.Variable(varname, self.spos)
        t = typer.TypeCollector()
        self.assertEquals(typer.Type(varname), t.dispatch(node))
        self.assertEqual([], t.equalities)

    def test_BinOp(self):
        varname = "a"
        op = "+"
        lhs = ast.ConstantInt(1, self.spos)
        rhs = ast.Variable(varname, self.spos)
        node = ast.BinOp(op, lhs, rhs, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertEqual(typer.FunctionCallType(op, [typer.INT, typer.Type(varname)]), rtype)
        self.assertEqual([], t.equalities)

    def test_Conditional(self):
        varname = "a"
        condition = ast.ConstantInt(1, self.spos)
        block = ast.Assignment(ast.Variable(varname, self.spos), ast.ConstantInt(2, self.spos), self.spos)
        node = ast.Conditional(condition, block, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(2, len(t.equalities))
        self.assertEqual((typer.BOOL, typer.INT), t.equalities[0])
        self.assertEqual((typer.Type(varname), typer.INT), t.equalities[1])

    def test_While(self):
        varname = "a"
        condition = ast.ConstantInt(1, self.spos)
        block = ast.Assignment(ast.Variable(varname, self.spos), ast.ConstantInt(2, self.spos), self.spos)
        node = ast.While(condition, block, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(2, len(t.equalities))
        self.assertEqual((typer.BOOL, typer.INT), t.equalities[0])
        self.assertEqual((typer.Type(varname), typer.INT), t.equalities[1])

    def test_Function_noargs(self):
        fname = "foo"
        node = ast.Function(fname, [], self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertEqual(typer.FunctionCallType(fname, []), rtype)
        self.assertEqual([], t.equalities)

    def test_Function_args(self):
        fname = "foo"
        arg = ast.ConstantInt(1, self.spos)
        node = ast.Function(fname, [arg], self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertEqual(typer.FunctionCallType(fname, [typer.INT]), rtype)
        self.assertEqual([], t.equalities)

    def test_FuncDef(self):
        fname = "foo"
        argname = "bar"
        arg = ast.Variable(argname, self.spos)
        code = ast.Return(ast.Variable(argname, self.spos), self.spos)
        node = ast.FuncDef(fname, [arg], code, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual([], t.equalities)
        self.assertEqual(1, len(t.child_contexts))
        context = t.child_contexts[0]
        self.assertIsInstance(context, typer.TypeCollector)
        self.assertEqual([(context.rtype, typer.Type(argname))], context.equalities)

    def test_FuncDef_with_annotations(self):
        fname = "foo"
        argname = "bar"
        rtype = typer.Type("declared return")
        argtype = typer.Type("declared argtype")
        arg = ast.Variable(argname, self.spos)
        code = ast.Return(ast.Variable(argname, self.spos), self.spos)
        node = ast.FuncDef(fname, [arg], code, self.spos, rtype=rtype, argtypes=[argtype])
        t = typer.TypeCollector()
        self.assertIs(None, t.dispatch(node))
        self.assertEqual([], t.equalities)
        self.assertEqual(1, len(t.child_contexts))
        context = t.child_contexts[0]
        self.assertIsInstance(context, typer.TypeCollector)
        self.assertEqual([(context.rtype, argtype)], context.equalities)
        self.assertIs(rtype, context.rtype)
        self.assertIs(argtype, context.varmap[argname])

    def test_Block(self):
        # Test that children are dispatched to by default
        varname = "a"
        rhs = ast.ConstantInt(2, self.spos)
        lhs = ast.Variable(varname, self.spos)
        node = ast.Assignment(lhs, rhs, self.spos)
        t = typer.TypeCollector()
        t.dispatch(ast.Block([node], self.spos))
        self.assertEqual(1, len(t.equalities))
