from testtools import TestCase
from rpython.rlib.parsing.lexer import SourcePos

from .. import ast, testing


class GatherNamesTests(TestCase):

    spos = SourcePos(0, 0, 0)

    def test_Variable(self):
        varname = "a"
        node = ast.Variable(varname, self.spos)
        self.assertEqual([varname], ast.GatherNames().dispatch(node))

    def test_ConstantInt(self):
        node = ast.ConstantInt(1, self.spos)
        self.assertEqual([], ast.GatherNames().dispatch(node))

    def test_Block(self):
        varname = "a"
        var = ast.Variable(varname, self.spos)
        node = ast.Block([var], self.spos)
        self.assertEqual([varname], ast.GatherNames().dispatch(node))

    def test_FuncDef(self):
        fname = "foo"
        code = ast.ConstantInt(1, self.spos)
        node = ast.FuncDef(fname, [], code, self.spos)
        self.assertEqual([fname], ast.GatherNames().dispatch(node))


class NodeTests(TestCase):

    def setUp(self):
        super(NodeTests, self).setUp()
        self.factory = testing.ASTFactory(self)

    def test_Block(self):
        statements = [self.factory.statement(), self.factory.statement()]
        block = ast.Block(statements, self.factory.spos)
        self.assertIs(statements, block.statements)
        self.assertIs(self.factory.spos, block.sourcepos)

    def test_Stmt(self):
        expr = self.factory.return_()
        stmt = ast.Stmt(expr, self.factory.spos)
        self.assertIs(expr, stmt.expr)
        self.assertIs(self.factory.spos, stmt.sourcepos)

    def test_Assignment(self):
        source = self.factory.int()
        target = self.factory.variable()
        stmt = ast.Assignment(target, source, self.factory.spos)
        self.assertIs(target, stmt.target)
        self.assertIs(source, stmt.source)
        self.assertIs(self.factory.spos, stmt.sourcepos)

    def test_Function(self):
        fname = self.factory.variable()
        args = [self.factory.int()]
        stmt = ast.Function(fname, args, self.factory.spos)
        self.assertIs(fname, stmt.fname)
        self.assertEqual(args, stmt.args)
        self.assertIs(self.factory.spos, stmt.sourcepos)

    def test_BinOp(self):
        fname = "+"
        left = self.factory.int()
        right = self.factory.int()
        stmt = ast.BinOp(fname, left, right, self.factory.spos)
        self.assertIs(fname, stmt.fname)
        self.assertEqual([left, right], stmt.args)
        self.assertIs(self.factory.spos, stmt.sourcepos)

    def test_Conditional(self):
        condition = self.factory.variable()
        true_block = self.factory.int()
        false_block = self.factory.int()
        stmt = ast.Conditional(condition, true_block, false_block, self.factory.spos)
        self.assertIs(condition, stmt.condition)
        self.assertIs(true_block, stmt.true_block)
        self.assertIs(false_block, stmt.false_block)
        self.assertIs(self.factory.spos, stmt.sourcepos)

    def test_While(self):
        condition = self.factory.variable()
        block = self.factory.int()
        stmt = ast.While(condition, block, self.factory.spos)
        self.assertIs(condition, stmt.condition)
        self.assertIs(block, stmt.block)
        self.assertIs(self.factory.spos, stmt.sourcepos)

    def test_Funcdef(self):
        name = "afunction"
        args = ["a", "b"]
        code = self.factory.block()
        stmt = ast.FuncDef(name, args, code, self.factory.spos)
        self.assertIs(name, stmt.name)
        self.assertIs(args, stmt.args)
        self.assertIs(code, stmt.code)
        self.assertIs(self.factory.spos, stmt.sourcepos)

    def test_Return_noarg(self):
        stmt = ast.Return(None, self.factory.spos)
        self.assertIs(None, stmt.arg)
        self.assertIs(self.factory.spos, stmt.sourcepos)

    def test_Return(self):
        arg = self.factory.int()
        stmt = ast.Return(arg, self.factory.spos)
        self.assertIs(arg, stmt.arg)
        self.assertIs(self.factory.spos, stmt.sourcepos)

    def test_NewType(self):
        block = self.factory.block()
        type_type = "Type"
        stmt = ast.NewType(block, type_type, self.factory.spos)
        self.assertIs(block, stmt.block)
        self.assertIs(self.factory.spos, stmt.sourcepos)

    def test_Case(self):
        target = self.factory.variable()
        cases = [self.factory.case_case()]
        stmt = ast.Case(target, cases, self.factory.spos)
        self.assertIs(target, stmt.target)
        self.assertEqual(cases, stmt.cases)
        self.assertIs(self.factory.spos, stmt.sourcepos)

    def test_CaseCase(self):
        label = self.factory.variable()
        block = self.factory.block()
        stmt = ast.CaseCase(label, block, self.factory.spos)
        self.assertIs(label, stmt.label)
        self.assertIs(block, stmt.block)
        self.assertIs(self.factory.spos, stmt.sourcepos)

    def test_Attribute(self):
        name = "attr"
        target = self.factory.variable()
        stmt = ast.Attribute(target, name, self.factory.spos)
        self.assertIs(name, stmt.name)
        self.assertIs(target, stmt.target)
        self.assertIs(self.factory.spos, stmt.sourcepos)
