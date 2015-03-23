from testtools import TestCase
from rpython.rlib.parsing.lexer import SourcePos

from .. import ast


class GatherNamesTests(TestCase):

    spos = SourcePos(0, 0, 0)

    def test_Variable(self):
        varname = "a"
        node = ast.Variable(varname, self.spos)
        self.assertEqual(set([varname]), ast.GatherNames().dispatch(node))

    def test_ConstantInt(self):
        node = ast.ConstantInt(1, self.spos)
        self.assertEqual(set(), ast.GatherNames().dispatch(node))

    def test_Block(self):
        varname = "a"
        var = ast.Variable(varname, self.spos)
        node = ast.Block([var], self.spos)
        self.assertEqual(set([varname]), ast.GatherNames().dispatch(node))

    def test_FuncDef(self):
        fname = "foo"
        code = ast.ConstantInt(1, self.spos)
        node = ast.FuncDef(fname, [], code, self.spos)
        self.assertEqual(set([fname]), ast.GatherNames().dispatch(node))
