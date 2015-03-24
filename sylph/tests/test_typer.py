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
        vartype = typer.TypeVariable(varname)
        self.assertEqual(vartype, rtype)
        self.assertEqual(1, len(t.equalities))
        self.assertEqual((vartype, typer.INT, [self.spos]), t.equalities[0])
        self.assertEqual(vartype, t.varmap[varname])

    def test_Return(self):
        rhs = ast.ConstantInt(2, self.spos)
        node = ast.Return(rhs, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(1, len(t.equalities))
        self.assertEqual((t.rtype, typer.INT, [self.spos]), t.equalities[0])

    def test_Return_noarg(self):
        node = ast.Return(None, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(1, len(t.equalities))
        self.assertEqual((t.rtype, typer.NONE, [self.spos]), t.equalities[0])

    def test_Variable_existing(self):
        varname = "a"
        node = ast.Variable(varname, self.spos)
        t = typer.TypeCollector()
        vartype = typer.TypeVariable(varname)
        t.varmap[varname] = vartype
        self.assertIs(vartype, t.dispatch(node))
        self.assertEqual([], t.equalities)

    def test_Variable_nonexisting(self):
        varname = "a"
        node = ast.Variable(varname, self.spos)
        t = typer.TypeCollector()
        self.assertEquals(typer.TypeVariable(varname), t.dispatch(node))
        self.assertEqual([], t.equalities)

    def test_BinOp(self):
        varname = "a"
        op = "+"
        lhs = ast.ConstantInt(1, self.spos)
        rhs = ast.Variable(varname, self.spos)
        node = ast.BinOp(op, lhs, rhs, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertEqual(typer.FunctionCallType(op, [typer.INT, typer.TypeVariable(varname)]), rtype)
        self.assertEqual([], t.equalities)

    def test_nested_BinOp(self):
        varname = "a"
        op = "+"
        lhs = ast.ConstantInt(1, self.spos)
        rhs = ast.Variable(varname, self.spos)
        node = ast.BinOp(op, lhs, rhs, self.spos)
        node = ast.BinOp(op, lhs, node, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        new_tvar = typer.TypeVariable("(rtype of + int -> a)")
        self.assertEqual(typer.FunctionCallType(op, [typer.INT, new_tvar]), rtype)
        self.assertEqual(
            [(new_tvar,
              typer.FunctionCallType(op, [typer.INT, typer.TypeVariable(varname)]),
              [self.spos])],
            t.equalities)
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
        self.assertEqual(2, len(t.equalities))
        self.assertEqual((typer.BOOL, typer.INT, [self.spos]), t.equalities[0])
        self.assertEqual((typer.TypeVariable(varname), typer.INT, [self.spos]), t.equalities[1])

    def test_Conditional_with_else(self):
        varname = "a"
        condition = ast.ConstantInt(1, self.spos)
        block = ast.Assignment(ast.Variable(varname, self.spos), ast.ConstantInt(2, self.spos), self.spos)
        node = ast.Conditional(condition, block, block, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(3, len(t.equalities))
        self.assertEqual((typer.BOOL, typer.INT, [self.spos]), t.equalities[0])
        self.assertEqual((typer.TypeVariable(varname), typer.INT, [self.spos]), t.equalities[1])
        self.assertEqual((typer.TypeVariable(varname), typer.INT, [self.spos]), t.equalities[2])

    def test_While(self):
        varname = "a"
        condition = ast.ConstantInt(1, self.spos)
        block = ast.Assignment(ast.Variable(varname, self.spos), ast.ConstantInt(2, self.spos), self.spos)
        node = ast.While(condition, block, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual(2, len(t.equalities))
        self.assertEqual((typer.BOOL, typer.INT, [self.spos]), t.equalities[0])
        self.assertEqual((typer.TypeVariable(varname), typer.INT, [self.spos]), t.equalities[1])

    def test_Function_noargs(self):
        fname = "foo"
        node = ast.Function(fname, [], self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertEqual(typer.TypeVariable("return of %s (noargs)" % fname), rtype)
        
        self.assertEqual([(rtype, typer.FunctionCallType(typer.TypeVariable(fname), []), [self.spos])], t.equalities)

    # TODO: test recursion

    def test_Function_args(self):
        fname = "foo"
        arg = ast.ConstantInt(1, self.spos)
        node = ast.Function(fname, [arg], self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertEqual(typer.TypeVariable("return of %s int" % fname), rtype)
        self.assertEqual(
            [(rtype, typer.FunctionCallType(typer.TypeVariable(fname), [typer.INT]), [self.spos])],
            t.equalities)

    def test_FuncDef(self):
        fname = "foo"
        argname = "bar"
        code = ast.Return(ast.Variable(argname, self.spos), self.spos)
        node = ast.FuncDef(fname, [argname], code, self.spos)
        t = typer.TypeCollector()
        rtype = t.dispatch(node)
        self.assertIs(None, rtype)
        self.assertEqual([], t.equalities)
        self.assertEqual(1, len(t.child_contexts))
        self.assertIn(fname, t.child_contexts)
        context = t.child_contexts[fname]
        self.assertIsInstance(context, typer.TypeCollector)
        self.assertEqual([(context.rtype, typer.TypeVariable(argname), [self.spos])], context.equalities)
        self.assertEqual({argname: typer.TypeVariable(argname)}, context.varmap)

    def test_FuncDef_with_annotations(self):
        fname = "foo"
        argname = "bar"
        rtype = "Boolean"
        argtype = "int"
        code = ast.Return(ast.Variable(argname, self.spos), self.spos)
        node = ast.FuncDef(fname, [argname], code, self.spos, rtype=rtype, argtypes=[argtype])
        t = typer.TypeCollector()
        self.assertIs(None, t.dispatch(node))
        self.assertEqual([], t.equalities)
        self.assertEqual(1, len(t.child_contexts))
        self.assertIn(fname, t.child_contexts)
        context = t.child_contexts[fname]
        self.assertIsInstance(context, typer.TypeCollector)
        # XXX: check that these are actually BOOL and INT
        self.assertEqual([(context.rtype, typer.Type(argtype), [self.spos])], context.equalities)
        self.assertEqual(typer.Type(rtype), context.rtype)
        self.assertEqual(typer.Type(argtype), context.varmap[argname])

    def test_Block(self):
        # Test that children are dispatched to by default
        varname = "a"
        rhs = ast.ConstantInt(2, self.spos)
        lhs = ast.Variable(varname, self.spos)
        node = ast.Assignment(lhs, rhs, self.spos)
        t = typer.TypeCollector()
        t.dispatch(ast.Block([node], self.spos))
        self.assertEqual(1, len(t.equalities))


class SatisfyEqualitiesTests(TestCase):

    spos = SourcePos(0, 0, 0)

    def satisfy_equalities(self, equalities):
        varmap = {}
        functions = typer.FUNCTIONS
        return typer.satisfy_equalities(equalities, varmap, functions)

    def test_empty(self):
        self.satisfy_equalities([])

    def test_trivially_equal(self):
        self.satisfy_equalities([(typer.INT, typer.INT, [self.spos])])

    def test_trivially_not_equal(self):
        self.assertRaises(typer.SylphTypeError, self.satisfy_equalities,
            [(typer.NONE, typer.INT, [self.spos])])

    def test_not_equal_across_two(self):
        vartype = typer.TypeVariable("a")
        self.assertRaises(typer.SylphTypeError, self.satisfy_equalities,
            [(vartype, typer.INT, [self.spos]),
             (vartype, typer.NONE, [self.spos])])

    def test_equal_across_two(self):
        vartype = typer.TypeVariable("a")
        self.satisfy_equalities(
            [(vartype, typer.INT, [self.spos]),
             (vartype, typer.INT, [self.spos])])

    def test_not_equal_across_three(self):
        vartype1 = typer.TypeVariable("a")
        vartype2 = typer.TypeVariable("b")
        self.assertRaises(typer.SylphTypeError, self.satisfy_equalities,
            [(vartype1, typer.INT, [self.spos]),
             (vartype2, vartype1, [self.spos]),
             (vartype2, typer.NONE, [self.spos])])

    def test_equal_across_three(self):
        vartype1 = typer.TypeVariable("a")
        vartype2 = typer.TypeVariable("b")
        self.satisfy_equalities(
            [(vartype1, typer.INT, [self.spos]),
             (vartype2, vartype1, [self.spos]),
             (vartype2, typer.INT, [self.spos])])

    def test_use_before_assignment(self):
        vartype1 = typer.TypeVariable("a")
        vartype2 = typer.TypeVariable("b")
        self.assertRaises(typer.SylphNameError, self.satisfy_equalities,
            [(vartype1, typer.INT, [self.spos]),
             (vartype1, vartype2, [self.spos]),
             (vartype2, typer.NONE, [self.spos])])

    def test_function(self):
        self.satisfy_equalities(
            [(typer.INT, typer.FunctionCallType('+', [typer.INT, typer.INT]), [self.spos])])

    def test_no_function(self):
        self.assertRaises(typer.SylphTypeError,
            self.satisfy_equalities,
            [(typer.NONE,
              typer.FunctionCallType('+', [typer.INT, typer.INT]),
              [self.spos])])

    def test_function_with_typevar_arg(self):
        vartype = typer.TypeVariable("a")
        self.satisfy_equalities(
            [(vartype, typer.INT, [self.spos]),
             (typer.INT, typer.FunctionCallType('+', [vartype, typer.INT]), [self.spos])])

    def test_function_with_undefined_typevar_arg(self):
        vartype = typer.TypeVariable("a")
        self.assertRaises(typer.SylphNameError,
            self.satisfy_equalities,
            [(typer.INT, typer.FunctionCallType('+', [vartype, typer.INT]), [self.spos])])

    def test_function_with_noargs(self):
        self.satisfy_equalities(
            [(typer.BOOL, typer.FunctionCallType('true', []), [self.spos])])

    def test_equal_to_multiple_functions(self):
        vartype = typer.TypeVariable("a")
        self.satisfy_equalities(
            [(vartype, typer.INT, [self.spos]),
             (vartype, typer.FunctionCallType('+', [vartype, typer.INT]), [self.spos]),
             (vartype, typer.FunctionCallType('-', [vartype, typer.INT]), [self.spos])])

    def test_no_matching_function_with_typevar(self):
        vartype = typer.TypeVariable("a")
        self.assertRaises(typer.SylphTypeError, self.satisfy_equalities,
            [(vartype, typer.BOOL, [self.spos]),
             (vartype, typer.FunctionCallType('+', [vartype, typer.INT]), [self.spos])])

    def test_function_with_alias(self):
        vartype = typer.TypeVariable("a")
        self.assertRaises(typer.SylphTypeError, self.satisfy_equalities,
            [(vartype, typer.BOOL, [self.spos]),
             (vartype, typer.FunctionCallType('+', [vartype, typer.INT]), [self.spos])])
