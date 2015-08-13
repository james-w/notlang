from rpython.rlib.parsing.parsing import ParseError
from testtools import TestCase

from .. import ast
from ..parsing import parse as _parse


def parse(code):
    try:
        return _parse(code, trace_lexer=True)
    except ParseError as e:
        print e.nice_error_message(source=code)
        raise


class BasicParsingTests(TestCase):
    # TODO: some tests for the shape of the parse tree

    def assert_parses_ok(self, string):
        try:
            assert isinstance(parse(string), ast.Block)
        except ParseError as e:
            raise AssertionError(e.nice_error_message(source=string))

    def test_empty(self):
        self.skip("Bug in rlib? that means getsourcepos on blocks with no statements fail")
        self.assert_parses_ok("")

    def test_newline(self):
        self.skip("Bug in rlib? that means getsourcepos on blocks with no statements fail")
        self.assert_parses_ok("\n")

    def test_pass(self):
        self.assert_parses_ok("pass\n")

    def test_int(self):
        self.assert_parses_ok("1\n")

    def test_var(self):
        self.assert_parses_ok("a\n")

    def test_attr(self):
        self.assert_parses_ok("a.b\n")

    def test_attr_of_attr(self):
        self.assert_parses_ok("a.b.c\n")

    def test_expr(self):
        self.assert_parses_ok("1 + 1\n")

    def test_expr_chain(self):
        self.assert_parses_ok("1 + 1 + 1\n")

    def test_comparison(self):
        self.assert_parses_ok("a == b\n")

    def test_comparison_chain(self):
        self.assert_parses_ok("a == b == c\n")

    def test_function_call(self):
        self.assert_parses_ok("b()\n")

    def test_function_call_with_args(self):
        self.assert_parses_ok("b(1)\n")

    def test_function_call_with_attr_access(self):
        self.assert_parses_ok("b(c.d)\n")

    def test_function_call_with_variable_in_args(self):
        self.assert_parses_ok("b(a)\n")

    def test_function_call_with_expr_in_args(self):
        self.assert_parses_ok("b(a+1)\n")

    def test_function_call_with_two_args(self):
        self.assert_parses_ok("b(1, 2)\n")

    def test_function_call_on_object(self):
        self.assert_parses_ok("a.b()\n")

    def test_attr_of_func_result(self):
        self.assert_parses_ok("b().a\n")

    def test_func_on_attr_of_func_result(self):
        self.assert_parses_ok("b().a.c()\n")

    def test_assignment(self):
        self.assert_parses_ok("a = 1\n")

    def test_assign_expr(self):
        self.assert_parses_ok("a = 1 + 1\n")

    def test_assign_expr_chain(self):
        self.assert_parses_ok("a = 1 + 1 + 1\n")

    def test_assign_var(self):
        self.assert_parses_ok("a = b\n")

    def test_assign_function(self):
        self.assert_parses_ok("a = b(1)\n")

    def test_return(self):
        self.assert_parses_ok("return\n")

    def test_return_with_value(self):
        self.assert_parses_ok("return a\n")

    def test_if(self):
        self.assert_parses_ok("if a == a:\n    b = 1\n\n")

    def test_while(self):
        self.assert_parses_ok("while a == a:\n    b = 1\n\n")

    def test_funcdef(self):
        self.assert_parses_ok("def a():\n    return 1\n\n")

    def test_funcdef_one_arg(self):
        self.assert_parses_ok("def a(b):\n    return 1\n\n")

    def test_funcdef_two_args(self):
        self.assert_parses_ok("def a(b, c):\n    return 1\n\n")

    def test_funcdef_with_rtype(self):
        self.assert_parses_ok("def a() -> int:\n    return 1\n\n")

    def test_funcdef_with_one_arg_rtype(self):
        self.assert_parses_ok("def a(b) -> int:\n    return 1\n\n")

    def test_funcdef_with_argtype(self):
        self.assert_parses_ok("def a(b:int):\n    return 1\n\n")

    def test_funcdef_with_type_param(self):
        self.assert_parses_ok("def a<c>(b:c):\n    return 1\n\n")

    def test_new_decl(self):
        self.assert_parses_ok("Foo = new Type:\n    pass\n\n")

    def test_new_decl_with_type_param(self):
        self.assert_parses_ok("Foo = new Type<a>:\n    pass\n\n")

    def test_new_decl_with_field(self):
        self.assert_parses_ok("Foo = new Type<a>:\n    a = 1\n\n")

    def test_new_decl_with_method(self):
        self.assert_parses_ok("Foo = new Type<a>:\n    def foo(self):\n        pass\n\n")

    def test_new_decl_with_blank_line(self):
        self.assert_parses_ok("Foo = new Type:\n\n    pass\n\n")

    def test_new_enum(self):
        self.assert_parses_ok("A = new Enum(B, C):\n    pass\n\n")

    def test_case(self):
        self.assert_parses_ok("case a:\n\n    A:\n\n        pass\n\n")

    def test_case_with_two_cases(self):
        self.assert_parses_ok("case a:\n\n    A:\n\n        pass\n    B:\n        pass\n\n")


class ASTTests(TestCase):

    def test_Block(self):
        node = parse(" 1\n")
        self.assertIsInstance(node, ast.Block)
        self.assertEqual(1, node.sourcepos.i)

    def test_Stmt(self):
        node = parse(" 1\n")
        self.assertIsInstance(node, ast.Block)
        stmt = node.children[0]
        self.assertIsInstance(stmt, ast.Stmt)
        self.assertEqual(1, stmt.sourcepos.i)

    def test_Pass(self):
        node = parse(" pass\n")
        self.assertIsInstance(node, ast.Block)
        pass_node = node.children[0].children[0]
        self.assertIsInstance(pass_node, ast.Pass)
        self.assertEqual(1, pass_node.sourcepos.i)

    def test_ConstantInt(self):
        node = parse(" 1\n")
        self.assertIsInstance(node, ast.Block)
        cint = node.children[0].children[0]
        self.assertIsInstance(cint, ast.ConstantInt)
        self.assertEqual(1, cint.intval)
        self.assertEqual(1, cint.sourcepos.i)

    def test_Variable(self):
        node = parse(" a\n")
        self.assertIsInstance(node, ast.Block)
        var = node.children[0].children[0]
        self.assertIsInstance(var, ast.Variable)
        self.assertEqual("a", var.varname)
        self.assertEqual(1, var.sourcepos.i)

    def test_Assignment(self):
        node = parse(" a = 1\n")
        self.assertIsInstance(node, ast.Block)
        ass = node.children[0].children[0]
        self.assertIsInstance(ass, ast.Assignment)
        self.assertIsInstance(ass.var, ast.Variable)
        self.assertIsInstance(ass.children[0], ast.ConstantInt)
        self.assertEqual(1, ass.sourcepos.i)

    def test_Assignment_to_non_var(self):
        err = self.assertRaises(ParseError, parse, " 1 = 1\n")
        self.assertEqual("variable", err.errorinformation.failure_reasons[0])
        self.assertEqual(1, err.source_pos.i)

    def test_comparison(self):
        node = parse(" a == 1\n")
        self.assertIsInstance(node, ast.Block)
        comp = node.children[0].children[0]
        self.assertIsInstance(comp, ast.BinOp)
        self.assertEqual("==", comp.op)
        self.assertIsInstance(comp.children[0], ast.Variable)
        self.assertIsInstance(comp.children[1], ast.ConstantInt)
        self.assertEqual(1, comp.sourcepos.i)

    def test_arithmetic(self):
        node = parse(" a + 1\n")
        self.assertIsInstance(node, ast.Block)
        comp = node.children[0].children[0]
        self.assertIsInstance(comp, ast.BinOp)
        self.assertEqual("+", comp.op)
        self.assertIsInstance(comp.children[0], ast.Variable)
        self.assertIsInstance(comp.children[1], ast.ConstantInt)
        self.assertEqual(1, comp.sourcepos.i)

    def test_nested_arithmetic(self):
        node = parse(" a + 1 - b\n")
        self.assertIsInstance(node, ast.Block)
        comp = node.children[0].children[0]
        self.assertIsInstance(comp, ast.BinOp)
        self.assertEqual("-", comp.op)
        self.assertIsInstance(comp.children[0], ast.BinOp)
        self.assertIsInstance(comp.children[1], ast.Variable)
        self.assertEqual(5, comp.sourcepos.i)
        child = comp.children[0]
        self.assertEqual("+", child.op)
        self.assertIsInstance(child.children[0], ast.Variable)
        self.assertIsInstance(child.children[1], ast.ConstantInt)
        self.assertEqual(1, child.sourcepos.i)

    def test_Function(self):
        node = parse(" foo()\n")
        self.assertIsInstance(node, ast.Block)
        func = node.children[0].children[0]
        self.assertIsInstance(func, ast.Function)
        self.assertIsInstance(func.fname, ast.Variable)
        self.assertEqual("foo", func.fname.varname)
        self.assertEqual([], func.args)
        self.assertEqual(1, func.sourcepos.i)

    def test_Function_with_args(self):
        node = parse(" foo(a)\n")
        self.assertIsInstance(node, ast.Block)
        func = node.children[0].children[0]
        self.assertIsInstance(func, ast.Function)
        self.assertEqual("foo", func.fname.varname)
        self.assertEqual(1, len(func.args))
        self.assertEqual(1, func.sourcepos.i)
        arg = func.args[0]
        self.assertIsInstance(arg, ast.Variable)
        self.assertEqual("a", arg.varname)
        self.assertEqual(5, arg.sourcepos.i)

    def test_Function_with_type(self):
        node = parse(" foo<int>()\n")
        self.assertIsInstance(node, ast.Block)
        func = node.children[0].children[0]
        self.assertIsInstance(func, ast.Function)
        self.assertEqual("foo", func.fname.varname)
        self.assertEqual(0, len(func.args))
        self.assertEqual(['int'], func.type_params)
        self.assertEqual(1, func.sourcepos.i)

    def test_Return(self):
        node = parse(" return 1\n")
        self.assertIsInstance(node, ast.Block)
        ret = node.children[0].children[0]
        self.assertIsInstance(ret, ast.Return)
        self.assertEqual(1, len(ret.children))
        self.assertIsInstance(ret.children[0], ast.ConstantInt)
        self.assertEqual(1, ret.sourcepos.i)

    def test_Return_no_arg(self):
        node = parse(" return\n")
        self.assertIsInstance(node, ast.Block)
        ret = node.children[0].children[0]
        self.assertIsInstance(ret, ast.Return)
        self.assertEqual(0, len(ret.children))
        self.assertEqual(1, ret.sourcepos.i)

    def test_Conditional(self):
        node = parse(" if a == 1:\n     2\n\n")
        self.assertIsInstance(node, ast.Block)
        cond = node.children[0].children[0]
        self.assertIsInstance(cond, ast.Conditional)
        self.assertEqual(3, len(cond.children))
        self.assertIsInstance(cond.children[0], ast.BinOp)
        self.assertIsInstance(cond.children[1], ast.Block)
        self.assertIs(None, cond.children[2])
        self.assertEqual(1, cond.sourcepos.i)

    def test_Conditional_with_else(self):
        node = parse("if a == 1:\n    2\nelse:\n    1\n\n")
        self.assertIsInstance(node, ast.Block)
        cond = node.children[0].children[0]
        self.assertIsInstance(cond, ast.Conditional)
        self.assertEqual(3, len(cond.children))
        self.assertIsInstance(cond.children[0], ast.BinOp)
        self.assertIsInstance(cond.children[1], ast.Block)
        self.assertIsInstance(cond.children[2], ast.Block)
        self.assertEqual(0, cond.sourcepos.i)

    def test_While(self):
        node = parse(" while a == 1:\n     2\n\n")
        self.assertIsInstance(node, ast.Block)
        loop = node.children[0].children[0]
        self.assertIsInstance(loop, ast.While)
        self.assertEqual(2, len(loop.children))
        self.assertIsInstance(loop.children[0], ast.BinOp)
        self.assertIsInstance(loop.children[1], ast.Block)
        self.assertEqual(1, loop.sourcepos.i)

    def test_FuncDef(self):
        node = parse(" def foo():\n   2\n\n")
        self.assertIsInstance(node, ast.Block)
        func = node.children[0].children[0]
        self.assertIsInstance(func, ast.FuncDef)
        self.assertEqual(1, len(func.children))
        self.assertEqual([], func.args)
        self.assertEqual("foo", func.name)
        self.assertEqual([], func.argtypes)
        self.assertEqual(None, func.rtype)
        self.assertIsInstance(func.children[0], ast.Block)
        self.assertEqual(1, func.sourcepos.i)

    def test_FuncDef_with_type_params(self):
        node = parse(" def foo<a>():\n   2\n\n")
        self.assertIsInstance(node, ast.Block)
        func = node.children[0].children[0]
        self.assertIsInstance(func, ast.FuncDef)
        self.assertEqual(1, len(func.children))
        self.assertEqual([], func.args)
        self.assertEqual("foo", func.name)
        self.assertEqual([], func.argtypes)
        self.assertEqual(None, func.rtype)
        self.assertEqual(["a"], func.type_params)
        self.assertIsInstance(func.children[0], ast.Block)
        self.assertEqual(1, func.sourcepos.i)

    def test_FuncDef_with_rtype(self):
        node = parse(" def foo() -> a:\n   2\n\n")
        self.assertIsInstance(node, ast.Block)
        func = node.children[0].children[0]
        self.assertIsInstance(func, ast.FuncDef)
        self.assertEqual(1, len(func.children))
        self.assertEqual([], func.args)
        self.assertEqual("foo", func.name)
        self.assertEqual([], func.argtypes)
        self.assertEqual("a", func.rtype)
        self.assertEqual([], func.type_params)
        self.assertIsInstance(func.children[0], ast.Block)
        self.assertEqual(1, func.sourcepos.i)

    def test_FuncDef_with_type_params_and_rtype(self):
        node = parse(" def foo<a>() -> a:\n   2\n\n")
        self.assertIsInstance(node, ast.Block)
        func = node.children[0].children[0]
        self.assertIsInstance(func, ast.FuncDef)
        self.assertEqual(1, len(func.children))
        self.assertEqual([], func.args)
        self.assertEqual("foo", func.name)
        self.assertEqual([], func.argtypes)
        self.assertEqual("a", func.rtype)
        self.assertEqual(["a"], func.type_params)
        self.assertIsInstance(func.children[0], ast.Block)
        self.assertEqual(1, func.sourcepos.i)

    # TODO: more complete FuncDef tests

    def test_NewType(self):
        node = parse(" a = new Type:\n    pass\n\n")
        self.assertIsInstance(node, ast.Block)
        ass = node.children[0].children[0]
        self.assertIsInstance(ass, ast.Assignment)
        t = ass.children[0]
        self.assertIsInstance(t, ast.NewType)
        self.assertEqual("Type", t.type_type)
        self.assertEqual(5, t.sourcepos.i)

    def test_NewType_with_params(self):
        node = parse(" a = new Type<b>:\n    pass\n\n")
        self.assertIsInstance(node, ast.Block)
        ass = node.children[0].children[0]
        self.assertIsInstance(ass, ast.Assignment)
        t = ass.children[0]
        self.assertIsInstance(t, ast.NewType)
        self.assertEqual(["b"], t.type_params)
        self.assertEqual(5, t.sourcepos.i)

    def test_NewType_with_block(self):
        node = parse(" a = new Type<b>:\n    pass\n\n")
        self.assertIsInstance(node, ast.Block)
        ass = node.children[0].children[0]
        self.assertIsInstance(ass, ast.Assignment)
        t = ass.children[0]
        self.assertIsInstance(t, ast.NewType)
        self.assertEqual(1, len(t.children))
        self.assertEqual([], t.options)
        self.assertEqual(["b"], t.type_params)
        self.assertEqual(5, t.sourcepos.i)
        self.assertIsInstance(t.children[0], ast.Block)
        self.assertIsInstance(t.children[0].children[0].children[0], ast.Pass)

    def test_NewType_with_options(self):
        node = parse(" a = new Type(A):\n    pass\n\n")
        self.assertIsInstance(node, ast.Block)
        ass = node.children[0].children[0]
        self.assertIsInstance(ass, ast.Assignment)
        t = ass.children[0]
        self.assertIsInstance(t, ast.NewType)
        self.assertEqual(1, len(t.children))
        self.assertEqual(["A"], t.options)
        self.assertEqual([], t.type_params)
        self.assertEqual(5, t.sourcepos.i)

    def test_NewType_with_multiple_options(self):
        node = parse(" a = new Type(A, B):\n    pass\n\n")
        self.assertIsInstance(node, ast.Block)
        ass = node.children[0].children[0]
        self.assertIsInstance(ass, ast.Assignment)
        t = ass.children[0]
        self.assertIsInstance(t, ast.NewType)
        self.assertEqual(1, len(t.children))
        self.assertEqual(["A", "B"], t.options)
        self.assertEqual([], t.type_params)
        self.assertEqual(5, t.sourcepos.i)

    def test_NewType_with_params_and_options(self):
        node = parse(" a = new Type<B>(A):\n    pass\n\n")
        self.assertIsInstance(node, ast.Block)
        ass = node.children[0].children[0]
        self.assertIsInstance(ass, ast.Assignment)
        t = ass.children[0]
        self.assertIsInstance(t, ast.NewType)
        self.assertEqual(1, len(t.children))
        self.assertEqual(["A"], t.options)
        self.assertEqual(["B"], t.type_params)
        self.assertEqual(5, t.sourcepos.i)

    def test_Enum(self):
        node = parse(" a = new Enum(A):\n    pass\n\n")
        self.assertIsInstance(node, ast.Block)
        ass = node.children[0].children[0]
        self.assertIsInstance(ass, ast.Assignment)
        t = ass.children[0]
        self.assertIsInstance(t, ast.NewType)
        self.assertEqual("Enum", t.type_type)

    def test_Enum_without_options(self):
        err = self.assertRaises(ParseError, parse, " a = new Enum:\n    pass\n\n")
        self.assertEqual("options for Enum", err.errorinformation.failure_reasons[0])
        self.assertEqual(5, err.source_pos.i)

    def test_Attribute(self):
        node = parse(" a.b\n")
        self.assertIsInstance(node, ast.Block)
        attr = node.children[0].children[0]
        self.assertIsInstance(attr, ast.Attribute)
        self.assertEqual('b', attr.name)
        self.assertIsInstance(attr.children[0], ast.Variable)
        self.assertEqual('a', attr.children[0].varname)

    def test_Attribute_of_Attribute(self):
        node = parse(" a.b.c\n")
        self.assertIsInstance(node, ast.Block)
        attr = node.children[0].children[0]
        self.assertIsInstance(attr, ast.Attribute)
        self.assertEqual('c', attr.name)
        self.assertIsInstance(attr.children[0], ast.Attribute)
        self.assertEqual('b', attr.children[0].name)
        self.assertIsInstance(attr.children[0].children[0], ast.Variable)
        self.assertEqual('a', attr.children[0].children[0].varname)

    def test_Function_on_Attribute(self):
        node = parse(" a.b()\n")
        self.assertIsInstance(node, ast.Block)
        func = node.children[0].children[0]
        self.assertIsInstance(func, ast.Function)
        self.assertIsInstance(func.fname, ast.Attribute)
        self.assertEqual('b', func.fname.name)
        self.assertEqual('a', func.fname.children[0].varname)

    def test_Attribute_on_Function(self):
        node = parse(" a().b\n")
        self.assertIsInstance(node, ast.Block)
        attr = node.children[0].children[0]
        self.assertIsInstance(attr, ast.Attribute)
        self.assertEqual('b', attr.name)
        self.assertIsInstance(attr.children[0], ast.Function)
        self.assertIsInstance(attr.children[0].fname, ast.Variable)
        self.assertEqual('a', attr.children[0].fname.varname)

    def test_case(self):
        node = parse(" case a:\n    B:\n        pass\n    C:\n       pass\n")
        self.assertIsInstance(node, ast.Block)
        case = node.children[0].children[0]
        self.assertIsInstance(case, ast.Case)
        self.assertIsInstance(case.target, ast.Variable)
        self.assertEqual('a', case.target.varname)
        self.assertEqual(1, case.sourcepos.i)
        self.assertEqual(2, len(case.cases))
        self.assertIsInstance(case.cases[0], ast.CaseCase)
        self.assertIsInstance(case.cases[0].label, ast.Variable)
        self.assertEqual("B", case.cases[0].label.varname)
        self.assertIsInstance(case.cases[0].block, ast.Block)
        self.assertEqual(13, case.cases[0].sourcepos.i)
        self.assertEqual("C", case.cases[1].label.varname)
