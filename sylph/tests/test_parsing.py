from rpython.rlib.parsing.parsing import ParseError
from testtools import TestCase

from ..parsing import parse, Block


class ParsingTests(TestCase):
    # TODO: some tests for the shape of the parse tree

    def assert_parses_ok(self, string):
        try:
            assert isinstance(parse(string), Block)
        except ParseError as e:
            raise AssertionError(e.nice_error_message())

    def test_empty(self):
        self.assert_parses_ok("")

    def test_newline(self):
        self.assert_parses_ok("\n")

    def test_int(self):
        self.assert_parses_ok("1\n")

    def test_var(self):
        self.assert_parses_ok("a\n")

    def test_expr(self):
        self.assert_parses_ok("1 + 1\n")

    def test_expr_chain(self):
        self.assert_parses_ok("1 + 1 + 1\n")

    def test_comparison(self):
        self.assert_parses_ok("a == b\n")

    def test_comparison_chain(self):
        self.assert_parses_ok("a == b == c\n")

    def test_function_call(self):
        # XXX: optional args not supported by parser yet
        return
        self.assert_parses_ok("b()\n")

    def test_function_call_with_args(self):
        self.assert_parses_ok("b(1)\n")

    def test_function_call_with_variable_in_args(self):
        self.assert_parses_ok("b(a)\n")

    def test_function_call_with_expr_in_args(self):
        self.assert_parses_ok("b(a+1)\n")

    def test_assignment(self):
        self.assert_parses_ok("a = 1\n")

    def test_assignment_chain(self):
        self.assert_parses_ok("a = 1\n")

    def test_assign_expr(self):
        self.assert_parses_ok("a = 1 + 1\n")

    def test_assign_expr(self):
        self.assert_parses_ok("a = 1 + 1\n")

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
        self.assert_parses_ok("def a(b):\n    return 1\n\n")

    def test_funcdef_with_rtype(self):
        self.assert_parses_ok("def a(b) -> int:\n    return 1\n\n")

    def test_funcdef_with_argtype(self):
        self.assert_parses_ok("def a(b:int):\n    return 1\n\n")
