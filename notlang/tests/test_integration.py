from rpython.rlib.parsing.parsing import ParseError

from .. import testing
from ..interpreter import interpret as _interpret, Panic
from ..typer import NotTypeError
from testtools import TestCase
from testtools.content import text_content


def interpret(source, testcase):
    testcase.addDetail('source', text_content(source))
    try:
        return _interpret(source, trace=True, trace_lexer=True, trace_typer=True)
    except (ParseError, NotTypeError) as e:
        print e.nice_error_message(source=source)
        raise


class VariableTests(TestCase):

    def test_assign(self):
        ret = interpret("""
a = 1
return a
""", self)
        self.assertThat(ret, testing.IsW_Int(1))

    def test_double_assign(self):
        ret = interpret("""
a = 1
a = 2
return a
""", self)
        self.assertThat(ret, testing.IsW_Int(2))


class BasicOpsTests(TestCase):

    def test_add(self):
        ret = interpret("""
a = 1
b = 2
return a + b
""", self)
        self.assertThat(ret, testing.IsW_Int(3))

    def test_subtract(self):
        ret = interpret("""
a = 3
b = 1
return a - b
""", self)
        self.assertThat(ret, testing.IsW_Int(2))


class ControlFlowTests(TestCase):

    def test_if(self):
        ret = interpret("""
a = 1
if a == 1:
    a = 2
else:
    a = 3
return a
""", self)
        self.assertThat(ret, testing.IsW_Int(2))

    def test_else(self):
        ret = interpret("""
a = 1
if a == 2:
    a = 2
else:
    a = 3
return a
""", self)
        self.assertThat(ret, testing.IsW_Int(3))

    def test_while(self):
        ret = interpret("""
a = 1
while a > 0:
    a = a - 1
return a
""", self)
        self.assertThat(ret, testing.IsW_Int(0))

    def test_case_pattern_match_failure(self):
        e = self.assertRaises(Panic, interpret, """
Answer = new Enum(Y, N):
    pass

y = Answer.Y

case y:
    Answer.N:
        return 0
""", self)
        self.assertEqual("Pattern match failure: 'Instance of Y'", e.msg.strval)


class FunctionTests(TestCase):

    def test_call(self):
        ret = interpret("""
def foo():
    return 2

return foo()
""", self)
        self.assertThat(ret, testing.IsW_Int(2))

    def test_arg(self):
        ret = interpret("""
def foo(a):
    return a + 1

return foo(1)
""", self)
        self.assertThat(ret, testing.IsW_Int(2))


class TypeTests(TestCase):

    def test_attribute(self):
        ret = interpret("""
Dog = new Type:
   a = 1

d = Dog()
return d.a
""", self)
        self.assertThat(ret, testing.IsW_Int(1))

    def test_method(self):
        ret = interpret("""
Dog = new Type:
    a = 2

    def age(self):
        return self.a

d = Dog()
return d.age()
""", self)
        self.assertThat(ret, testing.IsW_Int(2))


class BuiltinTests(TestCase):

    def test_List(self):
        ret = interpret("""
l = List().append(1)
return l.first()
""", self)
        self.assertThat(ret, testing.IsW_Int(1))


class TupleTests(TestCase):

    def test_basic_types(self):
        ret = interpret("""
Toople = new Tuple(int, int):
    pass


t = Toople(1, 2)
return t.second()
""", self)
        self.assertThat(ret, testing.IsW_Int(2))

    def test_custom_types(self):
        ret = interpret("""
Dog = new Type:
    a = 1


Toople = new Tuple(Dog, int):
    pass


d = Dog()
t = Toople(d, 2)
return t.first().a
""", self)
        self.assertThat(ret, testing.IsW_Int(1))


class EnumTests(TestCase):

    def test_case(self):
        ret = interpret("""
Answer = new Enum(Y, N):
    pass

y = Answer.Y

case y:
    Answer.Y:
         a = 1
    Answer.N:
         a = 2

return a
""", self)
        self.assertThat(ret, testing.IsW_Int(1))

    def test_methods(self):
        ret = interpret("""
Answer = new Enum(Y, N):

    def is_yes(self):
        case self:
            Answer.Y:
                return 1
            Answer.N:
                return 0

y = Answer.Y

return y.is_yes()
""", self)
        self.assertThat(ret, testing.IsW_Int(1))

    def test_parameterised(self):
        ret = interpret("""
Option = new Enum<A>(Nothing, Some(A)):
    pass

o = Option.Some(1)

case o:
    Option.Nothing:
        a = 0
    Option.Some(b):
        a = b

return a
""", self)
        self.assertThat(ret, testing.IsW_Int(1))
