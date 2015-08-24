from testtools import TestCase

from .. import fmt, parsing, testing


class FormatterTests(TestCase):

    def setUp(self):
        super(FormatterTests, self).setUp()
        self.factory = testing.ASTFactory(self)

    def test_Variable(self):
        name = "a"
        variable = self.factory.variable(name=name)
        self.assertEqual(name, fmt.Formatter().dispatch(variable))

    def test_Block(self):
        var1 = self.factory.variable(name="a")
        var2 = self.factory.variable(name="b")
        block = self.factory.block(children=[var1, var2])
        self.assertEqual("ab", fmt.Formatter().dispatch(block))

    def test_Stmt(self):
        var1 = self.factory.variable(name="a")
        stmt = self.factory.stmt(child=var1)
        self.assertEqual("a\n", fmt.Formatter().dispatch(stmt))

    def test_Pass(self):
        pass_ = self.factory.pass_()
        self.assertEqual("pass", fmt.Formatter().dispatch(pass_))

    def test_ConstantInt(self):
        intval = "2"
        i = self.factory.int(int(intval))
        self.assertEqual(intval, fmt.Formatter().dispatch(i))

    def test_Assignment(self):
        source = self.factory.int(value=2)
        target = self.factory.variable(name="a")
        assignment = self.factory.assignment(source=source, target=target)
        self.assertEqual("a = 2", fmt.Formatter().dispatch(assignment))

    def test_Function(self):
        fname = self.factory.variable(name="afunction")
        args = [self.factory.int(value=2)]
        function = self.factory.function_call(function=fname, args=args)
        self.assertEqual("afunction(2)", fmt.Formatter().dispatch(function))

    def test_Function_with_params(self):
        fname = self.factory.variable(name="afunction")
        args = [self.factory.int(value=2)]
        params = [self.factory.type_reference(name="a")]
        function = self.factory.function_call(function=fname, args=args, type_params=params)
        self.assertEqual("afunction<a>(2)", fmt.Formatter().dispatch(function))

    def test_BinOp(self):
        op = "+"
        left = self.factory.int(value=2)
        right = self.factory.int(value=3)
        function = self.factory.binop(op=op, a=left, b=right)
        self.assertEqual("2 + 3", fmt.Formatter().dispatch(function))

    def test_Conditional(self):
        condition = self.factory.variable(name="a")
        true_block = self.factory.stmt(child=self.factory.int(value=2))
        conditional = self.factory.conditional(condition=condition, true_block=true_block)
        self.assertEqual("if a:\n    2\n", fmt.Formatter().dispatch(conditional))

    def test_Conditional_with_else_block(self):
        condition = self.factory.variable(name="a")
        true_block = self.factory.stmt(child=self.factory.int(value=2))
        false_block = self.factory.stmt(child=self.factory.int(value=3))
        conditional = self.factory.conditional(condition=condition, true_block=true_block, false_block=false_block)
        self.assertEqual("if a:\n    2\nelse:\n    3\n", fmt.Formatter().dispatch(conditional))

    def test_While(self):
        condition = self.factory.variable(name="a")
        block = self.factory.stmt(child=self.factory.int(value=2))
        loop = self.factory.while_(condition=condition, block=block)
        self.assertEqual("while a:\n    2\n", fmt.Formatter().dispatch(loop))

    def test_Attribute(self):
        target = self.factory.variable(name="a")
        attr = self.factory.attribute(target=target, name="b")
        self.assertEqual("a.b", fmt.Formatter().dispatch(attr))

    def test_Return(self):
        arg = self.factory.variable(name="a")
        ret = self.factory.return_(arg=arg)
        self.assertEqual("return a", fmt.Formatter().dispatch(ret))

    def test_Return_noarg(self):
        ret = self.factory.return_()
        self.assertEqual("return", fmt.Formatter().dispatch(ret))

    def test_FuncDef(self):
        name = "afunction"
        body = self.factory.stmt(child=self.factory.int(value=2))
        args = ["b", "c"]
        ret = self.factory.funcdef(name=name, body=body, args=args)
        self.assertEqual("def afunction(b, c):\n    2\n", fmt.Formatter().dispatch(ret))

    def test_FuncDef_with_rtype(self):
        name = "afunction"
        body = self.factory.stmt(child=self.factory.int(value=2))
        ret = self.factory.funcdef(name=name, body=body, rtype=self.factory.type_reference(name="c"))
        self.assertEqual("def afunction() -> c:\n    2\n", fmt.Formatter().dispatch(ret))

    def test_FuncDef_with_argtype(self):
        name = "afunction"
        body = self.factory.stmt(child=self.factory.int(value=2))
        ret = self.factory.funcdef(name=name, args=["b"], body=body, argtypes=[self.factory.type_reference(name="c")])
        self.assertEqual("def afunction(b: c):\n    2\n", fmt.Formatter().dispatch(ret))

    def test_FuncDef_with_type_params(self):
        name = "afunction"
        body = self.factory.stmt(child=self.factory.int(value=2))
        ret = self.factory.funcdef(name=name, body=body, type_params=[self.factory.type_reference(name="A")])
        self.assertEqual("def afunction<A>():\n    2\n", fmt.Formatter().dispatch(ret))

    def test_Case(self):
        case = self.factory.case(
            target=self.factory.variable(name="a"),
            cases=[
                self.factory.case_case(label=self.factory.variable(name="A"), block=self.factory.stmt(child=self.factory.int(value=2))),
                self.factory.case_case(label=self.factory.variable(name="B"), block=self.factory.stmt(child=self.factory.int(value=3))),
                ])
        self.assertEqual("case a:\n    A:\n        2\n    B:\n        3\n", fmt.Formatter().dispatch(case))

    def test_NewType(self):
        t = self.factory.newtype(
            block = self.factory.stmt(child=self.factory.pass_()),
            )
        self.assertEqual("new Type:\n    pass\n", fmt.Formatter().dispatch(t))

    def test_NewType_with_type_params(self):
        t = self.factory.newtype(
            block = self.factory.stmt(child=self.factory.pass_()),
            type_params=[self.factory.type_reference(name="A"), self.factory.type_reference(name="B")],
            )
        self.assertEqual("new Type<A, B>:\n    pass\n", fmt.Formatter().dispatch(t))

    def test_NewType_with_other_metatype(self):
        t = self.factory.newtype(
            block = self.factory.stmt(child=self.factory.pass_()),
            type_type="Enum",
            )
        self.assertEqual("new Enum:\n    pass\n", fmt.Formatter().dispatch(t))

    def test_NewType_with_options(self):
        t = self.factory.newtype(
            block = self.factory.stmt(child=self.factory.pass_()),
            options=[self.factory.type_option(name="A"), self.factory.type_option(name="B", members=["b"])],
            )
        self.assertEqual("new Type(A, B(b)):\n    pass\n", fmt.Formatter().dispatch(t))


class RoundtripTests(TestCase):

    def test_int(self):
        source = "1\n"
        self.assertEqual(source, fmt.Formatter().dispatch(parsing.parse(source)))

    def test_stuff(self):
        source = """\
Dog = new Type:
    a = 1

def dogger():
    return Dog()

dog = dogger()
print(dog.a)

def triple(arg: int, count: int) -> int:
    if count > 1:
        return arg * triple(arg, count - 1)
    else:
        return arg

def aaa(x):
    def ccc(x):
        return bbb(x)

    return ccc(x)

def bbb(i):
    if i > 0:
        return aaa(i - 1)
    else:
        return 0

def caller(arg):
    return plusone(arg)

def plusone(arg):
    return arg + 1

def truer(arg):
    return arg

def mappish(f, a):
    return f(a)

mappish(truer, 1)
mappish(truer, 2)
caller(1)
d = truer(2)
e = truer(1)
f = d + e
a = 1
b = a

while a > 0:
    a = a - 1

a = 1

if a == 1:
    b = a + 1 + 2
else:
    b = 5

trip = triple
c = trip(b + 1, 3) + 1
print(c)
l = List().append(1)
print(l.first())

Answer = new Enum(Y, N):
    def is_yes(self):
        case self:
            Answer.Y:
                return 1
            Answer.N:
                return 0

y = Answer.Y
print(y.is_yes())

Toople = new Tuple(Dog, Answer):
    pass

t = Toople(dog, Answer.Y)
print(t.second().is_yes())
"""
        self.assertEqual(source, fmt.Formatter().dispatch(parsing.parse(source)))
