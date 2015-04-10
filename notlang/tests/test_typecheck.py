from rpython.rlib.parsing.lexer import SourcePos
from testtools import TestCase
from testtools.matchers import Equals, Is, IsInstance, MatchesListwise

from .. import testing, typecheck, typer


class ConstraintGenerationTests(TestCase):

    spos = SourcePos(0, 0, 0)

    def setUp(self):
        super(ConstraintGenerationTests, self).setUp()
        self.factory = testing.ASTFactory(self)

    def get_type_checker(self):
        return typecheck.TypeChecker(typecheck.TypeEnv())

    def test_ConstantInt(self):
        node = self.factory.int()
        checker = self.get_type_checker()
        t = checker.dispatch(node)
        self.assertThat(t, Is(typecheck.INT))
        self.assertThat(checker.constraints, Equals([]))

    def test_Variable_missing(self):
        node = self.factory.variable()
        checker = self.get_type_checker()
        self.assertRaises(typecheck.NotNameError, checker.dispatch, node)

    def test_Variable(self):
        node = self.factory.variable()
        checker = self.get_type_checker()
        checker.env.extend(node.varname, typecheck.INT)
        t = checker.dispatch(node)
        self.assertThat(t, Is(typecheck.INT))
        self.assertThat(checker.constraints, Equals([]))

    def test_Assignment(self):
        node = self.factory.assignment()
        checker = self.get_type_checker()
        t = checker.dispatch(node)
        self.assertThat(t, IsInstance(typecheck.TypeVar))
        self.assertThat(checker.env.env[node.var.varname], Is(t))
        self.assertThat(
            checker.constraints,
            MatchesListwise([
                testing.ConstraintMatches(
                    Is(t),
                    typer.SUPERTYPE_OF,
                    Is(typecheck.INT),
                    [Is(self.factory.spos)])]))

    def test_Pass(self):
        node = self.factory.pass_()
        checker = self.get_type_checker()
        t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(checker.constraints, Equals([]))

    def test_Conditional(self):
        node = self.factory.conditional()
        checker = self.get_type_checker()
        t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(
            checker.constraints,
            MatchesListwise([
                testing.ConstraintMatches(
                    Is(typecheck.INT),
                    typer.SUBTYPE_OF,
                    Is(typecheck.BOOL),
                    [Is(self.factory.spos)],
                    ),
                ]))

    def test_Conditional_visits_true_block(self):
        node = self.factory.conditional(true_block=self.factory.assignment())
        checker = self.get_type_checker()
        t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(len(checker.constraints), Equals(2))
        self.assertThat(
            checker.constraints[1],
            testing.ConstraintMatches(
                IsInstance(typecheck.TypeVar),
                typer.SUPERTYPE_OF,
                Is(typecheck.INT),
                [Is(self.factory.spos)],
                ),
            )

    def test_Conditional_visits_false_block(self):
        node = self.factory.conditional(false_block=self.factory.assignment())
        checker = self.get_type_checker()
        t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(len(checker.constraints), Equals(2))
        self.assertThat(
            checker.constraints[1],
            testing.ConstraintMatches(
                IsInstance(typecheck.TypeVar),
                typer.SUPERTYPE_OF,
                Is(typecheck.INT),
                [Is(self.factory.spos)],
                ),
            )

    def test_While(self):
        node = self.factory.while_(block=self.factory.assignment())
        checker = self.get_type_checker()
        t = checker.dispatch(node)
        self.assertThat(t, Is(None))
        self.assertThat(
            checker.constraints,
            MatchesListwise([
                testing.ConstraintMatches(
                    Is(typecheck.INT),
                    typer.SUBTYPE_OF,
                    Is(typecheck.BOOL),
                    [Is(self.factory.spos)],
                    ),
                testing.ConstraintMatches(
                    IsInstance(typecheck.TypeVar),
                    typer.SUPERTYPE_OF,
                    Is(typecheck.INT),
                    [Is(self.factory.spos)],
                    ),
                ]))

    def test_BinOp(self):
        node = self.factory.binop()
        checker = self.get_type_checker()
        t = checker.dispatch(node)
        self.assertThat(t, IsInstance(typecheck.TypeVar))
        self.assertThat(checker.constraints, Equals([]))
