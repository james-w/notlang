from hypothesis import strategies
from rpython.rlib.parsing.lexer import SourcePos

from ... import ast


keywords = ["if", "while", "else", "def", "new"]


def SourcePosStrategy():
    # This is boring, so make it easy
    return strategies.just(SourcePos(0, 0, 0))
    # return strategies.builds(SourcePos, strategies.integers(), strategies.integers(), strategies.integers())


def IdentifierStrategy(name=None):
    # TODO: better alphabet (unicode, numbers in middle, underscores etc.)
    return strategies.text(alphabet="abcdefghijklmnopqrstuvwxyz", min_size=1, average_size=2).filter(lambda x: x not in keywords)


def VariableStrategy(name=None):
    if name is None:
        name = IdentifierStrategy()
    return strategies.builds(ast.Variable, name, SourcePosStrategy())


def IntStrategy():
    return strategies.builds(ast.ConstantInt, strategies.integers(), SourcePosStrategy())


def BinOpStrategy(children=None):
    if children is None:
        children = IntStrategy()
    return strategies.builds(ast.BinOp, strategies.sampled_from(['+', '-', '*', '>', '<', '==']), children, children, SourcePosStrategy())


def ExpressionStrategy():
    return strategies.recursive(IntStrategy() | VariableStrategy(), lambda children: BinOpStrategy(children=children), max_leaves=10)


def AssignmentStrategy():
    return strategies.builds(ast.Assignment, VariableStrategy(), ExpressionStrategy(), SourcePosStrategy())


def FunctionCallStrategy(name=None, args=None):
    if name is None:
        name = VariableStrategy()
    if args is None:
        args = strategies.lists(ExpressionStrategy(), average_size=2)
    return strategies.builds(ast.Function, name, args, SourcePosStrategy())


def PrintStrategy():
    return FunctionCallStrategy(name=VariableStrategy(name=strategies.just('print')), args=strategies.lists(ExpressionStrategy(), min_size=1, max_size=1))


def StatementStrategy():
    def make_children(children):
        wrapped = strategies.builds(ast.Stmt, children, SourcePosStrategy())
        return ConditionalStrategy(statements=wrapped) | WhileStrategy(statements=wrapped) | FuncDefStrategy(statements=wrapped)
    expr = strategies.recursive(ExpressionStrategy() | PrintStrategy() | ReturnStrategy(), make_children, max_leaves=10)
    return strategies.builds(ast.Stmt, expr, SourcePosStrategy())


def BlockStrategy(statements=None):
    if statements is None:
        statements = strategies.lists(StatementStrategy(), min_size=1, average_size=2)
    else:
        statements = strategies.lists(statements, min_size=1, average_size=2)
    return strategies.builds(ast.Block, statements, SourcePosStrategy())


def ConditionalStrategy(statements=None):
    target = ExpressionStrategy()
    true_block = BlockStrategy(statements=statements)
    else_block = strategies.one_of(BlockStrategy(statements=statements), strategies.none())
    return strategies.builds(ast.Conditional, target, true_block, else_block,  SourcePosStrategy())


def WhileStrategy(statements=None):
    condition = ExpressionStrategy()
    block = BlockStrategy(statements=statements)
    return strategies.builds(ast.While, condition, block, SourcePosStrategy())


def FuncDefStrategy(statements=None):
    name = IdentifierStrategy()
    args = strategies.lists(IdentifierStrategy(), average_size=2)
    code = BlockStrategy(statements=statements)
    # TODO: better type descriptor strategies
    rtype = IdentifierStrategy()
    argtypes = strategies.lists(IdentifierStrategy(), average_size=2)
    # TODO: parser only supports 1 type param today
    type_params = strategies.lists(IdentifierStrategy(), max_size=1)
    return strategies.builds(ast.FuncDef, name, args, code, SourcePosStrategy(), rtype=rtype, argtypes=argtypes, type_params=type_params)


def ReturnStrategy():
    arg = strategies.one_of(ExpressionStrategy(), strategies.none())
    return strategies.builds(ast.Return, arg, SourcePosStrategy())
