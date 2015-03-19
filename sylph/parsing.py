import py
from rpython.rlib.objectmodel import we_are_translated
from rpython.rlib.parsing.ebnfparse import parse_ebnf
from rpython.rlib.parsing.parsing import PackratParser, ParseError
from rpython.rlib.parsing.tree import RPythonVisitor

from . import bytecode, compilercontext, lexer as mod_lexer, sylphdir
from .objectspace import TheNone


def check_for_missing_names(names, regexs, rules):
    known_names = mod_lexer.dict_from_keys(names, True)
    known_names["EOF"] = True
    known_names["INDENT"] = True
    known_names["DEDENT"] = True
    known_names["NEWLINE"] = True
    for rule in rules:
        known_names[rule.nonterminal] = True
    for rule in rules:
        for expansion in rule.expansions:
            for symbol in expansion:
                if symbol not in known_names:
                    raise ValueError("symbol '%s' not known" % (symbol, ))


def make_parse_function(regexs, rules, eof=False):
    names, regexs = zip(*regexs)
    if "IGNORE" in names:
        ignore = ["IGNORE"]
    else:
        ignore = []
    check_for_missing_names(names, regexs, rules)
    lexer = mod_lexer.IndentTrackingLexer(list(regexs), list(names), ignore=ignore)
    parser = PackratParser(rules, rules[0].nonterminal)
    def parse(s):
        tokens = lexer.tokenize(s, eof=eof)
        s = parser.parse(tokens)
        if not we_are_translated():
            try:
                if py.test.config.option.view:
                    s.view()
            except AttributeError:
                pass

        return s
    return parse


grammar = py.path.local(sylphdir).join('grammar.txt').read("rt")
try:
    regexs, rules, ToAST = parse_ebnf(grammar)
except ParseError as e:
    print e.nice_error_message()
    raise
_parse = make_parse_function(regexs, rules, eof=True)


class Node(object):
    """ The abstract AST node
    """

    def __init__(self):
        pass

    def __eq__(self, other):
        return (self.__class__ == other.__class__ and
                self.__dict__ == other.__dict__)

    def __ne__(self, other):
        return not self == other

    def get_extra_dot_info(self):
        return ""


class NonTerminal(Node):

    def __init__(self):
        super(NonTerminal, self).__init__()
        self.children = []

    def compile(self, ctx):
        for stmt in self.children:
            stmt.compile(ctx)


class Block(NonTerminal):
    """ A list of statements
    """
    def __init__(self, stmts):
        super(Block, self).__init__()
        self.children = stmts


class Stmt(NonTerminal):
    """ A single statement
    """
    def __init__(self, expr):
        super(Stmt, self).__init__()
        self.children = [expr]


class ConstantInt(Node):
    """ Represent a constant
    """
    def __init__(self, intval):
        super(ConstantInt, self).__init__()
        self.intval = intval

    def compile(self, ctx):
        from .interpreter import W_Int
        w = W_Int(self.intval)
        ctx.emit(bytecode.LOAD_CONSTANT, ctx.register_constant(w))

    def get_extra_dot_info(self):
        return str(self.intval)


class BinOp(NonTerminal):
    """ A binary operation
    """
    def __init__(self, op, left, right):
        super(BinOp, self).__init__()
        self.op = op
        self.children = [left, right]

    def compile(self, ctx):
        self.children[0].compile(ctx)
        self.children[1].compile(ctx)
        ctx.emit(bytecode.BINOP[self.op])

    def get_extra_dot_info(self):
        return str(self.op)


class Variable(Node):
    """ Variable reference
    """
    def __init__(self, varname):
        super(Variable, self).__init__()
        self.varname = varname

    def compile(self, ctx):
        ctx.emit(bytecode.LOAD_VAR, ctx.register_var(self.varname))

    def get_extra_dot_info(self):
        return str(self.varname)


class Assignment(NonTerminal):
    """ Assign to a variable
    """
    def __init__(self, var, expr):
        super(Assignment, self).__init__()
        self.var = var
        self.children = [expr]

    def compile(self, ctx):
        self.children[0].compile(ctx)
        ctx.emit(bytecode.ASSIGN, ctx.register_var(self.var.varname))

    def get_extra_dot_info(self):
        return self.var.varname


class Function(NonTerminal):
    """Call a function"""

    def __init__(self, fname, expr):
        super(Function, self).__init__()
        self.fname = fname
        self.children = [expr]

    def compile(self, ctx):
        if self.fname == 'print':
            self.children[0].compile(ctx)
            ctx.emit(bytecode.PRINT)
        else:
            ctx.emit(bytecode.LOAD_VAR, ctx.register_var(self.fname))
            self.children[0].compile(ctx)
            ctx.emit(bytecode.CALL_FUNCTION, 1)

    def get_extra_dot_info(self):
        return self.fname


class Conditional(NonTerminal):

    def __init__(self, condition, true_block):
        super(Conditional, self).__init__()
        self.children = [condition, true_block]

    def compile(self, ctx):
        self.children[0].compile(ctx)
        jump_instr = ctx.next_instruction_index()
        ctx.emit(bytecode.JUMP_IF_FALSE)
        self.children[1].compile(ctx)
        ctx.adjust_arg(jump_instr, ctx.next_instruction_index()-jump_instr)


class While(NonTerminal):

    def __init__(self, condition, block):
        super(While, self).__init__()
        self.children = [condition, block]

    def compile(self, ctx):
        start_instr = ctx.next_instruction_index()
        self.children[0].compile(ctx)
        jump_instr = ctx.next_instruction_index()
        ctx.emit(bytecode.JUMP_IF_FALSE)
        self.children[1].compile(ctx)
        ctx.emit(bytecode.JUMP_BACK, ctx.next_instruction_index()-start_instr)
        ctx.adjust_arg(jump_instr, ctx.next_instruction_index()-jump_instr)


class FuncDef(NonTerminal):

    def __init__(self, name, arg, code, rtype=None):
        super(FuncDef, self).__init__()
        self.name = name
        self.arg = arg
        self.children = [code]
        self.rtype = rtype

    def get_extra_dot_info(self):
        rtype = self.rtype or "ANY"
        return self.name + " ( -> " + rtype + ")"

    def compile(self, ctx):
        closure_context = compilercontext.CompilerContext()
        closure_context.register_var(self.arg)
        self.children[0].compile(closure_context)
        closure_context.emit(bytecode.LOAD_CONSTANT, closure_context.register_constant(TheNone))
        closure_context.emit(bytecode.RETURN)
        code = closure_context.create_bytecode()
        code_const = ctx.register_constant(code)
        ctx.emit(bytecode.LOAD_CONSTANT, code_const)
        ctx.emit(bytecode.MAKE_FUNCTION)
        ctx.emit(bytecode.ASSIGN, ctx.register_var(self.name))


class Return(NonTerminal):

    def __init__(self, arg):
        super(Return, self).__init__()
        self.children = [arg]

    def compile(self, ctx):
        self.children[0].compile(ctx)
        ctx.emit(bytecode.RETURN)


class VisitError(Exception):
    def __init__(self, node):
        self.node = node
        self.args = (node, )

    def __str__(self):
        return "could not visit %s" % (self.node, )


def make_dispatch_function(__general_nonterminal_visit=None,
                           __general_terminal_visit=None,
                           __general_visit=None,
                           **dispatch_table):
    def dispatch(self, node):
        name = node.__class__.__name__
        if isinstance(node, NonTerminal):
            func = dispatch_table.get(name, None)
            if func is None:
                if __general_nonterminal_visit:
                    return __general_nonterminal_visit(self, node)
            else:
                return func(self, node)
        else:
            func = dispatch_table.get(name, None)
            if func is None:
                if __general_terminal_visit:
                    return __general_terminal_visit(self, node)
            else:
                return func(self, node)
        if __general_visit:
            return __general_visit(self, node)
        raise VisitError(node)
    return dispatch


class CreateDispatchDictionaryMetaclass(type):
    def __new__(cls, name_, bases, dct):
        dispatch_table = {}
        for name, value in dct.iteritems():
            if name.startswith("visit_"):
                dispatch_table[name[len("visit_"):]] = value
        for special in ["general_terminal_visit",
                        "general_nonterminal_visit",
                        "general_visit"]:
            if special in dct:
                dispatch_table["__" + special] = dct[special]
        dct["dispatch"] = make_dispatch_function(**dispatch_table)
        return type.__new__(cls, name_, bases, dct)


class ASTVisitor(object):
    __metaclass__ = CreateDispatchDictionaryMetaclass


class DotVisitor(ASTVisitor):

    def general_terminal_visit(self, node):
        return ['"%s" [label="%s"];' % (id(node), node.__class__.__name__ + " " + node.get_extra_dot_info())]

    def general_nonterminal_visit(self, node):
        lines = []
        lines.extend(self.general_terminal_visit(node))
        for child in node.children:
            lines.append('"%s" -> "%s";' % (id(node), id(child)))
            lines.extend(self.dispatch(child))
        return lines


class Transformer(RPythonVisitor):

    def visit_main(self, node):
        stmts = []
        for child in node.children:
            stmts.append(self.dispatch(child))
        return Block(stmts)

    visit_suite = visit_main

    def visit_assignment(self, node):
        if len(node.children) == 1:
            return self.dispatch(node.children[0])
        return Assignment(self.dispatch(node.children[0]),
                          self.dispatch(node.children[1]))

    def visit_statement(self, node):
        return Stmt(self.dispatch(node.children[0]))

    def visit_comparison(self, node):
        if len(node.children) == 1:
            return self.dispatch(node.children[0])
        return BinOp(node.children[1].children[0].additional_info,
                     self.dispatch(node.children[0]),
                     self.dispatch(node.children[1].children[1]))

    def visit_arith_expr(self, node):
        if len(node.children) == 1:
            return self.dispatch(node.children[0])
        target = self.dispatch(node.children[0])
        for i in range(1, len(node.children)):
            target = BinOp(node.children[i].children[0].additional_info,
                           target,
                           self.dispatch(node.children[i].children[1]))
        return target

    def visit_term(self, node):
        if len(node.children) == 1:
            return self.dispatch(node.children[0])
        return Function(self.dispatch(node.children[0]).varname,
                        self.dispatch(node.children[1].children[0]))

    def visit_return_statement(self, node):
        return Return(self.dispatch(node.children[0]))

    def visit_conditional(self, node):
        assert node.children[0].additional_info == 'if'
        return Conditional(self.dispatch(node.children[1]), self.dispatch(node.children[2]))

    def visit_while_loop(self, node):
        return While(self.dispatch(node.children[0]), self.dispatch(node.children[1]))

    def visit_funcdef(self, node):
        name = node.children[0].additional_info
        arg = node.children[1].additional_info
        if len(node.children) < 4:
            rtype = None
            block = node.children[2]
        else:
            rtype = node.children[2].children[0].additional_info
            block = node.children[3]
        return FuncDef(name, arg, self.dispatch(block), rtype=rtype)

    def visit_IDENTIFIER(self, node):
        return Variable(node.additional_info)

    def visit_DECIMAL(self, node):
        return ConstantInt(int(node.additional_info))

    def general_visit(self, node):
        children = getattr(node, 'children')
        if children and len(children) == 1:
            return self.dispatch(children[0])
        else:
            raise AssertionError("Need specific dispatch for %s" % node.symbol)


transformer = Transformer()


def view_raw_parse_tree(source):
    return ToAST().transform(_parse(source)).view()


def graphview(tree):
    from dotviewer import graphclient
    content = ["digraph G{"]
    visitor = DotVisitor()
    content.extend(visitor.dispatch(tree))
    content.append("}")
    try:
        p = py.test.ensuretemp("automaton").join("temp.dot")
        remove = False
    except AttributeError: # pytest lacks ensuretemp, make a normal one
        p = py.path.local.mkdtemp().join('automaton.dot')
        remove = True
    p.write("\n".join(content))
    graphclient.display_dot_file(str(p))
    if remove:
        p.dirpath().remove()


def view_processed_parse_tree(source):
    return graphview(parse(source))


def parse(source):
    return transformer.dispatch(ToAST().transform(_parse(source)))
