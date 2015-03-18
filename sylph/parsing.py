import py
from rpython.rlib.objectmodel import we_are_translated
from rpython.rlib.parsing.ebnfparse import parse_ebnf
from rpython.rlib.parsing.parsing import PackratParser, ParseError
from rpython.rlib.parsing.tree import RPythonVisitor

from . import bytecode, lexer as mod_lexer, sylphdir


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
    def __eq__(self, other):
        return (self.__class__ == other.__class__ and
                self.__dict__ == other.__dict__)

    def __ne__(self, other):
        return not self == other

    def view(self):
        from dotviewer import graphclient
        content = ["digraph G{"]
        content.extend(self.dot())
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

    def dot(self):
        yield '"%s" [label="%s"];' % (id(self), self.__class__.__name__ + " " + self.get_extra_dot_info())
        for child in self.get_dot_children():
            yield '"%s" -> "%s";' % (id(self), id(child))
            for line in child.dot():
                yield line

    def get_extra_dot_info(self):
        return ""

    def get_dot_children(self):
        return []


class Block(Node):
    """ A list of statements
    """
    def __init__(self, stmts):
        self.stmts = stmts

    def compile(self, ctx):
        for stmt in self.stmts:
            stmt.compile(ctx)

    def get_dot_children(self):
        return self.stmts


class Stmt(Node):
    """ A single statement
    """
    def __init__(self, expr):
        self.expr = expr

    def compile(self, ctx):
        self.expr.compile(ctx)

    def get_dot_children(self):
        return [self.expr]


class ConstantInt(Node):
    """ Represent a constant
    """
    def __init__(self, intval):
        self.intval = intval

    def compile(self, ctx):
        from .interpreter import W_Int
        w = W_Int(self.intval)
        ctx.emit(bytecode.LOAD_CONSTANT, ctx.register_constant(w))

    def get_extra_dot_info(self):
        return str(self.intval)


class BinOp(Node):
    """ A binary operation
    """
    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right

    def compile(self, ctx):
        self.left.compile(ctx)
        self.right.compile(ctx)
        ctx.emit(bytecode.BINOP[self.op])

    def get_extra_dot_info(self):
        return str(self.op)

    def get_dot_children(self):
        return [self.left, self.right]


class Variable(Node):
    """ Variable reference
    """
    def __init__(self, varname):
        self.varname = varname

    def compile(self, ctx):
        ctx.emit(bytecode.LOAD_VAR, ctx.register_var(self.varname))

    def get_extra_dot_info(self):
        return str(self.varname)


class Assignment(Node):
    """ Assign to a variable
    """
    def __init__(self, varname, expr):
        self.varname = varname
        self.expr = expr

    def compile(self, ctx):
        self.expr.compile(ctx)
        ctx.emit(bytecode.ASSIGN, ctx.register_var(self.varname))

    def get_extra_dot_info(self):
        return self.varname

    def get_dot_children(self):
        return [self.expr]


class Function(Node):
    """Call a function"""

    def __init__(self, fname, expr):
        self.fname = fname
        self.expr = expr

    def compile(self, ctx):
        if self.fname == 'print':
            self.expr.compile(ctx)
            ctx.emit(bytecode.PRINT)
        else:
            ctx.emit(bytecode.LOAD_GLOBAL, ctx.register_var(self.fname))
            self.expr.compile(ctx)
            ctx.emit(bytecode.FUNCTION)

    def get_extra_dot_info(self):
        return self.fname

    def get_dot_children(self):
        return [self.expr]


class Conditional(Node):

    def __init__(self, condition, true_block):
        self.condition = condition
        self.true_block = true_block

    def compile(self, ctx):
        self.condition.compile(ctx)
        jump_instr = ctx.next_instruction_index()
        ctx.emit(bytecode.JUMP_IF_FALSE)
        self.true_block.compile(ctx)
        ctx.adjust_arg(jump_instr, ctx.next_instruction_index()-jump_instr)

    def get_dot_children(self):
        return [self.condition, self.true_block]


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
        return Assignment(self.dispatch(node.children[0]).varname,
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

    def visit_conditional(self, node):
        assert node.children[0].additional_info == 'if'
        return Conditional(self.dispatch(node.children[1]), self.dispatch(node.children[2]))

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
    content.extend(tree.dot())
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
