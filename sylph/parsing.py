import py
from rpython.rlib.objectmodel import we_are_translated
from rpython.rlib.parsing import deterministic, regex
from rpython.rlib.parsing.ebnfparse import parse_ebnf
from rpython.rlib.parsing.lexer import Lexer, SourcePos, Token
from rpython.rlib.parsing.parsing import PackratParser, ParseError
from rpython.rlib.parsing.tree import RPythonVisitor

from . import bytecode, sylphdir


class AbstractIndentTrackingLexingDFARunner(deterministic.DFARunner):

    i = 0

    def __init__(self, matcher, automaton, text, eof=False):
        self.automaton = automaton
        self.state = 0
        self.text = text
        self.last_matched_state = 0
        self.last_matched_index = -1
        self.eof = eof
        self.matcher = matcher
        self.lineno = 0
        self.columnno = 0
        self.check_indent = False
        self.current_indent = 0

    def find_next_token(self):
        while 1:
            self.state = 0
            start = self.last_matched_index + 1
            assert start >= 0

            # Handle end of file situation
            if start == len(self.text) and self.eof:
                self.last_matched_index += 1
                return self.make_token(start, -1, "", eof=True)
            elif start >= len(self.text):
                raise StopIteration

            if self.check_indent:
                self.check_indent = False
                indent = 0
                for char in self.text[start:]:
                    if char == ' ':
                        indent += 1
                    else:
                        break
                if indent > self.current_indent:
                    self.current_indent = indent
                    return self.make_token(start, -2, "")
                elif indent < self.current_indent:
                    self.current_indent = indent
                    return self.make_token(start, -3, "")

            i = self.inner_loop(start)
            if i < 0:
                i = ~i
                stop = self.last_matched_index + 1
                assert stop >= 0
                if start == stop:
                    source_pos = self.token_position_class(i - 1, self.lineno, self.columnno)
                    raise deterministic.LexerError(self.text, self.state,
                                                   source_pos)
                source = self.text[start:stop]
                result = self.make_token(start, self.last_matched_state, source)
                if result.name == 'NEWLINE':
                    self.check_indent = True
                self.adjust_position(source)
                if self.ignore_token(self.last_matched_state):
                    continue
                return result
            if self.last_matched_index == i - 1:
                source = self.text[start: ]
                result = self.make_token(start, self.last_matched_state, source)
                if result.name == 'NEWLINE':
                    self.check_indent = True
                self.adjust_position(source)
                if self.ignore_token(self.last_matched_state):
                    if self.eof:
                        self.last_matched_index += 1
                        return self.make_token(i, -1, "", eof=True)
                    else:
                        raise StopIteration
                return result
            source_pos = self.token_position_class(i - 1, self.lineno, self.columnno)
            raise deterministic.LexerError(self.text, self.state, source_pos)

    def adjust_position(self, token):
        """Update the line# and col# as a result of this token."""
        newlines = token.count("\n")
        self.lineno += newlines
        if newlines==0:
            self.columnno += len(token)
        else:
            self.columnno = token.rfind("\n")

    def inner_loop(self, i):
        return self.matcher(self, i)

    next = find_next_token

    def __iter__(self):
        return self


class IndentTrackingLexingDFARunner(AbstractIndentTrackingLexingDFARunner):

    def __init__(self, matcher, automaton, text, ignore, eof=False,
                 token_class=None):

        if not token_class:
            self.token_class = Token
            self.token_position_class = SourcePos

        else:
            self.token_class = token_class
            self.token_position_class = token_class.source_position_class

        AbstractIndentTrackingLexingDFARunner.__init__(self, matcher, automaton, text, eof)
        self.ignore = ignore

    def ignore_token(self, state):
        return self.automaton.names[state] in self.ignore

    def make_token(self, index, state, text, eof=False):
        assert state in [-2, -3] or (eof and state == -1) or 0 <= state < len(self.automaton.names)

        source_pos = self.token_position_class(index, self.lineno, self.columnno)
        if eof:
            return self.token_class("EOF", "EOF", source_pos)
        if state == -2:
            return self.token_class("INDENT", "INDENT", source_pos)
        if state == -3:
            return self.token_class("DEDENT", "DEDENT", source_pos)

        return self.token_class(self.automaton.names[state], text, source_pos)


def dict_from_keys(keys, value=None):
    d = {}
    for key in keys:
        d[key] = value
    return d


class IndentTrackingLexer(Lexer):
    """A white-space aware parser that injects INDENT/DEDENT tokens."""

    def __init__(self, token_regexs, names, ignore=None):
        self.token_regexs = token_regexs
        self.names = names
        self.rex = regex.LexingOrExpression(self.token_regexs, self.names)
        automaton = self.rex.make_automaton()
        self.automaton = automaton.make_deterministic(self.names)
        self.automaton.optimize() # XXX not sure whether this is a good idea
        if ignore is None:
            ignore = []
        for ign in ignore:
            assert ign in self.names
        self.ignore = dict_from_keys(ignore)
        self.matcher = self.automaton.make_lexing_code()

    def get_runner(self, text, eof=False, token_class=None):
        return IndentTrackingLexingDFARunner(self.matcher, self.automaton, text,
                                             self.ignore, eof, token_class=token_class)



def check_for_missing_names(names, regexs, rules):
    known_names = dict_from_keys(names, True)
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
    lexer = IndentTrackingLexer(list(regexs), list(names), ignore=ignore)
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

    visit_arith_expr = visit_comparison

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
