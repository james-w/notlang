import py
from rpython.rlib.objectmodel import we_are_translated
from rpython.rlib.parsing.ebnfparse import parse_ebnf
from rpython.rlib.parsing.parsing import PackratParser, ParseError, ErrorInformation
from rpython.rlib.parsing.tree import RPythonVisitor

from . import ast, lexer as mod_lexer, notdir


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


grammar_filename = py.path.local(notdir).join('grammar.txt')
grammar = grammar_filename.read("rt")
try:
    regexs, rules, ToAST = parse_ebnf(grammar)
except ParseError as e:
    print e.nice_error_message(filename=grammar_filename, source=grammar)
    raise
_parse = make_parse_function(regexs, rules, eof=True)


class DotVisitor(ast.ASTVisitor):

    def general_terminal_visit(self, node):
        return ['"%s" [label="%s"];' % (id(node), node.__class__.__name__ + " " + node.get_extra_dot_info())]

    def general_nonterminal_visit(self, node):
        lines = []
        lines.extend(self.general_terminal_visit(node))
        for child in node.children:
            lines.append('"%s" -> "%s";' % (id(node), id(child)))
            lines.extend(self.dispatch(child))
        return lines


class TypeReferenceParser(RPythonVisitor):

    def visit_type_param(self, node):
        base = self.dispatch(node.children[0])
        if len(node.children) > 1:
            type_params = [self.dispatch(n) for n in node.children[1].children]
            base.type_params = type_params
        return base

    def visit_IDENTIFIER(self, node):
        return ast.TypeReference(node.additional_info, node.getsourcepos())


class Transformer(RPythonVisitor):

    def visit_main(self, node):
        stmts = []
        for child in node.children:
            stmt = self.dispatch(child)
            if stmt:
                stmts.append(self.dispatch(child))
        return ast.Block(stmts, node.getsourcepos())

    visit_suite = visit_main

    def visit_assignment(self, node):
        if len(node.children) == 1:
            return self.dispatch(node.children[0])
        target = self.dispatch(node.children[0])
        if not isinstance(target, ast.Variable):
            raise ParseError(target.sourcepos, ErrorInformation(target.sourcepos.i, ["variable"]))
        return ast.Assignment(target,
                              self.dispatch(node.children[1]), node.getsourcepos())

    def visit_statement(self, node):
        if node.children:
            return ast.Stmt(self.dispatch(node.children[0]), node.getsourcepos())

    def visit_comparison(self, node):
        if len(node.children) == 1:
            return self.dispatch(node.children[0])
        return ast.BinOp(node.children[1].children[0].additional_info,
                         self.dispatch(node.children[0]),
                         self.dispatch(node.children[1].children[1]), node.getsourcepos())

    def visit_arith_expr(self, node):
        if len(node.children) == 1:
            return self.dispatch(node.children[0])
        sourcepos = node.children[0].getsourcepos()
        target = self.dispatch(node.children[0])
        for i in range(1, len(node.children)):
            target = ast.BinOp(node.children[i].children[0].children[0].additional_info,
                               target,
                               self.dispatch(node.children[i].children[1]), sourcepos)
            sourcepos = node.children[i].children[1].getsourcepos()
        return target

    def visit_term(self, node):
        term = self.dispatch(node.children[0])
        for child in node.children[1:]:
            if child.symbol == 'function_call_trailer':
                args = []
                arg_children = []
                type_params = []
                if len(child.children) > 1:
                    if child.children[0].symbol == 'type_params':
                        type_parser = TypeReferenceParser()
                        type_params = [type_parser.dispatch(a) for a in child.children[0].children]
                        if len(child.children) > 2:
                            arg_children = child.children[2].children
                    else:
                        arg_children = child.children[1].children
                if arg_children:
                    args = [self.dispatch(c) for c in arg_children]
                term = ast.Function(term, args, term.sourcepos, type_params=type_params)
            elif child.symbol == 'attribute_trailer':
                term = ast.Attribute(term, child.children[0].additional_info, child.getsourcepos())
            else:
                raise AssertionError("Unknown term trailer: %s" % child.symbol)
        return term

    def visit_return_statement(self, node):
        if len(node.children) > 1:
            arg = self.dispatch(node.children[1])
        else:
            arg = None
        return ast.Return(arg, node.getsourcepos())

    def visit_conditional(self, node):
        if len(node.children) > 3:
            else_block = self.dispatch(node.children[3])
        else:
            else_block = None
        return ast.Conditional(self.dispatch(node.children[1]), self.dispatch(node.children[2]), else_block, node.getsourcepos())

    def visit_while_loop(self, node):
        return ast.While(self.dispatch(node.children[1]), self.dispatch(node.children[2]), node.getsourcepos())

    def visit_funcdef(self, node):
        name = node.children[1].additional_info
        args = []
        argtypes = []
        rtype = None
        type_parser = TypeReferenceParser()

        params = node.children[2]
        if params.symbol == 'type_params':
            type_params = [type_parser.dispatch(a) for a in params.children]
            params = node.children[3]
            if len(node.children) > 5:
                rtype = type_parser.dispatch(node.children[4].children[0])
                block = node.children[5]
            else:
                block = node.children[4]
        else:
            type_params = []
            if len(node.children) > 4:
                rtype = type_parser.dispatch(node.children[3].children[0])
                block = node.children[4]
            else:
                block = node.children[3]
        if params.children:
            for argnode in params.children[0].children:
                args.append(argnode.children[0].additional_info)
                if len(argnode.children) > 1:
                    argtypes.append(type_parser.dispatch(argnode.children[1]))
                else:
                    argtypes.append(None)
        return ast.FuncDef(name, args, self.dispatch(block), node.getsourcepos(), rtype=rtype,
                argtypes=argtypes, type_params=type_params)

    def _parse_type_option(self, node):
        if len(node.children) > 1:
            members = [n.additional_info for n in node.children[1].children]
            return ast.TypeOption(node.children[0].additional_info, node.children[0].getsourcepos(), members=members)
        else:
            return ast.TypeOption(node.children[0].additional_info, node.children[0].getsourcepos())

    def visit_new_decl(self, node):
        type_type = node.children[1].additional_info
        type_params = []
        options = []
        block_node = node.children[-1]
        type_parser = TypeReferenceParser()
        if len(node.children) > 4:
            type_params = [type_parser.dispatch(a) for a in node.children[2].children]
            options = [self._parse_type_option(c) for c in node.children[3].children[1].children]
        elif len(node.children) > 3:
            if node.children[2].symbol == 'type_params':
                type_params = [type_parser.dispatch(a) for a in node.children[2].children]
            else:
                options = [self._parse_type_option(c) for c in node.children[2].children[1].children]
        if type_type in ('Enum', 'Tuple') and not options:
            raise ParseError(node.getsourcepos(), ErrorInformation(node.getsourcepos().i, ["options for {}".format(type_type)]))
        if type_type != 'Enum':
            for option in options:
                if option.members:
                    raise ParseError(option.sourcepos, ErrorInformation(option.sourcepos.i, ["plain options only for {}".format(type_type)]))
        block = self.dispatch(block_node)
        return ast.NewType(block, type_type, node.getsourcepos(), type_params=type_params, options=options)

    def visit_case(self, node):
        target = self.dispatch(node.children[1])
        cases = []
        else_case = None
        for case in node.children[2].children:
            n = case.children[0]
            if len(n.children) > 1:
                # normal block
                case_node = ast.CaseCase(self.dispatch(n.children[0]), self.dispatch(n.children[1]), n.children[0].getsourcepos())
                cases.append(case_node)
            else:
                # else block
                case_node = ast.CaseCase(ast.Variable("else", n.getsourcepos()), self.dispatch(n.children[0]), n.getsourcepos())
                if else_case is not None:
                    raise ParseError(case_node.sourcepos, ErrorInformation(case_node.sourcepos.i, ["Only one else block allowed"]))
                else_case = case_node
        return ast.Case(target, cases, else_case, node.getsourcepos())

    def visit_pass(self, node):
        return ast.Pass(node.getsourcepos())

    def visit_variable(self, node):
        name = ()
        for child in node.children:
            name += (child.additional_info,)
        return ast.Variable(name, node.getsourcepos())

    def visit_IDENTIFIER(self, node):
        return ast.Variable(node.additional_info, node.getsourcepos())

    def visit_decimal(self, node):
        if len(node.children) > 1:
            val = -1 * int(node.children[1].additional_info)
        else:
            val = int(node.children[0].additional_info)
        return ast.ConstantInt(val, node.getsourcepos())

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
