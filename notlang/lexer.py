from rpython.rlib.parsing import deterministic, regex
from rpython.rlib.parsing.lexer import Lexer, SourcePos, Token


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
        self.indents = [0]

    def find_next_token(self, trace=False):
        token = self._find_next_token()
        if trace:
            print("lexer: " + str(token))
        return token

    def _find_next_token(self):
        while 1:
            self.state = 0
            start = self.last_matched_index + 1
            assert start >= 0

            # Handle end of file situation
            if start == len(self.text) and self.eof:
                if len(self.indents) > 1:
                    self.indents.pop()
                    return self.make_token(start, -3, "")
                self.last_matched_index += 1
                return self.make_token(start, -1, "", eof=True)
            elif start >= len(self.text):
                raise StopIteration

            if self.check_indent:
                self.check_indent = False
                indent = 0
                newlines = 0
                for char in self.text[start:]:
                    if char == ' ':
                        indent += 1
                    elif indent == 0 and char == '\n':
                        newlines += 1
                    else:
                        break
                if indent > self.indents[-1] and newlines == 0:
                    self.indents.append(indent)
                    return self.make_token(start, -2, "")
                elif indent < self.indents[-1]:
                    self.check_indent = True
                    self.indents.pop()
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

    def tokenize(self, text, eof=False, trace=False):
        """Return a list of Token's from text."""
        r = self.get_runner(text, eof)
        result = []
        while 1:
            try:
                tok = r.find_next_token(trace=trace)
                result.append(tok)
            except StopIteration:
                break
        return result
