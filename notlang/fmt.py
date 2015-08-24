from ast import ASTVisitor


class Formatter(ASTVisitor):

    def __init__(self, indent="", toplevel=True):
        self.indent = indent
        self.toplevel = toplevel
        self.first = True
        self.did_block = False

    def indented(self, additional=None):
        if additional is None:
            additional = "    "
        return Formatter(indent=self.indent + additional, toplevel=False)

    def visit_Variable(self, node):
        return node.varname

    def visit_Block(self, node):
        return "".join([self.dispatch(c) for c in node.children])

    def visit_Stmt(self, node):
        did_block = False
        line = self.indent + self.dispatch(node.expr)
        if line.endswith("\n"):
            if self.toplevel and not self.did_block and not self.first:
                line = "\n" + self.indent + line
            did_block = True
            if not line.endswith("\n\n"):
                line += "\n"
        else:
            line += "\n"
        self.first = False
        self.did_block = did_block
        return line

    def visit_Pass(self, node):
        return "pass"

    def visit_ConstantInt(self, node):
        return str(node.intval)

    def visit_Assignment(self, node):
        return self.dispatch(node.target) + " = " + self.dispatch(node.source)

    def visit_Function(self, node):
        base = self.dispatch(node.fname)
        argstr = "(" + ", ".join([self.dispatch(c) for c in node.args]) + ")"
        type_params = ""
        if node.type_params:
            type_params = "<" + ", ".join([str(n) for n in node.type_params]) + ">"
        return base + type_params + argstr

    def visit_BinOp(self, node):
        return self.dispatch(node.args[0]) + " " + node.op + " " + self.dispatch(node.args[1])

    def visit_Conditional(self, node):
        condition = "if " + self.dispatch(node.condition) + ":\n"
        true_block = self.indented().dispatch(node.true_block)
        false_block = ""
        if node.false_block:
            false_block = self.indent + "else:\n" + self.indented().dispatch(node.false_block)
        return condition + true_block + false_block

    def visit_While(self, node):
        condition = "while " + self.dispatch(node.condition) + ":\n"
        block = self.indented().dispatch(node.block)
        return condition + block

    def visit_Attribute(self, node):
        return self.dispatch(node.target) + "." + node.name

    def visit_Return(self, node):
        trailer = ""
        if node.arg:
            trailer = " " + self.dispatch(node.arg)
        return "return" + trailer

    def visit_FuncDef(self, node):
        header = "def " + node.name
        type_params = ""
        if node.type_params:
            type_params = "<" + ", ".join([str(n) for n in node.type_params]) + ">"
        argstr = "("
        first = True
        for i, arg in enumerate(node.args):
            if not first:
                argstr += ", "
            argstr += arg
            if len(node.argtypes) > i and node.argtypes[i]:
                argstr += ": " + str(node.argtypes[i])
            first = False
        argstr += ")"
        rtype = ""
        if node.rtype:
            rtype = " -> " + str(node.rtype)
        block = self.indented().dispatch(node.code)
        return header + type_params + argstr + rtype + ":\n" + block

    def visit_Case(self, node):
        cases = "".join(self.indented().dispatch(c) for c in node.cases)
        return "case " + self.dispatch(node.target) + ":\n" + cases

    def visit_CaseCase(self, node):
        header = self.indent + self.dispatch(node.label) + ":\n"
        return header + self.indented().dispatch(node.block)

    def visit_TypeOption(self, node):
        members = ""
        if node.members:
            members = "(" + ", ".join(node.members) + ")"
        return node.name + members

    def visit_NewType(self, node):
        type_params = ""
        if node.type_params:
            type_params = "<" + ", ".join([str(n) for n in node.type_params]) + ">"
        options = ""
        if node.options:
            options = "(" + ", ".join([self.dispatch(o) for o in node.options]) + ")"
        body = self.indented().dispatch(node.block)
        return "new " + node.type_type + type_params + options + ":\n" + body
