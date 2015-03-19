from .parsing import ASTVisitor


class Typer(ASTVisitor):

    def __init__(self):
        self.varmap = {}

    def visit_ConstantInt(self, node):
        return "int"

    def visit_Assignment(self, node):
        source = node.children[0]
        target = node.var
        source_type = self.dispatch(source)
        target_type = self.varmap.get(target.varname)
        explanation = "assigning %s:%s to %s:%s" % (source, source_type, target, target_type)
        print(explanation)
        if source_type is not None:
            if (target_type is not None and target_type != source_type):
                raise AssertionError("Type mismatch " + explanation)
            if target_type is None:
                print "Making %s %s" % (target, source_type)
                self.varmap[target.varname] = source_type

    def visit_FuncDef(self, node):
        self.varmap[node.name] = "function"

    def visit_Variable(self, node):
        return self.varmap.get(node.varname)

    def visit_BinOp(self, node):
        left, right = map(self.dispatch, node.children)
        explanation = "for %s %s:%s and %s:%s" % (node.op, node.children[0], left, node.children[1], right)
        if left != right:
            raise AssertionError("Type mismatch " + explanation)
        return left

    def general_nonterminal_visit(self, node):
        map(self.dispatch, node.children)

    def general_terminal_visit(self, node):
        pass
