from . import typer
from .ast import ASTVisitor


INT = object()
BOOL = object()


class TypeVar(object):

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "<TypeVar:%s>" % self.name


class NotNameError(Exception):

    def __init__(self, message, positions):
        self.message = message
        self.positions = positions

    def __str__(self):
        return "%s at %s" % (self.message, self.positions)

    def nice_error_message(self, source=None, filename="<string>"):
        lines = []
        lines.append("NameError: " + self.message)
        lines.append("at line %d of %s" % (self.positions[0].lineno, filename))
        if source:
            for pos in self.positions:
                lines.append(source.splitlines()[pos.lineno])
                lines.append(" " * (pos.columnno) + "^~~~~")
        return "\n".join(lines)


class TypeEnv(object):

    def __init__(self):
        self.env = {}
        self.counter = -1

    def newvar(self):
        self.counter += 1
        return TypeVar(str(self.counter))

    def register(self, k):
        if k in self.env:
            return self.env[k]
        else:
            return self.env.setdefault(k, self.newvar())

    def extend(self, k, v):
        self.env[k] = v

    def restrict(self, k):
        del self.env[k]

    def lookup(self, name, sourcepos):
        if name not in self.env:
            raise NotNameError("%s referenced before assignment" % name, [sourcepos])
        return self.env[name]


class Substitution(object):

    def __init__(self):
        self.subst = {}


class TypeChecker(ASTVisitor):

    def __init__(self, env):
        self.env = env
        self.constraints = []

    def visit_ConstantInt(self, node):
        return INT

    def visit_Variable(self, node):
        return self.env.lookup(node.varname, node.sourcepos)

    def visit_Assignment(self, node):
        # TODO: handle Attribute
        t = self.env.register(node.var.varname)
        self.constraints.append(typer.Constraint(t, typer.SUPERTYPE_OF, self.dispatch(node.children[0]), [node.sourcepos]))
        return t

    def visit_Pass(self, node):
        pass

    def visit_Conditional(self, node):
        condition, true_block, false_block = node.children
        self.constraints.append(typer.Constraint(self.dispatch(condition), typer.SUBTYPE_OF, BOOL, [condition.sourcepos]))
        self.dispatch(true_block)
        if false_block is not None:
            self.dispatch(false_block)
        return None

    def visit_While(self, node):
        condition, block = node.children
        self.constraints.append(typer.Constraint(self.dispatch(condition), typer.SUBTYPE_OF, BOOL, [condition.sourcepos]))
        self.dispatch(block)
        return None

    def visit_BinOp(self, node):
        args = [self.dispatch(c) for c in node.children]
        rtype = self.env.newvar()
        return rtype
