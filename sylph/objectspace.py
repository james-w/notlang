class W_Root(object):

    __slots__ = ()
    _settled_ = True

    def add(self, other):
        raise NotImplementedError

    def sub(self, other):
        raise NotImplementedError

    def multiply(self, other):
        raise NotImplementedError

    def eq(self, other):
        raise NotImplementedError

    def str(self):
        raise NotImplementedError

    def repr(self):
        return "<%s at %d>" % (self.__class.__.__name__, id(self))

    def lt(self, other):
        raise NotImplementedError

    def gt(self, other):
        raise NotImplementedError

    def is_true(self):
        raise NotImplementedError


class W_Code(W_Root):
    _immutable_ = True
    _immutable_fields_ = ['bytecode', 'constants[*]', 'names']
   
    def __init__(self, code, constants, names):
        self.bytecode = code
        self.constants = constants
        self.names = names

    def repr(self):
        return "<W_Code at %d>" % id(self)


class W_None(W_Root):

    def repr(self):
        return "None"


TheNone = W_None()
