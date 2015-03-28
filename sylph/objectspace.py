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
    _immutable_fields_ = ['bytecode', 'constants[*]', 'names', 'max_stacksize']
   
    def __init__(self, code, constants, names, max_stacksize):
        self.bytecode = code
        self.constants = constants
        self.names = names
        self.max_stacksize = max_stacksize

    def repr(self):
        return "<W_Code at %d>" % id(self)


class W_None(W_Root):

    def repr(self):
        return "None"


TheNone = W_None()


class W_Int(W_Root):

    def __init__(self, intval):
        assert(isinstance(intval, int))
        self.intval = intval

    def add(self, other):
        if not isinstance(other, W_Int):
            raise Exception("wrong type")
        return W_Int(self.intval + other.intval)

    def sub(self, other):
        if not isinstance(other, W_Int):
            raise Exception("wrong type")
        return W_Int(self.intval - other.intval)

    def multiply(self, other):
        if not isinstance(other, W_Int):
            raise Exception("wrong type")
        return W_Int(self.intval * other.intval)

    def lt(self, other): 
        if not isinstance(other, W_Int):
            raise Exception("wrong type")
        return W_Int(self.intval < other.intval)

    def gt(self, other): 
        if not isinstance(other, W_Int):
            raise Exception("wrong type")
        return W_Int(self.intval > other.intval)

    def is_true(self):
        return self.intval != 0

    def eq(self, other):
        if not isinstance(other, W_Int):
            raise Exception("wrong type")
        return W_Int(self.intval == other.intval)

    def int(self):
        return self.intval

    def str(self):
        return str(self.intval)

    def repr(self):
        return str(self.intval)

    def __repr__(self):
        return "<%s.%s object at %s value:%d>" % (self.__class__.__module__,
                self.__class__.__name__, id(self), self.intval)


class W_Func(W_Root):

    __slots__ = ['code']
    _immutable_fields_ = ['code']

    def __init__(self, code):
        self.code = code

    def call(self, space, args, globals, trace=False):
        return space.call_function(self.code, args, globals, trace=trace)


class W_Type(W_Root):

    __slots__ = ['name']
    _immutable_fields_ = ['name']

    def __init__(self, name):
        self.name = name

    @classmethod
    def call(cls, space, args, globals, trace=False):
        return cls.__new__(cls, args)
