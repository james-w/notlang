from pyrsistent import pvector


def builtin(f):
    f._is_builtin = True
    return f


def is_builtin(f):
    return getattr(f, '_is_builtin', False)


class W_Root(object):

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
        assert(isinstance(intval, (int, long)))
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

    def __init__(self, code):
        self.code = code

    def call(self, space, args, globals):
        return space.call_function(self.code, args, globals, {})

    def __get__(self, instance, cls):
        """When accessed via an attribute on an instance, transform to a method"""
        return W_Method(self.code, instance)


class W_Method(W_Root):

    def __init__(self, code, instance):
        self.code = code
        self.instance = instance

    def call(self, space, args, globals):
        return space.call_function(self.code, [self.instance] + args, globals, {})


class W_Type(W_Root):

    @classmethod
    def call(cls, space, args, globals):
        obj = cls()
        return obj

    @builtin
    def repr(self):
        return W_String(repr("Instance of " + self.__class__.__name__))


class W_Dict(W_Root):

    def __init__(self, dictval):
        self.dictval = dictval

    def repr(self):
        return str(self.dictval)


class W_String(W_Root):

    def __init__(self, strval):
        self.strval = strval

    def repr(self):
        return repr(self.strval)

    def str(self):
        return self.strval

    @builtin
    def add(self, other):
        return W_String(self.strval + other.strval)


class W_List(W_Type):

    def __init__(self, listval):
        self.listval = listval

    def repr(self):
        return repr(self.listval)

    def append(self, space, args, globals):
        val = args[0]
        return W_List(listval=self.listval.append(val))

    def first(self, space, args, globals):
        return self.listval[0]

    @classmethod
    def call(cls, space, args, globals):
        return cls(pvector([]))


class W_Tuple(W_Type):

    def __init__(self, val):
        self.val = val

    def first(self, space, args, globals):
        return self.val[0]

    def second(self, space, args, globals):
        return self.val[1]

    @classmethod
    def call(cls, space, args, globals):
        return cls(tuple(args))


class W_IsInstance(W_Func):

    @classmethod
    def call(cls, space, args, globals):
        # XXX: args seem backwards
        return W_Int(int(isinstance(args[1], args[0])))


class W_Enum(W_Type):
    pass
