# notlang

This is most certainly not a language. If you are looking for a language
then look elsewhere. You won't find a language here, especially not a good
one that you should use.

## Syntax

If this were a language, it would have syntax roughly the same as Python's,
with significant whitespace (spaces only though.)

However, Python's a language, so it's not really a good comparison to make.

## Features

These are some features that this not-language definitely doesn't have.

### Basic types

Currently there are only really integers. What sort of language would only
have integers?

Some integers

    1
    2
    100
    -1

### Variables

You can assign values to variables, using the syntax you would expect.

    a = 1
    b = a

### Operators

There are a few operators you can use.

    1 + 1
    2 - 1
    1 < 2
    1 > 2
    1 == 2

### Conditionals

You can write an if statement, with then and else blocks:

    if a == b:
       c = 1
    else:
       c = 2

### Loops

There's only while loops currently

    while  a > 0:
       a = a - 1

### Functions

You can create a function, it can take arguments and return things.

    def foo(a):
        return a + 1

Note that currently there are no closures, you can't reference locals
from the parent scope, only their functions.

### Types

You can create your own types and instantiate them. You can't do
very much with them at all currently. You can't have a non-default
constructor. You probably can't assign to instance variables.


    Dog = new Type:
        age = 7

        def age_fn(self):
            return self.age

    d = Dog()
    d.age
    d.age_fn()

### Enums

You can define an enumerated type like this:

    Thing = new Enum(A, B):
        pass


And access one of the values like:

    Thing.A

They can be used with a case statement:

    case x:
        Thing.A:
            pass
        Thing.B:
            pass

### Builtins

#### print

Prints something to the console.

    print(1)

It currently can only print numbers.

#### List

There is a homogenous immutable List type.

    l = List().append(1)
    print(l.first())

## Types

not is statically typed. Though it's not a language, so it's not really
clear what that even means.

It uses type inference, so you don't have to write type annotations for
most things. It's definitely buggy though.

### Annotations

You can annotate a function with the types that it uses:

     def increment(a: int) -> int:
         return a + 1

Hopefully the inference system will confirm that the annotations match
the use, that they are as general as possible etc., but it probably
doesn't work.

### Type variables

You can specify a type variable for a function if you want.

     def apply<A>(f, a: A):
         return f(a)

The parser is pretty limited as it stands though, so you can't annotate
this function correctly (multiple variables, function types). It would
infer the type variables if you did nothing though.

### User-defined types

User defined types can be parameterised too.

     List = new Type<A>:
         pass

## Usage

If you wanted to try this not-a-language (which you don't), then
see the HACKING file.

## Inspiration

There aren't new ideas here, I'm just playing with ideas from these
places:

   - Python for the syntax, interpreter
   - PyPy for the JIT
   - Haskell for the type system
   - http://eev.ee/blog/2015/02/28/sylph-the-programming-language-i-want for lots of interesting ideas.
