#!/usr/bin/env python

import sys

from sylph import typer
from sylph.parsing import parse


def get_substituted(var, substitutions):
    while var in substitutions:
        var = substitutions[var][0]
    return var


def substitute(t, substitutions):
    t = get_substituted(t, substitutions)
    if isinstance(t, typer.FunctionType):
        t = typer.FunctionType(t.name, [substitute(a, substitutions) for a in t.args], substitute(t.rtype, substitutions))
    return t


def dump(checker, substitutions):
    fnames = []
    for name, context in checker.child_contexts.items():
        fnames.append(name)
        print("function %s has type %s" % (name, substitute(checker.varmap[name], substitutions)))
        for varname, t in context.varmap.items():
            if varname != name:
                print("%s has type %s" % (varname, substitute(t, substitutions)))
        print("")
    for varname, t in checker.varmap.items():
        if varname not in fnames:
            print("%s has type %s" % (varname, substitute(t, substitutions)))


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.stderr.write("Pass a single file to show\n")
        sys.exit(1)
    with open(sys.argv[1]) as f:
        source = f.read()
    parsed = parse(source)
    try:
        checker, substitutions = typer.typecheck(parsed)
    except (typer.SylphNameError, typer.SylphTypeError) as e:
        print e.nice_error_message(source=source, filename=sys.argv[1])
        sys.exit(1)
    else:
        dump(checker, substitutions)
