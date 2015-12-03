#!/usr/bin/env python

import sys

from notlang.compiler import dump, get_compiler, max_stacksize
from notlang.parsing import parse

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.stderr.write("Pass a single file to show\n")
        sys.exit(1)
    with open(sys.argv[1]) as f:
        source = f.read()
    parsed = parse(source)
    compiler = get_compiler(parsed)
    print(dump(compiler.create_bytecode(), compiler))
    print("")
    print("Max stacksize: %d after line %d (%s)" % max_stacksize(compiler.create_bytecode()))
