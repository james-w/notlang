#!/usr/bin/env python

import sys

from sylph.bytecode import dump, get_compiler
from sylph.parsing import parse

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.stderr.write("Pass a single file to show\n")
        sys.exit(1)
    with open(sys.argv[1]) as f:
        source = f.read()
    parsed = parse(source)
    compiler = get_compiler(parsed)
    print(dump(compiler.create_bytecode(), compiler))
