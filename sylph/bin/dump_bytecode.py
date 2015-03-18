#!/usr/bin/env python

import sys

from sylph.interpreter import get_bytecode

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.stderr.write("Pass a single file to show\n")
        sys.exit(1)
    with open(sys.argv[1]) as f:
        source = f.read()
    print(get_bytecode(source).dump())
