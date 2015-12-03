#!/usr/bin/env python

import sys

from notlang.parsing import view_raw_parse_tree

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.stderr.write("Pass a single file to show\n")
        sys.exit(1)
    with open(sys.argv[1]) as f:
        source = f.read()
    view_raw_parse_tree(source)
