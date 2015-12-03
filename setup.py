from setuptools import setup

setup(
    name="notlang",
    version="0",
    install_requires=[
        'pyrsistent',
        'pytest',
        'rpython',
        'testtools',
        'gorun',
    ],
    packages=['notlang'],
    scripts=[
        'notlang/bin/not-dump-bytecode',
        'notlang/bin/not-dump-type-info',
        'notlang/bin/not-view-parse-tree',
        'notlang/bin/not-view-raw-parse-tree',
    ],
)

