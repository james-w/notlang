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
)

