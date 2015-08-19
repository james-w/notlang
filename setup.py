from setuptools import setup

setup(
    name="notlang",
    version="0",
    install_requires=[
        'pyrsistent',
        'pytest',
        'testtools',
        'gorun',
    ],
    packages=['notlang'],
)

