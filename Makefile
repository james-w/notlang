NAME = notlang
BASEDIR = $(NAME)
MAIN = target$(NAME).py
TARGET = $(patsubst %.py,%-c,$(MAIN))

build: deps $(TARGET)

$(TARGET): $(MAIN) $(BASEDIR)/*.py $(BASEDIR)/grammar.txt
	PYTHONPATH=pypy virtualenv/bin/python ./pypy/rpython/translator/goal/translate.py --opt=jit $(MAIN)

package-deps:
	sudo apt-get install $$(xargs < package-deps.txt)

deps: virtualenv pypy
	virtualenv/bin/python setup.py develop

pypy:
	hg clone https://bitbucket.org/pypy/pypy

virtualenv:
	virtualenv virtualenv --python /usr/bin/pypy

clean:
	[ ! -f $(TARGET) ] || rm $(TARGET)
	find $(BASEDIR) -name \*.pyc -delete

test:
	PYTHONPATH=pypy ./virtualenv/bin/py.test $(NAME)

check: test

lint:
	pyflakes $(BASEDIR)/*.py $(BASEDIR)/tests/*.py $(MAIN) $(BASEDIR)/bin/*.py

.PHONY: clean package-deps lint test check deps build
