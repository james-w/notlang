NAME = notlang
BASEDIR = $(NAME)
MAIN = target$(NAME).py
TARGET = $(patsubst %.py,%-c,$(MAIN))
VIRTUALENV = virtualenv
VIRTUALENV_BIN = $(VIRTUALENV)/bin

TEST_FILTER ?= ""
ifdef TEST_FAILFAST
	TEST_FAILFAST = "1"
else
	TEST_FAILFAST = "100"
endif

build: deps $(TARGET)

$(TARGET): $(MAIN) $(BASEDIR)/*.py $(BASEDIR)/grammar.txt
	$(VIRTUALENV_BIN)/rpython --opt=jit $(MAIN)

package-deps:
	sudo apt-get install $$(xargs < package-deps.txt)

deps: $(VIRTUALENV)
	$(VIRTUALENV_BIN)/python setup.py develop

$(VIRTUALENV):
	virtualenv $(VIRTUALENV) --python $(which /usr/bin/pypy)

clean:
	[ ! -f $(TARGET) ] || rm $(TARGET)
	find $(BASEDIR) -name \*.pyc -delete

test:
	$(VIRTUALENV_BIN)/py.test $(NAME) -k $(TEST_FILTER) --maxfail=$(TEST_FAILFAST)

profile_tests:
	$(VIRTUALENV_BIN)/python -m cProfile -o profile $(VIRTUALENV_BIN)/py.test $(NAME) -k $(TEST_FILTER)
	python -c "import pstats; p = pstats.Stats('profile'); p.strip_dirs(); p.sort_stats('cumtime'); p.print_stats(50); p.sort_stats('tottime'); p.print_stats(50)"

autotest:
	$(VIRTUALENV_BIN)/gorun.py

check: test

lint:
	pyflakes $(BASEDIR)/*.py $(BASEDIR)/tests/*.py $(MAIN) $(BASEDIR)/bin/*.py

.PHONY: clean package-deps lint test check deps build profile_tests
