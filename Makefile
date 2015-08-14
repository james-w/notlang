NAME = notlang
BASEDIR = $(NAME)
MAIN = target$(NAME).py
TARGET = $(patsubst %.py,%-c,$(MAIN))

TEST_FILTER ?= ""
ifdef TEST_FAILFAST
	TEST_FAILFAST = "1"
else
	TEST_FAILFAST = "100"
endif

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
	PYTHONPATH=pypy ./virtualenv/bin/py.test $(NAME) -k $(TEST_FILTER) --maxfail=$(TEST_FAILFAST)

profile_tests:
	PYTHONPATH=pypy ./virtualenv/bin/python -m cProfile -o profile ./virtualenv/bin/py.test $(NAME) -k $(TEST_FILTER)
	python -c "import pstats; p = pstats.Stats('profile'); p.strip_dirs(); p.sort_stats('cumtime'); p.print_stats(50); p.sort_stats('tottime'); p.print_stats(50)"

check: test

lint:
	pyflakes $(BASEDIR)/*.py $(BASEDIR)/tests/*.py $(MAIN) $(BASEDIR)/bin/*.py

.PHONY: clean package-deps lint test check deps build profile_tests
