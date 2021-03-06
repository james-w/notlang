NAME = notlang
BASEDIR = $(NAME)
MAIN = target$(NAME).py
TARGET = $(patsubst %.py,%-c,$(MAIN))
VIRTUALENV ?= virtualenv
VIRTUALENV_BIN = $(VIRTUALENV)/bin

TEST_FILTER ?= ""
ifdef TEST_FAILFAST
	TEST_FAILFAST = "1"
else
	TEST_FAILFAST = "100"
endif
HYPOTHESIS_PROFILE ?= dev

build: deps $(TARGET)

$(TARGET): $(MAIN) $(BASEDIR)/*.py $(BASEDIR)/grammar.txt
	$(VIRTUALENV_BIN)/rpython --opt=jit $(MAIN)

package-deps:
	sudo apt-get install $$(xargs < package-deps.txt)

deps: $(VIRTUALENV)
	$(VIRTUALENV_BIN)/python setup.py develop

$(VIRTUALENV):
	virtualenv $(VIRTUALENV)

clean:
	[ ! -f $(TARGET) ] || rm $(TARGET)
	find $(BASEDIR) -name \*.pyc -delete

test:
	$(VIRTUALENV_BIN)/py.test $(NAME) -k $(TEST_FILTER) --maxfail=$(TEST_FAILFAST) --hypothesis-profile=$(HYPOTHESIS_PROFILE)

profile_tests:
	$(VIRTUALENV_BIN)/python -m cProfile -o profile $(VIRTUALENV_BIN)/py.test $(NAME) -k $(TEST_FILTER) --hypothesis-profile=$(HYPOTHESIS_PROFILE)
	python -c "import pstats; p = pstats.Stats('profile'); p.strip_dirs(); p.sort_stats('cumtime'); p.print_stats(50); p.sort_stats('tottime'); p.print_stats(50)"

autotest:
	$(VIRTUALENV_BIN)/gorun.py

check: test

lint:
	pyflakes $(BASEDIR)/*.py $(BASEDIR)/tests/*.py $(MAIN) $(BASEDIR)/bin/*.py

docker-setup:
	docker build -t $(NAME) .

docker-test:
	docker run -t -v $(CURDIR):/notlang $(NAME)

.PHONY: clean package-deps lint test check deps build profile_tests
