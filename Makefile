build: deps targetsylph-c

targetsylph-c: targetsylph.py sylph/*.py sylph/grammar.txt
	PYTHONPATH=pypy virtualenv/bin/python ./pypy/rpython/translator/goal/translate.py targetsylph.py

package-deps:
	sudo apt-get install $$(xargs < package-deps.txt)

deps: virtualenv pypy
	virtualenv/bin/python setup.py develop

pypy:
	hg clone https://bitbucket.org/pypy/pypy	

virtualenv:
	virtualenv virtualenv

clean:
	rm targetsylph-c

.PHONY: clean package-deps
