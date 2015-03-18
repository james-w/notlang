build: deps targetsylph-c

targetsylph-c: targetsylph.py
	PYTHONPATH=pypy python ./pypy/rpython/translator/goal/translate.py targetsylph.py

deps: pypy

pypy:
	hg clone https://bitbucket.org/pypy/pypy	

clean:
	rm targetsylph-c

.PHONY: clean
