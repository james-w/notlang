FROM ubuntu

ADD package-deps.txt package-deps.txt

RUN apt-get update
RUN cat package-deps.txt | xargs apt-get install -y --no-install-recommends

WORKDIR /working
ADD Makefile Makefile
ADD notlang notlang
ADD setup.py setup.py
RUN make deps

VOLUME ['/notlang']
WORKDIR /notlang

ENV VIRTUALENV /working/virtualenv
CMD make autotest
