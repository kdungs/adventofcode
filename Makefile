CXXFLAGS=-O3 -Wall -Werror -pedantic -std=c++14
CXXFLAGS+=-I${HOME}/Code/C++/range-v3/include/

SOURCES=$(wildcard src/*.cc)
TARGETS=$(patsubst src/%.cc,bin/%,$(SOURCES))

all: ${TARGETS}

bin/04: src/04.cc
	@mkdir -p bin/
	${CXX} ${CXXFLAGS} -lcrypto -o $@ $^

bin/%: src/%.cc
	@mkdir -p bin/
	${CXX} ${CXXFLAGS} -o $@ $^

clean:
	rm -rf bin/
