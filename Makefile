CXXFLAGS=-O3 -Wall -Werror -pedantic -std=c++14
CXXFLAGS+=-I${HOME}/Code/C++/range-v3/include/

SOURCES=$(wildcard src/*.cc)
TARGETS=$(patsubst src/%.cc,bin/%,$(SOURCES))

all: ${TARGETS}

bin/%: src/%.cc
	@mkdir -p bin/
	${CXX} ${CXXFLAGS} -o $@ $^

clean:
	rm -rf bin/
