all: compile

compile: deps
	./compile.sh

deps:
	./install_deps.sh
	touch deps

test: compile
	./run_tests.sh

clean:
	stack clean

.PHONY: all compile test clean
