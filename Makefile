stack=stack

all: build

build: deps
	$(stack) build

deps:
	./install_deps.sh
	touch deps

test:
	$(stack) test

clean:
	$(stack) clean

.PHONY: all build test clean
