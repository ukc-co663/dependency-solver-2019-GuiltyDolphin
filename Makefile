stack=stack

all: build

build: deps
	$(stack) build

deps:
	@./install_deps.sh

test:
	$(stack) test

clean:
	$(stack) clean

ghci:
	$(stack) ghci

.PHONY: all build deps test clean ghci
