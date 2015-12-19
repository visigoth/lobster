.PHONY: all ghc clean

all: ghc dist/bin
	stack install --local-bin-path dist/bin

clean: ghc
	stack clean
	rm -rf dist/

dist/bin:
	mkdir -p dist/bin

ghc:
	stack setup

# vim: set noet:
