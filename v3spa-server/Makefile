.PHONY: all ghc clean

THIS_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

all: ghc dist/bin
	stack install --local-bin-path dist/bin

clean: ghc
	stack clean
	rm -rf dist/

dist/bin:
	mkdir -p dist/bin

ghc:
	stack setup

serve:
	cd $(THIS_DIR)/data && stack exec v3spa-server

# vim: set noet:
