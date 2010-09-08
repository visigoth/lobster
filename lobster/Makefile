##############################################################################
# Lobster Makefile
# (c) 2009 Galois, Inc.
##############################################################################

BNFC = bnfc

CABAL = runghc Setup.hs

BNFC_FILES = \
  src/Lobster/Abs.hs \
  src/Lobster/Doc.tex \
  src/Lobster/ErrM.hs \
  src/Lobster/Lex.x \
  src/Lobster/Par.y \
  src/Lobster/Print.hs \
  src/Lobster/Skel.hs \
  src/Lobster/Test.hs

.PHONY: default
default:
	$(MAKE) install # need to install for test suite to get built

.PHONY: clean
clean:
	rm -f $(BNFC_FILES)
	$(CABAL) clean
	(cd src/Lobster;make clean)
	(cd src/Lobster/Test;make clean)
	rm -f test/examples/*.lobster test/examples/*.flatten

$(BNFC_FILES): src/Lobster/Lobster.cf
	rm -f $(BNFC_FILES)
	cd src/Lobster && $(BNFC) -d Lobster.cf
	mv src/Lobster/Lobster/*.* src/Lobster
	rmdir src/Lobster/Lobster

.PHONY: configure
configure: $(BNFC_FILES)
	$(CABAL) configure --user

.PHONY: build
build: configure
	$(CABAL) build

.PHONY: install
install: build
	$(CABAL) install

.PHONY: test
test: src/Lobster/dist/build/lobster-parser-test/lobster-parser-test src/Lobster/Test/dist/build/lobster-test/lobster-test
	@for f in test/examples/example*.lsr ; do src/Lobster/dist/build/lobster-parser-test/lobster-parser-test $$f > /dev/null && echo "Successfully parsed $$f" ; done
	src/Lobster/Test/dist/build/lobster-test/lobster-test

src/Lobster/dist/build/lobster-parser-test/lobster-parser-test :
	(cd src/Lobster;make)

src/Lobster/Test/dist/build/lobster-test/lobster-test :
	(cd src/Lobster/Test;make)

.PHONY: doc
doc:
	$(CABAL) haddock
