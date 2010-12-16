CABAL = runhaskell Setup.hs

.PHONY: default
default:install

.PHONY: clean
clean::
	$(CABAL) clean

.PHONY: configure
configure:
	$(CABAL) configure --user

.PHONY: build
build: configure
	$(CABAL) build

.PHONY: install
install: build
	$(CABAL) install

.PHONY: test
test: install
#	time genLobster --multiple dist/test/refpolicy ../../data/Reference-Policy/refpolicy
	for i in dist/test/refpolicy/*.lsr; do lobster-validate $$i || echo $$i failed; done
