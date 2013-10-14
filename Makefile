
CABAL := cabal-dev --sandbox=../cabal-dev --force-reinstalls

all : nolviz

nolviz :
	(cd lobster && $(CABAL) install)
	(cd SCD && $(CABAL) install)
	(cd lobster-selinux && $(CABAL) install)
	(cd lobster-xsm && $(CABAL) install)
	(cd lobster-validate && $(CABAL) install)
	(cd shrimp && $(CABAL) install)
	(cd genLobster && $(CABAL) install)

lviz : nolviz
	(cd lviz && $(CABAL) install)

clean :
	rm -rf cabal-dev
	(cd lobster && cabal clean)
	(cd SCD && cabal clean)
	(cd lobster-selinux && cabal clean)
	(cd lobster-xsm && cabal clean)
	(cd lobster-validate && cabal clean)
	(cd shrimp && cabal clean)
	(cd lviz && cabal clean)
	(cd genLobster && cabal clean)
