all : nolviz
	(cd lviz && cabal-dev --sandbox=../cabal-dev install)

nolviz :
	(cd lobster && cabal-dev --sandbox=../cabal-dev install)
	(cd SCD && cabal-dev --sandbox=../cabal-dev install)
	(cd lobster-selinux && cabal-dev --sandbox=../cabal-dev install)
	(cd lobster-xsm && cabal-dev --sandbox=../cabal-dev install)
	(cd lobster-validate && cabal-dev --sandbox=../cabal-dev install)
	(cd shrimp && cabal-dev --sandbox=../cabal-dev install)
	(cd genLobster && cabal-dev --sandbox=../cabal-dev install)

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
