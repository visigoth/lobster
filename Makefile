
.PHONY: lobster lobster-selinux lobster-xsm lobster-validate genLobster
.PHONY: scd shrimp

all : nolviz lviz

lobster-all: lobster lobster-selinux lobster-xsm lobster-validate genLobster

nolviz : lobster-all scd shrimp

lobster:
	(cd lobster && cabal-dev --sandbox=../cabal-dev install)

scd:
	(cd SCD && cabal-dev --sandbox=../cabal-dev install)

lobster-selinux:
	(cd lobster-selinux && cabal-dev --sandbox=../cabal-dev install)

lobster-xsm:
	(cd lobster-xsm && cabal-dev --sandbox=../cabal-dev install)

lobster-validate:
	(cd lobster-validate && cabal-dev --sandbox=../cabal-dev install)

shrimp:
	(cd shrimp && cabal-dev --sandbox=../cabal-dev install)

genLobster:
	(cd genLobster && cabal-dev --sandbox=../cabal-dev install)

lviz:
	(cd lviz && cabal-dev --sandbox=../cabal-dev install)

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
