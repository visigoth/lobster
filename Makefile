
# Libraries to add with "cabal sandbox add-source".
SUBPROJECTS :=                 \
  lobster                      \
  SCD                          \
  lobster-selinux

.PHONY: all
all: proj-lobster-selinux

.PHONY: clean
clean:
	rm -rf cabal.sandbox.config .cabal-sandbox

# Create the Cabal sandbox if it doesn't exist or
# the Makefile changes (to update the source links).
.cabal-sandbox: $(MAKEFILE_LIST)
	@cabal sandbox init
	@cabal sandbox add-source $(SUBPROJECTS)

proj-%: .cabal-sandbox
	@cabal install $*

