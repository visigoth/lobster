
# Libraries to add with "cabal sandbox add-source".
SUBPROJECTS :=                 \
  lobster                      \
  SCD                          \
  genLobster                   \
  v3spa-server                 \
  iptables-helpers             \
  iptables-lobster

.PHONY: all
all: .cabal-sandbox
	@cabal install genLobster v3spa-server iptables-lobster

.PHONY: clean
clean:
	rm -rf cabal.sandbox.config .cabal-sandbox

# Create the Cabal sandbox if it doesn't exist or
# the Makefile changes (to update the source links).
.cabal-sandbox: $(MAKEFILE_LIST)
	@cabal sandbox init
	@cabal sandbox add-source $(SUBPROJECTS)

release: all
	@sh release.sh

# vim: set noet:
