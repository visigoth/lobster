GHC?=ghc-7.6.3
# snap doesn't build properly with profiling
CABAL_OPTS?=--disable-executable-profiling --disable-library-profiling


# Libraries to add with "cabal sandbox add-source".
SUBPROJECTS :=                 \
  lobster                      \
  lobster-core                 \
	lobster-selinux              \
  SCD                          \
  genLobster                   \
  v3spa-server                 \
  iptables-helpers             \
  iptables-lobster

.PHONY: all
all: .cabal-sandbox
	@cabal install v3spa-server -w ${GHC} ${CABAL_OPTS}

.PHONY: clean
clean:
	rm -rf cabal.sandbox.config .cabal-sandbox
	rm -rf $(addsuffix /dist,$(SUBPROJECTS))

# Create the Cabal sandbox if it doesn't exist or
# the Makefile changes (to update the source links).
.cabal-sandbox: $(MAKEFILE_LIST)
	@cabal sandbox init
	@cabal sandbox add-source $(SUBPROJECTS)

release: all
	@sh release.sh

# vim: set noet:
