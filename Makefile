.PHONY: all clean release test

all:
	$(MAKE) -C v3spa-server all

clean:
	$(MAKE) -C v3spa-server clean

release: all
	@sh release.sh

test:
	cd iptables-lobster; stack clean && stack test
	cd lobster-core; stack clean && stack test
	cd genLobster; stack clean && stack test

serve: all
	$(MAKE) -C v3spa-server serve

# vim: set noet:
