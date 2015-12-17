.PHONY: all clean release

all:
	$(MAKE) -C v3spa-server all

clean:
	$(MAKE) -C v3spa-server clean

release: all
	@sh release.sh

# vim: set noet:
