#!/bin/bash
#
# Use Nix[] to create a build environment that installs stack and the
# appropriate GHC version. If you use Nix you can run:
#
#   $ ./shell.sh
#
# That will provide a shell with build dependencies set up. Then run build or
# test tasks. E.g.:
#
#   $ make && make test && make release
#

function ldPathfromFlags {
  while IFS=' ' read -ra FLAGS; do
	for line in "${FLAGS[@]}"; do
	  if [[ $line =~ ^-L ]]; then
		LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${line#-L}"
	  fi
	done
  done <<< "$1"
  export LD_LIBRARY_PATH
}
export -f ldPathfromFlags

nix-shell \
  --packages haskell.compiler.ghc784 glibcLocales stack stdenv zlib \
  --command 'ldPathfromFlags "$NIX_LDFLAGS"; return'
