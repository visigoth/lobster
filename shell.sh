#!/bin/bash

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
