#! /bin/sh
#
# release.sh --- Prepare a zip file of tools for release.
#
# Copyright (C) 2013, Galois, Inc.
# All Rights Reserved.
#

set -x
set -e

PROGS="genLobster lobster-dot lobster-json lobster-selinux v3spa-server"

d=`date +"%Y%m%d"`
dir="lobster-tools-$d"

echo "Preparing release in $d ..."
mkdir "$dir"

for f in $PROGS; do
  cp ".cabal-sandbox/bin/$f" "$dir"
done

zip -r "$dir.zip" "$dir"
rm -rf "$dir"
