#! /bin/sh
#
# release.sh --- Prepare a zip file of tools for release.
#
# Copyright (C) 2013, Galois, Inc.
# All Rights Reserved.
#

set -x
set -e

PROGS="lobster-json m4-lobster v3spa-server"

IPTABLES_EXAMPLES="ftp.iptables example.iptables"

d=`date +"%Y%m%d"`
dir="lobster-tools-$d"

echo "Preparing release in $d ..."
mkdir "$dir"

for f in $PROGS; do
  cp ".cabal-sandbox/bin/$f" "$dir"
done

mkdir "$dir/examples"
mkdir "$dir/examples/iptables"

for f in $IPTABLES_EXAMPLES; do
  cp "iptables-lobster/$f" "$dir/examples/iptables"
done

zip -r "$dir.zip" "$dir"
rm -rf "$dir"
