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
SELINUX_EXAMPLES="test.json"

if [ -z "$1" ]; then
  d=`date +"%Y%m%d"`
  dir="lobster-tools-$d"
else
  dir="$1"
fi

echo "Preparing release in $dir ..."
mkdir "$dir"

for f in $PROGS; do
  cp ".cabal-sandbox/bin/$f" "$dir"
done

mkdir "$dir/examples"
mkdir "$dir/examples/iptables"
mkdir "$dir/examples/selinux"

for f in $IPTABLES_EXAMPLES; do
  cp "iptables-lobster/$f" "$dir/examples/iptables"
done

for f in $SELINUX_EXAMPLES; do
  cp "v3spa-server/$f" "$dir/examples/selinux"
done

cp -a "v3spa-server/data/refpolicy" "$dir"

zip -r "$dir.zip" "$dir"
rm -rf "$dir"
