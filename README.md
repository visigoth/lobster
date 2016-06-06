# v3spa-lobster

## Prerequisites

- Make
- Stack
- zlib (zlib-devel on Redhat)

Stack is a Haskell build tool. It fetches the appropriate compiler version, and
fetches library dependencies from Stackage and Hackage. See installation
instructions at http://docs.haskellstack.org/en/stable/README/#how-to-install

## Building

To run tests:

    $ make test

To build release files:

    $ make release

The release target produces a file called `lobster-tools-${DATE}.zip` in the
project root. The v3spa server can be run using the `v3spa-server` executable from the zip file, or by running:

    $ make serve
