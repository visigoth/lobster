# v3spa-lobster

## Running with Vagrant

The V3SPA tools can be built inside a VM created by Vagrant using a
Fedora 23 base box.

To create the VM and install dependencies:

    $ vagrant up

To log into the virtual machine and run the server:

    $ vagrant ssh
    $ cd /vagrant
    $ make serve

To import an empty project using the '20140311' refpolicy:

    $ vagrant ssh
    $ cd /vagrant
    $ curl -X POST http://localhost:8000/projects/test/import/selinux --data-binary @v3spa-server/empty.json

To export this project as JSON for visualization:

    $ curl http://localhost:8000/projects/test/json

## Manual Build Instructions

### Dependencies

- Make
- Stack
- zlib (zlib-devel on Redhat)

Stack is a Haskell build tool. It fetches the appropriate compiler version, and
fetches library dependencies from Stackage and Hackage. See installation
instructions at http://docs.haskellstack.org/en/stable/README/#how-to-install

### Building

To run tests:

    $ make test

To build release files:

    $ make release

The release target produces a file called `lobster-tools-${DATE}.zip` in the
project root. The v3spa server can be run using the `v3spa-server` executable from the zip file, or by running:

    $ make serve

### Running the server

v3spa-server stores uploaded modules on the filesystem. It creates a directory
called `projects` in the working directory. It is a good idea to also create
a directory called `log` in the working directory - v3spa-server well output
access and error logs there.
