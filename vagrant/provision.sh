#! /bin/sh

curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/23/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo
sudo dnf -y install zlib-devel stack
