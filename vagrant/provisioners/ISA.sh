#! /bin/sh

## Quit immediately on error
set -e

## Want to run this as 'vagrant', so rerun if root
if [ "$(id -u)" = "0" ]; then
    sudo -u vagrant bash $0 $@
    exit 0
fi

if [ ! -e ISA ]; then
    git clone https://github.com/gaborcsardi/ISA.git
fi
