#!/bin/sh
source "./hdevtools-options.sh"
PKGDB=""
SANDDIR="$(echo .cabal-sandbox/ghc-*-packages.conf.d)"
if [ -d $SANDDIR ]
    then
        PKGDB="-g -package-conf${SANDDIR}"
fi
hdevtools check $HDEVTOOLSOPTS $PKGDB $@
