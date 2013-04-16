#!/bin/sh
SANDBOX="$(echo .cabal-sandbox/ghc-*-packages.conf.d)"
./hdevtools-check.sh -g -package-db=$SANDBOX $@
