#!/bin/sh
cabal configure -v --disable-optimization --disable-library-profiling \
--disable-split-objs -f test --ghc-options=-rtsopts --ghc-options=-O0 "$@"
