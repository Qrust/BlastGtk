#!/bin/sh
PKGDB=""
SANDDIR="$(echo .cabal-sandbox/*ghc*packages.conf.d)"
if [ -d $SANDDIR ]
    then
        PKGDB="-g -package-conf${SANDDIR}"
fi
HDEVTOOLSOPTS="${PKGDB} -g -package-confdist/package.conf.inplace -g -idist/build/autogen -g -optP-include -g -optPdist/build/autogen/cabal_macros.h -g -XNoImplicitPrelude -g -XPackageImports -g -XCPP -g -XOverloadedStrings -g -XRecordWildCards -g -XNamedFieldPuns -g -XScopedTypeVariables -g -XFlexibleContexts -g -XDeriveDataTypeable -g -XBangPatterns -g -XConstraintKinds -g -isrc -g -Wall -g -DTEST"
