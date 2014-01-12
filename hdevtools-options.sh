#!/bin/sh
if [ -z "$NO_G_OPTION" ]
    then __g="-g"
fi
if [ -f cabal.sandbox.config ]
    then HDEVTOOLSOPTS="$__g -package-db=$(sed -nr 's/(package-db: *)(.*)/\2/p' cabal.sandbox.config | head -n 1)"
fi
if [ -f "dist/package.conf.inplace" ]
    then HDEVTOOLSOPTS="$HDEVTOOLSOPTS $__g -package-db=dist/package.conf.inplace\
 $__g -idist/build/autogen $__g -optP-include $__g -optPdist/build/autogen/cabal_macros.h"
fi
HDEVTOOLSOPTS="${HDEVTOOLSOPTS} $__g -v1 $__g -XNoImplicitPrelude $__g -XPackageImports $__g -XCPP $__g -XOverloadedStrings $__g -XRecordWildCards $__g -XNamedFieldPuns $__g -XScopedTypeVariables $__g -XFlexibleContexts $__g -XDeriveDataTypeable $__g -XBangPatterns $__g -XConstraintKinds $__g -isrc $__g -Wall $__g -DTEST"
