#!/bin/sh
if [ ! -x ./${1} ]
    then
        echo "compiling ${1}"
        ghc -outputdir=/tmp/${1}GHCTEMP --make scripts/${1}.hs -o ${1} \
        -XCPP -XScopedTypeVariables -XFlexibleContexts -XNoImplicitPrelude \
        -XOverloadedStrings -XRecordWildCards \
        -idist/build/autogen -optP-include -optPdist/build/autogen/cabal_macros.h \
        && echo "removing temp dir..." && rm -rfv /tmp/${1}GHCTEMP
    else
        echo "Using precompiled version of ${1}, if you want to recompile - delete it."
fi
