#!/bin/sh
case `uname` in
    MINGW*)
        ldopt="-optl-mwindows";;
    *)
        ldopt="-optl-Wl,-rpath,'\$ORIGIN/libs'";;
esac
echo $ldopt
rm -rf distribution
mkdir distribution
mkdir distribution/tempprefixdir
cabal clean
cabal configure -f bindist\
 --enable-optimization=2 --enable-executable-stripping\
 --disable-library-profiling\
 --disable-executable-profiling\
 --ghc-options=-O2 --ghc-options=$ldopt\
 --prefix=`pwd`/distribution/tempprefixdir --bindir=distribution
cabal build
cabal copy
echo "Removing tempprefixdir"
rm -rf distribution/tempprefixdir
echo "Copying images"
cp -r images distribution/images
echo "Copying resources and pastas"
cp -r resources distribution/resources
echo "Copying libraries"
cp -r libs/. distribution
echo "Finished building"
