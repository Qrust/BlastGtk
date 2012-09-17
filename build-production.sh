#!/bin/sh
case `uname` in
    MINGW*)
        ghcopt="--ghc-options=-optl-mwindows"
        lbdir="dos"
        foldr="dos-dist";;
    *)
        ghcopt="--ghc-options=-fllvm --ghc-options=-optl-Wl,-rpath,\$ORIGIN"
        lbdir="linux"
        foldr="linux-dist";;
esac
echo $ghcopt
echo "\n"
echo $lbdir
echo "\n"
echo $foldr
echo "\n"
rm -rf $foldr
mkdir $foldr
mkdir $foldr/tempprefixdir
cabal clean
cabal configure -f bindist\
 --enable-optimization=2 --enable-executable-stripping\
 --disable-library-profiling\
 --disable-executable-profiling\
 --ghc-options=-O2 $ghcopt\
 --prefix=`pwd`/$foldr/tempprefixdir --bindir=$foldr/BlastItWithPiss
cabal build
cabal copy
echo "Removing ${foldr}/tempprefixdir"
rm -rf $foldr/tempprefixdir
echo "Copying images"
cp -r images $foldr/BlastItWithPiss/images
echo "Copying resources and pastas"
cp -r resources $foldr/BlastItWithPiss/resources
echo "Copying libraries"
cp -r libs/$lbdir/. $foldr/BlastItWithPiss
echo "Copying license, source instructions and music recommendations"
cp LICENSE $foldr/BlastItWithPiss
cp WHERETOGETTHESOURCE $foldr/BlastItWithPiss
cp music $foldr/BlastItWithPiss
cabal clean
echo "Finished building, don't forget to check the contents of distrib, and get rid of any unwanted dependencies/GLIBC symbols"
echo "\n"
