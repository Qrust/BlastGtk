#!/bin/sh
case `uname` in
    MINGW*)
        lbdir="dos"
        foldr="dos-dist";;
    *)
        lbdir="linux"
        foldr="linux-dist";;
esac
case $1 in
    fast*)
        optimi="--ghc-options=-O0 --disable-optimization";;
    *)
        case `uname` in
            MINGW*) optimi="--ghc-options=-O2 --enable-optimization=2";;
            *) optimi="--ghc-options=-fllvm --ghc-options=-O2 --enable-optimization=2";;
        esac;;
esac
echo $lbdir
echo "\n"
echo $foldr
echo "\n"
echo $optimi
echo "\n"
rm -rfv $foldr
mkdir $foldr
mkdir $foldr/tempprefixdir
cabal configure --builddir=builddir/$foldr -f bindist --verbose\
 --enable-executable-stripping --disable-split-objs\
 --disable-library-profiling\
 --disable-executable-profiling\
 $optimi\
 --prefix=`pwd`/$foldr/tempprefixdir --bindir=$foldr/BlastItWithPiss &&\
 if cabal build --builddir=builddir/$foldr --verbose
 then
  cabal copy --builddir=builddir/$foldr --verbose
  echo "Removing ${foldr}/tempprefixdir"
  rm -rfv $foldr/tempprefixdir
  echo "Copying images"
  cp -rv images $foldr/BlastItWithPiss/images
  echo "Copying resources and pastas"
  cp -rv resources $foldr/BlastItWithPiss/resources
  echo "Copying libraries"
  cp -rv libs/$lbdir/. $foldr/BlastItWithPiss
  echo "Copying license, source dist and music recommendations"
  cp -v LICENSE $foldr/BlastItWithPiss
  cabal sdist --builddir=builddir/$foldr --output-directory=$foldr/BlastItWithPiss/source-code
  cp -v music $foldr/BlastItWithPiss
  echo "Finished building, don't forget to check the contents of distrib, and get rid of any unwanted dependencies/GLIBC symbols" &&\
  echo "\n"
 else
  exit 1
 fi
