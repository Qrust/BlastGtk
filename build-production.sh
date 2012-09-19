#!/bin/sh
case `uname` in
    MINGW*)
        ghcopt="--ghc-options=-optl-mwindows"
        lbdir="dos"
        foldr="dos-dist";;
    *)
        ghcopt="--ghc-options=-optl-Wl,-rpath,\$ORIGIN"
        lbdir="linux"
        foldr="linux-dist";;
esac
case $1 in
    fast*)
        optimi="--ghc-options=-O0 --disable-optimization";;
    *)
        case `uname` in
            MINGW*) optimi="--ghc-options=-O2 --enable-optimization=2";;
            MINGW*) optimi="--ghc-options=-fllvm --ghc-options=-O2 --enable-optimization=2";;
        esac;;
esac
echo $ghcopt
echo "\n"
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
 --enable-executable-stripping\
 --disable-library-profiling\
 --disable-executable-profiling\
 $optimi $ghcopt\
 --prefix=`pwd`/$foldr/tempprefixdir --bindir=$foldr/BlastItWithPiss &&\
 if cabal build --builddir=builddir/$foldr --verbose
 then
  cabal copy --builddir=builddir/$foldr --verbose &&\
  echo "Removing ${foldr}/tempprefixdir" &&\
  rm -rfv $foldr/tempprefixdir &&\
  echo "Copying images" &&\
  cp -rv images $foldr/BlastItWithPiss/images &&\
  echo "Copying resources and pastas" &&\
  cp -rv resources $foldr/BlastItWithPiss/resources &&\
  echo "Copying libraries" &&\
  cp -rv libs/$lbdir/. $foldr/BlastItWithPiss &&\
  echo "Copying license, source instructions and music recommendations" &&\
  cp -v LICENSE $foldr/BlastItWithPiss &&\
  cp -v WHERETOGETTHESOURCE $foldr/BlastItWithPiss &&\
  cp -v music $foldr/BlastItWithPiss &&\
  echo "Finished building, don't forget to check the contents of distrib, and get rid of any unwanted dependencies/GLIBC symbols" &&\
  echo "\n"
 else
  exit 1
 fi
