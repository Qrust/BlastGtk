#!/bin/sh
if [ -d "cabal-dev" ]
    then
        _cabal="cabal-dev"
    else
        _cabal="cabal"
fi
case `uname` in
    MINGW*)
        lbdir="dos"
        foldr="dos-dist"
        cbl="cabal"
        cr=`pwd`;;
    *)
        case $2 in
            wine*)
                lbdir="dos"
                foldr="dos-dist"
                cbl="wine cabal"
                cr=`winepath -w \`pwd\``;;
            *)
                lbdir="linux"
                foldr="linux-dist"
                cbl="$_cabal"
                cr=`pwd`;;
        esac;;
esac
case $1 in
    fast*)
        optimi="--ghc-options=-O0 --disable-optimization";;
    *)
        case `uname` in
            MINGW*) optimi="--ghc-options=-O2 --enable-optimization=2";;
            *) case $2 in
                wine*) optimi="--ghc-options=-O2 --enable-optimization=2";;
                *) optimi="--ghc-options=-O2 --enable-optimization=2";;
               esac;;
        esac;;
esac
echo $lbdir
echo $foldr
echo $cbl
echo $optimi
echo $cr
rm -rfv $foldr
mkdir $foldr
mkdir $foldr/tempprefixdir
$cbl configure --builddir=builddir/$foldr -f bindist --verbose\
 --enable-executable-stripping --disable-split-objs\
 --disable-library-profiling\
 --disable-executable-profiling\
 --ghc-options=-rtsopts\
 $optimi\
 --prefix=$cr/$foldr/tempprefixdir --bindir=$foldr/BlastItWithPiss --datadir=$foldr/BlastItWithPiss --datasubdir=. --docdir=$foldr/BlastItWithPiss &&\
 if $cbl build --builddir=builddir/$foldr --verbose
 then
  $cbl copy --builddir=builddir/$foldr --verbose
  echo "Removing ${foldr}/tempprefixdir"
  rm -rfv $foldr/tempprefixdir
  echo "Copying libraries"
  cp -rv libs/$lbdir/. $foldr/BlastItWithPiss
  echo "source dist"
  $cbl sdist --builddir=builddir/$foldr --output-directory=$foldr/BlastItWithPiss/source-code
  echo "Finished building"
 else
  echo "Build failed"
  exit 1
 fi