#!/bin/bash
set -e

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
        cfgr="configure"
        cr=`pwd`;;
    *)
        case "$@" in
            *wine*)
                lbdir="dos"
                foldr="dos-dist"
                cbl="wine cabal"
                cfgr="configure"
                cr=`winepath -w \`pwd\``;;
            *)
                lbdir="linux"
                foldr="linux-dist"
                cbl="$_cabal"
                cfgr="configure"
                cr=`pwd`;;
        esac;;
esac
case "$@" in
    *fast*)
        optimi="--ghc-options=-O0 --disable-optimization";;
    *llvm*)
        optimi="--ghc-options=-O2 --ghc-options=-fllvm --ghc-options=-funbox-strict-fields --enable-optimization=2";;
    *)
        optimi="--ghc-options=-O2  --ghc-options=-funbox-strict-fields --enable-optimization=2";;
esac
echo $lbdir
echo $foldr
echo $cbl
echo $optimi
echo $cr
rm -rfv $foldr
mkdir $foldr
mkdir $foldr/tempprefixdir
$cbl $cfgr --builddir=builddir/$foldr -f bindist --verbose\
 --enable-executable-stripping --disable-split-objs\
 --disable-library-profiling\
 --disable-executable-profiling\
 --ghc-options=-rtsopts\
 $optimi\
 --ghc-options=-Llibs/$lbdir/\
 --prefix=$cr/$foldr/tempprefixdir\
 --bindir=$foldr/BlastItWithPiss\
 --datadir=$foldr/BlastItWithPiss\
 --datasubdir=.\
 --docdir=$foldr/BlastItWithPiss &&\
 if $cbl build --builddir=builddir/$foldr --verbose
 then
  $cbl copy --builddir=builddir/$foldr --verbose
  echo "Removing ${foldr}/tempprefixdir"
  rm -rfv $foldr/tempprefixdir
  echo "Copying libraries"
  cp -rv libs/$lbdir/. $foldr/BlastItWithPiss
  echo "source dist"
  $cbl sdist --builddir=builddir/$foldr --output-directory=$foldr/BlastItWithPiss/source-code
  echo "copying self ($0) to sourcedist"
  cp -v $0 $foldr/BlastItWithPiss/source-code
  echo "Finished building"
 else
  echo "Build failed"
  exit 1
 fi
