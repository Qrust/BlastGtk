#!/bin/bash
set -m
if [ ! -x ./VersionIncrementerMain ]
    then
        echo "compiling version incrementer"
        ghc -outputdir=/tmp/VersionIncrementerMainGHCTEMP --make VersionIncrementerMain.hs\
        && echo "removing temp dir..." && rm -rfv /tmp/VersionIncrementerMainGHCTEMP
fi
currentversion=`./VersionIncrementerMain --get`
echo "Current version: \"$currentversion\""
echo "Removing previous pack"
rm -fv wipe-image-pack*.zip
echo "Packaging image pack"
zip -rj wipe-image-pack-$currentversion.zip imagepack/
