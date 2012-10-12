#!/bin/bash
set -m
if [ ! -x ./VersionIncrementerMain ]
    then
        echo "compiling version incrementer"
        ghc -outputdir=/tmp/VersionIncrementerMainGHCTEMP --make VersionIncrementerMain.hs\
        && echo "removing temp dir..." && rm -rfv /tmp/VersionIncrementerMainGHCTEMP
fi
case $1 in
    nobump);;
    *)
        echo "Bumping package version..."
        ./VersionIncrementerMain;;
esac
currentversion=`./VersionIncrementerMain --get`
echo "Current version: \"$currentversion\""
echo "Removing all previous dist packages..."
# FIXME Update when no longer is BETA
rm -fv BlastItWithPiss-BETA-*-x86-*.zip
# FIXME Update when no longer is BETA
(echo "Packaging Linux";
 sh build-production.sh &&\
 cd linux-dist && zip -r ../BlastItWithPiss-BETA-linux-x86-$currentversion.zip BlastItWithPiss/) &&\
 # FIXME Update when no longer is BETA
 (echo "Packaging DOS";
 wine sh build-production.sh &&\
 cd dos-dist && zip -r ../BlastItWithPiss-BETA-windows-x86-$currentversion.zip BlastItWithPiss/)
# TODO is there a github api for downloads?
# http://developer.github.com/v3/repos/downloads/ here it is
# TODO automate uploading of new versions and manifest generation
# CLARIFICATION Manifest should be generated AFTER upload
git commit -a && git push
echo "Done packaging, now upload those."
firefox "https://github.com/exbb2/BlastItWithPiss/downloads"&
