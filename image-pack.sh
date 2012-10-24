#!/bin/sh
sh compile-script.sh VersionIncrementerMain
currentversion=`./VersionIncrementerMain --get`
echo "Current version: \"$currentversion\""
echo "Removing previous pack"
rm -fv wipe-image-pack*.zip
echo "Packaging image pack"
zip -rj wipe-image-pack-$currentversion.zip imagepack/
echo "Adding images to git"
git add imagepack/*
