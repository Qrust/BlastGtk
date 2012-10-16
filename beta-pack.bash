#!/bin/bash
set -m
sh compile-script.sh VersionIncrementerMain
sh compile-script.sh DownloadsUploader
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
linuxzip="BlastItWithPiss-BETA-linux-x86-$currentversion.zip"
windowszip="BlastItWithPiss-BETA-windows-x86-$currentversion.zip"
echo "Linux archive will be \"$linuxzip\""
echo "DOS archive will be \"$windowszip\""
# FIXME Update when no longer is BETA
(echo "Packaging Linux";
 sh build-production.sh &&\
 cd linux-dist && zip -r ../$linuxzip BlastItWithPiss/) &&\
 # FIXME Update when no longer is BETA
 (echo "Packaging DOS";
 wine sh build-production.sh &&\
 cd dos-dist && zip -r ../$windowszip BlastItWithPiss/)
# TODO is there a github api for downloads?
# http://developer.github.com/v3/repos/downloads/ here it is
# TODO automate uploading of new versions and manifest generation
# CLARIFICATION Manifest should be generated AFTER upload
if [ -r $linuxzip ] && [ -r $windowszip ]
    then
        echo "Updating manifest and uploading archives..."
        ./DownloadsUploader $currentversion $linuxzip $windowszip "True"
    else
        echo "Something bad happened. Scroll up for error messages."
fi
