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
 # Running any msys binary does strange things to my linux shell,
 # input doesn't get echoed and 'echo' output is printed without a newline.
 # So instead, we'll use following workaround.
 sh build-production.sh _ wine &&\
 cd dos-dist && zip -r ../$windowszip BlastItWithPiss/)
if [ -r $linuxzip ] && [ -r $windowszip ]
    then
        echo "Updating manifest and uploading archives..."
        ./DownloadsUploader $currentversion $linuxzip $windowszip "True"
    else
        echo "Something bad happened. Scroll up for error messages."
fi
