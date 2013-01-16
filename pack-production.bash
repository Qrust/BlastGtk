#!/bin/bash
set -m
sh compile-script.sh VersionIncrementerMain
sh compile-script.sh DownloadsUploader
delete=False
case "$@" in
    *nobump*)
        echo "Nobump";;
    *)
        echo "Bumping package version..."
        ./VersionIncrementerMain;;
esac
case "$@" in
    *delete*)
        delete=True;;
esac
echo "Delete: " $delete
currentversion=`./VersionIncrementerMain --get`
echo "Current version: \"$currentversion\""
echo "Removing all previous dist packages..."
rm -fv BlastItWithPiss-*-x86-*.zip
linuxzip="BlastItWithPiss-linux-x86-$currentversion.zip"
windowszip="BlastItWithPiss-windows-x86-$currentversion.zip"
echo "Linux archive will be \"$linuxzip\""
echo "DOS archive will be \"$windowszip\""
(echo "Packaging Linux";
 bash build-production.bash &&\
 cd linux-dist && zip -r ../$linuxzip BlastItWithPiss/) &&\
 (echo "Packaging DOS";
 # Running any msys binary does strange things to my linux shell,
 # input doesn't get echoed and 'echo' output is printed without a newline.
 # So instead, we'll use following workaround.
 bash build-production.bash _ wine &&\
 cd dos-dist && zip -r ../$windowszip BlastItWithPiss/)
if [ -r $linuxzip ] && [ -r $windowszip ]
    then
        echo "Updating manifest and uploading archives..."
        ./DownloadsUploader $currentversion $linuxzip $windowszip "True" $delete
    else
        echo "Something bad happened. Scroll up for error messages."
fi
