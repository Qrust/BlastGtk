#!/bin/sh
echo "Removing previous pack"
rm -fv wipe-image-pack-*.zip
echo "Packaging image pack"
zip -rj wipe-image-pack.zip imagepack/
