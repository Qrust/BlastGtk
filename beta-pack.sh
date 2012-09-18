echo "Packaging Linux"
sh build-production.sh &&\
 rm -f BlastItWithPiss-BETA-0.0.0.0-linux-x86.7z ;\
 7za a BlastItWithPiss-BETA-0.0.0.0-linux-x86.7z linux-dist/./BlastItWithPiss
echo "Packaging DOS"
wine sh build-production.sh &&\
 rm -f BlastItWithPiss-BETA-0.0.0.0-windows-x86.7z ;\
 7za a BlastItWithPiss-BETA-0.0.0.0-windows-x86.7z dos-dist/./BlastItWithPiss
git commit -a
git push
echo "Done packaging, now upload those https://github.com/exbb2/BlastItWithPiss/downloads"
