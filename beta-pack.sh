echo "Packaging Linux"
sh build-production.sh &&\
 rm -f BlastItWithPiss-BETA-linux-x86-0.0.0.0.zip ;\
 zip -r BlastItWithPiss-BETA-linux-x86-0.0.0.0.zip linux-dist/./BlastItWithPiss
echo "Packaging DOS"
wine sh build-production.sh &&\
 rm -f BlastItWithPiss-BETA-windows-x86-0.0.0.0.zip ;\
 zip -r BlastItWithPiss-BETA-windows-x86-0.0.0.0.zip dos-dist/./BlastItWithPiss
git commit -a && git push
echo "Done packaging, now upload those https://github.com/exbb2/BlastItWithPiss/downloads"
