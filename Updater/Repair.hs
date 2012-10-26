module Updater.Repair
    (needRepair
    ,gtkblastBinary
    ,blastItWithPissBinary
    ,binaries
    ,distribFiles
    ) where
import Import
import System.Directory

gtkblastBinary :: FilePath
gtkblastBinary =
#ifdef mingw32_HOST_OS
    "gtkblast.exe"
#else
    "gtkblast"
#endif

blastItWithPissBinary :: FilePath
blastItWithPissBinary =
#ifdef mingw32_HOST_OS
    "BlastItWithPiss.exe"
#else
    "BlastItWithPiss"
#endif

binaries :: [FilePath]
binaries =
#ifdef mingw32_HOST_OS
    [gtkblastBinary, blastItWithPissBinary, "proxychecker.exe"] -- ++ ["cliblast.exe"]
#else
    [gtkblastBinary, blastItWithPissBinary, "proxychecker"] -- ++ ["cliblast"]
#endif

distribFiles :: [FilePath]
distribFiles = binaries ++ ["resources/2ch.so.png"
                           ,"resources/blast.glade"
                           ,"resources/agitka.png"]

needRepair :: IO Bool
needRepair = do
    flip anyM distribFiles $ \f -> do
        not <$> doesFileExist f
