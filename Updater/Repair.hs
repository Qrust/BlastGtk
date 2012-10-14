module Updater.Repair
    (needRepair
    ,blastgtkBinary
    ,binaries
    ,distribFiles
    ) where
import Import
import System.Directory

blastgtkBinary :: FilePath
blastgtkBinary =
#ifdef mingw32_HOST_OS
    "blastgtk.exe"
#else
    "blastgtk"
#endif

binaries :: [FilePath]
binaries =
#ifdef mingw32_HOST_OS
    [blastgtkBinary, "BlastItWithPiss.exe", "proxychecker.exe"] -- ++ ["blastcli.exe"]
#else
    [blastgtkBinary, "BlastItWithPiss", "proxychecker"] -- ++ ["blastcli"]
#endif

distribFiles :: [FilePath]
distribFiles = binaries ++ ["resources/2ch.so.png", "resources/blast.glade"]

needRepair :: IO Bool
needRepair = do
    flip anyM distribFiles $ \f -> do
        not <$> doesFileExist f
