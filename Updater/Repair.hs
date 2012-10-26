module Updater.Repair
    (needRepair
    ,gtkblastBinary
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

binaries :: [FilePath]
binaries =
#ifdef mingw32_HOST_OS
    [gtkblastBinary, "BlastItWithPiss.exe", "proxychecker.exe"] -- ++ ["cliblast.exe"]
#else
    [gtkblastBinary, "BlastItWithPiss", "proxychecker"] -- ++ ["cliblast"]
#endif

distribFiles :: [FilePath]
distribFiles = binaries ++ ["resources/2ch.so.png"
                           ,"resources/blast.glade"
                           ,"resources/agitka.png"]

needRepair :: IO Bool
needRepair = do
    flip anyM distribFiles $ \f -> do
        not <$> doesFileExist f
