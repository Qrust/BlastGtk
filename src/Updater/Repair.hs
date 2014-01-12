module Updater.Repair
    (needRepair
    ,gtkblastBinary
    ,blastItWithPissBinary
    ,binaries
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
-- TODO remove smyvalka
binaries :: [FilePath]
binaries =
#ifdef mingw32_HOST_OS
    [gtkblastBinary, blastItWithPissBinary, "proxychecker.exe", "smyvalka.exe"] -- ++ ["cliblast.exe"]
#else
    [gtkblastBinary, blastItWithPissBinary, "proxychecker", "smyvalka"] -- ++ ["cliblast"]
#endif

distribFiles :: [FilePath]
distribFiles = binaries ++ ["resources/2ch.so.png"
                           ,"resources/blast.glade"
                           ]

needRepair :: IO Bool
needRepair = do
    flip anyM distribFiles $ \f -> do
        not <$> doesFileExist f
