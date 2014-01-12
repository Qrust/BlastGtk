module GtkBlast.Directory
    (ModificationTime
    ,bundledFile
    ,configDir
    ,nullTime
    ) where
import Import

#if !MIN_VERSION_directory(1,2,0)
import System.Time -- old-time
#endif

#if !defined(BINDIST) && !defined(TEST)
import System.FilePath
import System.Directory
import Paths_blast_it_with_piss
import qualified System.IO.Unsafe as U
#endif

#if MIN_VERSION_directory(1,2,0)
type ModificationTime = UTCTime
#else
type ModificationTime = ClockTime
#endif

bundledFile :: String -> String
#if defined(BINDIST) || defined(TEST)
bundledFile x = x
#else
bundledFile x = U.unsafePerformIO $ getDataFileName x
#endif

configDir :: IO String
#if defined(BINDIST) || defined(TEST)
configDir = return "."
#else
configDir = do c <- getAppUserDataDirectory "BlastItWithPiss"
               createDirectoryIfMissing False c
               return c
#endif

nullTime :: ModificationTime
#if MIN_VERSION_directory(1,2,0)
nullTime = UTCTime (ModifiedJulianDay 0) 0
#else
nullTime = TOD 0 0
#endif
