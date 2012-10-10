module GtkBlast.Directory
    (ModificationTime
    ,getResourceFile
    ,configDir
    ,nullTime
    ,timeRightNow
    ,timeJustAfterNullTime'ie'forceUpdateJustOnce
    ) where
import Import
import GtkBlast.IO
import System.FilePath
import System.Directory
import Paths_blast_it_with_piss
#if !MIN_VERSION_directory(1,2,0)
import System.Time
#endif

#if MIN_VERSION_directory(1,2,0)
type ModificationTime = UTCTime
#else
type ModificationTime = ClockTime
#endif

getResourceFile :: MonadIO m => String -> m String
#if defined(BINDIST)||defined(TEST)
getResourceFile x = return $ "resources" </> x
#else
getResourceFile x = io $ getDataFileName $ "resources" </> x
#endif

configDir :: IO String
#if defined(BINDIST)||defined(TEST)
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

timeRightNow :: MonadIO m => m ModificationTime
#if MIN_VERSION_directory(1,2,0)
timeRightNow = io getCurrentTime
#else
timeRightNow = io getClockTime
#endif

timeJustAfterNullTime'ie'forceUpdateJustOnce :: ModificationTime
#if MIN_VERSION_directory(1,2,0)
timeJustAfterNullTime'ie'forceUpdateJustOnce = UTCTime (ModifiedJulianDay 0) 1
#else
timeJustAfterNullTime'ie'forceUpdateJustOnce = TOD 0 1
#endif
