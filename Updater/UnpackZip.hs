module Updater.UnpackZip
    (unpackBlastItWithPissUpdateZip
    ,blastEntry
    ,writeWithBackup
    ,stripBlastItWithPiss
    ) where
import Import
import System.Directory
import System.FilePath
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as L

stripBlastItWithPiss :: Entry -> Maybe Entry
stripBlastItWithPiss x
    | Just a <- stripPrefix "BlastItWithPiss/" (eRelativePath x)
    , not $ null a = Just x{eRelativePath = a}
    | otherwise = Nothing

-- FIXME HACK
ourExecutables :: [String]
ourExecutables =
    ["BlastItWithPiss"
    ,"blastgtk"
    ,"proxychecker"
    ,"blastcli"
    ]

-- HORRIBLE HACK both zip-archive and zip-conduit don't preserve file permissions
-- so instead we'll simply set executable bit for those files we know are executables.
-- Of course we could always switch to tar or LibZip, or
-- add proper permission handling to zip-archive, but I'm too lazy for that.
setExecutableBitForOurBinaries :: FilePath -> IO ()
setExecutableBitForOurBinaries filepath
    | f <- takeBaseName filepath
    , f `elem` ourExecutables = do
        p <- getPermissions filepath
        setPermissions filepath p{executable=True}
    | otherwise = return ()
-- /HORRIBLE HACK

writeWithBackup :: FilePath -> FilePath -> LByteString -> IO ()
writeWithBackup backupdir filepath content = do
    whenM (doesFileExist filepath) $ do
        let bfilepath = backupdir </> filepath
        let bfiledir = takeDirectory bfilepath
        unlessM (doesDirectoryExist bfiledir) $
            createDirectoryIfMissing True bfiledir
        renameFile filepath bfilepath
    L.writeFile filepath content
    setExecutableBitForOurBinaries filepath

blastEntry :: FilePath -> Entry -> IO ()
blastEntry _ Entry{eRelativePath=[]} = return ()
blastEntry backupdir e@Entry{..}
    | last eRelativePath == '/' = do
        createDirectoryIfMissing True eRelativePath
    | otherwise = do
        let dir = takeDirectory eRelativePath
        unlessM (doesDirectoryExist dir) $
            createDirectoryIfMissing True dir
        writeWithBackup backupdir eRelativePath (fromEntry e)

unpackBlastItWithPissUpdateZip :: FilePath -> FilePath -> IO ()
unpackBlastItWithPissUpdateZip backupdir zipfile = do
    rawarc <- toArchive <$> L.readFile "BlastItWithPiss-BETA-linux-x86-0.1.0.607.zip"
    mapM_ (blastEntry backupdir) $ mapMaybe stripBlastItWithPiss $ zEntries rawarc

{-
showBits :: Word32 -> String
showBits i =
    concatMap (show . fromEnum . testBit i) [0..31]
-}

--mapM_ putStrLn $ map show $ nubBy (\(_,a) (_,b) -> a==b) $ map (\Entry{..} -> (eRelativePath, showBits eExternalFileAttributes)) $ zEntries rawarc
