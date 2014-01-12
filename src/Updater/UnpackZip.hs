module Updater.UnpackZip
    (unpackBlastItWithPissUpdateZip
    ,unpackBlastItWithPissUpdateZipFromLBS
    ,unpackBlastItWithPissUpdateZipFromFile
    ,blastEntry
    ,writeWithBackup
    ,stripBlastItWithPiss
    ,restoreFromBackup
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

writeWithBackup :: FilePath -> FilePath -> LByteString -> IO ()
writeWithBackup backupdir filepath content = do
    whenM (doesFileExist filepath) $ do
        let bfilepath = backupdir </> filepath
        let bfiledir = takeDirectory bfilepath
        unlessM (doesDirectoryExist bfiledir) $
            createDirectoryIfMissing True bfiledir
        renameFile filepath bfilepath
    L.writeFile filepath content

blastEntry :: FilePath -> Entry -> IO ()
blastEntry _ Entry{eRelativePath=[]} = return ()
blastEntry backupdir e@Entry{ eRelativePath }
    | last eRelativePath == '/' = do
        createDirectoryIfMissing True eRelativePath
    | otherwise = do
        let dir = takeDirectory eRelativePath
        unlessM (doesDirectoryExist dir) $
            createDirectoryIfMissing True dir
        writeWithBackup backupdir eRelativePath (fromEntry e)

unpackBlastItWithPissUpdateZip :: FilePath -> Archive -> IO ()
unpackBlastItWithPissUpdateZip backupdir rawarc = do
    mapM_ (blastEntry backupdir) $ mapMaybe stripBlastItWithPiss $ zEntries rawarc

unpackBlastItWithPissUpdateZipFromLBS :: FilePath -> LByteString -> IO ()
unpackBlastItWithPissUpdateZipFromLBS backupdir zipbytes = do
    unpackBlastItWithPissUpdateZip backupdir $ toArchive zipbytes

unpackBlastItWithPissUpdateZipFromFile :: FilePath -> FilePath -> IO ()
unpackBlastItWithPissUpdateZipFromFile backupdir zipfile = do
    unpackBlastItWithPissUpdateZipFromLBS backupdir =<< L.readFile zipfile

--seriously?
restoreFromBackup :: FilePath -> FilePath -> IO ()
restoreFromBackup backupdir destinationdir = do
    createDirectoryIfMissing True destinationdir
    backupfiles <- filter (\x -> x /= "." && x /= "..") <$> getDirectoryContents backupdir
    forM_ backupfiles $ \relative_f -> do
        let backf = backupdir </> relative_f
            destf = destinationdir </> relative_f
        ifM (doesDirectoryExist backf) -- is it a directory?
            (restoreFromBackup backf destf) -- recurse
            (renameFile backf destf)
    removeDirectory backupdir

{-
showBits :: Word32 -> String
showBits i =
    concatMap (show . fromEnum . testBit i) [0..31]
-}

--mapM_ putStrLn $ map show $ nubBy (\(_,a) (_,b) -> a==b) $ map (\Entry{..} -> (eRelativePath, showBits eExternalFileAttributes)) $ zEntries rawarc
