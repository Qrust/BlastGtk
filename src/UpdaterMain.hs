module
#ifdef COMBINED_CHECK
    UpdaterMain
#else
    Main
#endif
    (main)
  where
import Import hiding (on)

import Updater.Manifest
import Updater.UnpackZip
import qualified Updater.Repair as Repair
import Updater.DownloadWithMD5
import Updater.GuiXML

import qualified Paths_blast_it_with_piss as Paths

import Data.Version

import Control.Concurrent

import System.Directory
import System.FilePath

import System.Environment
import System.Environment.Executable
import System.Exit
import System.Process

import Data.Aeson

import Graphics.UI.Gtk

import qualified Text.Show
import Text.ParserCombinators.ReadP

import Network
import Network.HTTP.Conduit

currentPlatform :: Platform
#if defined(linux_HOST_OS)
currentPlatform = Linux
#elif defined(mingw32_HOST_OS)
currentPlatform = Windows
#else
currentPlatform = Mac
#endif

manifestUrl :: String
manifestUrl = "https://raw.github.com/exbb2/BlastItWithPiss/master/UPDATE_MANIFEST"

helpMessage :: String
helpMessage =
    "Version: " ++ showVersion Paths.version ++ ", " ++ "Platform: " ++ show currentPlatform ++ "\n" ++
    "Autoupdater for BlastItWithPiss, checks for updates then calls gtkblast\n" ++
    "Use --repair to force update.\n" ++
    "Use --postinstall <version> to execute post install hooks"

mainNoBindist :: IO ()
mainNoBindist = do
    (path, _) <- splitExecutablePath
    let gtkblast = path </> Repair.gtkblastBinary
    void $ ifM
        (doesFileExist gtkblast)
        (createProcess $ proc gtkblast [])
        (createProcess $ proc Repair.gtkblastBinary [])

data BadEnd = ChecksumMismatch String MD5Sum
            | NoBuildAvailable
            | UnparseableManifest
    deriving (Typeable)

instance Show BadEnd where
    show (ChecksumMismatch url md5) =
        "Контрольная сумма для скачанного архива не совпала, возможно данные были повреждены.\n" ++
        "URL: " ++ url ++ "\n" ++
        "MD5: " ++ md5
    show NoBuildAvailable = errorNoBuildAvailable currentPlatform
      where errorNoBuildAvailable Linux = "Случилось абсолютно невозможное, не обнаружено версии вайпалки для единственной операционной системы!"
            errorNoBuildAvailable Windows = "Не обнаружено версии вайпалки для утятницы \"Пекач\"\nРешение:\n1. Соснуть хуйцов\n2.Сделать бочку."
            errorNoBuildAvailable Mac = "Не обнаружено версии вайпалки для мака, возможно эта ошибка появляется потому что MAKOBLYADI SOSNOOLEY\nРешение:\n1.Пососать разложившийся хуец жопса"
    show UnparseableManifest =
        "Не удалось распарсить манифест из " ++ manifestUrl

instance Exception BadEnd

data Message = ChangeMessage String
             | CrashWith SomeException
             | GoodEnd (Maybe String)
    deriving (Show)

{-# INLINE filterAndReverseSortChangelog #-}
filterAndReverseSortChangelog :: [(Version, String)] -> [(Version, String)]
filterAndReverseSortChangelog =
    reverse . sortBy (\(v1, _) (v2, _) -> compare v1 v2) .
        filter (\(v, _) -> v > Paths.version)

renderChangelog :: [(Version, String)] -> String
renderChangelog = unlines . map renderChange . filterAndReverseSortChangelog
  where renderChange (v, s) =
            "<b>" ++ showVersion v ++ ":</b> " ++ s

needUpdate :: UpdateManifest -> Bool
needUpdate um = version um > Paths.version

downloadManifest :: IO UpdateManifest
downloadManifest = do
    req <- parseUrl manifestUrl
    m <- withManager $ httpLbs req{responseTimeout = Just $ 5000000} -- 5 seconds
    maybe (throwIO UnparseableManifest) return $ decode' $ responseBody m

doesFSExist :: FilePath -> IO Bool
doesFSExist f = (||) <$> doesDirectoryExist f <*> doesFileExist f

uniqueDirectoryName :: String -> IO String
uniqueDirectoryName str = do
    ifM (doesFSExist str)
        (go str 1)
        (return str)
  where go rfn (n::Int) = do
            let fn = rfn ++ "." ++ show n
            ifM (doesFSExist fn)
                (go rfn $ n+1)
                (return fn)

updateWorker :: MVar Message -> UpdateManifest -> IO String
updateWorker mv UpdateManifest{ binaryAndResourcesZipArchives, changelog } = do
    _ <- tryPutMVar mv $ ChangeMessage $ "Ищем версию для " ++ show currentPlatform ++ "..."

    (url, md5) <- maybe (throwIO NoBuildAvailable) return $
        lookup currentPlatform binaryAndResourcesZipArchives

    putMVar mv $ ChangeMessage $ "Скачиваем архив... " ++ show (url, md5)

    lbs <- maybe (throwIO $ ChecksumMismatch url md5) return =<< downloadWithMD5 url md5

    putMVar mv $ ChangeMessage $ "Распаковываем..."

    -- FIXME dangerous, we can get mixed versions if we restore from backup when unpacking fails and we updated before.
    backupdir <- return $ ".BlastItWithPiss-update-backup"
    e <- try $ unpackBlastItWithPissUpdateZipFromLBS backupdir lbs
    case e of
        Left (x::SomeException) -> do
            void $ tryPutMVar mv $ ChangeMessage "Случился пиздец, восстанавливаем старое"
            restoreFromBackup backupdir "." -- ? implying that . is the executable path
            throwIO x
        Right _ -> return $
            case renderChangelog changelog of
              [] -> "Обновление успешно"
              ch -> ch

mainWorker :: MVar Message -> IO ()
mainWorker mv = finalizeWork $ do
    putMVar mv $ ChangeMessage "Скачиваем манифест..."
    manifest <- downloadManifest
    void $ tryPutMVar mv $ ChangeMessage "Обрабатываем манифест..."
    let update = needUpdate manifest
    if update
        then Just <$> updateWorker mv manifest
        else do
            args <- getArgs
            void $ tryPutMVar mv $ ChangeMessage "Проверяем целостность вайпалки..."
            repair <-
                if elem "--repair" args
                    then return True
                    else Repair.needRepair
            if repair
                then Just <$> updateWorker mv manifest
                else return Nothing
  where finalizeWork m = do
            e <- try m
            case e of
                Left ex -> putMVar mv $ CrashWith ex
                Right end -> putMVar mv $ GoodEnd end

setExecutable :: FilePath -> IO ()
setExecutable file = do
    p <- getPermissions file
    when (not $ executable p) $ do
        setPermissions file p{executable=True}

postInstall :: FilePath -> IO ()
postInstall executablePath = do
    let updater = executablePath </> Repair.blastItWithPissBinary
    setExecutable updater
    void $ createProcess $ proc updater ["--postinstall", showVersion Paths.version]
    mainQuit

-- HORRIBLE HACK both zip-archive and zip-conduit do not preserve file permissions
-- so instead we'll simply set executable bit for our executables based on filename.
-- Of course I could always switch to tar or LibZip, or add proper permission
-- handling to zip-archive, but I'm too lazy for that.
setExecutableBitForOurBinaries :: FilePath -> FilePath -> IO ()
setExecutableBitForOurBinaries executablePath filepath = do
    when (takeFileName filepath `elem` Repair.binaries) $ do
        setExecutable $ executablePath </> filepath
-- /HORRIBLE HACK

postInstallHooks :: FilePath -> Version -> IO ()
postInstallHooks executablePath _ = do
    files <- map (executablePath </>) . filter (\x -> x /= "." && x /= "..") <$> getDirectoryContents executablePath
    forM_ files (setExecutableBitForOurBinaries executablePath)

launchGtkblast :: FilePath -> IO ()
launchGtkblast executablePath = do
    let gtkblast = executablePath </> Repair.gtkblastBinary
    setExecutable gtkblast
    _ <- createProcess $ proc gtkblast []
    mainQuit

main :: IO ()
main = withSocketsDo $ do
    (executablePath, _) <- splitExecutablePath
    args <- getArgs
    when (any (`elem` args) ["--help", "-h", "-?"]) $ do
        putStrLn $ fromString $ helpMessage
        exitSuccess
    when (any (`elem` args) ["-V", "--version"]) $ do
        putStrLn $ fromString $ showVersion Paths.version
        exitSuccess
    case args of
        ["--postinstall", rawv] ->
            ((case fst <$> lastMay (readP_to_S parseVersion rawv) of
                Just oldv -> postInstallHooks executablePath oldv
                Nothing -> return ()
             ) `finally` launchGtkblast executablePath
            ) `finally` exitSuccess
        _ -> return ()
#ifndef BINDIST
    mainNoBindist
#else
    setCurrentDirectory executablePath

    commvar <- newEmptyMVar

    void $ forkIO $ mainWorker commvar

    void $ initGUI

    b <- builderNew
    builderAddFromString b guiXML

    updaterwindow <- builderGetObject b castToWindow "updaterwindow"
    wlabelupdatemessage <- builderGetObject b castToLabel "labelupdatemessage"
    wprogressupdate <- builderGetObject b castToProgressBar "progressupdate"

    changelogwindow <- builderGetObject b castToWindow "changelogwindow"
    wlabelchangelog <- builderGetObject b castToLabel "labelchangelog"
    wbuttonchangelog <- builderGetObject b castToButton "buttonchangelog"

    errorwindow <- builderGetObject b castToWindow "errorwindow"
    wlabelerror <- builderGetObject b castToLabel "labelerror"
    wbuttonerror <- builderGetObject b castToButton "buttonerror"

    -- FIXME (graceful exit)
    void $ onDestroy updaterwindow $ launchGtkblast executablePath

    widgetShowAll updaterwindow

    void $ timeoutAddFull (do
        progressBarPulse wprogressupdate
        e <- tryTakeMVar commvar
        flip (maybe $ return True) e $ \x -> case x of
            ChangeMessage msg -> do
                labelSetText wlabelupdatemessage msg
                return True
            CrashWith ex -> do
                widgetHide updaterwindow
                widgetShow errorwindow
                labelSetText wlabelerror $ show ex
                void $ on wbuttonerror buttonActivated $ launchGtkblast executablePath
                void $ onDestroy errorwindow $ launchGtkblast executablePath
                return False
            GoodEnd mmarkup ->
                case mmarkup of
                    Nothing -> False <$ launchGtkblast executablePath
                    Just markup -> do
                        widgetHide updaterwindow
                        widgetShow changelogwindow
                        labelSetMarkup wlabelchangelog markup
                        void $ on wbuttonchangelog buttonActivated $ postInstall executablePath
                        void $ onDestroy changelogwindow $ postInstall executablePath
                        return False
        ) priorityDefaultIdle 20

    mainGUI
#endif
