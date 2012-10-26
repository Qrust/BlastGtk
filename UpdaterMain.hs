module Main (main) where
import Import hiding (on)
import Updater.Manifest
import Updater.UnpackZip
import Updater.Repair
import Updater.DownloadWithMD5
import Updater.GuiXML
import Data.Version
import System.Directory
import System.FilePath
import System.Environment
import System.Environment.Executable
import System.Exit
import System.Process
import Control.Concurrent
import Network
import qualified Paths_blast_it_with_piss as Paths
import Data.Aeson
import Network.HTTP.Conduit
import Graphics.UI.Gtk
import qualified Text.Show
import Text.ParserCombinators.ReadP

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

{-
data UpdaterConf = UpdaterConf
        {updateWithoutAsking :: Bool
        ,updateQuietly :: Bool
        ,manifestUrl :: String
        }
-}

{-
- скачиваем манифест с файла, в манифесте версия, список архивов и чексумм
- скачиваем архивы, сверяем чексуммы, если что-то не совпадает выдаем диалог типа "Retry-Abort-Cancel"
- создаем папку blast.old.$СТАРАЯ-ВЕРСИЯ, если уже есть то добавляем цифры
    -- переименовываем файлы и директории для которых есть версия из архива в бэкап-папку.
    -- ^ как часть распаковки уже
- распаковываем.
-}

mainNoBindist :: IO ()
mainNoBindist = do
    (path, _) <- splitExecutablePath
    let gtkblast = path </> gtkblastBinary
    void $ ifM
        (doesFileExist gtkblast)
        (createProcess $ proc gtkblast [])
        (createProcess $ proc gtkblastBinary [])

data BadEnd = ChecksumMismatch URL MD5Sum
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
            errorNoBuildAvailable Mac = "Не обнаружено версии вайпалки для мака, возможно эта ошибка появляется потому что MAKOBLYADI SOSNOOLEY\nРешение:\n1.Пососать разложившийся хуец жопса\nАльтернативное решение:\n1. Связаться с автором(контакты в .cabal файле или через тред)\n2. Скомпилять версию для мака\n3. Пососать разложившийся хуец жопса."
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
    let req = fromJust $ parseUrl manifestUrl
    m <- withManager $ httpLbs req{responseTimeout = Just $ 1500000} -- 1.5 seconds
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

updateWorker :: MVar Message -> UpdateManifest -> IO (Maybe String)
updateWorker mv UpdateManifest{..} = do
    void $ tryPutMVar mv $ ChangeMessage $ "Ищем версию для " ++ show currentPlatform ++ "..."
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
            void $ tryPutMVar mv $ ChangeMessage $ "Случился пиздец, восстанавливаем"
            restoreFromBackup backupdir "." -- ? implying that . is the executable path
            throwIO x
        Right _ -> do
            return $ justIf (not . null) $ renderChangelog changelog

mainWorker :: MVar Message -> IO ()
mainWorker mv = finalizeWork $ do
    putMVar mv $ ChangeMessage "Скачиваем манифест..."
    manifest <- downloadManifest
    void $ tryPutMVar mv $ ChangeMessage "Обрабатываем манифест..."
    let update = needUpdate manifest
    if update
        then updateWorker mv manifest
        else do
            args <- getArgs
            void $ tryPutMVar mv $ ChangeMessage "Проверяем целостность вайпалки..."
            repair <-
                if elem "--repair" args
                    then return True
                    else needRepair
            if repair
                then updateWorker mv manifest
                else return Nothing
  where finalizeWork m = do
            e <- try m
            case e of
                Left ex -> putMVar mv $ CrashWith ex
                Right end -> putMVar mv $ GoodEnd end

helpMessage :: String
helpMessage =
    "Version: " ++ showVersion Paths.version ++ ", " ++ "Platform: " ++ show currentPlatform ++ "\n" ++
    "Autoupdater for BlastItWithPiss, checks for updates then calls gtkblast\n" ++
    "Use --repair to force update.\n" ++
    "Use --postinstall <version> to execute post install hooks"

postInstall :: FilePath -> IO ()
postInstall executablePath = do
    let updater = executablePath </> "BlastItWithPiss"
    p <- getPermissions updater
    when (not $ executable p) $ do
        setPermissions updater p{executable=True}
    void $ createProcess $ proc updater ["--postinstall", showVersion Paths.version]
    mainQuit

postInstallHooks :: FilePath -> Version -> IO ()
postInstallHooks _ _ = return ()

launchGtkblast :: FilePath -> IO ()
launchGtkblast executablePath = do
    let gtkblast = executablePath </> gtkblastBinary
    p <- getPermissions gtkblast
    when (not $ executable p) $ do
        setPermissions gtkblast p{executable=True}
    void $ createProcess $ proc gtkblast []
    mainQuit

main :: IO ()
main = withSocketsDo $ do
    (executablePath, _) <- splitExecutablePath
    args <- getArgs
    when (any (`elem` args) ["--help", "-h", "-?"]) $ do
        putStrLn helpMessage
        exitSuccess
    when (any (`elem` args) ["-V", "--version"]) $ do
        putStrLn $ showVersion Paths.version
        exitSuccess
    case args of
        ["--postinstall", rawv] -> do
            let moldv = fst <$> lastMay (readP_to_S parseVersion rawv)
            case moldv of
                Nothing -> return ()
                Just oldv -> postInstallHooks executablePath oldv
            launchGtkblast executablePath
            exitSuccess
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
                    Nothing -> False <$ postInstall executablePath
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
