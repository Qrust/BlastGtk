module GtkBlast.Log
    (rawPutStdout
    ,withOpenLog
    ,rawPutLog
    ,writeLogIO
    ,writeLog
    ,showMessage
    ,tempError
    ,banMessage
    ,updMessage
    ,appFile
    ,updWipeMessage
    ) where
import Import
import GtkBlast.IO
import GtkBlast.Achievement
import GtkBlast.Environment
import GtkBlast.GtkUtils
import Graphics.UI.Gtk

rawPutStdout :: String -> IO ()
rawPutStdout s = 
#ifdef mingw32_HOST_OS
    return () -- don't output anything on windows because of some errors.(There shouldn't be any output anyway because of the check below, but users are not very responsive and not having windows i can't be sure)
#else
    whenM (hIsWritable stdout) $ do
        putStrLn s
        hFlush stdout
#endif

withOpenLog :: (String -> IO ()) -> FilePath -> String -> IO ()
withOpenLog put logfile str = do
    catch (withFile logfile AppendMode $ \h -> do
            hSetEncoding h utf8
--          hSeek h SeekFromEnd 0
            hPutStrLn h str
          )
    $ \(a::SomeException) -> do
            put $ "Got exception while trying to write to log file \"" ++ logfile ++ "\": " ++ show a ++ "\nAttempted to write: " ++ str

rawPutLog :: String -> IO ()
rawPutLog s = do
    withOpenLog rawPutStdout "log.txt" s
    rawPutStdout s -- duplicate log to stdout for convinience

writeLogIO :: TextBuffer -> String -> IO ()
writeLogIO wbuf s = do
    rawPutLog s
    e <- textBufferGetEndIter wbuf
    textBufferInsert wbuf e (s++"\n")

writeLog :: String -> E ()
writeLog s = ask >>= \w -> io $ writeLogIO (wbuf w) s

showMessage :: (Env -> CheckButton) -> String -> Maybe Int -> Bool -> String -> E ()
showMessage getCheck msgname mUnlockT red msg = do
    E{wlabelmessage=wlabel, ..} <- ask
    wcheck <- asks getCheck
    io $ modifyIORef messageLocks (+1)
    n <- io getPOSIXTime
    writeLog $ "blasgtk, " ++ show n ++ ": " ++ msgname ++ ": " ++ msg
    if red
        then io $ labelSetMarkup wlabel $ "<span foreground=\"#ff0000\">" ++ msg ++ "</span>"
        else io $ labelSetText wlabel msg
    io $ whenM (toggleButtonGetActive wcheck) $ windowPopup window
    case mUnlockT of
        Just t -> 
            void $ io $ timeoutAdd (modifyIORef messageLocks (subtract 1) >> return False) (t * 1000)
        Nothing -> return ()
    
tempError :: Int -> String -> E ()
tempError t s = do
    showMessage wcheckannoyerrors "Displayed error message" (Just t) True s

banMessage :: Int -> String -> E ()
banMessage t s =
    showMessage wcheckannoy "Ban message" (Just t) True s

updMessage :: String -> E ()
updMessage s = do
    E{..} <- ask
    locks <- io $ readIORef messageLocks
    if locks==0
        then io $ labelSetText wlabelmessage s
        else when (locks < 0) $ do
                io $ writeIORef messageLocks 0
                tempError 5 "Ой-ой, случилось невозможное, messageLocks < 0, срочно доложите об этом автору"

appFile :: a -> (FilePath -> IO a) -> FilePath -> E a
appFile d m f = fromIOEM (do tempError 3 $ "Невозможно прочитать файл \"" ++ f ++ "\""
                             return d) $ io $ m f

updWipeMessage :: E ()
updWipeMessage = do
    E{..} <- ask
    pc <- io $ readIORef postCount
    let psc = "Сделано постов: " ++ show pc
    bnd <- do ac <- io $ readIORef activeCount
              bn <- io $ readIORef bannedCount
              return $ "\nЗабанен на досках: " ++ show bn ++ "/" ++ show ac
    let ach = getAchievementString pc
    updMessage $ psc ++ bnd ++ ach
