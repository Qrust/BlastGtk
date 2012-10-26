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
    ,uncMessage
    ,redMessage
    ,uncAnnoyMessage
    ,appFile
    ) where
import Import
import GtkBlast.IO
import GtkBlast.Environment
import GtkBlast.GtkUtils
import Graphics.UI.Gtk

red :: String -> String
red s = "<span foreground=\"#ff0000\">" ++ s ++ "</span>"

rawPutStdout :: String -> IO ()
rawPutStdout s = 
    whenM (hIsTerminalDevice stdout) $ do
        putStrLn s
        hFlush stdout

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
writeLogIO wbuf raws = do
    st <- show <$> getZonedTime
    let s = ("[" ++ st ++ "]:\n  " ++ raws) 
    rawPutLog s
    e <- textBufferGetEndIter wbuf
    textBufferInsert wbuf e (s++"\n")

writeLog :: String -> E ()
writeLog s = ask >>= \w -> io $ writeLogIO (wbuf w) s

showMessage :: (Env -> CheckButton) -> String -> Maybe Int -> Bool -> String -> E ()
showMessage getCheck msgname mUnlockT mkRed msg = do
    E{wlabelmessage=wlabel, ..} <- ask
    wcheck <- asks getCheck
    io $ modifyIORef messageLocks (+1)
    n <- io getZonedTime
    writeLog $ "gtkblast, " ++ show n ++ ": " ++ msgname ++ ": " ++ msg
    if mkRed
        then io $ labelSetMarkup wlabel $ red msg
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
banMessage t s = showMessage wcheckannoy "Ban message" (Just t) True s

updMessage :: String -> E ()
updMessage s = do
    E{..} <- ask
    locks <- io $ readIORef messageLocks
    if locks==0
        then io $ labelSetText wlabelmessage s
        else when (locks < 0) $ do
                io $ writeIORef messageLocks 0
                tempError 5 "Ой-ой, случилось невозможное, messageLocks < 0, срочно доложите об этом автору"

uncMessage :: String -> E ()
uncMessage s = do
    E{..} <- ask
    writeLog $ "gtkblast, Unconditinal message: " ++ s
    io $ labelSetText wlabelmessage s

redMessage :: String -> E ()
redMessage s = do
    E{..} <- ask
    writeLog $ "gtkblast, Red message: " ++ s
    io $ labelSetMarkup wlabelmessage $ red s

uncAnnoyMessage :: String -> E ()
uncAnnoyMessage s = do
    E{..} <- ask
    writeLog $ "gtkblast, Unconditional annoying message: " ++ s
    io $ labelSetMarkup wlabelmessage $ red s
    io $ whenM ((||) <$> toggleButtonGetActive wcheckannoy <*> toggleButtonGetActive wcheckannoyerrors) $
        windowPopup window

appFile :: a -> (FilePath -> IO a) -> FilePath -> E a
appFile d m f = fromIOEM (do tempError 3 $ "Невозможно прочитать файл \"" ++ f ++ "\""
                             return d) $ io $ m f
