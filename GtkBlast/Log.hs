module GtkBlast.Log
    (putInvisibleLog
    ,writeLog
    ,showMessage
    ,tempError
    ,banMessage
    ,updMessage
    ,uncMessage
    ,redMessage
    ,uncAnnoyMessage
    ,appFile -- FIXME appFile shouldn't be here
    ) where
import Import
import GtkBlast.IO
import GtkBlast.Environment
import GtkBlast.GtkUtils
import Graphics.UI.Gtk

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

red :: String -> String
red s = "<span foreground=\"#ff0000\">" ++ s ++ "</span>"

rawPutStdout :: String -> IO ()
rawPutStdout s = 
    fromIOEM (return ()) $ whenM (hIsTerminalDevice stdout) $ do
        putStrLn s
        hFlush stdout

rawPutLog :: (String -> IO ()) -> FilePath -> String -> IO ()
rawPutLog err' logfile str = do {
    withFile logfile AppendMode $ \h -> do
        hSetEncoding h utf8
--      hSeek h SeekFromEnd 0
        TIO.hPutStrLn h $ T.pack str
    } `catch`
        \(a :: IOException) ->
            err' $ "Got exception while trying to write to log file \"" ++
                    logfile ++ "\": " ++ show a ++ "\nAttempted to write: " ++
                    str

rawGUILog :: TextBuffer -> String -> IO ()
rawGUILog wbuf msg = do
    e <- textBufferGetEndIter wbuf
    textBufferInsert wbuf e (msg++"\n")

writeLogIO :: TextBuffer -> String -> IO ()
writeLogIO wbuf rawmsg = do
    st <- show <$> getZonedTime
    let frmt = ("[" ++ st ++ "]:\n  " ++ rawmsg)
    rawPutLog (\er -> do rawGUILog wbuf er; rawPutStdout er) "log.txt" frmt
    rawPutStdout frmt
    rawGUILog wbuf frmt

-- | Write to logfile and stdout, but not to the GUI.
-- Use only when GUI is uninitialized.
putInvisibleLog :: String -> IO ()
putInvisibleLog msg = do
    rawPutLog rawPutStdout "log.txt" msg
    rawPutStdout msg -- duplicate log to stdout for convinience

writeLog :: String -> E ()
writeLog s = do
    buf <- asks wbuf
    io $ writeLogIO buf s

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

-- | Specialized 'fromIOEM' showing 'tempError' message on exceptions
appFile :: a -> (FilePath -> IO a) -> FilePath -> E a
appFile def' ac file =
    fromIOEM err $ io $ ac file
  where err = do
          tempError 3 $ "Невозможно прочитать файл \"" ++ file ++ "\""
          return def'
