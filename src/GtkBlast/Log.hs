module GtkBlast.Log
    (-- * 'IO'
     putInvisibleLog
    -- * 'E'
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

import GtkBlast.Environment
import GtkBlast.GtkUtils
import GtkBlast.MuVar
import Graphics.UI.Gtk hiding (get, set, labelSetMarkup, labelSetText)
import qualified Graphics.UI.Gtk as Gtk

import qualified Data.ByteString as B
import qualified Data.Text as T

import qualified Data.Text.IO.Locale as LTIO

{-# INLINE labelSetMarkup #-}
labelSetMarkup :: MonadIO m => Label -> Text -> m ()
labelSetMarkup l = liftIO . Gtk.labelSetMarkup l . T.unpack

{-# INLINE labelSetText #-}
labelSetText :: MonadIO m => Label -> Text -> m ()
labelSetText l = liftIO . Gtk.labelSetText l . T.unpack

red :: Text -> Text
red s = "<span foreground=\"#ff0000\">" ++ s ++ "</span>"

rawPutStdout :: Text -> IO ()
rawPutStdout s =
    fromIOException (return ()) $
        whenM (hIsTerminalDevice stdout) $ do
            LTIO.putStrLn s
            hFlush stdout

rawPutLog :: (Text -> IO ()) -> FilePath -> Text -> IO ()
rawPutLog err' logfile str = do {
    withBinaryFile logfile AppendMode $ \h -> do
        B.hPutStrLn h $ encodeUtf8 str
    } `catch`
        \(a :: IOException) ->
            err' $ "Got exception while trying to write to log file \"" ++
                    T.pack logfile ++ "\": " ++ show a ++
                    "\nAttempted to write: " ++ str

rawGUILog :: TextBuffer -> Int -> Text -> IO ()
rawGUILog wbuf maxLines msg = do
    e <- textBufferGetEndIter wbuf
    textBufferInsertByteString wbuf e $ encodeUtf8 msg
    textBufferInsertByteString wbuf e "\n"
    l <- textBufferGetLineCount wbuf
    when (l > maxLines) $ do
        s <- textBufferGetStartIter wbuf
        il <- textBufferGetIterAtLine wbuf (l - maxLines)
        textBufferDelete wbuf s il

writeLogIO :: TextBuffer -> Int -> Text -> IO ()
writeLogIO wbuf maxLines rawmsg = do
    st <- getZonedTime
    let !frmt = ("[" ++ show st ++ "]:\n  " ++ rawmsg)
    rawPutLog (toGUIAndStdout wbuf maxLines) "log.txt" frmt
    rawPutStdout frmt
    rawGUILog wbuf maxLines frmt
  where
    toGUIAndStdout buf lin er = do
        rawGUILog buf lin er
        rawPutStdout er

-- | Write to logfile and stdout, but not to the GUI.
-- Use only when GUI is uninitialized.
putInvisibleLog :: Text -> IO ()
putInvisibleLog msg = do
    rawPutLog rawPutStdout "log.txt" msg
    rawPutStdout msg -- duplicate log to stdout for convinience

writeLog :: Text -> E ()
writeLog s = do
    E{..} <- ask
    maxLines <- round <$> get wspinmaxlines
    io $ writeLogIO wbuf maxLines s

showMessage :: (Env -> CheckButton) -> Text -> Maybe Int -> Bool -> Text -> E ()
showMessage getCheck msgname mUnlockT mkRed msg = do
    E{wlabelmessage=wlabel, ..} <- ask
    wcheck <- asks getCheck
    io $ modifyIORef messageLocks (+1)
    n <- io getZonedTime
    writeLog $ "gtkblast, " ++ show n ++ ": " ++ msgname ++ ": " ++ msg
    if mkRed
        then labelSetMarkup wlabel $ red msg
        else labelSetText wlabel msg
    io $ whenM (toggleButtonGetActive wcheck) $ windowPopup window
    case mUnlockT of
        Just t ->
            void $ io $ timeoutAdd (modifyIORef messageLocks (subtract 1) >> return False) (t * 1000)
        Nothing -> return ()

tempError :: Int -> Text -> E ()
tempError t s = do
    showMessage wcheckannoyerrors "Displayed error message" (Just t) True s

banMessage :: Int -> Text -> E ()
banMessage t s = showMessage wcheckannoy "Ban message" (Just t) True s

updMessage :: Text -> E ()
updMessage s = do
    E{..} <- ask
    locks <- io $ readIORef messageLocks
    if locks==0
        then labelSetText wlabelmessage s
        else when (locks < 0) $ do
                io $ writeIORef messageLocks 0
                tempError 5 "Ой-ой, случилось невозможное, messageLocks < 0, срочно доложите об этом автору"

uncMessage :: Text -> E ()
uncMessage s = do
    E{..} <- ask
    writeLog $ "gtkblast, Unconditinal message: " ++ s
    labelSetText wlabelmessage s

redMessage :: Text -> E ()
redMessage s = do
    E{..} <- ask
    writeLog $ "gtkblast, Red message: " ++ s
    labelSetMarkup wlabelmessage $ red s

uncAnnoyMessage :: Text -> E ()
uncAnnoyMessage s = do
    E{..} <- ask
    writeLog $ "gtkblast, Unconditional annoying message: " ++ s
    labelSetMarkup wlabelmessage $ red s
    io $ whenM ((||) <$> toggleButtonGetActive wcheckannoy <*> toggleButtonGetActive wcheckannoyerrors) $
        windowPopup window

-- | Specialized 'fromIOException' showing 'tempError' message on exceptions
appFile :: a -> (FilePath -> IO a) -> FilePath -> E a
appFile def' ac file =
    fromIOException err $ io $ ac file
  where err = do
          tempError 3 $ "Невозможно прочитать файл \"" ++ T.pack file ++ "\""
          return def'
