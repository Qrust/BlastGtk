module GtkBlast.Log
    (
    -- * 'IO'
      putInvisibleLog

    -- * 'E'
    , writeLog

    , showMessage
    , clearMessage
    , tempError
    , banMessage
    , uncMessage
    , redMessage

    -- * this shouldn't be here
    , appFile
    ) where
import Import

import GtkBlast.Environment
import GtkBlast.GtkUtils
import GtkBlast.MuVar
import Graphics.UI.Gtk hiding (get, set, labelSetMarkup, labelSetText)
import qualified Graphics.UI.Gtk as Gtk

import qualified Data.ByteString.Char8 as B8
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
        B8.hPutStrLn h $ encodeUtf8 str
    } `catch`
        \(a :: IOException) ->
            err' $ "Got exception while trying to write to log file \"" ++
                    fromString logfile ++ "\": " ++ show a ++
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
    E{ wspinmaxlines
     , wbuf } <- ask
    maxLines <- round <$> get wspinmaxlines
    io $ writeLogIO wbuf maxLines s

showMessage :: (Env -> CheckButton) -> Text -> Maybe Int -> Bool -> Text -> E ()
showMessage getCheck msgname mUnlockT mkRed msg = do
    E{ wlabelmessage
     , messageLocks
     , window
     } <- ask
    wcheck <- asks getCheck
    io $ modifyIORef messageLocks $
        \x -> if x < 0
            then 0 + 1
            else x + 1
    n <- io getZonedTime
    writeLog $ "gtkblast, " ++ show n ++ ": " ++ msgname ++ ": " ++ msg
    if mkRed
        then labelSetMarkup wlabelmessage (red msg)
        else labelSetText wlabelmessage msg
    io $ whenM (toggleButtonGetActive wcheck) $ windowPopup window
    case mUnlockT of
        Just t -> void $ io $ timeoutAdd
                (False <$ modifyIORef messageLocks (subtract 1))
                (t * 1000)
        Nothing -> return ()

clearMessage :: E ()
clearMessage = do
    E{..} <- ask
    locks <- io $ readIORef messageLocks
    when (locks == 0) $
        labelSetText wlabelmessage ""

tempError :: Int -> Text -> E ()
tempError t s = do
    showMessage wcheckannoyerrors "Displayed error message" (Just t) True s

banMessage :: Int -> Text -> E ()
banMessage t s = showMessage wcheckannoy "Ban message" (Just t) True s

uncMessage :: Text -> E ()
uncMessage s = do
    E{ wlabelmessage } <- ask
    writeLog $ "gtkblast, Unconditinal message: " ++ s
    labelSetText wlabelmessage s

redMessage :: Text -> E ()
redMessage s = do
    E{ wlabelmessage } <- ask
    writeLog $ "gtkblast, Red message: " ++ s
    labelSetMarkup wlabelmessage $ red s

-- | Specialized 'fromIOException' showing 'tempError' message on exceptions
appFile :: a -> (FilePath -> IO a) -> FilePath -> E a
appFile def' ac file =
    fromIOException err $ io $ ac file
  where err = do
          tempError 3 $ "Невозможно прочитать файл \"" ++ fromString file ++ "\""
          return def'
