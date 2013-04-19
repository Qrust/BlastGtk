module GtkBlast.GtkUtils
    (windowPopup
    ,windowToggle
    ,onFileChooserEntryButton
    ,tvarCheck
    ,tvarSpinCheck

    ,editorWidget, Apply(..), Ok(..), Cancel(..), Spawn(..)

    ,postAsyncWhenPathModified, CloseWatcher, closeWatcher
    ) where
import Import hiding (on)
import Graphics.UI.Gtk
import Control.Concurrent.STM hiding (check)

import System.Directory
import qualified Filesystem.Path.CurrentOS as F
import qualified System.FSNotify as Notify

newtype Apply = Apply Button
newtype Ok = Ok Button
newtype Cancel = Cancel Button
newtype Spawn = Spawn Button

editorWidget :: IO Text -> (Text -> IO ()) -> Window -> TextView -> Apply -> Ok -> Cancel -> Spawn -> IO ()
editorWidget getInit commit window textView
    (Apply buttonApply) (Ok buttonOk) (Cancel buttonCancel) (Spawn buttonSpawn) = do

    void $ onDelete window $ const $
        True <$ widgetHide window

    void $ on buttonSpawn buttonActivated $ do
        visible <- get window widgetVisible
        if visible
          then
            widgetHide window
          else do
            initialText <- getInit
            buf <- textViewGetBuffer textView
            textBufferSetByteString buf $ encodeUtf8 initialText
            widgetShowAll window

    void $ on buttonCancel buttonActivated $ do
        widgetHide window

    void $ on buttonOk buttonActivated $ do
        buttonClicked buttonApply
        widgetHide window

    void $ on buttonApply buttonActivated $ do
        buf <- textViewGetBuffer textView
        start <- textBufferGetStartIter buf
        end <- textBufferGetEndIter buf
        text <- decodeUtf8 <$> textBufferGetByteString buf start end True
        commit text

windowPopup :: Window -> IO ()
windowPopup window = do
    whenM (not <$> get window widgetVisible) $ do
        windowPresent window

windowToggle :: Window -> IO ()
windowToggle window =
    ifM (get window widgetVisible)
        (widgetHide window)
        (windowPopup window)

-- if only FileChooserButton worked...
onFileChooserEntryButton :: Bool -> Button -> Entry -> (Text -> IO ()) -> IO () -> IO ()
onFileChooserEntryButton b wfbutton wfentry putErr fin = void $ do
    if b
      then
        aux FileChooserActionSelectFolder $
            \fc -> fileChooserSetCurrentFolder fc =<< entryGetText wfentry
      else
        aux FileChooserActionOpen $
            \fc -> fileChooserSetCurrentFolder fc "."
  where
    aux m fd = do
        void $ onEntryActivate wfentry $ do
            buttonClicked wfbutton

        void $ on wfbutton buttonActivated $ do
            d <- fileChooserDialogNew Nothing Nothing m
                [("gtk-cancel", ResponseCancel)
                ,("gtk-open", ResponseAccept)]
            void $ fd d
            widgetShow d
            r <- dialogRun d
            case r of
              ResponseAccept -> do
                mfname <-  fileChooserGetFilename d
                case mfname of
                  Nothing ->
                    putErr "Impossible happened: ResponseAccept with Nothing."
                  Just f -> do
                    entrySetText wfentry f
                    fin
              _ -> return ()
            widgetHide d

tvarCheck :: (CheckButton -> IO a) -> CheckButton -> IO (TVar a)
tvarCheck getcheck check = do
    tvar <- atomically . newTVar =<< getcheck check

    void $ on check toggled $
        atomically . writeTVar tvar =<< getcheck check

    return tvar

tvarSpinCheck :: (SpinButton -> IO a) -> CheckButton -> SpinButton -> IO (TVar (Maybe a))
tvarSpinCheck getspin check spin = do
    tvar <- atomically . newTVar =<<
        ifM (toggleButtonGetActive check)
            (Just <$> getspin spin)
            (return Nothing)

    void $ on check buttonActivated $ do
        ifM (toggleButtonGetActive check)
            (atomically . writeTVar tvar . Just =<< getspin spin)
            (atomically $ writeTVar tvar Nothing)

    void $ onValueSpinned spin $ do
        whenM (toggleButtonGetActive check) $
            atomically . writeTVar tvar . Just =<< getspin spin

    return tvar

newtype CloseWatcher = CloseWatcher (IO ())

closeWatcher :: MonadIO m => CloseWatcher -> m ()
closeWatcher (CloseWatcher m) = liftIO m

postAsyncWhenPathModified :: MonadIO m => String -> IO () -> m CloseWatcher
postAsyncWhenPathModified _fileOrDir action = liftIO $ do
    fromIOException (return $ CloseWatcher $ return ()) $
        canonicalizePath _fileOrDir >>= \fileOrDir -> do
            watchman <- Notify.startManager

            let post = const $ do
                    Notify.stopManager watchman
                    postGUIAsync action

            isDir <- doesDirectoryExist fileOrDir
            if isDir
            then do
                let dname = F.fromText $ fromString fileOrDir
                Notify.watchDir watchman dname (const True) post
                return $ CloseWatcher $ Notify.stopManager watchman
            else do
                isFile <- doesFileExist fileOrDir
                if isFile
                then do
                    let fname = F.fromText $ fromString fileOrDir
                        dname = F.directory fname
                    Notify.watchDir watchman dname (\e -> getEventFname e == fname) post
                    return $ CloseWatcher $ Notify.stopManager watchman
                else do
                    return $ CloseWatcher $ return () -- silently fail
  where
    getEventFname (Notify.Added    f _) = f
    getEventFname (Notify.Modified f _) = f
    getEventFname (Notify.Removed  f _) = f
