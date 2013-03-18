module GtkBlast.GtkUtils
    (windowPopup
    ,windowToggle
    ,onFileChooserEntryButton
    ,tvarCheck
    ,tvarSpinCheck
    ) where
import Import hiding (on)
import Graphics.UI.Gtk
import Control.Concurrent.STM hiding (check)

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
onFileChooserEntryButton :: Bool -> Button -> Entry -> (String -> IO ()) -> IO () -> IO ()
onFileChooserEntryButton b wfbutton wfentry put fin = void $ do
    if b
        then aux FileChooserActionSelectFolder (\d -> fileChooserSetCurrentFolder d =<< entryGetText wfentry)
        else aux FileChooserActionOpen (flip fileChooserSetCurrentFolder ".")
  where aux m fd = do
            void $ onEntryActivate wfentry $ do
                buttonClicked wfbutton
            void $ on wfbutton buttonActivated $ do
                d <- fileChooserDialogNew Nothing Nothing m
                        [("gtk-cancel", ResponseCancel)
                        ,("gtk-open", ResponseAccept)
                        ]
                void $ fd d
                widgetShow d
                r <- dialogRun d
                case r of
                    ResponseAccept -> do
                        fileChooserGetFilename d >>=
                            maybe (put "Impossible happened: ResponseAccept with Nothing.")
                                  (\f -> entrySetText wfentry f >> fin)
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
