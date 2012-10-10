module GtkBlast.GtkUtils
    (windowPopup
    ,windowToggle
    ,onFileChooserEntryButton
    ) where
import Import hiding (on)
import Graphics.UI.Gtk

windowPopup :: Window -> IO ()
windowPopup window = do
    widgetShow window
    windowDeiconify window

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
