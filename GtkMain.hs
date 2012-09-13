{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where
import Import hiding (on)
import Graphics.UI.Gtk
import System.IO
import Network (withSocketsDo)

#ifdef BINDIST
import System.Environment.Executable (splitExecutablePath)
import System.Directory (setCurrentDirectory)
import GHC.IO.Handle (hDuplicateTo)
#endif

main :: IO ()
main = withSocketsDo $ do
#ifdef BINDIST
    -- change workdir
    (path, _) <- splitExecutablePath
    setCurrentDirectory path
    -- redirect stdout
    new_out <- openFile "stdout.txt" AppendMode
    hDuplicateTo new_out stdout
    hDuplicateTo new_out stderr
#endif
    putStrLn =<< ("Starting blastgtk. Current time is " ++) . show <$> getCurrentTime
    initGUI
    b <- builderNew
    builderAddFromFile b "resources/blast.glade"

    window <- builderGetObject b castToWindow "window1"

    -- setup tray

    wtray <- statusIconNewFromFile "resources/2ch.so.png"
    statusIconSetTooltip wtray "Вайпалка мочана"
    statusIconSetName wtray "blast-it-with-piss"

    wmenushow <- checkMenuItemNewWithMnemonic "_Показать вайпалку"
    wmenuexit <- imageMenuItemNewFromStock stockQuit
    wmenu <- menuNew
    menuShellAppend wmenu wmenushow
    menuShellAppend wmenu wmenuexit
    widgetShowAll wmenu

    let toggleWindow = do
        v <- get window widgetVisible
        if v
            then widgetHide window
            else do widgetShow window
                    windowDeiconify window

    -- tray signals

    on wtray statusIconActivate toggleWindow
    on wtray statusIconPopupMenu $ \(Just mb) t -> menuPopup wmenu $ Just (mb, t)
    wmenushowConnId <- on wmenushow menuItemActivate toggleWindow
    on wmenuexit menuItemActivate $ mainQuit

    -- setup main window

    wprogresswipe <- builderGetObject b castToProgressBar "wipeprogress"
    wbuttonwipe <- builderGetObject b castToButton "wipebutton"
    wradiokopipasta <- builderGetObject b castToRadioButton "radio-kopipasta"
    wradiokakashki <- builderGetObject b castToRadioButton "radio-kakashki"
    wradionum <- builderGetObject b castToRadioButton "radio-num"
    wradiochar <- builderGetObject b castToRadioButton "radio-char"
    wradionotext <- builderGetObject b castToRadioButton "radio-notext"
    wcheckthread <- builderGetObject b castToCheckButton "check-thread"
    wcheckimages <- builderGetObject b castToCheckButton "check-images"
    wcheckwatermark <- builderGetObject b castToCheckButton "check-watermark"
    wcheckcheckannoy <- builderGetObject b castToCheckButton "check-annoy"
    wcheckcheckannoyerrors <- builderGetObject b castToCheckButton "check-annoyerrors"
    wchecktray <- builderGetObject b castToCheckButton "check-tray"
    wcheckthreadonly <- builderGetObject b castToCheckButton "check-threadonly"
#ifdef BINDIST
    toggleButtonSetActive wchecktray True
#endif
    wfilechooser <- builderGetObject b castToFileChooserButton "filechooserbutton1"
    fileChooserSetFilename wfilechooser "images"
    wmode <- builderGetObject b castToComboBox "mode"
    wspinthread <- builderGetObject b castToSpinButton "spin-thread"
    wlog <- builderGetObject b castToTextView "log"

    -- main window signals

    let setCheckActive ca = do
        signalBlock wmenushowConnId
        checkMenuItemSetActive wmenushow ca
        signalUnblock wmenushowConnId

    onDelete window $ \_ -> do noTray <- not <$> statusIconIsEmbedded wtray
                               closePlease <- not <$> toggleButtonGetActive wchecktray
                               if noTray || closePlease
                                   then return False
                                   else True <$ widgetHide window
    onShow window $ setCheckActive True
    onHide window $ setCheckActive False
    onDestroy window mainQuit

    -- main loop

    let loop = do
        progressBarPulse wprogresswipe

    timeoutAdd (loop >> return True) 50 --kiloseconds

    widgetShowAll window
    mainGUI
