{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where
import Import hiding (on)
import Graphics.UI.Gtk
#ifdef BINDIST
import System.Environment.Executable (splitExecutablePath)
import System.Directory (setCurrentDirectory)
import System.IO (openFile, stdout)
import GHC.IO.Handle (hDuplicateTo)
#endif

main :: IO ()
main = do
#ifdef BINDIST
    -- change workdir
    (path, _) <- splitExecutablePath
    setCurrentDirectory path
    -- redirect stdout
    real_out <- openFile "stdout.txt" AppendMode
    hDuplicateTo real_out stdout
#endif
    putStrLn =<< ("Starting blastgtk. Current time is " ++) . show <$> getCurrentTime
    initGUI
    b <- builderNew
    builderAddFromFile b "resources/blast.glade"

    window <- builderGetObject b castToWindow "window1"
    checktray <- builderGetObject b castToCheckButton "check-tray"
    f <- builderGetObject b castToFileChooserButton "filechooserbutton1"
    wipebutton <- builderGetObject b castToButton "wipebutton"

    fileChooserSetFilename f "images"

    s <- statusIconNewFromFile "resources/2ch.so.png"
    statusIconSetTooltip s "Вайпалка мочана"
    statusIconSetName s "blast-it-with-piss"

    mshow <- checkMenuItemNewWithMnemonic "_Показать вайпалку"
    mexit <- imageMenuItemNewFromStock stockQuit
    m <- menuNew
    menuShellAppend m mshow
    menuShellAppend m mexit
    widgetShowAll m

    let toggleWindow = do
        v <- get window widgetVisible
        if v
            then widgetHide window
            else do widgetShow window
                    windowDeiconify window

    _ <- on s statusIconActivate toggleWindow
    _ <- on s statusIconPopupMenu $ \(Just mb) t -> menuPopup m $ Just (mb, t)
    mshowConnId <- on mshow menuItemActivate toggleWindow
    _ <- on mexit menuItemActivate $ mainQuit

    let setCheckActive ca = do
        signalBlock mshowConnId
        checkMenuItemSetActive mshow ca
        signalUnblock mshowConnId

    onDelete window $ \_ -> do noTray <- not <$> statusIconIsEmbedded s
                               closePlease <- not <$> toggleButtonGetActive checktray
                               if noTray || closePlease
                                   then return False
                                   else True <$ widgetHide window
    onShow window $ setCheckActive True
    onHide window $ setCheckActive False
    onDestroy window mainQuit
    
    widgetShowAll window
    mainGUI
