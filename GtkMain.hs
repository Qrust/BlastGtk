{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where
import Import hiding (on)
import Graphics.UI.Gtk
import System.Directory
import System.IO
import Network (withSocketsDo)
import Control.Exception
import GHC.IO.Handle (hDuplicateTo, hDuplicate)
import Data.IORef

import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.Choice
import "blast-it-with-piss" BlastItWithPiss.MonadChoice

#ifdef BINDIST
import System.Environment.Executable (splitExecutablePath)
#endif

mochanNames :: [String]
mochanNames =
    ["мочан"
    ,"сосач"
    ,"ссач"
    ,"педальчан"
    ,"уринач"
    ,"мочеиспускач"
    ,"абучан"
    ,"мочепарашу"
    ,"мочепарашу 2ch.so"
    ,"педальный обоссач"
    ,"педальный уринач"
    ,"педальный абучан"
    ,"педальную мочепарашу"
    ,"педальную мочепарашу 2ch.so"
    ,"уринальный мочеиспускач"
    ,"уринальный абучан"
    ,"уринальную мочепарашу"
    ,"уринальную мочепарашу 2ch.so"
    ,"трипфажный обоссач"
    ,"трипфажный мочан"
    ,"трипфажный мочеиспускач"
    ,"трипфажный абучан"
    ,"трипфажную мочепарашу"
    ,"трипфажную мочепарашу 2ch.so"
    ]

main :: IO ()
main = withSocketsDo $ do
#ifdef BINDIST
    -- change workdir
    (path, _) <- splitExecutablePath
    setCurrentDirectory path
#endif
    -- redirect stdout
    -- FIXME voobshe-to eto nenuzhno, no poka pust' pobudet kostyl'
    putStrLn "If program doesn't start, look for error messages in stdout.txt"
    new_out <- do
        h <- openFile "stdout.txt" ReadWriteMode
        size <- hFileSize h
        if size >= (10 * 1024 * 1024)
            then do hClose h
                    renameFile "stdout.txt" "stdout.txt.bak"
                    openFile "stdout.txt" ReadWriteMode
            else return h
    hSetEncoding new_out utf8
    hSeek new_out SeekFromEnd 0

    hDuplicateTo new_out stdout
    hDuplicateTo new_out stderr

    let getOut = do
        hFlush stdout
        hFlush stderr
        hSeek new_out AbsoluteSeek 0
        c <- unlines . reverse <$> (fix $ \this a -> do
                x <- try $ hGetLine new_out
                case x of
                    Left (_::IOError) -> return a
                    Right s -> this (s:a)) []
        hSeek new_out SeekFromEnd 0
        return c

    -- init

    putStrLn =<< ("Starting blastgtk. Current UTC time is " ++) . show <$> getCurrentTime
    putStrLn =<< ("Current POSIX time is " ++) . show <$> getPOSIXTime
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
#ifdef BINDIST
    toggleButtonSetActive wchecktray True
#endif
    wfilechooser <- builderGetObject b castToFileChooserButton "filechooserbutton1"
    fileChooserSetFilename wfilechooser "images"
    wlog <- builderGetObject b castToTextView "log"
    wbuf <- textViewGetBuffer wlog
    wad <- textViewGetVadjustment wlog
    adjustmentSetValue wad =<< adjustmentGetUpper wad

    -- main loop

    --TODO reject empty captcha input

    wipeStarted <- newIORef False
    previousUpper <- newIORef =<< adjustmentGetUpper wad
    previousStdout <- newIORef =<< getOut

    join $ liftM2 (textBufferInsert wbuf)
                  (textBufferGetEndIter wbuf)
                  (readIORef previousStdout)

    let loop = do
        whenM (readIORef wipeStarted) $
            progressBarPulse wprogresswipe

        end <- textBufferGetEndIter wbuf
        pout <- readIORef previousStdout
        out <- try getOut
        case out of
            Left (x::SomeException) -> print x
            -- FIXME FIXME FIXME FIXME FIXME FIXME
            Right x | Just n <- stripPrefix pout x ->
                        unless (null n) $ textBufferInsert wbuf end n
                    | otherwise -> do
                        unless (null x) $ textBufferSetText wbuf x

    timeoutAdd (loop >> return True) 50 --kiloseconds

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

    on wbuttonwipe buttonActivated $ do
        ifM (readIORef wipeStarted)
            (do buttonSetLabel wbuttonwipe "Начать _Вайп"
                progressBarSetText wprogresswipe "Вайп ещё не начат"
                progressBarSetFraction wprogresswipe 0)
            (do buttonSetLabel wbuttonwipe "Прекратить _Вайп"
                progressBarSetText wprogresswipe =<< ("Вайпаем " ++) <$> chooseFromList mochanNames
                progressBarPulse wprogresswipe)
        modifyIORef wipeStarted not

    onAdjChanged wad $ do
        v <- adjustmentGetValue wad
        p <- adjustmentGetPageSize wad
        pu <- subtract p <$> readIORef previousUpper
        when (v >= pu) $ do
            u <- adjustmentGetUpper wad
            adjustmentSetValue wad $ subtract p u
            writeIORef previousUpper u

    -- start main gui

    widgetShowAll window
    mainGUI
