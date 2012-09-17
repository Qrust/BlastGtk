{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where
import Import hiding (on)
import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.Board
import "blast-it-with-piss" BlastItWithPiss.Choice
import "blast-it-with-piss" BlastItWithPiss.MonadChoice
import Graphics.UI.Gtk
import GHC.Conc
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import System.IO
import System.Environment
import System.FilePath
import System.Directory
import Network (withSocketsDo)
import Control.Exception
#ifdef BINDIST
import System.Environment.Executable (splitExecutablePath)
#endif
#if MIN_VERSION_time(1,2,0)
import System.Time
#endif

--TODO FIX FREEZES
--TODO mochepasta resources/mocha

data PastaSet = Mocha
              | Kakashki
              | Num
              | Char
              | NoText
    deriving (Eq, Show, Ord, Read, Enum, Bounded)

data Conf = Conf {coActiveBoards :: [Board]
                 ,coPastaSet :: PastaSet
                 ,coCreateThreads :: Bool
                 ,coImageFolder :: String
                 ,coAttachImages :: Bool
                 ,coAnnoy :: Bool
                 ,coTray :: Bool
                 ,coWatermark :: Bool
                 ,coAnnoyErrors :: Bool
                 }
    deriving (Eq, Show, Read)

data WipeUnit = WipeUnit {wuThreadId :: ThreadId
                         -- TODO right now we don't support configuring per-board
                         --      wipe preferences
                         --,wuMuSettings :: MuSettings
                         }

data BoardUnit = BoardUnit {buBoard :: Board
                           ,buWidget :: CheckButton
                           -- TODO We don't support proxys yet, so boardunit
                           --      never has more than one wipeunit
                           ,buWipeUnits :: IORef [WipeUnit]
                           ,buBanned :: IORef Bool
                           }

#if MIN_VERSION_directory(1,2,0)
nullTime :: UTCTime
nullTime = UTCTime (ModifiedJulianDay 0) 0
#else
nullTime :: ClockTime
nullTime = TOD 0 0
#endif

#if MIN_VERSION_directory(1,2,0)
timeRightNow :: IO UTCTime
timeRightNow = getCurrentTime
#else
timeRightNow :: IO ClockTime
timeRightNow = getClockTime
#endif

readPasta :: FilePath -> IO [String]
readPasta f = filter (not . all isSpace) . delimitByLE "\n\n\n\n" <$> readFile f

generateRandomString :: (Int, Int) -> (Char, Char) -> IO String
generateRandomString lengthBounds charBounds = do
    len <- getRandomR lengthBounds
    take len <$> getRandomRs charBounds

generateRandomStrings :: (Int, Int) -> (Int, Int) -> (Char, Char) -> IO [String]
generateRandomStrings lengthBounds a b = do
    len <- getRandomR lengthBounds
    replicateM len $ generateRandomString a b

defaultConf :: Conf
defaultConf =
    Conf { -- FIXME coActiveBoards = [B, BB, ABU, D, VG]
          coActiveBoards = [NE, MDK]
         ,coPastaSet = Mocha
         ,coCreateThreads = True
         ,coImageFolder = "images"
         ,coAttachImages = True
         ,coAnnoy = True
#ifdef BINDIST
         ,coTray = True
#else
         ,coTray = False
#endif
         ,coWatermark = False
         ,coAnnoyErrors = True
         }

mochanNames :: [String]
mochanNames =
    ["мочан"
    ,"сосач"
    ,"ссач"
    ,"педальчан"
    ,"уринач"
    ,"мочеиспускач"
    ,"абучан"
    ,"двасо"
    ,"хачан"
    ,"мочепарашу"
    ,"мочепарашу 2ch.so"
    ,"педальный обоссач"
    ,"педальный уринач"
    ,"педальный абучан"
    ,"педальный хачан"
    ,"педальную мочепарашу"
    ,"педальную мочепарашу 2ch.so"
    ,"уринальный мочеиспускач"
    ,"уринальный абучан"
    ,"уринальный хачан"
    ,"уринальную мочепарашу"
    ,"уринальную мочепарашу 2ch.so"
    ,"трипфажный обоссач"
    ,"трипфажный мочан"
    ,"трипфажный мочеиспускач"
    ,"трипфажный абучан"
    ,"трипфажную мочепарашу"
    ,"трипфажную мочепарашу 2ch.so"
    ]

achievements :: (Int, [(Int, String)])
achievements =
    let (.) = (,) in
        (50, [10 . "Анон"
             ,20 . "Няша"
             ,50 . "Троллер"
             ,75 . "Мега-троллер"
             ,100 . "Сотня разорванных анусов"
             ,150 . "Вайпер"
             ,200 . "Братишка"
             ,300 . "Бэтмен"
             ,500 . "Ультрахардкорщик"
             ,1000 . "Тысяча порванных срак"
             ,3000 . "Поехавший"
             ,20000 . "Поле устиланное вырванными аналами неверных"
             ,50000 . "Прости Марио, но Принцесса в другом замке"
             ,100000 . "Супер-пупер-мега-гипер-охуительный пиздец"
             ,200000 . "Словил все геты разом"
             ,1000000 . "Накрутил же, бака"
             ]
        )

getAchievement :: Int -> Maybe String
getAchievement i
    | i >= fst achievements =
        findMap (\(p, a) -> if i >= p then Just a else Nothing) $ snd achievements
    | otherwise = Nothing

ignoreExceptions :: IO () -> IO ()
ignoreExceptions m = do
    void (try m :: IO (Either SomeException ()))

bugMessage :: String
bugMessage = "If you experience crashes, bugs, or any kind strange or illogical behavior,"
          ++ " file a bug report to the author(https://github.com/exbb2/BlastItWithPiss/issues)"
          ++ " with attached files blastgtk-log.txt, and, if you have one, blastgtk-log.txt.bak.\n"
          ++ "NOTE: aforementioned logs contain address of the image folder that you specified"
          ++ " if you customized the program. If this data is sensitive to you, you might want"
          ++ " to clear it from the logs before you submit them.\n"
          ++ "Thanks, and have fun. Hopefully, it has been worth the weight."

-- TODO helpMessage
helpMessage :: String
helpMessage = "No help message for now, sorry" ++ "\n\n" ++ bugMessage

main :: IO ()
main = withSocketsDo $ do
 args <- getArgs
 if any (`elem` args) ["--help", "-h", "-?"]
  then putStrLn helpMessage
  else do
#ifdef BINDIST
  -- change workdir
  (path, _) <- splitExecutablePath
  setCurrentDirectory path
#endif
  -- setup loging
#ifdef BINDIST
  putStrLn bugMessage
#endif
  hlog <- do
    eh <- try $ do
        h <- openFile "blastgtk-log.txt" ReadWriteMode
        size <- hFileSize h
        if size >= (10 * 1024 * 1024)
            then do hClose h
                    renameFile "blastgtk-log.txt" "blastgtk-log.txt.bak"
                    openFile "blastgtk-log.txt" ReadWriteMode
            else return h
    case eh of
        Left (a::SomeException) -> do
            putStrLn $ "Got exception while trying to open log file: " ++ show a
            return Nothing
        Right h -> do hSetEncoding h utf8
                      hSeek h SeekFromEnd 0
                      return $ Just h

  let rawPutLog s = do
        when (isJust hlog) $
            flip hPutStrLn s $ fromJust hlog
        putStrLn s

  let readLog =
        maybe (return "")
              (\h -> do
                hSeek h AbsoluteSeek 0
                c <- unlines . reverse <$>
                         (fix $ \this a -> do
                                x <- try $ hGetLine h
                                case x of
                                    Left (_::IOError) -> return a
                                    Right s -> this (s:a)
                         ) []
                hSeek h SeekFromEnd 0
                return c
                )
              hlog

  handle (\(a::SomeException) -> rawPutLog $ "Uncaught exception terminated program, sorry: " ++ show a) $ do
    -- read configuration
    -- TODO saving configuration

    rawPutLog =<< ("Starting blastgtk. Current POSIX time is " ++) . show <$> getPOSIXTime

    Conf{..} <- do x <- try $ readFile "config"
                   case x of
                    Left (a::SomeException) -> do rawPutLog $ "Couldn't read config, loading defaults. Exception was: " ++ show a
                                                  return defaultConf
                    Right c -> case readMay c of
                                Nothing -> do rawPutLog $ "Couldn't read config because of syntax error, overwriting with defaults. Old version saved at config.old.faulty"
                                              ignoreExceptions $ writeFile "config.old.faulty" c
                                              return defaultConf
                                Just n -> return n

    rawPutLog $ "Loaded config: " ++ show Conf{..}

    -- init

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

    let hideWindow = widgetHide window

    let popupWindow = do widgetShow window; windowDeiconify window

    let toggleWindow = do
        ifM (get window widgetVisible)
            hideWindow
            popupWindow

    -- tray signals

    on wtray statusIconActivate toggleWindow
    on wtray statusIconPopupMenu $ \(Just mb) t -> menuPopup wmenu $ Just (mb, t)
    wmenushowConnId <- on wmenushow menuItemActivate toggleWindow
    on wmenuexit menuItemActivate $ mainQuit

    -- setup widgets

    wlabelmessage <- builderGetObject b castToLabel "labelmessage"
    wprogresswipe <- builderGetObject b castToProgressBar "wipeprogress"
    wbuttonwipe <- builderGetObject b castToButton "wipebutton"

    wradiomocha <- builderGetObject b castToRadioButton "radio-mocha"
    wradiokakashki <- builderGetObject b castToRadioButton "radio-kakashki"
    wradionum <- builderGetObject b castToRadioButton "radio-num"
    wradiochar <- builderGetObject b castToRadioButton "radio-char"
    wradionotext <- builderGetObject b castToRadioButton "radio-notext"

    let pastaradio =
            [(Mocha, wradiomocha)
            ,(Kakashki, wradiokakashki)
            ,(Num, wradionum)
            ,(Char, wradiochar)
            ,(NoText, wradionotext)
            ]

    forM_ pastaradio $ \(p, w) ->
        if p == coPastaSet
            then toggleButtonSetActive w True
            else return ()

    wcheckthread <- builderGetObject b castToCheckButton "check-thread"
    toggleButtonSetActive wcheckthread coCreateThreads

    wcheckimages <- builderGetObject b castToCheckButton "check-images"
    toggleButtonSetActive wcheckimages coAttachImages

    wcheckwatermark <- builderGetObject b castToCheckButton "check-watermark"
    toggleButtonSetActive wcheckwatermark coWatermark

    wcheckannoy <- builderGetObject b castToCheckButton "check-annoy"
    toggleButtonSetActive wcheckannoy coAnnoy

    wcheckannoyerrors <- builderGetObject b castToCheckButton "check-annoyerrors"
    toggleButtonSetActive wcheckannoyerrors coAnnoyErrors

    wchecktray <- builderGetObject b castToCheckButton "check-tray"
    toggleButtonSetActive wchecktray coTray

    wentryimagefolder <- builderGetObject b castToEntry "entryimagefolder"
    entrySetText wentryimagefolder coImageFolder
    wbuttonimagefolder <- builderGetObject b castToButton "buttonimagefolder"

    wlog <- builderGetObject b castToTextView "log"
    wbuf <- textViewGetBuffer wlog
    -- VERY SLOW for some reason on texts > 100 kilobytes
    -- Not a haskell issue, print prints instantly, widget lags like hell for a minute.
    -- textBufferSetText wbuf =<< readLog
    wad <- textViewGetVadjustment wlog
    adjustmentSetValue wad =<< adjustmentGetUpper wad

    wprogressalignment <- builderGetObject b castToAlignment "progressalignment"
    wvboxcaptcha <- builderGetObject b castToVBox "vboxcaptcha"
    wentrycaptcha <- builderGetObject b castToEntry "entrycaptcha"
    wbuttoncaptchaok <- builderGetObject b castToButton "buttoncaptchaok"

    wvboxboards <- builderGetObject b castToVBox "vbox-boards"

    boardunits <- forM (fst $ unzip $ ssachBoardsSortedByPostRate) $ \board -> do
        wc <- checkButtonNewWithLabel $ renderBoard board
        when (board `elem` coActiveBoards) $ toggleButtonSetActive wc True
        boxPackStart wvboxboards wc PackNatural 0
        BoardUnit board wc <$> newIORef [] <*> newIORef False

    wbuttonselectall <- builderGetObject b castToButton "buttonselectall"
    wbuttonselectnone <- builderGetObject b castToButton "buttonselectnone"

    -- setup mutable variables

    --TODO reject empty captcha input
    --TODO ability to cancel captcha
    --TODO captcha origin label

    messageLock <- newIORef False

    let writeLog s = do
        rawPutLog s
        e <- textBufferGetEndIter wbuf
        textBufferInsert wbuf e (s++"\n")

    let tempError t s = do
        writeIORef messageLock True
        n <- getPOSIXTime
        writeLog $ "blasgtk, " ++ show n ++ ": Displayed error message: " ++ s
        labelSetMarkup wlabelmessage $ "<span foreground=\"#ff0000\">" ++ s ++ "</span>"
        void $ timeoutAdd (writeIORef messageLock False >> return False) (t * 1000)

    let updMessage s = do
        unlessM (readIORef messageLock) $
            labelSetText wlabelmessage s

    let fromIOE v = handle (\(_::IOException) -> return v)
        fromIOEM v = handle (\(_::IOException) -> v)

    let generatePasta Mocha = fromIOEM (do tempError 3 "Невозможно прочитать файл resources/mocha"
                                           return []) $ readPasta "resources/mocha"
        generatePasta Kakashki = fromIOEM (do tempError 3 "Невозможно прочитать файл resources/sadism"
                                              return []) $ readPasta "resources/sadism"
        generatePasta Char = generateRandomStrings (1, 30) (100, 5000) ('a','z')
        generatePasta Num = generateRandomStrings (1, 30) (100, 5000) ('0', '9')
        generatePasta NoText = return []
    
    let pastaDate Mocha = fromIOEM (do tempError 3 "Невозможно прочитать файл resources/mocha"
                                       return nullTime) $ getModificationTime "resources/mocha"
        pastaDate Kakashki = fromIOEM (do tempError 3 "Невозможно прочитать файл resources/mocha"
                                          return nullTime) $ getModificationTime "resources/sadism"
        pastaDate _ = timeRightNow

    let filterImages = filter ((`elem` [".jpg",".jpe",".jpeg",".gif",".png"]) . takeExtension)

    previousUpper <- newIORef =<< adjustmentGetUpper wad

    wipeStarted <- newIORef False
    pastaSet <- newIORef coPastaSet
    imageFolder <- newIORef coImageFolder

    postCount <- newIORef 0
    activeCount <- newIORef 0
    bannedCount <- newIORef 0

    pastaMod <- newIORef nullTime
    imagesLast <- newIORef []

    -- setup shared mutable state

    tqLog <- atomically $ newTQueue
    tqOut <- atomically $ newTQueue

    tpastas <- atomically $ newTVar []
    timages <- atomically $ newTVar []

    tuseimages <- atomically . newTVar =<< toggleButtonGetActive wcheckimages
    on wcheckimages toggled $
        atomically . writeTVar tuseimages =<< toggleButtonGetActive wcheckimages

    tcreatethreads <- atomically . newTVar =<< toggleButtonGetActive wcheckthread
    on wcheckthread toggled $
        atomically . writeTVar tcreatethreads =<< toggleButtonGetActive wcheckthread

    tmakewatermark <- atomically . newTVar =<< toggleButtonGetActive wcheckwatermark
    on wcheckwatermark toggled $
        atomically . writeTVar tmakewatermark =<< toggleButtonGetActive wcheckwatermark

    let regeneratePasta = do
        ps <- readIORef pastaSet
        lastdate <- readIORef pastaMod
        npd <- pastaDate ps
        when (npd > lastdate) $ do
            writeLog "regen pasta"
            atomically . writeTVar tpastas =<< generatePasta ps
            writeIORef pastaMod npd

    let regenerateImages = do
        i <- readIORef imageFolder
        images <- filterImages <$> fromIOEM (do tempError 3 $ "Невозможно прочитать изображения из папки " ++ i
                                                return [])
                                            (map (i </>) <$> getDirectoryContents i)
        li <- readIORef imagesLast
        when (images /= li) $ atomically $ writeTVar timages images

    regeneratePasta
    regenerateImages

    let updWipeMessage = do
        --mo <- chooseFromList mochanNames
        pc <- readIORef postCount
        let psc = "Сделано постов: " ++ show pc
        bnd <- do ac <- readIORef activeCount
                  bn <- readIORef bannedCount
                  return $ "\nЗабанен на досках: " ++ show bn ++ "/" ++ show ac
        let ach = maybe [] (("\nAchievement unlocked: \"" ++) . (++ "\"")) $
                    getAchievement pc
        updMessage $ psc ++ bnd ++ ach

    let regenerateExcluding board exc = do
        if null exc -- TODO we don't support multiple proxys & wipeunits yet
            then do
                writeLog $ "Spawning new thread for " ++ renderBoard board
                mthread <- atomically $ newTVar Random
                mmode <- atomically $ newTVar Random
                threadid <- forkIO $
                    entryPoint Log (atomically . writeTQueue tqLog)
                               ShSettings{..} tqOut board MuSettings{..}
                return [WipeUnit threadid]
            else return []

    let maintainBoardUnit :: (Int, Int) -> BoardUnit -> IO (Int, Int)
        maintainBoardUnit (active, banned) BoardUnit{..} = do
        isActive <- toggleButtonGetActive buWidget
        isWiping <- readIORef wipeStarted
        let collectGarbage w@WipeUnit{..} = do
            do st <- threadStatus wuThreadId
               if st == ThreadDied || st == ThreadFinished
                then do writeLog $ "blasgtk: Thread for " ++ renderBoard buBoard ++ " died. Removing"
                        return Nothing
                else if not isActive || not isWiping
                        then do writeLog $ "blasgtk: Killing thread for " ++ renderBoard buBoard
                                killThread wuThreadId
                                return Nothing
                        else return $ Just w
        new <- catMaybes <$> (mapM collectGarbage =<< readIORef buWipeUnits)
        regend <- if isActive && isWiping
                    then regenerateExcluding buBoard new
                    else return []
        writeIORef buWipeUnits $ new ++ regend
        isBanned <- readIORef buBanned
        return (active + (if isActive then 1 else 0)
               ,banned + (if isBanned then 1 else 0))

    let maintainBoardUnits = do
            (active, banned) <- foldM maintainBoardUnit (0,0) boardunits
            writeIORef activeCount active
            writeIORef bannedCount banned

    let startWipe = do
        writeIORef wipeStarted True
        maintainBoardUnits

    let killWipe = do
        writeLog "Stopping wipe..."
        writeIORef wipeStarted False
        maintainBoardUnits

    -- main loop

    let mainloop = do
        whenM (readIORef wipeStarted) $ do
            progressBarPulse wprogresswipe
            regeneratePasta
            regenerateImages
            maintainBoardUnits
            updWipeMessage
            logs <- atomically $ untilNothing $ tryReadTQueue tqLog
            forM_ logs (writeLog . show)
            outs <- atomically $ untilNothing $ tryReadTQueue tqOut
            
            yield
        -- TODO Captcha
        -- TODO postCount
        -- TODO updateBanned
        -- TODO show thread outs
        -- TODO popup banned

    timeoutAdd (mainloop >> return True) 50 --kiloseconds, 20 fps.

    -- main window signals

    let setCheckActive ca = do
        signalBlock wmenushowConnId
        checkMenuItemSetActive wmenushow ca
        signalUnblock wmenushowConnId

    on wbuttonwipe buttonActivated $ do
        ifM (not <$> readIORef wipeStarted)
            (do buttonSetLabel wbuttonwipe "Прекратить _Вайп"
                updWipeMessage
                progressBarPulse wprogresswipe
                startWipe
                {-containerRemove wprogressalignment wprogresswipe
                containerAdd wprogressalignment wvboxcaptcha
                widgetGrabFocus wentrycaptcha
                widgetGrabDefault wbuttoncaptchaok-}
                )
            (do buttonSetLabel wbuttonwipe "Начать _Вайп"
                updMessage "Вайп ещё не начат"
                progressBarSetFraction wprogresswipe 0
                killWipe
                {-containerRemove wprogressalignment wvboxcaptcha
                containerAdd wprogressalignment wprogresswipe-}
                )

    _ <- forM pastaradio $ \(p, w) -> do
            on w toggled $
                whenM (toggleButtonGetActive w) $ do
                    writeIORef pastaMod nullTime
                    writeIORef pastaSet p

    onEntryActivate wentryimagefolder $ do
        buttonClicked wbuttonimagefolder

    -- if only filechooserbutton fucking worked properly...
    on wbuttonimagefolder buttonActivated $ do
        d <- fileChooserDialogNew Nothing Nothing FileChooserActionSelectFolder
                      [("gtk-cancel", ResponseCancel)
                      ,("gtk-open", ResponseAccept)
                      ]
        fileChooserSetCurrentFolder d =<< readIORef imageFolder
        widgetShow d
        r <- dialogRun d
        case r of
            ResponseAccept ->
                fileChooserGetFilename d >>=
                    maybe (writeLog "Impossible happened: ResponseAccept with Nothing.")
                          (\f -> do writeIORef imageFolder f
                                    entrySetText wentryimagefolder f)
            _ -> return ()
        widgetHide d

    on wbuttonselectall buttonActivated $ do
        forM_ boardunits $
            (`toggleButtonSetActive` True) . buWidget

    on wbuttonselectnone buttonActivated $ do
        forM_ boardunits $
            (`toggleButtonSetActive` False) . buWidget

    onAdjChanged wad $ do
        v <- adjustmentGetValue wad
        p <- adjustmentGetPageSize wad
        pu <- subtract p <$> readIORef previousUpper
        when (v >= pu) $ do
            u <- adjustmentGetUpper wad
            adjustmentSetValue wad $ subtract p u
            writeIORef previousUpper u

    onDelete window $ \_ -> do noTray <- not <$> statusIconIsEmbedded wtray
                               closePlease <- not <$> toggleButtonGetActive wchecktray
                               if noTray || closePlease
                                   then return False
                                   else True <$ widgetHide window
    onShow window $ setCheckActive True
    onHide window $ setCheckActive False                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;let game = widgetShowAll window
    onDestroy window mainQuit

    -- start main gui

    i am playing the game
    the one that'll take me to my end
    i am waiting for the rain
    to wash up who i am

    libera me from $osach:
       DO THE IMPOSSIBLE!
       SEE THE INVISIBLE!
       ROW! ROW!
       FIGHT THE POWER!
       
       TOUCH THE UNTOUCHABLE!
       BREAK THE UNBREAKABLE!
       ROW! ROW!
       FIGHT THE POWER!
       
       ROW! ROW!
       FIGHT THE POWER!                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             [you lost The Game]
       
    --  write config
    coActiveBoards <- map buBoard <$>
                    filterM (toggleButtonGetActive . buWidget) boardunits
    coPastaSet <- readIORef pastaSet
    coCreateThreads <- toggleButtonGetActive wcheckthread
    coAttachImages <- toggleButtonGetActive wcheckimages
    coWatermark <- toggleButtonGetActive wcheckwatermark
    coAnnoy <- toggleButtonGetActive wcheckannoy
    coAnnoyErrors <- toggleButtonGetActive wcheckannoyerrors
    coTray <- toggleButtonGetActive wchecktray
    coImageFolder <- readIORef imageFolder
       
    tw <- try $ writeFile "config" $ show Conf{..}
    case tw of
        Left (a::SomeException) -> writeLog $ "Couldn't write config, got exception: " ++ show a
        Right _ -> writeLog $ "Wrote config: " ++ show Conf{..}
       
    -- close log
       
    rawPutLog =<< ("Finished wipe session, current POSIX time is " ++) . show <$> getPOSIXTime       
       
    when (isJust hlog) $ do
        hFlush $ fromJust hlog
        hClose $ fromJust hlog
       
data {-ROW-}ROW__FIGHT_THE_POWER =
       DO THE IMPOSSIBLE
   |   SEE THE INVISIBLE
--     ROW ROW FIGHT THE POWER
   |   TOUCH THE UNTOUCHABLE
   |   BREAK THE UNBREAKABLE
   |   ROW ROW__FIGHT_THE_POWER
   | {-ROW ROW-}FIGHT THE POWER

















































































i :: a -> b -> b
i _ a = a

am :: a
am = undefined

playing :: Monad m => a -> m b -> m b
playing _ m = m

the :: a -> b -> c -> d -> e -> f -> g -> IO ()
the _ _ _ _ _ _ _ = return ()

one :: a
one = undefined

that'll :: a
that'll = undefined

me :: a
me = undefined

to :: a -> b -> c -> d -> e -> IO ()
to _ _ _ _ _ = return ()

my :: a
my = undefined

end :: a
end = undefined

waiting :: Monad m => a -> b -> c -> m ()
waiting _ _ _ = return ()

for :: a
for = undefined

rain :: a
rain = undefined

wash :: a
wash = undefined

up :: a
up = undefined

who :: a
who = undefined

libera :: a -> b -> [c] -> c
libera _ _ a = last a

from :: a
from = undefined

osach :: a
osach = undefined

you :: a -> b -> c -> IO ()
you _ _ _ = mainGUI

lost :: a
lost = undefined

data The = The

data Game = Game

infixr 6 !
(!) :: a -> b -> b
(!) _ b = b

data THE = THE

data IMPOSSIBLE = IMPOSSIBLE

data INVISIBLE = INVISIBLE

data UNTOUCHABLE = UNTOUCHABLE

data UNBREAKABLE = UNBREAKABLE

data POWER = POWER
