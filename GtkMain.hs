{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import Import hiding (on)
import "blast-it-with-piss" BlastItWithPiss
import "blast-it-with-piss" BlastItWithPiss.Blast
import "blast-it-with-piss" BlastItWithPiss.Post
import "blast-it-with-piss" BlastItWithPiss.Board
import "blast-it-with-piss" BlastItWithPiss.MonadChoice
import Graphics.UI.Gtk hiding (get, set)
import qualified Graphics.UI.Gtk as G (get)
import GHC.Conc
import Control.Concurrent.STM
import System.Environment.UTF8
import System.FilePath
import System.Directory
import System.IO.Temp
import Network (withSocketsDo)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Paths_blast_it_with_piss
import Data.Version (showVersion)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
#ifdef BINDIST
import System.Environment.Executable (splitExecutablePath)
#endif
#if !MIN_VERSION_directory(1,2,0)
import System.Time
#endif

#if MIN_VERSION_directory(1,2,0)
type ModificationTime = UTCTime
#else
type ModificationTime = ClockTime
#endif

getResourceFile :: String -> IO String
#if defined(BINDIST)||defined(TEST)
getResourceFile x = return $ "resources" </> x
#else
getResourceFile x = getDataFileName $ "resources" </> x
#endif

configDir :: IO String
#if defined(BINDIST)||defined(TEST)
configDir = return "."
#else
configDir = do c <- getAppUserDataDirectory "BlastItWithPiss"
               createDirectoryIfMissing False c
               return c
#endif

nullTime :: ModificationTime
#if MIN_VERSION_directory(1,2,0)
nullTime = UTCTime (ModifiedJulianDay 0) 0
#else
nullTime = TOD 0 0
#endif

timeRightNow :: IO ModificationTime
#if MIN_VERSION_directory(1,2,0)
timeRightNow = getCurrentTime
#else
timeRightNow = getClockTime
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

achievements :: [(Int, String)]
achievements =
        reverse $
        [(10, "Анон")
        ,(20, "Няша")
        ,(50, "Троллер")
        ,(100, "Сотня разорванных анусов")
        ,(150, "Вайпер")
        ,(200, "Братишка")
        ,(300, "Бэтмен")
        ,(500, "Ультрахардкорщик")
        ,(1000, "Тысяча порванных срак")
        ,(3000, "Поехавший")
        ,(20000, "Поле устиланное вырванными аналами неверных")
        ,(50000, "Прости Марио, но Принцесса в другом замке")
        ,(100000, "Супер-пупер-мега-гипер-охуительный пиздец")
        ,(200000, "Словил все геты разом")
        ,(1000000, "Накрутил же, бака")
        ]

getAchievement :: Int -> Maybe String
getAchievement a =
    findMap (\(p, t) -> if a >= p then Just t else Nothing) $ achievements

-- TODO прокси
-- TODO support ANTIGATE, CAPTCHABOT, DECAPTCHER etc.
-- TODO вайпать постами из треда/страницы choosePost
-- TODO don't escape RandomNum and RandomChar.
-- TODO don't regenerate banned
-- TODO switch to JSON for config and manifest
-- TODO Updater
-- TODO use appendFile,  don't output anything on shindos
-- TODO bundle proxy checker
-- TODO helpMessage
-- TODO реклама вайпалки в самом вайпе (в отдельном файле advertisement, постится и при садизме и при моче)
--      и соответствующая опция для отключения рекламы вайпалки
-- TODO mochepasta resources/mocha, change default boards
-- TODO Выскакивать попап о том куда писать баг-репорты, о том что любой фидбек
--      , даже "я посрал" — приветствуется.
--      И о том что если вы забанены или кажется что что-то не так, то можно
--      перезапустить вайпалку (с BlastItWithPiss(.exe), а не blastgtk(.exe))
--      и посмотреть если апдейты (Когда апдейтер будет готов)
-- TODO update mocha-repo description

-- FIXME Oh dog, what a mess.
--       Just look at all the copy-paste code, and env dependencies. It's gonna crumble!
-- TODO Less boilerplate, less explicit parameter passing. (Roll out some monad)
-- TODO More type safety.
-- TODO Increase modularity, fix mess with captcha keys and the like.

-- TODO АВТОМАТИЧЕСКОЕ ПЕРЕПОДКЛЮЧЕНИЕ
-- TODO update description when snoyman releases http-conduit-1.7.0
-- TODO add multipart/form-data to http-conduit
-- TODO add API as a fallback if can't parse html
-- TODO don't regenerate threads until asked to.
-- TODO configurable escaping
-- TODO configurable timeout
-- TODO config last thread time
-- TODO background mode
-- TODO FIX FREEZES
-- TODO Move ssach/recaptcha/cloudflare-specific functionality in their own modules
-- TODO Support 2chnu, alterchan.

data PastaSet = Mocha
              | Kakashki
              | Num
              | Char
              | FromThread
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
                 ,coSettingsShown :: Bool
                 ,coAdditionalShown :: Bool
                 ,coLogShown :: Bool
                 ,coFirstLaunch :: Bool
                 ,coUseHttpProxy :: Bool
                 ,coHttpProxyFile :: String
                 ,coUseSocksProxy :: Bool
                 ,coSocksProxyFile :: String
                 ,coUseNoProxy :: Bool
                 }
    deriving (Eq, Show, Read)

data WipeUnit = WipeUnit {wuProxy :: BlastProxy
                         ,wuThreadId :: ThreadId
                         ,wuBanned :: IORef Bool
                         }
    deriving (Eq)

data BoardUnit = BoardUnit {buBoard :: Board
                           ,buWidget :: CheckButton
                           ,buWipeUnits :: IORef [WipeUnit]
                           -- TODO right now we don't support configuring per-board
                           --      wipe preferences
                           --,buMuSettings :: MuSettings
                           }

data State = S
    {messageLock :: Bool
    ,previousUpper :: Double
    ,wipeStarted :: Bool
    ,postCount :: Int
    ,activeCount :: Int
    ,bannedCount :: Int
    ,pastaSet :: PastaSet
    ,pastaMod :: ModificationTime
    ,imagesLast :: [String]
    ,proxies :: M.Map BlastProxy ProxySettings
    ,httpproxyMod :: ModificationTime
    ,httpproxyLast :: [BlastProxy]
    ,socksproxyMod :: ModificationTime
    ,socksproxyLast :: [BlastProxy]
    ,pendingCaptchas :: [(OriginStamp, Message)]
    }

type S = ReaderT (IORef State) IO

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

-- if only FileChooserButton worked properly...
onFileChooserEntryButton :: Bool -> Button -> Entry -> (String -> IO ()) -> IO () -> IO ()
onFileChooserEntryButton b wfbutton wfentry writeLog fin = void $ do
    if b
        then aux FileChooserActionSelectFolder (\d -> fileChooserSetCurrentFolder d =<< entryGetText wfentry)
        else aux FileChooserActionOpen (flip fileChooserSetCurrentFolder ".")
  where aux m fd = do
            onEntryActivate wfentry $ do
                buttonClicked wfbutton
            on wfbutton buttonActivated $ do
                d <- fileChooserDialogNew Nothing Nothing m
                        [("gtk-cancel", ResponseCancel)
                        ,("gtk-open", ResponseAccept)
                        ]
                fd d
                widgetShow d
                r <- dialogRun d
                case r of
                    ResponseAccept -> do
                        fileChooserGetFilename d >>=
                            maybe (writeLog "Impossible happened: ResponseAccept with Nothing.")
                                  (\f -> entrySetText wfentry f >> fin)
                    _ -> return ()
                widgetHide d

defaultConf :: Conf
defaultConf =
    Conf { -- FIXME coActiveBoards = [B, BB, ABU, D, VG, PR, DEV]
          coActiveBoards = [NE, MDK]
         ,coPastaSet = Mocha
         ,coCreateThreads = True
         ,coImageFolder = "images"
         ,coAttachImages = True
         ,coAnnoy = True
#ifdef TEST
         ,coTray = False
#else
         ,coTray = True
#endif
         ,coWatermark = False
         ,coAnnoyErrors = True
         ,coSettingsShown = False
         ,coAdditionalShown = False
         ,coLogShown = False
         ,coFirstLaunch = True
         ,coUseHttpProxy = False
         ,coHttpProxyFile = ""
         ,coUseSocksProxy = False
         ,coSocksProxyFile = ""
         ,coUseNoProxy = True
         }

get :: S State
get = liftIO . readIORef =<< ask

set :: State -> S ()
set n = liftIO . (`writeIORef` n) =<< ask

mod :: (State -> State) -> S ()
mod f = set =<< f <$> get

modM :: (State -> S State) -> S ()
modM m = set =<< m =<< get

bugMessage :: String
bugMessage = "If you experience crashes, bugs, or any kind strange or illogical behavior,"
          ++ " file a bug report to the author(https://github.com/exbb2/BlastItWithPiss/issues)"
          ++ " with attached files log.txt, and, if you have one, log.txt.bak.\n"
          ++ "NOTE: aforementioned logs contain address of the image folder that you specified"
          ++ " if you customized the program. If this data is sensitive to you, you might want"
          ++ " to clear it from the logs before you submit them.\n"
          ++ "Thanks, and have fun. Hopefully, it has been worth the weight."

helpMessage :: String
helpMessage = "No help message for now, sorry\n\n" ++ bugMessage

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
#ifndef TEST
  putStrLn bugMessage
#endif
  hlog <- do
    eh <- try $ do
        h <- openFile "log.txt" AppendMode--ReadWriteMode
        size <- hFileSize h
        if size >= (10 * 1024 * 1024)
            then do hClose h
                    renameFile "log.txt" "log.txt.bak"
                    openFile "log.txt" AppendMode--ReadWriteMode
            else return h
    case eh of
        Left (a::SomeException) -> do
            putStrLn $ "Got exception while trying to open log file: " ++ show a
            return Nothing
        Right h -> do hSetEncoding h utf8
--                    hSeek h SeekFromEnd 0
                      return $ Just h

  let rawPutLog s = do
        when (isJust hlog) $
            whenM (hIsWritable (fromJust hlog)) $ do
                hPutStrLn (fromJust hlog) s
                hFlush (fromJust hlog)
#ifndef mingw32_HOST_OS
        whenM (hIsWritable stdout) $ do
            putStrLn s
            hFlush stdout
#endif
  handle (\(a::SomeException) -> rawPutLog $ "Uncaught exception terminated program, sorry: " ++ show a) $ do
    -- read configuration

    rawPutLog =<< ("Starting blastgtk. Current POSIX time is " ++) . show <$> getPOSIXTime

    --rawPutLog "Русский текст например"

    configfile <- (</> "config") <$> configDir

    Conf{..} <- do x <- try $ readFile $ configfile
                   case x of
                    Left (a::SomeException) -> do rawPutLog $ "Couldn't read config from \"" ++ configfile ++ "\" , loading defaults. Exception was: " ++ show a
                                                  return defaultConf
                    Right c -> case readMay c of
                                Nothing -> do let confold = configfile <.> "old.faulty"
                                              rawPutLog $ "Couldn't read config from \"" ++ configfile ++ "\" because of syntax error, overwriting with defaults. Old version saved at \"" ++ confold ++ "\""
                                              fromTrySome (return ()) $
                                                writeFile confold c
                                              return defaultConf
                                Just n -> return n

    rawPutLog $ "Loaded config: " ++ show Conf{..}

    -- init

    initGUI
    b <- builderNew
    builderAddFromFile b =<< getResourceFile "blast.glade"

    window <- builderGetObject b castToWindow "window1"
    windowSetTitle window "Вайпалка мочана"

    -- setup tray

    wtray <- statusIconNewFromFile =<< getResourceFile "2ch.so.png"
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
        ifM (G.get window widgetVisible)
            hideWindow
            popupWindow

    -- tray signals

    on wtray statusIconActivate toggleWindow
    on wtray statusIconPopupMenu $ \(Just mb) t -> menuPopup wmenu $ Just (mb, t)
    wmenushowConnId <- on wmenushow menuItemActivate toggleWindow
    on wmenuexit menuItemActivate $ mainQuit

    -- setup widgets

    wexpandersettings <- builderGetObject b castToExpander "expandersettings"
    expanderSetExpanded wexpandersettings coSettingsShown

    wexpanderlog <- builderGetObject b castToExpander "expanderlog"
    expanderSetExpanded wexpanderlog coLogShown

    wexpanderadditional <- builderGetObject b castToExpander "expanderadditional"
    expanderSetExpanded wexpanderadditional coAdditionalShown

    wvboxcaptcha <- builderGetObject b castToVBox "vboxcaptcha"
    weventboxcaptcha <- builderGetObject b castToEventBox "eventboxcaptcha"
    wimagecaptcha <- builderGetObject b castToImage "imagecaptcha"
    wentrycaptcha <- builderGetObject b castToEntry "entrycaptcha"
    wbuttoncaptchaok <- builderGetObject b castToButton "buttoncaptchaok"
    wbuttoncaptchacancel <- builderGetObject b castToButton "buttoncaptchacancel"

    wlabelmessage <- builderGetObject b castToLabel "labelmessage"
    wprogressalignment <- builderGetObject b castToAlignment "progressalignment"
    wprogresswipe <- builderGetObject b castToProgressBar "wipeprogress"
    wbuttonwipe <- builderGetObject b castToButton "wipebutton"

    wradiomocha <- builderGetObject b castToRadioButton "radio-mocha"
    wradiokakashki <- builderGetObject b castToRadioButton "radio-kakashki"
    wradionum <- builderGetObject b castToRadioButton "radio-num"
    wradiochar <- builderGetObject b castToRadioButton "radio-char"
    wradiofromthread <- builderGetObject b castToRadioButton "radio-fromthread"

    let pastaradio =
            [(Mocha, wradiomocha)
            ,(Kakashki, wradiokakashki)
            ,(Num, wradionum)
            ,(Char, wradiochar)
            ,(FromThread, wradiofromthread)
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

    wvboxboards <- builderGetObject b castToVBox "vbox-boards"

    boardunits <- forM (fst $ unzip $ ssachBoardsSortedByPostRate) $ \board -> do
        wc <- checkButtonNewWithLabel $ renderBoard board
        when (board `elem` coActiveBoards) $ toggleButtonSetActive wc True
        boxPackStart wvboxboards wc PackNatural 0
        BoardUnit board wc <$> newIORef []

    wbuttonselectall <- builderGetObject b castToButton "buttonselectall"
    wbuttonselectnone <- builderGetObject b castToButton "buttonselectnone"
    
    wlog <- builderGetObject b castToTextView "log"
    wbuf <- textViewGetBuffer wlog
    -- VERY SLOW for some reason on texts > 100 kilobytes
    -- Not a haskell issue, print prints instantly, widget lags like hell for a minute.
    --  textBufferSetText wbuf =<< readLog
    wad <- textViewGetVadjustment wlog
    adjustmentSetValue wad =<< adjustmentGetUpper wad

    wlabelversion <- builderGetObject b castToLabel "labelversion"
    labelSetMarkup wlabelversion $
        ("<small><a href=\"https://github.com/exbb2/BlastItWithPiss\">" ++) . (++"</a></small>") $
            showVersion version

    wcheckhttpproxy <- builderGetObject b castToCheckButton "checkhttpproxy"
    toggleButtonSetActive wcheckhttpproxy coUseHttpProxy
    wentryhttpproxyfile <- builderGetObject b castToEntry "entryhttpproxyfile"
    entrySetText wentryhttpproxyfile coHttpProxyFile
    wbuttonhttpproxyfile <- builderGetObject b castToButton "buttonhttpproxyfile"

    wchecksocksproxy <- builderGetObject b castToCheckButton "checksocksproxy"
    toggleButtonSetActive wchecksocksproxy coUseSocksProxy
    wentrysocksproxyfile <- builderGetObject b castToEntry "entrysocksproxyfile"
    entrySetText wentrysocksproxyfile coSocksProxyFile
    wbuttonsocksproxyfile <- builderGetObject b castToButton "buttonsocksproxyfile"

    wchecknoproxy <- builderGetObject b castToCheckButton "checknoproxy"
    toggleButtonSetActive wchecknoproxy coUseNoProxy

    -- setup mutable variables

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
        whenM (toggleButtonGetActive wcheckannoyerrors) $ popupWindow
        void $ timeoutAdd (writeIORef messageLock False >> return False) (t * 1000)

    let captchaMessage s = do
        writeIORef messageLock True
        n <- getPOSIXTime
        writeLog $ "blasgtk, " ++ show n ++ ": Captcha message: " ++ s
        whenM (toggleButtonGetActive wcheckannoy) $ popupWindow
        labelSetText wlabelmessage s
        -- we remove messageLock in removeCaptcha

    let banMessage t s = do
        writeIORef messageLock True
        n <- getPOSIXTime
        writeLog $ "blasgtk, " ++ show n ++ ": captcha or ban message: " ++ s
        labelSetMarkup wlabelmessage $ "<span foreground=\"#ff0000\">" ++ s ++ "</span>"
        whenM (toggleButtonGetActive wcheckannoy) $ popupWindow
        void $ timeoutAdd (writeIORef messageLock False >> return False) (t * 1000)

    let updMessage s = do
        unlessM (readIORef messageLock) $
            labelSetText wlabelmessage s

    let fromIOEM v = handle (\(_::IOException) -> v)

    let appFile d m f = fromIOEM (do tempError 3 $ "Невозможно прочитать файл \"" ++ f ++ "\""
                                     return d) $ m f

    let generatePasta Mocha = appFile [] readPasta =<<
#ifdef TEST
                                            return "./testkokoko"
#else
                                            getResourceFile "mocha"
#endif
        generatePasta Kakashki = appFile [] readPasta =<< getResourceFile "sadism"
        generatePasta Char = generateRandomStrings (1, 30) (100, 5000) ('a','z')
        generatePasta Num = generateRandomStrings (1, 30) (100, 5000) ('0', '9')
        generatePasta FromThread = error "NOT IMPLEMENTED"
    
    let pastaDate Mocha = appFile nullTime getModificationTime =<< getResourceFile "mocha"
        pastaDate Kakashki = appFile nullTime getModificationTime =<< getResourceFile "sadism"
        pastaDate _ = timeRightNow

    let filterImages = filter ((`elem` [".jpg",".jpe",".jpeg",".gif",".png"]) . takeExtension)

    previousUpper <- newIORef =<< adjustmentGetUpper wad

    wipeStarted <- newIORef False

    postCount <- newIORef 0
    activeCount <- newIORef 0
    bannedCount <- newIORef 0

    pastaSet <- newIORef coPastaSet
    pastaMod <- newIORef nullTime

    imagesLast <- newIORef []

    proxies <- newIORef M.empty
    httpproxyMod <- newIORef nullTime
    httpproxyLast <- newIORef []
    socksproxyMod <- newIORef nullTime
    socksproxyLast <- newIORef []

    pendingCaptchas <- newIORef []

    -- setup shared mutable state

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

    -- captcha functions

    let formatCaptchaMessage CaptchaPosting (OriginStamp _ proxy board _ thread) =
            "Введите капчу для " ++
                (maybe ("создания нового треда в " ++ renderBoard board)
                    (("Поста в тред " ++) . ssachThread board) thread) ++
                        maybeNoProxy [] (("с прокси {" ++) . (++"}") . show) proxy
        formatCaptchaMessage CaptchaCloudflare (OriginStamp _ proxy _ _ _) =
            "Введите капчу Cloudflare для " ++ show proxy

    let activateCaptcha = do
        pc <- readIORef pendingCaptchas
        case pc of
            [] -> tempError 2 $ "Switching while there are no captchas."
            ((st, c):_) ->
                withSystemTempFile "recaptcha-captcha-image.jpeg" $ \fn h -> do
                    L.hPut h $ captchaBytes c
                    hClose h
                    imageSetFromFile wimagecaptcha fn
                    writeLog "switched captcha"
                    entrySetText wentrycaptcha ""
                    captchaMessage $ formatCaptchaMessage (captchaType c) st

    let addCaptcha sp = do
        pc <- readIORef pendingCaptchas
        modifyIORef pendingCaptchas (++[sp])
        when (null pc) $ do
            containerRemove wprogressalignment wprogresswipe
            containerAdd wprogressalignment wvboxcaptcha
            widgetGrabFocus wentrycaptcha
            widgetGrabDefault wbuttoncaptchaok
            activateCaptcha

    let removeCaptcha a = do
        cs <- readIORef pendingCaptchas
        case cs of
            [] -> tempError 2 $ "Ответил на несуществующий запрос капчи"
            ((_,c):n) -> do
                writeLog $ "Sending " ++ show a ++ " to captcha requester"
                captchaSend c a
                writeIORef pendingCaptchas n
                if (null n)
                    then do
                        containerRemove wprogressalignment wvboxcaptcha
                        containerAdd wprogressalignment wprogresswipe
                        writeIORef messageLock False
                    else activateCaptcha

    -- generate pasta, images & proxys for the first time

    let regeneratePasta = do
        ps <- readIORef pastaSet
        lastdate <- readIORef pastaMod
        npd <- pastaDate ps
        when (npd > lastdate) $ do
            writeLog "regen pasta"
            atomically . writeTVar tpastas =<< generatePasta ps
            writeIORef pastaMod npd

    let regenerateImages = do
        ni <- entryGetText wentryimagefolder
        images <- filterImages <$> fromIOEM
                    (do whenM (toggleButtonGetActive wcheckimages) $ tempError 3 $ "Невозможно прочитать изображения из папки " ++ ni
                        return [])
                    (map (ni </>) <$> getDirectoryContents ni)
        li <- readIORef imagesLast
        when (images /= li) $ do
            atomically $ writeTVar timages images
            writeIORef imagesLast =<< readTVarIO timages

    let regenerateProxies = do
        let getProxyMap isSocks wcheckproxy wentryproxyfile proxymod proxylast = do
            ifM (toggleButtonGetActive wcheckproxy) (do
                pf <- entryGetText wentryproxyfile
                d <- readIORef proxymod
                nd <- appFile nullTime getModificationTime pf
                if (nd > d)
                    then do writeLog "regen http proxy"
                            writeIORef proxymod nd
                            nps <- catMaybes . map (readBlastProxy isSocks) . lines <$>
                                appFile [] readFile pf
                            writeIORef proxylast nps
                            return nps
                    else readIORef proxylast)
                (do writeIORef proxylast []; return [])
        let robustEnterpriseQualityBestPracticesSolution x a = do
                y <- M.fromList <$> forM a (\p -> (,) p <$> defPrS)
                return $ M.intersection x y `M.union` y
        nnp <- ifM (toggleButtonGetActive wchecknoproxy)
                   (return [NoProxy]) (return [])
        nhps <- getProxyMap False wcheckhttpproxy wentryhttpproxyfile httpproxyMod httpproxyLast
        nsps <- getProxyMap True wchecksocksproxy wentrysocksproxyfile socksproxyMod socksproxyLast
        modifyIORefM proxies
            (`robustEnterpriseQualityBestPracticesSolution` (nnp ++ nhps ++ nsps))

    regeneratePasta
    regenerateImages
    regenerateProxies

    -- main loop functions

    let updWipeMessage = do
        pc <- readIORef postCount
        let psc = "Сделано постов: " ++ show pc
        bnd <- do ac <- readIORef activeCount
                  bn <- readIORef bannedCount
                  return $ "\nЗабанен на досках: " ++ show bn ++ "/" ++ show ac
        let ach = maybe [] (("\nAchievement unlocked: \"" ++) . (++ "\"")) $
                    getAchievement pc
        updMessage $ psc ++ bnd ++ ach

    let regenerateExcluding board exc = do
        prx <- M.assocs <$> readIORef proxies
        catMaybes <$> forM prx (\(p, s) ->
            if any ((==p) . wuProxy) exc
                then return Nothing
                else do writeLog $ "Spawning new thread for " ++ renderBoard board
                        mthread <- atomically $ newTVar Nothing
                        mmode <- atomically $ newTVar Nothing
                        threadid <- forkIO $ runBlast $ do
                            entryPoint board p Log ShSettings{..} MuSettings{..} s tqOut
                        Just . WipeUnit p threadid <$> newIORef False
            )

    let setBanned :: Board -> BlastProxy -> Bool -> IO ()
        setBanned board proxy st = do
            maybe (return ()) ((`writeIORef` st) . wuBanned) =<< runMaybeT (do
                ws <- maybe mzero (liftIO . readIORef . buWipeUnits) $
                        find ((==board) . buBoard) boardunits
                maybe mzero return $ find ((==proxy) . wuProxy) ws)

    let maintainWipeUnit :: BoardUnit -> Bool -> Bool -> WipeUnit -> IO (Maybe WipeUnit)
        maintainWipeUnit BoardUnit{..} isActive isWiping w@WipeUnit{..} = do
            st <- threadStatus wuThreadId
            isBanned <- readIORef wuBanned
            pxs <- M.keys <$> readIORef proxies
            if st == ThreadDied || st == ThreadFinished
                then do
                    writeLog $ "blasgtk: Thread for " ++ renderBoard buBoard ++ " died. Removing"
                    return Nothing
                else if not isActive || not isWiping || isBanned || notElem wuProxy pxs
                        then do
                            writeLog $ "blasgtk: Killing thread for " ++ renderBoard buBoard
                            killThread wuThreadId
                            return Nothing -- TODO don't regenerate banned threads
                        else return $ Just w

    let maintainBoardUnit :: (Int, Int) -> BoardUnit -> IO (Int, Int)
        maintainBoardUnit (active, banned) bu@BoardUnit{..} = do
        isActive <- toggleButtonGetActive buWidget
        isWiping <- readIORef wipeStarted
        new <- catMaybes <$> (mapM (maintainWipeUnit bu isActive isWiping) =<< readIORef buWipeUnits)
        regend <- if isActive && isWiping
                    then regenerateExcluding buBoard new
                    else return []
        writeIORef buWipeUnits $ new ++ regend
        isBanned <- --FIXME FIXME FIXME readIORef buBanned
                    return False
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
        pc <- readIORef pendingCaptchas
        forM_ pc $ const $ removeCaptcha AbortCaptcha

    -- main loop

    let mainloop = do
        whenM (readIORef wipeStarted) $ do
            progressBarPulse wprogresswipe
            regeneratePasta
            regenerateImages
            regenerateProxies
            maintainBoardUnits
            updWipeMessage
            outs <- atomically $ untilNothing $ tryReadTQueue tqOut
            forM_ outs $ \s@(OutMessage st@(OriginStamp _ proxy board _ _) m) ->
              case m of
                OutcomeMessage o -> do
                    case o of
                        SuccessLongPost _ -> writeLog (show st ++ ": SuccessLongPost")
                        _ -> writeLog (show s)
                    case o of
                        Success -> do modifyIORef postCount (+1)
                                      setBanned board proxy False
                        SuccessLongPost _ -> do modifyIORef postCount (+1)
                                                
                        Wordfilter -> tempError 3 "Не удалось обойти вордфильтр"
                        Banned x -> do banMessage 5 $ "Забанен на доске " ++ renderBoard board
                                                    ++ " Причина: " ++ show x
                                                    ++ "\nВозможно стоит переподключится\nили начать вайпать /d/"
                                       setBanned board proxy True
                        SameMessage -> tempError 2 $ renderBoard board ++ ": Запостил одно и то же сообщение"
                        SameImage -> tempError 2 $ renderBoard board ++ ": Запостил одну и ту же пикчу"
                        TooFastPost -> return () -- tempError 2 $ renderBoard board ++ ": Вы постите слишком часто, умерьте пыл"
                        TooFastThread -> tempError 3 $ renderBoard board ++ ": Вы создаете треды слишком часто"
                        NeedCaptcha -> return ()
                        WrongCaptcha -> tempError 3 "Неправильно введена капча"
                        RecaptchaBan -> do banMessage 7 $ "Забанен рекапчой, охуеть. Переподключайся, мудило"
                                           setBanned board proxy True
                        LongPost -> tempError 1 $ renderBoard board ++ ": Запостил слишком длинный пост"
                        CorruptedImage -> tempError 2 $ renderBoard board ++ ": Запостил поврежденное изображение"
                        OtherError x -> tempError 7 $ renderBoard board ++ ": " ++ show x
                        InternalError x -> tempError 7 $ renderBoard board ++ ": " ++ show x
                        CloudflareCaptcha -> do banMessage 7 $ "Если эта ошибка появляется то это баг, сообщите нам об этом"
                                                setBanned board proxy True
                        CloudflareBan -> do banMessage 7 $ "Эту проксю пидорнули по клаудфлеру, она бесполезна"
                                            setBanned board proxy True
                        UnknownError -> tempError 4 $ renderBoard board ++ ": Неизвестная ошибка, что-то пошло не так"
                c@SupplyCaptcha{} -> addCaptcha (st, c)
                LogMessage _ -> writeLog (show s)
                NoPastas -> do writeLog (show s)
                               tempError 3 "Невозможно прочитать пасты, постим повторяющуюся строку \"NOPASTA\""
                NoImages -> do writeLog (show s)
                               tempError 3 "Невозможно прочитать пикчи, постим капчу"
            yield

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
                )
            (do buttonSetLabel wbuttonwipe "Начать _Вайп"
                updMessage "Вайп ещё не начат"
                progressBarSetFraction wprogresswipe 0
                killWipe
                )

    on weventboxcaptcha buttonPressEvent $ do
        liftIO $ removeCaptcha ReloadCaptcha
        return True

    on wbuttoncaptchaok buttonActivated $ do
        x <- entryGetText wentrycaptcha
        {-if null x
            then captchaMessage "Пожалуйста введите капчу"
            else removeCaptcha $ Answer x-}
        removeCaptcha $ Answer x

    on wbuttoncaptchacancel buttonActivated $ do
        removeCaptcha AbortCaptcha

    _ <- forM pastaradio $ \(p, w) -> do
            on w toggled $
                whenM (toggleButtonGetActive w) $ do
                    writeIORef pastaMod nullTime -- force update
                    writeIORef pastaSet p

    onFileChooserEntryButton True wbuttonimagefolder wentryimagefolder writeLog (return ())

    on wcheckhttpproxy buttonActivated $ do
        writeIORef httpproxyMod nullTime -- force update
        regenerateProxies

    onFileChooserEntryButton False wbuttonhttpproxyfile wentryhttpproxyfile writeLog $ do
        writeIORef httpproxyMod nullTime -- force update
        regenerateProxies

    on wchecksocksproxy buttonActivated $ do
        writeIORef socksproxyMod nullTime -- force update
        regenerateProxies

    onFileChooserEntryButton False wbuttonsocksproxyfile wentrysocksproxyfile writeLog $ do
        writeIORef socksproxyMod nullTime -- force update
        regenerateProxies

    on wchecknoproxy buttonActivated $ do
        regenerateProxies -- force update

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

    let writeConfig = do
            ncoActiveBoards <- map buBoard <$>
                            filterM (toggleButtonGetActive . buWidget) boardunits
            ncoPastaSet <- readIORef pastaSet
            ncoCreateThreads <- toggleButtonGetActive wcheckthread
            ncoAttachImages <- toggleButtonGetActive wcheckimages
            ncoWatermark <- toggleButtonGetActive wcheckwatermark
            ncoAnnoy <- toggleButtonGetActive wcheckannoy
            ncoAnnoyErrors <- toggleButtonGetActive wcheckannoyerrors
            ncoTray <- toggleButtonGetActive wchecktray
            ncoImageFolder <- entryGetText wentryimagefolder
            ncoUseHttpProxy <- toggleButtonGetActive wcheckhttpproxy
            ncoHttpProxyFile <- entryGetText wentryhttpproxyfile
            ncoUseSocksProxy <- toggleButtonGetActive wchecksocksproxy
            ncoSocksProxyFile <- entryGetText wentrysocksproxyfile
            ncoUseNoProxy <- toggleButtonGetActive wchecknoproxy
            ncoSettingsShown <- expanderGetExpanded wexpandersettings
            ncoAdditionalShown <- expanderGetExpanded wexpanderadditional
            ncoLogShown <- expanderGetExpanded wexpanderlog
        
            let nconf = Conf{coActiveBoards=ncoActiveBoards
                            ,coPastaSet=ncoPastaSet
                            ,coCreateThreads=ncoCreateThreads
                            ,coAttachImages=ncoAttachImages
                            ,coWatermark=ncoWatermark
                            ,coAnnoy=ncoAnnoy
                            ,coAnnoyErrors=ncoAnnoyErrors
                            ,coTray=ncoTray
                            ,coImageFolder=ncoImageFolder
                            ,coSettingsShown=ncoSettingsShown
                            ,coAdditionalShown=ncoAdditionalShown
                            ,coLogShown=ncoLogShown
                            ,coFirstLaunch=False
                            ,coUseHttpProxy=ncoUseHttpProxy
                            ,coHttpProxyFile=ncoHttpProxyFile
                            ,coUseSocksProxy=ncoUseSocksProxy
                            ,coSocksProxyFile=ncoSocksProxyFile
                            ,coUseNoProxy=ncoUseNoProxy
                            }
            
            tw <- try $ writeFile configfile $ show nconf
            case tw of
                Left (a::SomeException) -> writeLog $ "Couldn't write config to \"" ++ configfile ++ "\" , got exception: " ++ show a
                Right _ -> writeLog $ "Wrote config \"" ++ configfile ++"\": " ++ show nconf

    onDestroy window $ do
        writeConfig
        mainQuit

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
       
    -- close log
       
    rawPutLog =<< ("Finished wipe session, current POSIX time is " ++) . show <$> getPOSIXTime       
       
    when (isJust hlog) $ do
        hFlush $ fromJust hlog
        hClose $ fromJust hlog

#ifndef mingw32_HOST_OS
    whenM (hIsWritable stdout) $ do
        hFlush stdout
#endif
       
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
