module Main where
import Import hiding (loop, fail, all)

import Paths_blast_it_with_piss

import BlastItWithPiss.Post
import BlastItWithPiss.PastaGen
import BlastItWithPiss.Video
import BlastItWithPiss.ImageGen
import BlastItWithPiss.Image
import BlastItWithPiss.Parsing
import BlastItWithPiss.Blast
import BlastItWithPiss.Board
import BlastItWithPiss.MonadChoice

import qualified Data.Text       as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import qualified Data.Text.IO.Locale as LTIO

import qualified Data.Set as Set

import Text.Recognition.Antigate

import System.Console.CmdArgs.Implicit hiding (def)

import Data.Version

import Control.Concurrent

import Network.Socket
import System.Environment

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource


{-# INLINE putStrLn #-}
putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . LTIO.putStrLn

-- NOTE cmdargs is deeply magical, won't let me use strict fields
data Config = Config
    {_socks               :: Bool
    ,_board               :: String
    ,_proxyFile           :: String
    ,_exclude             :: [String]
    ,_successFile         :: Maybe String
    ,_antigateKey         :: String
    ,_antigateHost        :: String
    ,_disableRetryCaptcha :: Bool
    ,_disableRetryAlways  :: Bool
    ,_retryExceptions     :: Bool
    ,_imageDir            :: (Maybe FilePath)
    ,_pastaFile           :: (Maybe FilePath)
    ,_videoFile           :: (Maybe FilePath)
    }
  deriving (Show, Data, Typeable)

data Env = Env
    {manager             :: !Manager
    ,board               :: !Board
    ,successFile         :: !(Maybe FilePath)
    ,antigateKey         :: !ApiKey
    ,disableRetryCaptcha :: !Bool
    ,disableRetryAlways  :: !Bool
    ,retryExceptions     :: !Bool
    ,imageDir            :: !(Maybe FilePath)
    ,pastas              :: ![String]
    ,videos              :: ![Text]
    }

type M = ReaderT Env IO

impureAnnotatedCmdargsConfig :: Config
impureAnnotatedCmdargsConfig = Config
    {_socks = False
        &= explicit
        &= name "s"
        &= name "socks"
        &= help "Файл с проксями содержит Socks5 прокси? Если в имени файла есть \"socks\", то флаг включен автоматически."
    ,_board = []
        &= argPos 1
        &= typ "/Доска/"
    ,_proxyFile = []
        &= argPos 2
        &= typ "Файл_с_проксями"
    ,_exclude = []
        &= explicit
        &= name "e"
        &= name "exclude"
        &= help "Не использовать прокси из этого списка. Аргумент аккумулируется, пример: ./proxychecker -e badlist1.txt -e badlist2.txt -e badlist3.txt 123456789 /b/ goodlist.txt"
        &= typ "Файл_с_проксями..."
    ,_successFile = Nothing
        &= explicit
        &= name "S"
        &= name "success"
        &= help "Записывать прокси с которых удалось запостить в этот файл. Можно использовать вместе с -e чтобы не тратить капчу на использованные прокси: proxychecker 123456789 /b/ list.txt -S used.txt; proxychecker -e used.txt 123456789 /b/ list.txt"
    ,_antigateKey = []
        &= argPos 0
        &= typ "Ключ_антигейта"
    ,_antigateHost = "antigate.com"
        &= explicit
        &= name "H"
        &= name "antigate-host"
        &= help "Домен апи антигейта, например captchabot.com"
    ,_disableRetryCaptcha = False
        &= explicit
        &= name "dr"
        &= name "disable-retry-captcha"
        &= help "Отключить попытки снова решить капчу при неправильной капче."
    ,_disableRetryAlways = False
        &= explicit
        &= name "d#"
        &= name "disable-retry-always"
        &= help "Отключить попытки пробовать снова при ошибках кроме 503/mysql."
    ,_retryExceptions = False
        &= explicit
        &= name "re"
        &= name "retry-exceptions"
        &= help "Пробовать запостить снова при исключениях."
    ,_imageDir = Nothing
        &= explicit
        &= name "i"
        &= name "image-dir"
        &= help "Папка с картинками"
        &= typ "ПАПКА"
    ,_pastaFile = Nothing
        &= explicit
        &= name "p"
        &= name "pasta-file"
        &= help "Файл с пастой"
        &= typ "ФАЙЛ"
    ,_videoFile = Nothing
        &= explicit
        &= name "v"
        &= name "video-file"
        &= help "Файл с ссылками на видео"
        &= typ "ФАЙЛ"
    }
    &= program "smyvalka"
    &= helpArg [explicit, name "h", name "?", name "help", help "Показать вот эту хуйню"]
    &= versionArg [explicit, name "V", name "version", summary (showVersion version)]
    &= summary "Смывалка доски"
    &= help "Формат файла прокси - по прокси на строку, обязательно указывать порт."

displayCaptchaCounters :: IORef Int -> IORef Int -> IO ()
displayCaptchaCounters ready all = go 0 0
  where
    go r a = do
        rd <- readIORef ready
        al <- readIORef all
        when (rd /= r || a /= al) $ do
            putStrLn $ "Готово капчи: " ++ show rd ++ "/" ++ show al
        threadDelay 100000
        go rd al

newtype ProxyPoster
    = ProxyPoster {runProxyPoster :: IO ()}

createThreadThread
    :: MVar ProxyPoster
    -> IO (MVar ())
createThreadThread posterMVar = do
    doneMVar <- newEmptyMVar
    _ <- forkIO $ do
        poster <- takeMVar posterMVar
        runProxyPoster poster
            `finally` putMVar doneMVar ()
    return doneMVar

createThread :: Env -> CAnswer Blast (ResourceT IO) -> Maybe Image -> IO () -> Blast ()
createThread e@Env{..} cAnswer captchaImage badCaptcha = do
    proxy <- httpGetProxy
    (do
        pasta <- fromMaybe "" <$> chooseFromListMaybe pastas

        video <- fromMaybe "" <$> chooseFromListMaybe videos

        mimage <- do
            if T.null video
              then
                fmap Just . appendJunk =<<
                    case imageDir of
                      Nothing ->
                        fromMaybeM (liftIO builtinImageGen) captchaImage
                      Just dir ->
                        liftIO $ fromMaybeM builtinImageGen =<< folderImageGen dir
              else
                return Nothing

        let otherfields = ssachLastRecordedFields board

        (!req, ~_) <- prepare board Nothing
                PostData
                    {subject = ""
                    ,text = pasta
                    ,image = mimage
                    ,video = video
                    ,sage = False
                    ,makewatermark = False
                    ,escapeInv = False
                    ,escapeWrd = False}
                  cAnswer otherfields ssachLengthLimit

        fix $ \recurse -> do
            (!outcome, ~_) <- post (req, Success)
            liftIO $ putStrLn $
                "Finished {" ++ show proxy ++ "}, outcome: " ++ show outcome
            case outcome of
              o | Banned _ <- o ->
                    return ()
                | o == PostRejected || o == Five'o'ThreeError ->
                    recurse
                | o == LongPost || o == EmptyPost
                  || o == SameMessage || o == SameImage
                  || o == Wordfilter || o == CorruptedImage ->
                    -- restart
                    createThread e cAnswer captchaImage badCaptcha
                | o == NeedCaptcha || o == WrongCaptcha ->
                    liftIO $ do
                      badCaptcha
                      unless disableRetryCaptcha $ do
                        putStrLn $ "Retrying captcha for {" ++ show proxy ++
                                    "}"
                        new <- antigate e proxy
                        runProxyPoster new
                | o == TooFastThread || o == TooFastPost -> do
                    putStrLn $ show proxy ++ ": " ++ show o ++ ", TODO move captcha to other proxy"
                    return ()
                | successOutcome o ->
                    case successFile of
                        Nothing ->
                            putStrLn $ show proxy ++ ": SUCCESS, but no success-file specified."
                        Just s -> do
                            putStrLn $ show proxy ++ ": SUCCESS, appending to \"" ++ fromString s ++ "\"."
                            liftIO $ withFile s AppendMode $ \h ->
                                B8.hPutStrLn h $ encodeUtf8 $ show proxy
                | otherwise ->
                    if disableRetryAlways
                      then return ()
                      else recurse
        ) `catch`
            \(ex::SomeException) -> do
                liftIO $ putStrLn $ "{" ++ show proxy ++
                        "} Couldn't create thread, exception was: \""
                        ++ show ex ++ "\"" ++
                          (if retryExceptions then ", retrying." else "")
                if retryExceptions
                  then createThread e cAnswer captchaImage badCaptcha
                  else return ()

antigate
    :: Env
    -> BlastProxy
    -> IO ProxyPoster
antigate e@Env{..} proxy = do
  ua <- newUserAgent
  runBlastNew manager proxy ua $ do
    nc <- getNewCaptcha board Nothing ""
    case nc of
      Left adaptiveAnswer -> do
        let repBad = putStrLn $
                if cAdaptive adaptiveAnswer
                    then "Adaptive captcha failed"
                    else "Guessed captcha, but it was wrong."
        st <- getBlastState
        return $ ProxyPoster $ runBlast manager st $
            createThread e adaptiveAnswer Nothing repBad
      Right (chKey :: CurrentSsachCaptchaType) -> do
        cconf <- getCaptchaConf chKey
        (captchaBytes, ct) <- getCaptchaImage chKey
        fname <- mkImageFileName ct
        (cid, answerStr) <-
            -- Always without proxy
            solveCaptcha def antigateKey cconf fname captchaBytes manager

        answer <- applyCaptcha chKey answerStr
        let !captchaImg = Image fname ct captchaBytes
        let repBad = do
                x <- runResourceT $ reportBad antigateKey cid manager
                putStrLn $
                    "Reported bad captcha " ++ show cid ++ ":"
                    ++ show answerStr ++ "for proxy {" ++
                    show proxy ++ "}, report result: " ++ show x
        st <- getBlastState

        return $ ProxyPoster $ runBlast manager st $
            createThread e answer (Just captchaImg) repBad

antigateThread
    :: (IO (), IO ())
    -> Env
    -> BlastProxy
    -> MVar ProxyPoster
    -> IO ()
antigateThread (success, fail) e proxy mvar = do
    x <- try $ antigate e proxy
    case x of
      Left (ex::SomeException) -> do
        putMVar mvar $ ProxyPoster (putStrLn $
            "No captcha for proxy {" ++ show proxy ++ "}, failing")
        putStrLn $
            "{" ++ show proxy ++ "} Failed to get captcha, exception was: "
                ++ show ex
        fail
      Right a -> do
        putMVar mvar a
        success

collectCaptcha :: [BlastProxy] -> M [MVar ProxyPoster]
collectCaptcha [] =
    [] <$ (liftIO $ putStrLn "Пустой список проксей.")
collectCaptcha proxies = do
    st@Env{..} <- ask
    liftIO $ do
        readycount <- newIORef 0
        proxycount <- newIORef (length proxies)

        putStrLn "Нажмите Enter чтобы запустить пушки когда достаточно капчи будет готово. Пушки сами себя не запустят."
        _ <- forkIO $ displayCaptchaCounters readycount proxycount

        let success = atomicModifyIORef readycount $ \a -> (a+1, ())
            fail = atomicModifyIORef proxycount $ \a -> (a-1, ())

        resMVars <- forM proxies $ \p -> do
            mvar <- newEmptyMVar
            _ <- forkIO $ antigateThread (success, fail) st p mvar
            return mvar

        _ <- getLine
        putStrLn "BLAST IT WITH PISS"
        return resMVars

mainloop :: [BlastProxy] -> M ()
mainloop proxies = do
    Env{..} <- ask

    resmvars <- collectCaptcha proxies

    waitmvars <- liftIO $ forM resmvars $
        \mvar -> createThreadThread mvar

    loop waitmvars (length waitmvars)
  where
    loop [] _ = liftIO $ putStrLn "Ну вот и всё, ребята."
    loop ws fsl = do
        liftIO $ threadDelay 100000
        nws <- liftIO $ filterM isEmptyMVar ws
        let nwl = length nws
        when (nwl /= length ws) $ do
            liftIO $ putStrLn $
                "Завершено " ++ show (fsl-nwl) ++ " из " ++ show fsl
        loop nws fsl

readProxyStrings file = do
             Set.fromList
           . filter (not . T.null)
           . T.lines
           . T.filter (/= '\r')
           . decodeUtf8
            <$> B.readFile file

main :: IO ()
main = withSocketsDo $ do
    let md = cmdArgsMode impureAnnotatedCmdargsConfig
    ifM (null <$> getArgs)
        (putStrLn $ show md) $ do
        Config{..} <- cmdArgsRun md
        let !board =
              fromMaybe
                (error $ "Не смог прочитать \"" ++ _board ++
                  "\" как борду, возможно вы имели ввиду \"/" ++ _board ++
                    "/\"?")
                $ readBoard $ _board

        _proxyStrings <- readProxyStrings _proxyFile
        excludeStrings <- foldl' Set.union Set.empty <$> mapM readProxyStrings _exclude
        let proxyStrings =
                Set.toList $
                    _proxyStrings `Set.difference` excludeStrings

        let isSocks = _socks || "socks" `isInfixOf` map toLower _proxyFile

        proxies <- forMaybeM proxyStrings $ \ip ->
            case readBlastProxy isSocks (T.unpack ip) of
              Nothing -> do
                LTIO.hPutStrLn stderr $ "Couldn't parse as a proxy " ++ show ip
                return Nothing
              p -> return p

        pastas <- fromMaybe [] <$> maybe (return Nothing) readPastaFile _pastaFile

        videos <- fromMaybe [] <$> maybe (return Nothing) readVideoFile _videoFile

        withManagerSettings def{managerConnCount=1000000} $
            \m -> liftIO $ runReaderT (mainloop proxies)
                Env {manager = m
                    ,board = board
                    ,successFile = _successFile
                    ,antigateKey = def{api_key=_antigateKey, api_host=_antigateHost}
                    ,disableRetryCaptcha = _disableRetryCaptcha
                    ,disableRetryAlways = _disableRetryAlways
                    ,retryExceptions = _retryExceptions
                    ,imageDir = _imageDir
                    ,pastas = pastas
                    ,videos = videos
                    }
