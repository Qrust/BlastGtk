module
#ifdef COMBINED_CHECK
    SmyvalkaMain
#else
    Main
#endif
    (main) where
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
import BlastItWithPiss.ProxyReader

import qualified Data.Text       as T
import qualified Data.ByteString.Char8 as B8

import qualified Data.Set as Set

import Text.Recognition.Antigate

import System.Console.CmdArgs.Implicit hiding (def)

import Data.Version

import Control.Concurrent
import Control.Concurrent.STM

import Network.Socket
import System.Directory
import System.Environment

import Control.Monad.Trans.Resource

-- NOTE cmdargs is deeply magical, won't let me use strict fields
data Config = Config
    {_socks                  :: Bool
    ,_board                  :: String
    ,_proxyFile              :: String
    ,_exclude                :: [String]
    ,_antigateKey            :: String
    ,_antigateHost           :: String
    ,_disableRetryCaptcha    :: Bool
    ,_disableRetryWordfilter :: Bool
    ,_disableRetryAlways     :: Bool
    ,_retryExceptions        :: Bool
    ,_imageDir               :: (Maybe FilePath)
    ,_pastaFile              :: (Maybe FilePath)
    ,_videoFile              :: (Maybe FilePath)
    ,_captchaFieldName       :: String
    ,_output                 :: FilePath
    ,_banned                 :: FilePath
    ,_dead                   :: FilePath
    ,_other                  :: FilePath
    ,_domain                 :: String
    }
  deriving (Show, Data, Typeable)

data Env = Env
    {manager                :: !Manager
    ,board                  :: !Board
    ,antigateKey            :: !ApiKey
    ,disableRetryCaptcha    :: !Bool
    ,disableRetryWordfilter :: !Bool
    ,disableRetryAlways     :: !Bool
    ,retryExceptions        :: !Bool
    ,imageDir               :: !(Maybe FilePath)
    ,pastas                 :: ![String]
    ,videos                 :: ![Text]
    ,captchaFieldName       :: !Text
    ,writeQueue             :: !(TQueue (ProxyCat, Text))
    }

data ProxyCat
    = Good
    | Ban
    | Dead
    | Other
  deriving (Eq, Show)

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
    ,_disableRetryWordfilter = False
        &= explicit
        &= name "dw"
        &= name "disable-retry-wordfilter"
        &= help "Отключить попытки снова решить капчу при вордфильтре или поврежденном изображении."
    ,_disableRetryAlways = False
        &= explicit
        &= name "d#"
        &= name "disable-retry-always"
        &= help "Отключить попытки пробовать снова без решения капчи при остальных ошибках кроме 503/mysql."
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
    ,_captchaFieldName = "captcha_value_id_06"
        &= explicit
        &= name "C"
        &= name "captcha-field-name"
        &= help "TEMPORARY KLUDGE: переименовать captcha_value_id_06 в аргумент"
        &= typ "СТРОКА"
    ,_output =
        "smyvalka-output"
        &= explicit
        &= name "o"
        &= name "output"
        &= help "Записывать прокси с которых удалось запостить в этот файл. Можно использовать вместе с -e чтобы не тратить капчу на использованные прокси: proxychecker 123456789 /b/ list.txt -S used.txt; proxychecker -e used.txt 123456789 /b/ list.txt. По дефолту \"smyvalka-output\""
        &= typFile
    ,_banned =
        "smyvalka-banned"
        &= explicit
        &= name "b"
        &= name "banned"
        &= help "Файл куда писать забаненные прокси, по дефолту \"smyvalka-banned\""
        &= typFile
    ,_dead =
        "smyvalka-dead"
        &= name "d"
        &= name "bad"
        &= help "Файл куда писать прокси с которыми не удалось связаться по каким-то причинам, по дефолту \"dead\""
        &= typFile
    ,_other =
        "smyvalka-other"
        &= name "O"
        &= name "other"
        &= help "Куда писать остальные прокси, например те у которых обнаружился действующий таймаут на постинг"
        &= typFile
    ,_domain = "2ch.hk"
        &= explicit
        &= name "domain"
        &= help "Домен ссача, например 2ch.tf"
    }
    &= program "smyvalka"
    &= helpArg [explicit, name "h", name "?", name "help", help "Показать вот эту хуйню"]
    &= versionArg [explicit, name "V", name "version", summary (showVersion version)]
    &= summary "Смывалка доски"
    &= help "Формат файла прокси - по прокси на строку, обязательно указывать порт.Символы | и # используются для комментариев.\nФайлы banned и dead включают причины бана и эксепшоны http соответственно.\nЕсли файлы существуют, то запись будет производится в файлах помеченных номером, например если smyvalka-dead существует, то дохлота будет записываться в smyvalka-dead.1, если smyvalka-dead.1 существует, то в smyvalka-dead.2. При этом номер для всех файлов синхронизируется, чтобы легче было опознать из от какого чека образовался список, то есть если smyvalka-dead.1 существует, то хорошие прокси будут записаны в smyvalka-output.2 даже если smyvalka-output.1 не существует. Такая схема может показаться неудобной, что собственно так и есть. Мы работаем над этим по мере мотивации, а пока cat и man bash в руки."

data SolvedCaptchaInfo
    = SolvedCaptchaInfo
        { sciAnswer    :: !(CAnswer Blast (ResourceT IO))
        , sciImage     :: !(Maybe Image)
        , sciReportBad :: !(IO ())
        }

newtype ProxyPoster = ProxyPoster {runProxyPoster :: IO ()}

createThread :: Env -> SolvedCaptchaInfo -> Blast ()
createThread
  e@Env
    {
--    manager
      board
--  , antigateKey
    , disableRetryCaptcha
    , disableRetryWordfilter
    , disableRetryAlways
    , retryExceptions
    , imageDir
    , pastas
    , videos
    , captchaFieldName
    , writeQueue }
  sci@SolvedCaptchaInfo
    { sciAnswer
    , sciImage
    , sciReportBad } = do
    proxy <- httpGetProxy
    (do
        pasta <- fromMaybe mempty <$> chooseFromListMaybe pastas

        video <- fromMaybe mempty <$> chooseFromListMaybe videos

        mimage <- do
            if T.null video
              then
                fmap Just . appendJunk =<<
                  case imageDir of
                    Nothing ->
                      fromMaybeM (liftIO builtinImageGen) sciImage
                    Just dir ->
                      liftIO $ fromMaybeM builtinImageGen =<< folderImageGen dir
              else
                return Nothing

        let otherfields = ssachLastRecordedFields board

        (!req, ~_) <-
            prepare
                board
                Nothing
                PostData
                    {subject = ""
                    ,text = pasta
                    ,image = mimage
                    ,video = video
                    ,sage = False
                    ,makewatermark = False
                    ,escapeInv = False
                    ,escapeWrd = False}
                sciAnswer
                otherfields
                ssachLengthLimit
                (map $ \x ->
                    if partName x == "captcha_value_id_06"
                      then x{partName = captchaFieldName}
                      else x)

        (\x -> fix x 0) $ \recurse !(times :: Int) -> do

            (!outcome, ~_) <- post (req, Success)

            putStrLn $
                "Finished {" ++ show proxy ++ "}, outcome: " ++ show outcome

            case outcome of
              o | successOutcome o -> do
                    ape Good $ show proxy
                | Banned _ <- o ->
                    ape Ban $ show proxy ++ "| Ban, reason was: " ++ show o
                | o == TooFastThread || o == TooFastPost -> do
                    -- can't post
                    ape Other $ show proxy ++ "| " ++ show o
                |    o == Wordfilter  || o == CorruptedImage
                  || o == LongPost    || o == EmptyPost
                  || o == SameMessage || o == SameImage
                  || o == NeedCaptcha || o == WrongCaptcha -> do
                    -- restart
                    let captchaProblems = o == WrongCaptcha || o == NeedCaptcha

                    when captchaProblems (liftIO sciReportBad)

                    unless (if captchaProblems
                          then disableRetryCaptcha
                          else disableRetryWordfilter) $ do
                        putStrLn $
                            "{" ++ show proxy ++ "} " ++ show o
                         ++ ", retrying captcha"
                        createThread e =<< antigate e
                | o == PostRejected || o == Five'o'ThreeError ->
                    recurse (times + 1)
                | otherwise ->
                    if disableRetryAlways || times >= 5
                      then
                        ape Dead $
                            show proxy ++ "| failed, exception was: " ++ show o
                      else
                        recurse (times + 1)
     ) `catch` \(ex::SomeException) -> do
        putStrLn $
            "{" ++ show proxy ++ "} Couldn't create thread, exception was: \""
         ++ show ex ++ "\"" ++ (if retryExceptions then ", retrying." else "")
        if retryExceptions
          then createThread e sci
          else return ()
  where
    ape c t = liftIO $ atomically $ writeTQueue writeQueue (c, t)

antigate
    :: Env
    -> Blast SolvedCaptchaInfo
antigate Env
  { board
  , antigateKey
  , manager } = do
    proxy <- httpGetProxy

    nc <- fix $ \recurse ->
        getNewCaptcha board Nothing ""
        `catch` \ex -> case ex of
          StatusCodeException st _ _
            | statusCode st >= 500 && statusCode st <= 599 -> do
                putStrLn $
                    "{" ++ show proxy ++ "} couldn't fetch captcha, retrying,"
                    ++ " exception was: " ++ show ex
                recurse
          _ -> throwIO ex
    case nc of
      Left adaptiveAnswer -> do
        let repBad = putStrLn $
              if cAdaptive adaptiveAnswer
                then "Adaptive captcha failed"
                else "Guessed captcha, but it was wrong."
        return $ SolvedCaptchaInfo adaptiveAnswer Nothing repBad
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
                    ++ show answerStr ++ " for proxy {" ++
                    show proxy ++ "}, report result: " ++ show x

        return $ SolvedCaptchaInfo answer (Just captchaImg) repBad

antigateThread
    :: (IO (), IO ())
    -> Env
    -> BlastProxy
    -> MVar ProxyPoster
    -> IO ()
antigateThread (success, fail) e proxy mvar = do
    ua <- newUserAgent

    x <- try (do
        sci <- runBlastNew (manager e) proxy ua $ antigate e

        return $ ProxyPoster $
            runBlastNew (manager e) proxy ua $ createThread e sci
        )
    case x of
      Left (ex::SomeException) -> do
        putMVar mvar $ ProxyPoster $
            putStrLn $ "No captcha for proxy {" ++ show proxy ++ "}, failing"

        putStrLn $
            "{" ++ show proxy ++ "} Failed to get captcha, exception was: "
         ++ show ex
        atomically $ writeTQueue (writeQueue e)
            ( Dead
            ,   show proxy ++ "| Failed to get captcha, exception was: "
             ++ show ex
            )

        fail
      Right a -> do
        putMVar mvar a

        success

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

collectCaptcha :: [BlastProxy] -> Env -> IO [MVar ProxyPoster]
collectCaptcha [] _ = do
    putStrLn "Пустой список проксей."
    return []
collectCaptcha proxies e = do
    readycount <- newIORef 0
    proxycount <- newIORef (length proxies)

    putStrLn $
        "Нажмите Enter чтобы запустить пушки когда достаточно капчи будет "
     ++ "готово. Пушки сами себя не запустят."
    _ <- forkIO $ displayCaptchaCounters readycount proxycount

    let success = atomicModifyIORef readycount $ \a -> (a+1, ())
        fail = atomicModifyIORef proxycount $ \a -> (a-1, ())

    resMVars <- forM proxies $ \p -> do
        mvar <- newEmptyMVar
        _ <- forkIO $ antigateThread (success, fail) e p mvar
        return mvar

    _ <- getLine
    putStrLn "BLAST IT WITH PISS"
    return resMVars

createThreadThread
    :: MVar ProxyPoster
    -> IO (MVar ())
createThreadThread posterMVar = do
    doneMVar <- newEmptyMVar
    _ <- forkIO $ (do
        poster <- takeMVar posterMVar
        runProxyPoster poster
        ) `finally` putMVar doneMVar ()
    return doneMVar

mainloop :: [BlastProxy] -> Env -> IO ()
mainloop proxies e = do
    resmvars <- collectCaptcha proxies e

    waitmvars <- mapM createThreadThread resmvars

    loop waitmvars (length waitmvars)
  where
    loop [] _ = putStrLn "Ну вот и всё, ребята."
    loop ws fsl = do
        threadDelay 100000

        nws <- filterM isEmptyMVar ws
        let nwl = length nws

        when (nwl /= length ws) $ do
            putStrLn $ "Завершено " ++ show (fsl-nwl) ++ " из " ++ show fsl

        loop nws fsl

applySuccNum :: [String] -> Int -> [String]
applySuccNum fpaths 0 = fpaths
applySuccNum fpaths i = map (\x -> x ++ "." ++ show i) fpaths

filepathSuccNum :: [String] -> IO Int
filepathSuccNum fpaths' = go fpaths' 0
  where
    go fpaths !scnum = do
        somethingThere <-
            flip anyM (applySuccNum fpaths scnum) $ \fpath ->
                (||) <$> doesFileExist fpath <*> doesDirectoryExist fpath
        if somethingThere
          then do
            (+ 1) <$> go fpaths (scnum + 1)
          else
            return 0

writerThread
    :: TQueue (ProxyCat, Text)
    -> FilePath -> FilePath -> FilePath -> FilePath
    -> IO ()
writerThread tq output banned dead other = forever $ do
    (cat, text) <- atomically $ readTQueue tq

    let f = case cat of
            Good  -> output
            Ban   -> banned
            Dead  -> dead
            Other -> other

    putStrLn $
        "Записываем " ++ show cat ++
        " в файл \"" ++ fromString f ++ "\": "
        ++ text
    withBinaryFile f AppendMode $ \h -> do
        B8.hPutStrLn h $ encodeUtf8 text

main :: IO ()
main = withSocketsDo $ do
    let md = cmdArgsMode impureAnnotatedCmdargsConfig
    ifM (null <$> getArgs)
        (putStrLn $ show md) $ do
        Config{..} <- cmdArgsRun md

        writeIORef domainVar _domain

        let !board =
              fromMaybe
                (error $ "Не смог прочитать \"" ++ _board ++
                  "\" как борду, возможно вы имели ввиду \"/" ++ _board ++
                    "/\"?")
                $ readBoard $ _board

        let isSocks = _socks || filenameIsSocks _proxyFile

        let readProxyStrings file = do
              b <- doesFileExist file
              if b
                then do
                  xs <- readProxyFile isSocks file
                  forMaybeM xs $ either
                    ((Nothing <$) . putStrLn . ("Couln't read as a proxy: " ++))
                    (return . Just)
                else do
                  putStrLn $ "Can't read proxy from file \""
                      ++ T.pack file ++ "\": File does not exist"
                  return []

        proxySet1 <- Set.fromList <$> readProxyStrings _proxyFile
        excludeProxies <- foldl' Set.union Set.empty
                        <$> mapM (fmap Set.fromList . readProxyStrings) _exclude

        let proxies = Set.toList $ proxySet1 `Set.difference` excludeProxies

        pastas <- fromMaybe [] <$> maybe (return Nothing) readPastaFile _pastaFile

        videos <- fromMaybe [] <$> maybe (return Nothing) readVideoFile _videoFile

        [output, banned, dead, other] <- do
            let fs = [_output, _banned, _dead, _other]
            applySuccNum fs <$> filepathSuccNum  fs

        writeQueue <- newTQueueIO

        _ <- forkIO $ writerThread writeQueue output banned dead other

        withManagerSettings def{managerConnCount=1000000} $
            \m -> liftIO $ mainloop proxies
                Env {manager = m
                    ,board = board
                    ,antigateKey = def{api_key=_antigateKey, api_host=_antigateHost}
                    ,disableRetryCaptcha = _disableRetryCaptcha
                    ,disableRetryWordfilter = _disableRetryWordfilter
                    ,disableRetryAlways = _disableRetryAlways
                    ,retryExceptions = _retryExceptions
                    ,imageDir = _imageDir
                    ,pastas = pastas
                    ,videos = videos
                    ,captchaFieldName = T.pack _captchaFieldName
                    ,writeQueue = writeQueue
                    }
