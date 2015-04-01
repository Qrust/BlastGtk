module Main where
import Import

import Paths_blast_it_with_piss

import BlastItWithPiss.Post
import BlastItWithPiss.Image
import BlastItWithPiss.Parsing
import BlastItWithPiss.Blast
import BlastItWithPiss.Board
import BlastItWithPiss.MonadChoice
import BlastItWithPiss.ProxyReader

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import qualified Data.Map.Strict as M

import Control.Concurrent
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue

import Control.Monad.Trans.Resource

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Conduit.TQueue

import qualified Data.Set as Set

import System.Environment (getArgs)
import qualified System.Exit
import System.Directory
import Network (withSocketsDo)

import Text.Recognition.Antigate

import Data.Version (showVersion)
import System.Console.CmdArgs.Implicit hiding (def)

import Control.Monad.Trans.Reader


data Config = Config
    {_socks                :: Bool
    ,_workerCount          :: Int
    ,_timeout              :: Int
    ,_board                :: String
    ,_thread               :: Int
    ,_input                :: String
    ,_exclude              :: [String]
    ,_output               :: String
    ,_banned               :: String
    ,_four'o'four          :: String
    ,_four'o'three         :: String
    ,_five'o'three         :: String
    ,_cloudflare_captcha   :: String
    ,_cloudflare_ban       :: String
    ,_dead                 :: String
    ,_otherkludge          :: String
    ,_antigateKey          :: Maybe String
    ,_antigateHost         :: String
    ,_disableRetryCaptcha  :: Bool
    ,_disableRetryRejected :: Bool
    }
  deriving (Show, Data, Typeable)

data ProxyCat
    = Good
    | CloudCaptcha
    | CloudBan
    | Ban404
    | Ban403
    | Ban503
    | Ban
    | Dead
    | OtherKludge -- ^ Temp kludge
  deriving (Eq, Show, Ord, Enum, Bounded)

data Env = Env
    {manager              :: !Manager
    ,board                :: !Board
    ,antigateKey          :: !(Maybe ApiKey)
    ,workerCount          :: !Int
    ,timeout              :: !Int
    ,thread               :: !Int
    ,catFiles             :: !(M.Map ProxyCat FilePath)
    ,disableRetryCaptcha  :: !Bool
    ,disableRetryRejected :: !Bool
    }

impureAnnotatedCmdargsConfig :: Config
impureAnnotatedCmdargsConfig = Config
    {_socks =
        False
        &= explicit
        &= name "s"
        &= name "socks"
        &= help "Файл с проксями содержит Socks5 прокси? Если в имени файла есть \"socks\", то флаг включен автоматически."
    ,_workerCount =
        30
        &= explicit
        &= name "w"
        &= name "workers"
        &= help "Сколько проксей тестить одновременно, по дефолту 30"
    ,_timeout =
        30
        &= explicit
        &= name "t"
        &= name "timeout"
        &= help "Таймаут для проксей в секундах. На шинде вроде не работает, но можно попробовать. По дефолту 30"
    ,_board =
        []
        &= argPos 0
        &= typ "/Доска/"
    ,_thread =
        0
        &= argPos 1
        &= typ "Номер_треда"
    ,_input =
        []
        &= argPos 2
        &= typ "Файл_с_проксями"
    ,_exclude =
        []
        &= explicit
        &= name "e"
        &= name "exclude"
        &= help "Не тестировать прокси из этого списка. Аргумент аккумулируется, пример: ./proxychecker -e badlist1.txt -e badlist2.txt -e badlist3.txt /b/ 123456789 goodlist.txt"
        &= typ "Файл_с_проксями..."
    ,_output =
        "output"
        &= explicit
        &= name "o"
        &= name "output"
        &= help "Файл куда писать отчеканные хорошие прокси, по дефолту \"output\""
        &= typFile
    ,_banned =
        "banned"
        &= explicit
        &= name "b"
        &= name "banned"
        &= help "Файл куда писать забаненные прокси, по дефолту \"banned\""
        &= typFile
    ,_four'o'four =
        "404"
        &= explicit
        &= name "4"
        &= name "404"
        &= help "Файл куда писать забаненные по 404 прокси, по дефолту \"404\""
        &= typFile
    ,_four'o'three =
        "403"
        &= explicit
        &= name "3"
        &= name "403"
        &= help "Файл куда писать забаненные по 403 прокси, по дефолту \"403\""
        &= typFile
    ,_five'o'three =
        "503"
        &= explicit
        &= name "5"
        &= name "503"
        &= help "Файл куда писать наебнувшиеся на 503 прокси, по дефолту \"503\""
        &= typFile
    ,_cloudflare_captcha =
        "cloudflare-captcha"
        &= explicit
        &= name "cc"
        &= name "cloudflare-captcha"
        &= help "Файл куда писать закапченные по клаудфлеру прокси, по дефолту \"cloudflare-captcha\""
        &= typFile
    ,_cloudflare_ban =
        "cloudflare-ban"
        &= explicit
        &= name "cb"
        &= name "cloudflare-ban"
        &= help "Файл куда писать забаненные по клаудфлеру прокси, по дефолту \"cloudflare-ban\""
        &= typFile
    ,_dead =
        "dead"
        &= name "d"
        &= name "bad"
        &= help "Файл куда писать прокси с которыми не удалось связаться по каким-то причинам, по дефолту \"dead\""
        &= typFile
    ,_otherkludge =
        "OTHERPROXIES"
        &= name "O"
        &= name "other"
        &= help "Куда писать все остальные прокси вместе с результатами. Временный костыль"
        &= typFile
    ,_antigateKey =
        Nothing
        &= explicit
        &= name "K"
        &= name "antigate-key"
        &= help "Если указан ключ антигейта, то будет проводиться проверка на забаненность мочерами. В противном случае будут проверяться только баны по клауде, 404 и т.д."
        &= typ "Ключ_антигейта"
    ,_antigateHost =
        "antigate.com"
        &= explicit
        &= name "H"
        &= name "antigate-host"
        &= help "Домен апи антигейта, например captchabot.com"
    ,_disableRetryCaptcha = False
        &= explicit
        &= name "dr"
        &= name "disable-retry-captcha"
        &= help "Отключить попытки снова решить капчу при неправильной капче."
    ,_disableRetryRejected = False
        &= explicit
        &= name "dR"
        &= name "disable-retry-rejected"
        &= help "Отключить перепроверку при перегруженности вакабы (PostRejected)."
    }
    &= program "proxychecker"
    &= helpArg
        [explicit
        ,name "h"
        ,name "?"
        ,name "help"
        ,help "Показать вот эту хуйню"]
    &= versionArg
        [explicit
        ,name "V"
        ,name "version"
        ,summary (showVersion version)]
    &= summary "Проксичекер для ссача"
    &= help "Формат файла прокси - по прокси на строку, обязательно указывать порт.Символы | и # используются для комментариев.\nФайлы banned и dead включают причины бана и эксепшоны http соответственно.\nЕсли файлы существуют, то запись будет производится в файлах помеченных номером, например если dead существует, то дохлота будет записываться в dead.1, если dead.1 существует, то в dead.2. При этом номер для всех файлов синхронизируется, чтобы легче было опознать из от какого чека образовался список, то есть если dead.1 существует, то хорошие прокси будут записаны в output.2 даже если output.1 не существует. Такая схема может показаться неудобной, что собственно так и есть. Мы работаем над этим по мере мотивации, а пока cat и man bash в руки."

-- | Exceptions are handled inside 'post'
checkProxy :: BlastProxy -> ReaderT Env IO Outcome
checkProxy proxy = do
    e@Env{..} <- ask
    ua <- newUserAgent
    liftIO $ runBlastNew manager proxy ua $ do
        -- HACK Lock log / fast-logger
        putStrLn $ "Запущен тред для {" ++ show proxy ++ "}"

        setTimeout $ Just $ millions timeout
        setMaxRetryCount 0 -- why retry?

        -- HACK Lock log / fast-logger
        outcome' <- do
            case antigateKey of
              Just k -> do
                putStrLn $ show proxy ++ ": Поcтим с проверкой капчи"
                kludgeAntigateCheckProxy proxy e k
              Nothing -> do
                putStrLn $ show proxy ++ ": Поcтим без капчи"
                captchaid <- generateRandomString (32,32) ('A', 'Z')
                (outcome, ~_) <- post =<< prepare board (Just thread)
                    PostData
                        {subject = "САЖА"
                        ,text = ">>" ++ show thread ++ "\nОП-хуй, сажаскрыл."
                        ,image = Nothing
                        ,video = ""
                        ,sage = True
                        ,makewatermark = False
                        ,escapeInv = False
                        ,escapeWrd = False}
                    (unsafeMakeYandexCaptchaAnswer captchaid "421463")
                    (ssachLastRecordedFields board)
                    ssachLengthLimit
                    id
                return outcome
        return outcome'

kludgeAntigateCheckProxy :: BlastProxy -> Env -> ApiKey -> Blast Outcome
kludgeAntigateCheckProxy proxy e@Env{..} key = do
    (cAnswer, repBad) <- do
        nc <- getNewCaptcha board (Just thread) ""
        case nc of
          Left answer -> do
            return (answer, putStrLn "Guessed captcha, turned out wrong.")
          Right (chKey :: CurrentSsachCaptchaType) -> do
            cconf <- getCaptchaConf chKey
            (captchaBytes, ct) <- getCaptchaImage chKey
            putStrLn $ show proxy ++ ": Got captcha"
            fname <- mkImageFileName ct
            (cid, answerStr) <-
                -- Always without proxy
                solveCaptcha def key cconf fname captchaBytes manager
            answer <- applyCaptcha chKey answerStr
            let
              repBad = do
                x <- runResourceT $ reportBad key cid manager
                putStrLn $
                    "Reported bad captcha " ++ show cid ++ ":"
                    ++ show answerStr ++ "for proxy {" ++
                    show proxy ++ "}, report result: " ++ show x
            return (answer, repBad)

    image <- do
        fname <- mkImageFileName "image/gif"
        return $ Image fname "image/gif" "ХУЙ ВЫГРЫЗИ УЁБИЩЕ, выпроваживает он нас."

    let otherfields = ssachLastRecordedFields board

    (!req, ~_) <-
        prepare board (Just thread)
            PostData
                {subject = "САЖА"
                ,text = "Фейспалмлю с ОПа-хуя, сажа."
                ,image = Just $ JunkImage image
                ,video = ""
                ,sage = True
                ,makewatermark = False
                ,escapeInv = False
                ,escapeWrd = False}
            cAnswer
            otherfields
            ssachLengthLimit
            id

    (!outcome, ~_) <- post (req, Success)

    putStrLn $ "Finished {" ++ show proxy ++ "}, outcome: " ++ show outcome

    case outcome of
      o | o==NeedCaptcha || o==WrongCaptcha -> do
        putStrLn $ show proxy ++ ": Reporting bad captcha"
        liftIO $ repBad
        if disableRetryCaptcha
          then return outcome
          else do
            putStrLn $ show proxy ++ ": Retrying captcha"
            kludgeAntigateCheckProxy proxy e key
      _ ->
        return outcome

outcomeMessage :: BlastProxy -> Outcome -> ReaderT Env IO (ProxyCat, Text) -> ReaderT Env IO (ProxyCat, Text)
outcomeMessage proxy outcome recurse = do
    Env{..} <- ask
    case outcome of
      InternalError e ->
        case fromException $ unErrorException e of
          Just (StatusCodeException Status{statusCode=404} _ _) ->
            ape Ban404 $ show proxy
          _ -> ape Dead $ show proxy ++ "| failed, exception was: " ++ show e
      PostRejected ->
        if disableRetryRejected
          then do
            ape Dead $ show proxy ++ "| PostRejected"
          else do
            putStrLn $ "{" ++ show proxy ++ "} PostRejected"
            recurse
      CloudflareBan -> do
        ape CloudBan $ show proxy
      CloudflareCaptcha -> do
        ape CloudCaptcha $ show proxy
      Four'o'FourBan -> do
        ape Ban404 $ show proxy
      Four'o'ThreeBan -> do
        ape Ban403 $ show proxy
      Banned reason -> do
        ape Ban $ show proxy ++ "| Ban, reason was: " ++ show reason
      UnknownError -> do
        ape Dead $ show proxy ++ "| Возможно эта прокси не рабочая, и лишь отдает пустые ответы"
      Five'o'ThreeError -> do
        ape Ban503 $ show proxy ++ "| 503. FIXME прокся наткнулась на лимит мочаки, почему-то это ещё не починено. Наверное я просто забыл об этом."
      ThreadDoesNotExist -> do
        putStrLn $ show proxy ++ "| НЕТ ТАКОГО ТРЕДА, АВАРИЙНАЯ ОСТАНОВКА. FIXME должен просто находить новый тред"
        liftIO $ System.Exit.exitWith (System.Exit.ExitFailure 404)
      x -> do
        if isJust antigateKey
          then
            if x == CorruptedImage
              then do
                putStrLn $ "SUCCESS! Got through captcha"
                ape Good $ show proxy
              else if x == WrongCaptcha || x == NeedCaptcha
                  then do
                    putStrLn $
                        show proxy ++ ": INCORRECT Captcha, retrying."
                    recurse
                  else do
                    putStrLn $
                        "Configured to check with captcha, but got " ++ show x
                        ++ ". Throwing to Other"
                    ape OtherKludge $ show proxy
          else do
            putStrLn $ "got " ++ show x ++ ", assuming that a proxy is good..."
            ape Good $ show proxy
  where
    ape :: ProxyCat -> Text -> ReaderT Env IO (ProxyCat, Text)
    ape !cat !string = return $! (cat, string)

writerThread :: M.Map ProxyCat FilePath -> TQueue (ProxyCat, Text) -> IO ()
writerThread catFiles tq = forever $ do
    (cat, text) <- atomically $ readTQueue tq

    case M.lookup cat catFiles of
      Nothing -> do
        putStrLn $
            "Нет файла для " ++ show cat ++
            ", хотел записать: " ++ text
      Just f -> do
        putStrLn $
            "Записываем " ++ show cat ++
            " в файл \"" ++ fromString f ++ "\": " ++ text
        withBinaryFile f AppendMode $ \h -> do
            B8.hPutStrLn h $ encodeUtf8 text

mainloop :: [BlastProxy] -> ReaderT Env IO ()
mainloop [] = do
    putStrLn "Пустой список проксей."
mainloop proxies = do
    Env{..} <- ask

    let !maxCount = length proxies

    checkedMap <- liftIO $ newTVarIO M.empty

    proxyQueue <- liftIO $ newTBMQueueIO workerCount

    writeQueue <- liftIO $ newTQueueIO

    _ <- liftIO $ forkIO $
        CL.sourceList proxies C.$$ sinkTBMQueue proxyQueue True

    _ <- liftIO $ forkIO $
        writerThread catFiles writeQueue

    replicateM_ workerCount $ fork $ do
        sourceTBMQueue proxyQueue
            C.$$ CL.mapM_ (\proxy -> do
                res@(cat, _) <-
                    fix (\recurse -> do
                        outcome <- checkProxy proxy
                        outcomeMessage proxy outcome recurse
                    ) `catch` (\(e::SomeException) -> do
                        putStrLn $ "While checking {" ++ show proxy ++ "}, "
                            ++ "worker got exception: " ++ show e
                            ++ "; Writing as dead, IGNOREEM."
                        return (Dead, show proxy ++ "| failed, exception was: " ++ show e)
                        )
                liftIO $ atomically $
                    writeTQueue writeQueue res
                liftIO $ atomically $
                    modifyTVar' checkedMap $
                        M.insertWith (+) cat 1
                )

    putStrLn "Started checking."

    0 & fix (\recurse current -> do
        (checked, _map) <- liftIO $ atomically $ do
            _map <- readTVar checkedMap
            let len = M.foldl' (+) 0 _map
            if len > current
              then return (len, _map)
              else retry
        if checked < maxCount
          then do
            let
              str = M.foldrWithKey'
                (\cat count -> (show cat ++ ": " ++ show count ++ ", " ++))
                ("Осталось: " ++ show (maxCount - checked))
                _map
            putStrLn str
            recurse checked
          else
            putStrLn "Ну вот и всё, ребята.")

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

main :: IO ()
main = withSocketsDo $ do
    let md = cmdArgsMode impureAnnotatedCmdargsConfig
    _args <- getArgs

    if null _args
      then
        putStrLn $ show md
      else do
        conf@Config{..} <- cmdArgsRun md

        let
          !board = fromMaybe
                (error $ "Не смог прочитать \"" ++ _board ++
                    "\" как борду, возможно вы имели ввиду \"/" ++
                        _board ++ "/\"?")
                $ readBoard _board
          catFiles' =
            [(Good        , _output)
            ,(CloudCaptcha, _cloudflare_captcha)
            ,(CloudBan    , _cloudflare_ban)
            ,(Ban404      , _four'o'four)
            ,(Ban403      , _four'o'three)
            ,(Ban503      , _five'o'three)
            ,(Ban         , _banned)
            ,(Dead        , _dead)
            ,(OtherKludge , _otherkludge)
            ]

        catFiles <- do
            let (cats', fpaths') = unzip catFiles'
            M.fromList . zip cats' . applySuccNum fpaths' <$> filepathSuccNum fpaths'

        forM_ (M.toAscList catFiles) $ \(cat, fpath) -> do
            putStrLn $ show cat ++ " -> " ++ fromString fpath

        let isSocks = _socks || filenameIsSocks _input

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

        proxySet1 <- Set.fromList <$> readProxyStrings _input
        excludeProxies <- foldl' Set.union Set.empty
                        <$> mapM (fmap Set.fromList . readProxyStrings) _exclude

        let proxies = Set.toList $ proxySet1 `Set.difference` excludeProxies

        putStrLn $ show conf

        bracket (newManager def{managerConnCount=1000000}) closeManager $
            \manager -> runReaderT (mainloop proxies)
                Env {manager              = manager
                    ,board                = board
                    ,antigateKey          = _antigateKey <&> \k ->
                                          def{api_key=k, api_host=_antigateHost}
                    ,workerCount          = _workerCount
                    ,timeout              = _timeout
                    ,thread               = _thread
                    ,catFiles             = catFiles
                    ,disableRetryCaptcha  = _disableRetryCaptcha
                    ,disableRetryRejected = _disableRetryRejected
                    }
