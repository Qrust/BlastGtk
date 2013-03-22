module Main where
import Import

import Paths_blast_it_with_piss

import BlastItWithPiss.Post
import BlastItWithPiss.Parsing
import BlastItWithPiss.Blast
import BlastItWithPiss.Board
import BlastItWithPiss.MonadChoice

import qualified Data.ByteString as B
import qualified Data.Text as T
-- import qualified Data.Text.IO as T (putStrLn)

import qualified Data.Map.Strict as M

import Control.Concurrent
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Conduit.TMQueue

import System.Environment (getArgs)

import Network (withSocketsDo)

import Data.Version (showVersion)

import System.Console.CmdArgs.Implicit hiding (def)

import Control.Monad.Trans.Reader

import qualified System.IO as S (putStrLn)

import System.Directory


{-# INLINE putStrLn #-}
putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . S.putStrLn





-- TODO antigate








data Config = Config
    {_socks              :: Bool
    ,_atatime            :: Int
    ,_timeout            :: Int
    ,_board              :: String
    ,_thread             :: Int
    ,_input              :: String
    ,_output             :: String
    ,_banned             :: String
    ,_four'o'four        :: String
    ,_four'o'three       :: String
    ,_five'o'three       :: String
    ,_cloudflare_captcha :: String
    ,_cloudflare_ban     :: String
    ,_dead               :: String
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
  deriving (Eq, Show, Ord, Enum, Bounded)

data Env = Env
    {manager            :: !Manager
    ,board              :: !Board
    ,atatime            :: !Int
    ,timeout            :: !Int
    ,thread             :: !Int
    ,catFiles           :: !(M.Map ProxyCat FilePath)
    }

impureAnnotatedCmdargsConfig :: Config
impureAnnotatedCmdargsConfig = Config
    {_socks =
        False
        &= explicit
        &= name "s"
        &= name "socks"
        &= help "Файл с проксями содержит Socks5 прокси? Если в имени файла есть \"socks\", то флаг включен автоматически."
    ,_atatime =
        30
        &= explicit
        &= name "e"
        &= name "atatime"
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
        &= help "Файл куда писать прокси с которыми не удалось связаться по каким-то причинам"
        &= typFile
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
    &= help "Формат файла прокси - по прокси на строку, обязательно указывать порт.\nФайлы banned и dead включают причины бана и эксепшоны http соответственно.\nЕсли файлы существуют, то запись будет производится в файлах помеченных номером, например если dead существует, то дохлота будет записываться в dead.1, если dead.1 существует, то в dead.2."

-- | Exceptions are handled inside 'post'
checkProxy :: BlastProxy -> ReaderT Env IO Outcome
checkProxy proxy = do
    Env{..} <- ask
    liftIO $ runBlastNew manager proxy $ do
        -- HACK Lock log / fast-logger
        liftIO $ putStrLn $ "Запущен тред для {" ++ show proxy ++ "}"

        setTimeout $ Just $ timeout * 1000000
        setMaxRetryCount 0 -- why retry?

        -- HACK Lock log / fast-logger
        liftIO $ putStrLn $ show proxy ++ ": Поcтим"
        outcome' <- do
            captchaid <- generateRandomString (32,32) ('A', 'Z')
            post =<< prepare board (Just thread)
                (PostData
                    "САЖА"
                    (">>" ++ show thread ++ "\nОП-хуй, сажаскрыл.")
                    Nothing True False False False)
                        (unsafeMakeYandexCaptchaAnswer captchaid "42146")
                (ssachLastRecordedFields board) ssachLengthLimit
        return $ fst outcome'

outcomeWrite :: BlastProxy -> Outcome -> ReaderT Env IO ProxyCat
outcomeWrite proxy outcome = do
    Env{..} <- ask
    case outcome of
      InternalError e ->
        case fromException $ unErrorException e of
          Just (StatusCodeException Status{statusCode=404} _ _) ->
            ape Ban404 $ show proxy
          _ -> ape Dead $ show proxy ++ "| failed, exception was: " ++ show e
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
      x -> do
        putStrLn $ "got " ++ show x ++ ", assuming that a proxy is good..."
        ape Good $ show proxy
  where
    ape :: ProxyCat -> Text -> ReaderT Env IO ProxyCat
    ape cat string = cat <$ do
        Env{..} <- ask
        case M.lookup cat catFiles of
          Nothing -> do
            putStrLn $ show cat ++ ": " ++ T.unpack string
          Just f -> do
            putStrLn $
                "Записываем " ++ show cat ++
                " в файл \"" ++ f ++ "\": " ++
                T.unpack string
            liftIO $ B.appendFile f $ encodeUtf8 $ string ++ "\n"

proxyReader :: MonadIO m => Bool -> Text -> m (Maybe BlastProxy)
proxyReader socks ip = do
    case readBlastProxy socks (T.unpack ip) of
      Nothing -> do
        putStrLn $ "Couldn't parse as a proxy " ++ show ip
        return Nothing
      p -> return p

mainloop :: [BlastProxy] -> ReaderT Env IO ()
mainloop proxies = do
    Env{..} <- ask

    let !maxCount = length proxies

    checkedMap <- liftIO $ newTVarIO M.empty

    proxyQueue <- liftIO $ newTBMQueueIO atatime

    _ <- liftIO $ forkIO $
             CL.sourceList proxies
        C.$$ sinkTBMQueue proxyQueue

    replicateM_ atatime $ fork $ fix $ \zaignoreel -> (do
        sourceTBMQueue proxyQueue
            C.$$ CL.mapM_ (\proxy -> do
                outcome <- checkProxy proxy
                cat <- outcomeWrite proxy outcome
                liftIO $ atomically $
                    modifyTVar' checkedMap $
                        M.insertWith (+) cat 1
                )
        ) `catch` \(e::SomeException) -> do
            putStrLn $
                "Worker got exception: " ++ show e
                ++ ". POHOOY IGNOREEM."
            zaignoreel

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
            ]

        catFiles <- do
            let (cats', fpaths') = unzip catFiles'
            M.fromList . zip cats' . applySuccNum fpaths' <$> filepathSuccNum fpaths'

        proxyStrings <-
             nub
           . filter (not . T.null)
           . T.lines
           . decodeUtf8
            <$> B.readFile _input
        let isSocks = _socks || "socks" `isInfixOf` map toLower _input
        proxies <- mapMaybeM (proxyReader isSocks) proxyStrings

        putStrLn $ show conf

        bracket (newManager def{managerConnCount=1000000}) closeManager $
            \manager -> runReaderT (mainloop proxies)
                Env {manager            = manager
                    ,board              = board
                    ,atatime            = _atatime
                    ,timeout            = _timeout
                    ,thread             = _thread
                    ,catFiles           = catFiles
                    }
