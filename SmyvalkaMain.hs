module Main where
import Import hiding (loop, fail, all)
import BlastItWithPiss.Post
import BlastItWithPiss.Image
import BlastItWithPiss.Parsing
import BlastItWithPiss.Blast
import BlastItWithPiss.Board
import BlastItWithPiss.MonadChoice
import Text.Recognition.Antigate
import System.Console.CmdArgs.Implicit hiding (def)
import Data.Version
import Control.Concurrent
import System.Environment
import Network.Socket
import qualified Data.Text as T
import qualified Data.ByteString as B
import Paths_blast_it_with_piss
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

data Config = Config
    {socks :: Bool
    ,strBoard :: String
    ,proxyFile :: String
    ,_antigateKey :: String
    ,_antigateHost :: String
    ,_retryCaptcha :: Bool
    }
    deriving (Show, Data, Typeable)

data State = State
    {
     manager :: Manager
    ,board :: Board
    ,antigateKey :: ApiKey
    ,proxies :: [BlastProxy]
    ,retryCaptcha :: Bool
    }

type M = ReaderT State IO

impureAnnotatedCmdargsConfig :: Config
impureAnnotatedCmdargsConfig =
    Config
        {socks = False &= help "Файл с проксями содержит Socks5 прокси, а не HTTP?"
        ,strBoard = [] &= argPos 1 &= typ "/Доска/"
        ,proxyFile = [] &= argPos 2 &= typ "Файл_с_проксями"
        ,_antigateKey = [] &= argPos 0 &= typ "Ключ_антигейта"
        ,_antigateHost = "antigate.com" &= explicit &= name "a" &= name "antigate_host" &= help "Домен апи антигейта, например captchabot.com"
        ,_retryCaptcha = False &= explicit &= name "r" &= name "retrycaptcha" &= help "Пробовать решить капчу снова при фейле?"
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

antigate :: Board -> ApiKey -> BlastProxy -> Blast (CAnswer IO (ResourceT IO), Image, BrowserState, IO ())
antigate board key p = httpGetProxy >>= \oldProxy -> httpWithProxy NoProxy $ do
    m <- getManager
    nc <- do
        nc <- getNewCaptcha board Nothing ""
        case nc of
            Left (CAnswer {cAdaptive=True}) -> do
                httpWithProxy oldProxy $ getNewCaptcha board Nothing ""
            x -> return x
    case nc of
        Left a -> do
            -- filler image from recaptcha
            rc <- recaptchaChallengeKey cloudflareRecaptchaKey
            (img, ct) <- getCaptchaImage $ Recaptcha rc
            st <- getBrowserState
            gen <- mkImageFileName ct
            return (a, Image gen ct img, st,
                putStrLn $ if cAdaptive a
                    then "Adaptive captcha failed"
                    else "Guessed captcha, but it was wrong: " ++ show  a)
        Right chKey -> do
            let _ = chKey `asTypeOf` currentSsachCaptchaType
            cconf <- getCaptchaConf chKey
            (captchaBytes, ct) <- getCaptchaImage chKey
            gen <- mkImageFileName ct
            (cid, str) <- solveCaptcha def key cconf gen captchaBytes m
            st <- getBrowserState
            a <- applyCaptcha chKey str
            return
                (a
                ,Image gen ct captchaBytes
                ,st
                ,do runResourceT $ void $ reportBad key cid m
                    putStrLn $ "Reported bad captcha for proxy {" ++ show p ++ "}")

antigateThread :: (IO (), IO ()) -> Board -> ApiKey -> BlastProxy -> Manager -> MVar (Maybe (CAnswer IO (ResourceT IO), Image, BrowserState, IO ())) -> IO ()
antigateThread (success, fail) board antigateKey p manager mvar = do
    x <- try $ runBlastNew manager $ do
            void $ httpWithProxy p $ httpGetLbs $ ssachBoard board -- try to make a request to filter out dead.
            antigate board antigateKey p
    case x of
      Right a -> do
        putMVar mvar $ Just a
        success
      Left (e::SomeException) -> do
        putMVar mvar $ Nothing
        putStrLn $ "{" ++ show p ++"} Failed to get captcha, exception was: " ++ show e
        fail

collectCaptcha :: M [(BlastProxy, MVar (Maybe (CAnswer IO (ResourceT IO), Image, BrowserState, IO ())))]
collectCaptcha = do
    State{..} <- ask
    liftIO $ do
        readycount <- newIORef 0
        proxycount <- newIORef (length proxies)

        putStrLn "Нажмите Enter чтобы запустить пушки когда достаточно капчи будет готово. Пушки сами себя не запустят."
        _ <- forkIO $ displayCaptchaCounters readycount proxycount

        let success = atomicModifyIORef readycount $ \a -> (a+1, ())
            fail = atomicModifyIORef proxycount $ \a -> (a-1, ())

        res <- forM proxies $ \p -> do
            mvar <- newEmptyMVar
            _ <- forkIO $ antigateThread (success, fail) board antigateKey p manager mvar
            return (p, mvar)

        _ <- getLine
        putStrLn "BLAST IT WITH PISS"
        return res

createThread :: Bool -> ApiKey -> Manager -> Board -> BlastProxy -> MVar (Maybe (CAnswer IO (ResourceT IO), Image, BrowserState, IO ())) -> IO (MVar ())
createThread retrycaptcha key manager board proxy mvar = do
    putStrLn $ "Creating thread {" ++ show proxy ++ "}"
    m <- newEmptyMVar
    void $ forkIO $ do
        _x <- takeMVar mvar
        case _x of
            Nothing -> do
                putMVar m ()
            Just _x -> go m _x
    return m
  where
    go m (cAnswer, image, st, badCaptcha) = do
        handle (\(e::SomeException) -> liftIO $ print e) $ do
            putStrLn $ "{" ++ show proxy ++ "} Started."
            txt <- generateSymbolString 300
            let otherfields = ssachLastRecordedFields board
            req <- prepare board Nothing
                    (PostData "" txt (Just image) False False False False)
                      cAnswer otherfields ssachLengthLimit
            fix $ \goto -> do
                outcome <- fmap fst $ runBlast manager st $ do
                    httpSetProxy proxy
                    post req
                putStrLn $ "Finished {" ++ show proxy ++ "}, outcome: " ++ show outcome
                case outcome of
                    PostRejected -> goto
                    Five'o'ThreeError -> goto
                    o | o==NeedCaptcha || o==WrongCaptcha -> do
                        badCaptcha
                        when retrycaptcha $ do
                            putStrLn $ "Retrying captcha for {" ++ show proxy ++ "}"
                            r <- runBlast manager st $ antigate board key proxy
                            go m r
                    _ -> return ()
        putMVar m ()

mainloop :: M ()
mainloop = do
    State{..} <- ask
    assoc <- collectCaptcha
    waitmvars <- liftIO $ forM assoc $
        \(p, m) -> createThread retryCaptcha antigateKey manager board p m
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

main :: IO ()
main = withSocketsDo $ do
    let md = cmdArgsMode impureAnnotatedCmdargsConfig
    ifM (null <$> getArgs)
        (print md) $ do
        Config{..} <- cmdArgsRun md
        let board =
              fromMaybe
                (error $ "Не смог прочитать \"" ++ strBoard ++
                  "\" как борду, возможно вы имели ввиду \"/" ++ strBoard ++
                    "/\"?")
                $ readBoard $ strBoard
        rawIps <-
            nub . filter (not . null) . lines . T.unpack . decodeUtf8 <$>
                B.readFile proxyFile
        let (errors, proxies) =
              partitionEithers $
                map (\x -> maybe (Left x) Right $ readBlastProxy socks x)
                  rawIps
        forM_ errors $ hPutStrLn stderr . ("Couldn't read \"" ++) . (++ "\" as a proxy")
        withManagerSettings def{managerConnCount=1000000} $
            \m -> liftIO $ runReaderT mainloop
                State {manager = m
                      ,board = board
                      ,antigateKey = ApiKey{api_key=_antigateKey, api_host=_antigateHost}
                      ,proxies = proxies
                      ,retryCaptcha = _retryCaptcha
                      }
