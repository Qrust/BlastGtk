module Main where
import Import hiding (loop, fail, all)

import Paths_blast_it_with_piss

import BlastItWithPiss.Post
import BlastItWithPiss.PastaGen
import BlastItWithPiss.ImageGen
import BlastItWithPiss.Image
import BlastItWithPiss.Parsing
import BlastItWithPiss.Blast
import BlastItWithPiss.Board
import BlastItWithPiss.MonadChoice

import qualified Data.Text       as T
import qualified Data.ByteString as B

import Text.Recognition.Antigate

import System.Console.CmdArgs.Implicit hiding (def)

import Data.Version

import Control.Concurrent

import Network.Socket
import System.Environment

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

-- NOTE cmdargs is deeply magical, won't let me use strict fields
data Config = Config
    {_socks        :: Bool
    ,_board        :: String
    ,_proxyFile    :: String
    ,_antigateKey  :: String
    ,_antigateHost :: String
    ,_retryCaptcha :: Bool
    ,_imageDir     :: (Maybe FilePath)
    ,_pastaFile    :: (Maybe FilePath)
    }
  deriving (Show, Data, Typeable)

data State = State
    {manager      :: !Manager
    ,board        :: !Board
    ,antigateKey  :: !ApiKey
    ,proxies      :: ![BlastProxy]
    ,retryCaptcha :: !Bool
    ,imageDir     :: !(Maybe FilePath)
    ,pastas       :: ![String]
    }

type M = ReaderT State IO

impureAnnotatedCmdargsConfig :: Config
impureAnnotatedCmdargsConfig =
    Config
        {_socks = False &= explicit &= name "s" &= name "socks" &= help "Файл с проксями содержит Socks5 прокси, а не HTTP?"
        ,_board = [] &= argPos 1 &= typ "/Доска/"
        ,_proxyFile = [] &= argPos 2 &= typ "Файл_с_проксями"
        ,_antigateKey = [] &= argPos 0 &= typ "Ключ_антигейта"
        ,_antigateHost = "antigate.com" &= explicit &= name "a" &= name "antigate-host" &= help "Домен апи антигейта, например captchabot.com"
        ,_retryCaptcha = False &= explicit &= name "r" &= name "retry-captcha" &= help "Пробовать решить капчу снова при фейле?"
        ,_imageDir = Nothing &= explicit &= name "i" &= name "image-dir" &= help "Папка с картинками" &= typ "ПАПКА"
        ,_pastaFile = Nothing &= explicit &= name "p" &= name "pasta-file" &= help "Файл с пастой" &= typ "ФАЙЛ"
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

data TempAntigateRes
    = TAR
        {__CAnswer :: !(CAnswer IO (ResourceT IO))
        ,__PostImage :: !Image
        ,__BrowserState :: !BrowserState
        ,__ReportBad :: !(IO ())
        }

antigate
    :: Board
    -> ApiKey
    -> BlastProxy
    -> Maybe FilePath
    -> Blast TempAntigateRes
antigate board key proxy maybeImageDir = do
    oldProxy <- httpGetProxy
    httpWithProxy NoProxy $ do
      m <- getManager
      nc <- do
        x <- getNewCaptcha board Nothing ""
        case x of
          Left (CAnswer {cAdaptive=True}) -> do
            httpWithProxy oldProxy $ getNewCaptcha board Nothing ""
          nc -> return nc
      case nc of
        Left a -> do
            img <- liftIO $ fromMaybeM builtinImageGen $
                    maybe (return Nothing) folderImageGen maybeImageDir
            st <- getBrowserState
            return TAR
                {__CAnswer = a
                ,__PostImage = img
                ,__BrowserState = st
                ,__ReportBad =
                    putStrLn $
                        if cAdaptive a
                          then "Adaptive captcha failed"
                          else "Guessed captcha, but it was wrong: " ++ show  a
                }
        Right chKey -> do
            let _ = chKey `asTypeOf` currentSsachCaptchaType
            cconf <- getCaptchaConf chKey
            (captchaBytes, ct) <- getCaptchaImage chKey
            gen <- mkImageFileName ct
            (cid, str) <- solveCaptcha def key cconf gen captchaBytes m
            st <- getBrowserState
            a <- applyCaptcha chKey str
            img <-
                case maybeImageDir of
                  Nothing ->
                    return $ Image gen ct captchaBytes
                  Just dir ->
                    liftIO $ fromMaybeM builtinImageGen $ folderImageGen dir
            return TAR
                {__CAnswer = a
                ,__PostImage = img
                ,__BrowserState = st
                ,__ReportBad = do
                    runResourceT $ void $ reportBad key cid m
                    putStrLn $
                        "Reported bad captcha for proxy {" ++ show proxy ++ "}"
                }

antigateThread
    :: (IO (), IO ())
    -> State
    -> BlastProxy
    -> MVar (Maybe TempAntigateRes)
    -> IO ()
antigateThread (success, fail) State{..} proxy mvar = do
    x <- try $ runBlastNew manager $ do
        void $ httpWithProxy proxy $ httpGetLbs $ ssachBoard board -- try to make a request to filter out dead.
        antigate board antigateKey proxy imageDir
    case x of
      Right a -> do
        putMVar mvar $ Just a
        success
      Left (e::SomeException) -> do
        putMVar mvar $ Nothing
        putStrLn $
            "{" ++ show proxy ++ "} Failed to get captcha, exception was: "
                ++ show e
        fail

collectCaptcha :: M [(BlastProxy, MVar (Maybe TempAntigateRes))]
collectCaptcha = do
    st@State{..} <- ask
    liftIO $ do
        readycount <- newIORef 0
        proxycount <- newIORef (length proxies)

        putStrLn "Нажмите Enter чтобы запустить пушки когда достаточно капчи будет готово. Пушки сами себя не запустят."
        _ <- forkIO $ displayCaptchaCounters readycount proxycount

        let success = atomicModifyIORef readycount $ \a -> (a+1, ())
            fail = atomicModifyIORef proxycount $ \a -> (a-1, ())

        res <- forM proxies $ \p -> do
            mvar <- newEmptyMVar
            _ <- forkIO $ antigateThread (success, fail) st p mvar
            return (p, mvar)

        _ <- getLine
        putStrLn "BLAST IT WITH PISS"
        return res

createThread
    :: Bool
    -> State
    -> BlastProxy
    -> MVar (Maybe TempAntigateRes)
    -> IO (MVar ())
createThread retrycaptcha State{..} proxy mvar = do
    m <- newEmptyMVar
    void $ forkIO $ do
        _x <- takeMVar mvar
        case _x of
          Nothing -> do
            putStrLn $ "Thread creation failed for {" ++ show proxy ++ "}"
            putMVar m ()
          Just _x -> do
            putStrLn $ "Creating thread {" ++ show proxy ++ "}"
            go m _x
    return m
  where
    go m TAR{__CAnswer = cAnswer
            ,__PostImage = image
            ,__BrowserState = st
            ,__ReportBad = badCaptcha} = (do
        handle (\(e::SomeException) -> print e) $ do
            pasta <-
                if null pastas
                  then generateSymbolString 300
                  else chooseFromList pastas
            let otherfields = ssachLastRecordedFields board

            (!req, ~_) <- prepare board Nothing
                    (PostData "" pasta (Just image) False False False False)
                      cAnswer otherfields ssachLengthLimit

            fix $ \recurse -> do
                (!outcome, ~_) <- runBlast manager st $ do
                    httpSetProxy proxy
                    post (req, Success)
                putStrLn $ "Finished {" ++ show proxy ++ "}, outcome: " ++ show outcome
                case outcome of
                    PostRejected -> recurse
                    Five'o'ThreeError -> recurse
                    o | o==NeedCaptcha || o==WrongCaptcha -> do
                        badCaptcha
                        when retrycaptcha $ do
                            putStrLn $ "Retrying captcha for {" ++ show proxy ++ "}"
                            r <- runBlast manager st $
                                antigate board antigateKey proxy imageDir
                            go m r
                    _ -> return ()
        ) `finally` putMVar m ()

mainloop :: M ()
mainloop = do
    st@State{..} <- ask
    assoc <- collectCaptcha
    waitmvars <- liftIO $ forM assoc $
        \(proxy, mvar) -> createThread retryCaptcha st proxy mvar
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
                (error $ "Не смог прочитать \"" ++ _board ++
                  "\" как борду, возможно вы имели ввиду \"/" ++ _board ++
                    "/\"?")
                $ readBoard $ _board
        rawIps <-
            nub . filter (not . null) . lines . T.unpack . decodeUtf8 <$>
                B.readFile _proxyFile
        let (errors, proxies) =
              partitionEithers $
                map (\x -> maybe (Left x) Right $ readBlastProxy _socks x)
                  rawIps
        forM_ errors $ hPutStrLn stderr . ("Couldn't read \"" ++) . (++ "\" as a proxy")
        pastas <- fromMaybe [] <$> maybe (return Nothing) readPastaFile _pastaFile
        withManagerSettings def{managerConnCount=1000000} $
            \m -> liftIO $ runReaderT mainloop
                State {manager = m
                      ,board = board
                      ,antigateKey = def{api_key=_antigateKey, api_host=_antigateHost}
                      ,proxies = proxies
                      ,retryCaptcha = _retryCaptcha
                      ,imageDir = _imageDir
                      ,pastas = pastas
                      }
