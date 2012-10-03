module BlastItWithPiss
    (ShSettings(..)
    ,MuSettings(..)
    ,CaptchaType(..)
    ,CaptchaAnswer(..)
    ,OriginStamp(..)
    ,Message(..)
    ,OutMessage(..)
    ,LogSettings(..)
    ,LogDetail(..)
    ,ProxySettings(..)
    ,defMuS
    ,defLogS
    ,defPrS
    ,entryPoint
    ,sortSsachBoardsByPopularity
    ) where
import Import
import BlastItWithPiss.Blast
import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice
import BlastItWithPiss.MonadChoice
import BlastItWithPiss.MultipartFormData
import BlastItWithPiss.Post
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Text.Show as Show
import qualified Codec.Binary.UTF8.Generic as UTF8
import Text.HTML.TagSoup

{-
import Control.Concurrent (forkIO)
import GHC.Conc (threadStatus, ThreadStatus(..))
import System.Process
import System.Exit
import Network
import qualified Data.ByteString.Lazy as L
--}

data ShSettings = ShSettings {tpastas :: TVar [String]
                             ,timages :: TVar [FilePath]
                             ,tuseimages :: TVar Bool
                             ,tcreatethreads :: TVar Bool
                             ,tmakewatermark :: TVar Bool
                             }

data MuSettings = MuSettings {mthread :: TVar (Maybe (Maybe Int))
                             ,mmode :: TVar (Maybe Mode)
                             }

data CaptchaType = CaptchaPosting | CaptchaCloudflare

data CaptchaAnswer = Answer !String
                   | ReloadCaptcha
                   | AbortCaptcha
    deriving (Eq, Show, Ord)

data OriginStamp = OriginStamp {oTime :: !POSIXTime
                               ,oProxy :: !BlastProxy
                               ,oBoard :: !Board
                               ,oMode :: !Mode
                               ,oThread :: !(Maybe Int)
                               }

data Message = OutcomeMessage !Outcome
             | LogMessage !String
             | SupplyCaptcha {captchaType :: !CaptchaType
                             ,captchaBytes :: !LByteString
                             ,captchaSend :: !(CaptchaAnswer -> IO ())
                             }
             | NoPastas
             | NoImages

data OutMessage = OutMessage !OriginStamp !Message

-- memory hog (?)
-- TODO Either expose LogS to calling thread or replace it with stateT with mode and thread.
data LogSettings = LogS
                {gwakabapl :: TVar String
                ,gotherfields :: TVar [Field]
                ,gmode :: TVar Mode
                ,gthread :: TVar (Maybe Int)
                ,gimage :: TVar (Maybe Image)
                ,gpasta :: TVar String
                ,gchKey :: TVar String
                ,glthreadtime :: TVar POSIXTime
                ,glposttime :: TVar POSIXTime
                }

data LogDetail = Log
               | Don'tLog
    deriving (Eq, Show, Ord, Enum, Bounded)

data ProxySettings = ProxyS {psharedCookies :: TMVar CookieJar
                            ,pcloudflareCaptchaLock :: TMVar ()
                            }

data BlastLogData = BlastLogData
        {bldProxy :: BlastProxy
        ,bldBoard :: Board
        ,bldLogD :: LogDetail
        ,bldLogS :: LogSettings
        ,bldShS :: ShSettings
        ,bldMuS :: MuSettings
        ,bldPrS :: ProxySettings
        ,bldOut :: TQueue OutMessage -- ^ sendOut :: OutMessage -> IO () ?
        }

type BlastLog = ReaderT BlastLogData Blast

instance Show OriginStamp where
    show (OriginStamp time proxy board mode thread) =
        show time ++ " " ++ "{" ++ show proxy ++ "} " ++ renderBoard board ++
        " " ++ show mode ++ " [| " ++
        maybe (ssachBoard board) (ssachThread board) thread ++ " |]"

instance Show Message where
    show (OutcomeMessage o) = show o
    show (LogMessage o) = o
    show SupplyCaptcha{} = "SupplyCaptcha"
    show NoPastas = "NoPastas"
    show NoImages = "NoImages"

instance Show OutMessage where
    show (OutMessage s m) = show s ++ ": " ++ show m

instance NFData CaptchaType

instance NFData CaptchaAnswer where
    rnf (Answer s) = rnf s
    rnf ReloadCaptcha = ()
    rnf AbortCaptcha = ()

instance NFData OriginStamp where
    rnf (OriginStamp t p b m th) = rnf (t,p,b,m,th)

instance NFData Message where
    rnf (OutcomeMessage o) = rnf o
    rnf (LogMessage s) = rnf s
    rnf (SupplyCaptcha c b s) = rnf (c, b) `deepseq` s `seq` ()
    rnf NoPastas = ()
    rnf NoImages = ()

instance NFData OutMessage where
    rnf (OutMessage os m) = os `deepseq` m `deepseq` ()

flMaybeSTM :: MonadIO m => TVar (Maybe a) -> (a -> m b) -> m b -> m b
flMaybeSTM t d m = maybe m d =<< liftIO (readTVarIO t)

defMuS :: IO MuSettings
defMuS = atomically $ do
    mthread <- newTVar Nothing
    mmode <- newTVar Nothing
    return MuSettings{..}

defLogS :: IO LogSettings
defLogS = atomically $ do
    gwakabapl <- newTVar ""
    gotherfields <- newTVar []
    gmode <- newTVar CreateNew
    gthread <- newTVar Nothing
    gimage <- newTVar Nothing
    gpasta <- newTVar ""
    gchKey <- newTVar ""
    glthreadtime <- newTVar 0
    glposttime <- newTVar 0
    return LogS{..}

defPrS :: IO ProxySettings
defPrS = atomically $ do
    psharedCookies <- newEmptyTMVar
    pcloudflareCaptchaLock <- newTMVar ()
    return ProxyS{..}

askProxy :: BlastLog BlastProxy
askProxy = asks bldProxy

askBoard :: BlastLog Board
askBoard = asks bldBoard

askLogD :: BlastLog LogDetail
askLogD = asks bldLogD

askLogS :: BlastLog LogSettings
askLogS = asks bldLogS

askShS :: BlastLog ShSettings
askShS = asks bldShS

askBLSM :: BlastLog (Board, LogSettings, ShSettings, MuSettings)
askBLSM = asks $ \b -> (bldBoard b, bldLogS b, bldShS b, bldMuS b)

askProxyS :: BlastLog ProxySettings
askProxyS = asks bldPrS

askOut :: BlastLog (TQueue OutMessage)
askOut = asks bldOut

rec :: NFData a => TVar a -> a -> BlastLog a
rec t a = do
    d <- askLogD
    when (d == Log) $
        a `deepseq` liftIO (atomically $ writeTVar t a)
    return a 

recM :: NFData a => TVar a -> BlastLog a -> BlastLog a
recM t m = rec t =<< m

blastOut :: Message -> BlastLog ()
blastOut msg = do
    to <- askOut
    proxy <- askProxy
    board <- askBoard
    LogS{..} <- askLogS
    liftIO $ do
        (mode, thread) <- (,) <$> readTVarIO gmode <*> readTVarIO gthread
        now <- getPOSIXTime
        let a = OutMessage (OriginStamp now proxy board mode thread) msg
        atomically $ a `deepseq` writeTQueue to a

blastLog :: String -> BlastLog ()
blastLog msg = do
    d <- askLogD
    when (d == Log) $ do
        blastOut (LogMessage msg)

blast :: Blast a -> BlastLog a
blast = lift

blastImage :: Mode -> BlastLog (Maybe Image)
blastImage mode = do
    ShSettings{..} <- askShS
    use <- liftIO $ readTVarIO tuseimages
    if not use && mode /= CreateNew || mode == SagePopular
        then
            return Nothing
        else do
            images <- liftIO $ readTVarIO timages
            if null images
                then do
                    blastOut NoImages
                    blastLog "threw NoImages"
                    -- use recaptcha as a fallback
                    blast $ Just . Image "haruhi.jpg" "image/jpeg" <$>
                            (getCaptchaImage =<<
                                    getChallengeKey ssachRecaptchaKey)
                else blast $
                        Just <$> (readImageWithoutJunk =<< chooseFromList images)

blastPasta :: Maybe Image -> BlastLog String
blastPasta image = do
    ShSettings{..} <- askShS
    pastas <- liftIO $ readTVarIO tpastas
    mchooseFromList pastas

blastCaptcha :: String -> Maybe Int -> BlastLog (String, Maybe String)
blastCaptcha wakabapl thread = do
    chKey <- blast $ getChallengeKey ssachRecaptchaKey
    mbbytes <- blast $ ssachGetCaptcha wakabapl thread ssachRecaptchaKey chKey
    case mbbytes of
        Nothing -> return (chKey, Just "")
        Just bytes -> do
            m <- newEmptyMVar
            blastOut $ SupplyCaptcha CaptchaPosting bytes (putMVar m $!!)
            blastLog "blocking on captcha mvar"
            a <- takeMVar m
            blastLog "got captcha mvar"
            case a of
                Answer s -> return (chKey, Just s)
                ReloadCaptcha -> blastCaptcha wakabapl thread
                AbortCaptcha -> return (chKey, Nothing)

-- TODO It's probably buggy as hell.
blastCloudflare :: BlastLog [Tag String] -> String -> [Tag String] -> BlastLog [Tag String]
blastCloudflare what url tags
    | cloudflareBan tags = return []
    | cloudflareCaptcha tags = cloudflareChallenge
    | otherwise = return tags
  where cloudflareChallenge = do
            ProxyS{..} <- askProxyS
            (empt, work) <- liftIO $ atomically $ do
                empt <- isEmptyTMVar psharedCookies
                work <- isEmptyTMVar pcloudflareCaptchaLock
                when (empt && not work) $
                    takeTMVar pcloudflareCaptchaLock
                return (empt, work)
            if not empt || work
                then do
                    blastLog "Waiting for cloudflare cookies..."
                    void $ liftIO $ atomically $ readTMVar pcloudflareCaptchaLock
                    nothingyet <- liftIO $ atomically $ isEmptyTMVar psharedCookies
                    if nothingyet
                        then cloudflareChallenge
                        else do blastLog "Got cloudflare cookies"
                                blast $ setCookieJar =<< liftIO (atomically $ readTMVar psharedCookies)
                                what
                else handle (\(a::SomeException) -> do
                                liftIO $ atomically $ putTMVar pcloudflareCaptchaLock ()
                                throwIO a) $ do
                    blastLog "locked cloudflare captcha"
                    chKey <- blast $ getChallengeKey cloudflareRecaptchaKey
                    bytes <- blast $ getCaptchaImage chKey
                    m <- newEmptyMVar
                    blastOut $ SupplyCaptcha CaptchaCloudflare bytes (putMVar m $!!)
                    a <- takeMVar m
                    case a of
                        Answer s -> do
                            let rq = urlEncodedBody
                                    [("recaptcha_challenge_key", fromString chKey)
                                    ,("recaptcha_response_key", UTF8.fromString s)
                                    ,("message", "")
                                    ,("act", "captcha")
                                    ] $ (fromJust $ parseUrl url)
                                        {checkStatus = \_ _ -> Nothing
                                        ,redirectCount = 0}
                            void $ blast $ httpReq rq
                            ck <- blast $ getCookieJar
                            let ckl = length $ destroyCookieJar ck
                            if ckl==0
                                then do blastLog "Couldn't get Cloudflare cookies. Retrying."
                                        liftIO $ atomically $ putTMVar pcloudflareCaptchaLock ()
                                        blastCloudflare what url tags
                                else do blastLog $ "Cloudflare cookie count: " ++ show (length $ destroyCookieJar ck)
                                        liftIO $ atomically $ do
                                            putTMVar pcloudflareCaptchaLock ()
                                            putTMVar psharedCookies ck
                                        blastLog "finished working on captcha"
                                        what
                        ReloadCaptcha -> cloudflareChallenge
                        AbortCaptcha -> do
                            blastLog "Aborting cloudflare captcha. This might have unforeseen consequences."
                            liftIO $ atomically $ do
                                putTMVar pcloudflareCaptchaLock ()
                            return []

blastPost :: Bool -> POSIXTime -> POSIXTime -> (String, [Field]) -> Mode -> Maybe Int -> PostData -> BlastLog (POSIXTime, POSIXTime)
blastPost cap lthreadtime lposttime w@(wakabapl, otherfields) mode thread postdata = do
    (board, LogS{..}, ShSettings{..}, _) <- askBLSM
    (chKey, mcap) <- if cap || mode==CreateNew || not ssachAdaptivity
                        then do blastLog "querying captcha"
                                blastCaptcha wakabapl thread
                        else return ("", Just "")
    case mcap of
        Nothing -> return (lthreadtime, lposttime)
        Just captcha -> do
            void $ rec gchKey chKey
            -- TODO post reposts
            p <- blast $ prepare True board thread postdata chKey captcha wakabapl
                                 otherfields ssachLengthLimit
            beforeSleep <- liftIO getPOSIXTime
            let canPost = beforeSleep - lposttime >= ssachPostTimeout board
            when (mode /= CreateNew && not canPost) $ do
                let slptime = (lposttime + ssachPostTimeout board) - beforeSleep
                blastLog $ "sleeping " ++ show slptime ++ " seconds before post. FIXME using threadDelay for sleeping, instead of a more precise timer"
                liftIO $ threadDelay $ round $ slptime * 1000000
            blastLog "posting"
            -- FIXME beforePost <- liftIO $ getPOSIXTime
            (out, _) <- blast $ post p
            afterPost <- liftIO $ getPOSIXTime
            blastOut (OutcomeMessage out)
            when (successOutcome out) $ blastLog "post succeded"
            let (nthreadtime, nposttime) =
                    if mode == CreateNew
                        then (afterPost, lposttime)
                        else (lthreadtime, afterPost)
                ret = return (nthreadtime, nposttime)
            case out of
                Success -> ret
                SuccessLongPost rest ->
                    if mode /= CreateNew
                        then blastPost cap nthreadtime nposttime w mode thread
                                (PostData "" rest Nothing (sageMode mode) False)
                        else ret
                TooFastPost -> do
                        blastLog "TooFastPost, retrying in 0.5 seconds"
                        return (lthreadtime, afterPost - (ssachPostTimeout board - 0.5))
                TooFastThread -> do
                        blastLog "TooFastThread, retrying in 15 minutes"
                        return (afterPost - (ssachThreadTimeout board / 2), lposttime)
                o | o==NeedCaptcha || o==WrongCaptcha -> do
                        blastLog $ show o ++ ", requerying"
                        blastPost True lthreadtime lposttime w mode thread postdata
                  | otherwise -> do
                        blastLog "post failed"
                        ret

blastLoop :: (String, [Field]) -> POSIXTime -> POSIXTime -> BlastLog ()
blastLoop w lthreadtime lposttime = do
    let hands =
          [Handler $ \(a::HttpException) -> do
                blastLog $ "Got http exception, restarting. Exception was: " ++ show a
                blastLoop w lthreadtime lposttime -- Dunno what to do except restart.
          ,Handler $ \(a::AsyncException) -> throwIO a
          ,Handler $ \(a::SomeException) -> do
                blastLog $ "Terminated by exception " ++ show a
                throwIO a
          ]
    flip catches hands $ do
        (board, LogS{..}, ShSettings{..}, MuSettings{..}) <- askBLSM
        now <- liftIO $ getPOSIXTime
        canmakethread <- ifM (liftIO $ readTVarIO tcreatethreads)
                            (return $ now - lthreadtime >= ssachThreadTimeout board)
                            (return False)
        let getPage p = blast $ parsePage board <$> httpGetStrTags (ssachPage board p)
        p0 <- getPage 0
        --p0 <- return $ Page 0 0 90000 [Thread 19947 True False 9000 []]
        blastLog $ "page params" ++ show (pageId p0, lastpage p0, speed p0, length $ threads p0)
        mode <- recM gmode $
                    flMaybeSTM mmode return $ chooseMode board canmakethread p0
        (thread, p) <- flMaybeSTM mthread (\t -> return (t, p0)) $
                        chooseThread mode getPage p0
        void $ rec gthread thread
        blastLog $ "chose mode " ++ show mode
        rimage <- blastImage mode
        image <- recM gimage $
                    maybe (return Nothing) (\i -> Just <$> appendJunk i) rimage
        pasta <- recM gpasta $
                    blastPasta image
        watermark <- liftIO $ readTVarIO tmakewatermark
        (nthreadtime, nposttime) <-
            blastPost False lthreadtime lposttime w mode thread
                            (PostData "" pasta image (sageMode mode) watermark)
        void $ rec glthreadtime nthreadtime
        void $ rec glposttime nposttime
        blastLoop w nthreadtime nposttime

-- | Entry point should always be forked.
--
-- > thread <- forkIO (entryPoint print sh to Board ms)
--
-- You might want to resurrect thread if it gets killed.
--
-- > st <- threadStatus thread
-- > if st==ThreadDied || st==ThreadFinished
-- >    then resurrect
-- >    else continue
entryPoint :: Board -> BlastProxy -> LogDetail -> ShSettings -> MuSettings -> ProxySettings -> TQueue OutMessage -> Blast ()
entryPoint board proxy lgDetail shS muS prS output = do
    httpSetProxy proxy
    defl@LogS{..} <- liftIO defLogS
    flip runReaderT (BlastLogData proxy board lgDetail defl shS muS prS output) $ do
        let url = ssachPage board 0
        let chkStatus st@Status{statusCode=c} heads
                | c /= 200 && c /= 403 = Just $ toException $ StatusCodeException st heads Nothing
                | otherwise = Nothing
        x <- try $ do
            tgs <- blast $ withCheckStatus (Just chkStatus) $ httpGetStrTags url
            parseForm ssach <$> blastCloudflare (blast $ httpGetStrTags url) url tgs
        case x of
            Left (a::SomeException) -> do
                blastLog $ "Couldn't parse page form, got exception " ++ show a
            Right w@(wakabapl, otherfields) -> do
                void $ rec gwakabapl wakabapl
                void $ rec gotherfields otherfields
                blastLoop w 0 0

sortSsachBoardsByPopularity :: [Board] -> IO ([(Board, Int)], [Board])
sortSsachBoardsByPopularity boards = runBlast $ do
    maybeb <- forM boards $ \b -> do
                liftIO $ putStr $ "Processing " ++ renderBoard b ++ ". Speed: "
                spd <- parseSpeed <$> httpGetStrTags (ssachBoard b)
                liftIO $ putStrLn $ show spd
                return (b, spd)
    let (got, failed) = partition (isJust . snd) maybeb
        sorted = reverse $ sortBy (\(_,a) (_,b) -> compare (fromJust a) (fromJust b)) got
    return (map (appsnd fromJust) sorted, fst $ unzip $ failed)
